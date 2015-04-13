namespace BaseTypeProvider
open Microsoft.FSharp.Core.CompilerServices
open System
open System.Collections.Generic
open System.Reflection
open System.IO
open System.Xml.Linq
open Microsoft.FSharp.Quotations
open ProviderImplementation.ProvidedTypes

module Helper =
    let findFsprojects dir =
        Directory.GetFiles dir
        |> Array.filter(fun fi -> fi.EndsWith ".fsproj")
        |> Array.toList

    let findSln () =
        let searched = ref 0
        let notFound = ref true
        let directory = ref <| FileInfo(Assembly.GetExecutingAssembly().Location).Directory.FullName
        while !notFound && !searched < 9 do
            Directory.GetFiles !directory
            |> Seq.exists (fun i -> i.EndsWith ".sln")
            |> function
               | true ->
                    notFound := false
               | false ->
                    directory := DirectoryInfo(!directory).Parent.FullName
                    searched := !searched + 1
        !directory

    let projectScan param = 
        let rec directories current = seq {
            yield current
            for next in Directory.GetDirectories current do
                yield! directories next }
        directories param
        |> Seq.toList
        |> List.map findFsprojects
        |> List.collect id
        |> function
            | [] -> failwith "could not find any F# projects in the specifed directories"
            | xs -> xs

    let parseReferences (fullProjectPath:string) =
        let msbuild = XNamespace.op_Implicit "http://schemas.microsoft.com/developer/msbuild/2003"
        let projDefinition = XDocument.Load(fullProjectPath);
        projDefinition
            .Element(msbuild + "Project")
            .Elements(msbuild + "ItemGroup")
            .Elements(msbuild + "Reference")
            .Attributes(XName.op_Implicit "Include")
        |> Seq.map (fun i -> i.Value)
        |> Seq.map (fun i -> i.Split(',').[0])
        |> Seq.distinct

    let getAssembly name =
        AppDomain.CurrentDomain.GetAssemblies()
        |> Seq.tryFind (fun i -> i.FullName.StartsWith name)
        |> function
           | Some assem -> assem
           | None ->
                let localAssemFile = FileInfo(Assembly.GetExecutingAssembly().Location).Directory.FullName
                if File.Exists localAssemFile then
                    Assembly.LoadFile(Path.Combine(localAssemFile, sprintf "%s.dll" name))
                else
                    let frameworkReferenceLocation, fsharpCoreLocation =
                        if Type.GetType("Mono.Runtime") <> null then
                            let frameworkReferenceLocation = System.Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory()
                            let fsharpCoreLocation = Path.Combine(frameworkReferenceLocation, "FSharp.Core.dll")
                            frameworkReferenceLocation, fsharpCoreLocation
                        else
                            let pf = System.Environment.ExpandEnvironmentVariables("%programfiles%")
                            let frameworkReferenceLocation = Path.Combine(pf, @"Reference Assemblies\Microsoft\Framework\.NETFramework\v4.5.1")
                            let fsharpCoreLocation = Path.Combine(pf, @"Reference Assemblies\Microsoft\FSharp\.NETFramework\v4.0\4.3.1.0\FSharp.Core.dll")
                            frameworkReferenceLocation, fsharpCoreLocation
                    if name = "FSharp.Core" then
                        Assembly.LoadFile(fsharpCoreLocation)
                    else if File.Exists(Path.Combine(frameworkReferenceLocation, "Facades", sprintf "%s.dll" name)) then
                        Assembly.Load(Path.Combine(frameworkReferenceLocation, "Facades", sprintf "%s.dll" name))
                    else
                        Assembly.Load(Path.Combine(name))

[<TypeProvider>]
type BaseTypeProvider (config : TypeProviderConfig) as this =
    inherit TypeProviderForNamespaces ()

    let ns = "Simon.Blog.Provider"
    let asm = Assembly.GetExecutingAssembly()

    let getTypes ignoreFiles =
        Helper.findSln ()
        |> Helper.projectScan
        |> Seq.collect Helper.parseReferences
        |> Seq.filter (fun i -> Seq.exists (fun j -> i.StartsWith j) ignoreFiles |> not)
        |> Seq.map Helper.getAssembly
        |> Seq.where (fun i -> not i.IsDynamic)
        |> Seq.collect (fun i -> i.GetTypes())
        |> Seq.filter (fun i -> not <| (i.IsGenericType || i.ContainsGenericParameters))

    let toParameterInfo (param:ParameterInfo) =
        if param.HasDefaultValue then
            ProvidedParameter(param.Name, param.ParameterType, param.IsOut, param.DefaultValue)
        else
            ProvidedParameter(param.Name, param.ParameterType, param.IsOut)
        
    let addStaticMethods (t:Type) (rootType:ProvidedTypeDefinition) =
        let allmethods =
            t.GetMethods(BindingFlags.Static ||| BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.InvokeMethod)
            |> Seq.filter (fun i -> not <| i.IsGenericMethod || i.IsAbstract || i.IsGenericMethodDefinition || i.ContainsGenericParameters)
            |> Seq.map (fun i ->
                let typeString = t.FullName
                let token = i.MetadataToken
                ProvidedMethod(i.Name, i.GetParameters() |> (Seq.map toParameterInfo >> List.ofSeq), i.ReturnType,
                    IsStaticMethod = true,
                    InvokeCode = fun args ->
                        <@@
                            let t = Type.GetType typeString
                            let m = t.GetMethods() |> Seq.find (fun i -> i.MetadataToken = token)
                            m.Invoke(null, %%Expr.NewArray(typeof<obj>, args |> List.map (fun i -> Expr.Coerce(i, typeof<obj>))))
                        @@>))
        rootType.AddMembersDelayed(fun () -> allmethods |> Seq.toList)
        rootType

    let addInstanceMethods (t:Type) (rootType:ProvidedTypeDefinition) =
        let allmethods =
            t.GetMethods(BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.InvokeMethod)
            |> Seq.filter (fun i -> not <| i.IsGenericMethod || i.IsAbstract || i.IsGenericMethodDefinition || i.ContainsGenericParameters)
            |> Seq.map (fun i ->
                let typeString = t.FullName
                let token = i.MetadataToken
                ProvidedMethod(i.Name, ProvidedParameter("this", i.DeclaringType) :: (i.GetParameters() |> (Seq.map toParameterInfo >> List.ofSeq)), i.ReturnType,
                    IsStaticMethod = true,
                    InvokeCode = fun args ->
                        <@@
                            let t = Type.GetType typeString
                            let m = t.GetMethods() |> Seq.find (fun i -> i.MetadataToken = token)
                            m.Invoke(%%args.[0], %%Expr.NewArray(typeof<obj>, args |> List.tail |> List.map (fun i -> Expr.Coerce(i, typeof<obj>))))
                        @@>))
        rootType.AddMembersDelayed(fun () -> allmethods |> Seq.toList)
        rootType

    let addConstructors (t:Type) (rootType:ProvidedTypeDefinition) =
        let allmethods =
            t.GetMethods(BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.CreateInstance)
            |> Seq.filter (fun i -> not <| i.IsGenericMethod || i.IsAbstract || i.IsGenericMethodDefinition || i.ContainsGenericParameters)
            |> Seq.map (fun i ->
                let typeString = t.FullName
                let token = i.MetadataToken
                let returnType = 
                    if rootType.GetMembers() |> Seq.exists (fun i -> i.Name = t.Name) |> not then
                        let returnType = ProvidedTypeDefinition(t.Name, Some i.ReturnType)
                        rootType.AddMember(returnType)
                        returnType :> MemberInfo
                    else
                        rootType.GetMember(t.Name).[0]
                ProvidedMethod(t.Name, i.GetParameters() |> (Seq.map toParameterInfo >> List.ofSeq), i.ReturnType,
                    IsStaticMethod = true,
                    InvokeCode = fun args ->
                        <@@
                            let t = Type.GetType typeString
                            let m = t.GetMethods() |> Seq.find (fun i -> i.MetadataToken = token)
                            m.Invoke(null, %%Expr.NewArray(typeof<obj>, args |> List.map (fun i -> Expr.Coerce(i, typeof<obj>))))
                        @@>))
        rootType.AddMembersDelayed(fun () -> allmethods |> Seq.toList)
        rootType

    let addAllMethods rootType (t:Type) =
        rootType
        |> addInstanceMethods t
        //|> addStaticMethods t
        //|> addConstructors t

    let createTypes () =
        let rootType = ProvidedTypeDefinition(asm, ns, "Base", Some typeof<obj>)

        ["mscorlib";"System.Numerics";"System.Core"]
        |> getTypes
        |> Seq.iter (addAllMethods rootType >> ignore)

        [rootType]
 
    do
        this.AddNamespace(ns, createTypes())
 
[<assembly:TypeProviderAssembly>]
do ()