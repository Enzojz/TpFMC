open Assimp
open System

type Lua = 
    | V of int
    | S of string
    | P of (string * Lua)
    | A of Lua list
    | F of (string * Lua)
    | C of (bool * Lua)

let rec printLua indent (node : Lua) = 
    let ind str = 
        function 
        | true -> (Environment.NewLine + (String.replicate indent "  ") + str)
        | false -> str
    match node with
    | V num -> sprintf "%d" num
    | S str -> @"""" + str + @""""
    | P(name, x) -> name + " = " + (printLua (indent + 1) x)
    | A ls -> 
        "{ " + (ls
                |> List.map (printLua (indent + 1))
                |> String.concat ", ")
        |> fun str -> str + (ind " }" (str.Contains(Environment.NewLine)))
    | F(f, x) -> (sprintf "function %s() return " f) + (printLua (indent + 1) x) + " end"
    | C(c, x) -> 
        if c then (printLua (indent + 1) x)
        else ""
    |> fun rs -> ind rs (rs.Length > 30)


type ByteData = 
    { normals : byte list;
      vertices : byte list;
      tangents : byte list;
      uv0 : byte list;
      uv1 : byte list;
      nbFace : int }

type NumData = 
    { normals : int;
      vertices : int;
      tangents : int;
      uv0 : int;
      uv1 : int }

let rec namedNodes (node : Node) = 
    match (node.Name.Trim().Length > 0, node.HasMeshes, node.HasChildren) with
    | (true, true, _) -> [ node ]
    | (false, _, false) | (true, false, false) -> []
    | (false, _, true) | (true, false, true) -> 
        node.Children
        |> Seq.collect namedNodes
        |> Seq.toList

let flatten = List.fold (@) []

let toBytes f vectors = 
    let toByte (values : float32 list) = 
        values
        |> List.map (BitConverter.GetBytes >> Array.toList)
        |> flatten
    vectors
    |> List.map (f >> toByte)
    |> flatten

let revT = Matrix4x4(1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, -1.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f)

let convertMesh ((mesh, transform) : Mesh * Matrix4x4) = 
    let normals = mesh.Normals |> Seq.toList
    let vertices = mesh.Vertices |> Seq.toList
    let tangents = mesh.Tangents |> Seq.toList
    let uv0 = (Seq.item 0 mesh.TextureCoordinateChannels) |> Seq.toList
    let uv1 = (Seq.item 1 mesh.TextureCoordinateChannels) |> Seq.toList
    normals |> List.iter (fun n -> n.Normalize())
    { normals = normals |> toBytes (fun n -> [ n.X; n.Y; n.Z ]);
      vertices = vertices |> toBytes (fun n -> [ n.X; n.Y; n.Z ]);
      tangents = tangents |> toBytes (fun n -> [ n.X; n.Y; n.Z; 1.0f ]);
      uv0 = uv0 |> toBytes (fun n -> [ n.X; n.Y ]);
      uv1 = uv1 |> toBytes (fun n -> [ n.X; n.Y ]);
      nbFace = mesh.FaceCount }

let meshGen (meshes : (Material * ByteData) list) = 
    let d = sprintf "%d"
    
    let counts = 
        { normals = meshes |> List.sumBy (fun (_, m) -> m.normals.Length);
          vertices = meshes |> List.sumBy (fun (_, m) -> m.vertices.Length);
          tangents = meshes |> List.sumBy (fun (_, r) -> r.tangents.Length);
          uv0 = meshes |> List.sumBy (fun (_, r) -> r.uv0.Length);
          uv1 = meshes |> List.sumBy (fun (_, r) -> r.uv1.Length) }
    
    let offsets = 
        { vertices = 0;
          uv0 = counts.vertices;
          normals = counts.vertices + counts.uv0;
          tangents = counts.vertices + counts.uv0 + counts.normals;
          uv1 = counts.vertices + counts.uv0 + counts.normals + counts.tangents }
    
    let indiceAttr count offset = 
        A [ P("count", V count);
            P("offset", V offset) ]
    
    let rec subMeshIndices (meshes : (Material * ByteData) list) offset result = 
        match meshes with
        | [] -> A result
        | (m, r) :: rest -> 
            let indiceLength = r.nbFace * 3 * sizeof<int>
            result @ [ A [ P("indices", 
                             A [ P("normal", indiceAttr indiceLength offset);
                                 P("position", indiceAttr indiceLength offset);
                                 P("tangent", indiceAttr indiceLength offset);
                                 P("uv0", indiceAttr indiceLength offset);
                                 C(r.uv1.Length > 0, P("uv1", indiceAttr indiceLength offset)) ]);
                           P("materials", A [ S(m.Name + @".mtl") ]) ] ]
            |> subMeshIndices rest (offset + indiceLength)
    
    let vertexAttr count numComp offset = A(P("count", V count) :: P("numComp", V numComp) :: [ P("offset", V offset) ])
    F("data", 
      A [ P("animations", A []);
          P("matConfigs", A [ A (List.init meshes.Length (fun _ -> V 0)) ]);
          P("subMeshes", subMeshIndices meshes (offsets.uv1 + counts.uv1) []);
          P("vertexAttr", 
            A [ P("normal", vertexAttr counts.normals 3 offsets.normals);
                P("position", vertexAttr counts.vertices 3 offsets.vertices);
                P("tangent", vertexAttr counts.tangents 4 offsets.tangents);
                P("uv0", vertexAttr counts.uv0 2 offsets.uv0);
                C (counts.uv1 > 0, P("uv1", vertexAttr counts.uv1 2 offsets.uv1)) ]) ])
    |> printLua 0
    |> fun str -> str.Trim()

let blobGen mesh = 
    let indices = 
        [ 0..(mesh.nbFace * 3 - 1) ]
        |> List.map (BitConverter.GetBytes >> Array.toList)
        |> flatten
    mesh.vertices @ mesh.uv0 @ mesh.normals @ mesh.tangents @ mesh.uv1 @ indices  |> List.toArray


let prepareMesh (meshes : (Material * ByteData list) list) = 
    let mergeMeshes (meshes : ByteData list) = 
        let merge f = List.collect f meshes
        { normals = merge (fun r -> r.normals);
          vertices = merge (fun r -> r.vertices);
          tangents = merge (fun r -> r.tangents);
          uv0 = merge (fun r -> r.uv0);
          uv1 = merge (fun r -> r.uv1);
          nbFace = meshes |> List.sumBy (fun r -> r.nbFace) }
    (meshes
     |> List.map (fun (_, meshes) -> mergeMeshes meshes)
     |> mergeMeshes
     |> blobGen, 
     meshes
     |> List.map (fun (material, meshes) -> (material, mergeMeshes meshes))
     |> meshGen)

let exportMesh dir (meshName, blob, mesh) = 
    let mshPath = dir + "/" + meshName + ".msh"
    let blobPath = mshPath + ".blob"
    IO.File.WriteAllBytes(blobPath, blob)
    IO.File.WriteAllText(mshPath, mesh)
    (meshName + ".msh.blob", meshName + ".msh")

let transform (node : Node) =
    let rec work (node : Node) (tf : Matrix4x4) = 
        match node with
        | null -> tf
        | _ -> work node.Parent node.Transform * tf
    work node Matrix4x4.Identity


let convertNode (scene : Scene) (node : Node) = 
    let rec allMeshes (node : Node) = 
        (node.MeshIndices
         |> Seq.map (fun i -> (scene.Meshes.[i], transform node))
         |> Seq.toList)
        @ (node.Children
           |> Seq.collect allMeshes
           |> Seq.toList)
    node
    |> allMeshes
    |> List.groupBy (fun ((m, t) : (Mesh * Matrix4x4)) -> scene.Materials.[m.MaterialIndex])
    |> List.map (fun (material, meshes) -> (material, List.map convertMesh meshes))
    |> prepareMesh
    |> fun (blob, mesh) -> (node.Name, blob, mesh)

let convert (inputFile : string) = 
    let dir = IO.Path.GetDirectoryName(IO.Path.GetFullPath(inputFile))
    let assimpImporter = new AssimpContext()
    let scene = assimpImporter.ImportFile(inputFile, PostProcessSteps.CalculateTangentSpace ||| PostProcessSteps.Triangulate ||| PostProcessSteps.GenerateNormals)
    let nodes = namedNodes scene.RootNode
    printfn ""
    printfn "======== %s ========" inputFile
    printfn "%d mesh(es) found." nodes.Length
    printfn "Output path: %s" dir
    printfn "Converting..."
    nodes
    |> List.map (fun n -> 
           async { 
               return n
                      |> (convertNode scene)
                      |> (exportMesh dir)
           })
    |> Async.Parallel
    |> Async.RunSynchronously
    |> Array.iter (fun (b, m) -> 
           printfn "%-15s >>>>> Blob: %s" (IO.Path.GetFileName(inputFile)) b
           printfn "%-15s >>>>> Mesh: %s" (IO.Path.GetFileName(inputFile)) m)
    |> ignore
    assimpImporter.Dispose()
    printfn "======== Finished ========"


[<EntryPoint>]
let main argv = 
    let mutable start = true
    
    let files = 
        match argv with
        | [||] -> 
            printfn "Usage: tpfmc [filename]"
            printfn "or drag/drop the file in the console window."
            [ Console.ReadLine() ]
        | _ -> argv |> Array.toList
    while start do
        files
        |> List.map (fun filename -> 
               async { 
                   match (IO.File.Exists filename) with
                   | false -> printfn "%s doesn't exists!" filename
                   | true -> convert filename
               })
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore
        printfn @"Press ""R"" to repeat, any other key to close..."
        start <- Console.ReadKey().Key = ConsoleKey.R
    0 // return an integer exit code

