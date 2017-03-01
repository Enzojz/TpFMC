open Assimp
open System

type Lua = 
    | Val of int
    | Str of string
    | Ent of (string * Lua)
    | Arr of Lua list

let rec printLua indent (node: Lua) =
    match node with
    | Val num -> sprintf "%d" num
    | Str str -> str
    | Ent (name, x) -> name + " = " + (printLua indent x)
    | Arr ls -> "{" + (ls |> List.map (printLua indent) |> String.concat ", ") + "}" + Environment.NewLine

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

let revT = 
    Matrix4x4(
        1.0f, 0.0f, 0.0f, 0.0f, 
        0.0f, 0.0f, -1.0f, 0.0f, 
        0.0f, 1.0f, 0.0f, 0.0f, 
        0.0f, 0.0f, 0.0f, 1.0f)

let convertMesh ((mesh, transform) : (Mesh * Matrix4x4)) = 
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
    
    let rec subMeshIndices (meshes : (Material * ByteData) list) offset result = 
        match meshes with
        | [] -> result
        | (m, r) :: rest -> 
            let indiceLength = r.nbFace * 3 * sizeof<int>
            result @ [ @"       {";
                       @"           indices = {";
                       @"               normal =   { count = " + d indiceLength + ", offset = " + d offset + ", },";
                       @"               position = { count = " + d indiceLength + ", offset = " + d offset + ", },";
                       @"               tangent =  { count = " + d indiceLength + ", offset = " + d offset + ", },";
                       @"               uv0 =      { count = " + d indiceLength + ", offset = " + d offset + ", }," ]
                     @ if r.uv1.Length = 0 then []
                       else [ @"               uv1 =      { count = " + d indiceLength + ", offset = " + d offset + ", }," ]
                       @ [ @"           },";
                           @"           materials = { """ + m.Name + @".mtl"" },";
                           @"       }," ]
            |> subMeshIndices rest (offset + indiceLength)
    
    ([ @"function data() return {";
       @"   animations = {";
       @"   },";
       @"   matConfigs = { { 0, }, },";
       @"   vertexAttr =";
       @"   {";
       @"      normal =       { count = " + d counts.normals + ", numComp = 3, offset = " + d offsets.normals + ", },";
       @"      position =     { count = " + d counts.vertices + ", numComp = 3, offset = " + d offsets.vertices + ", },";
       @"      tangent =      { count = " + d counts.tangents + ", numComp = 4, offset = " + d offsets.tangents + ", },";
       @"      uv0 =          { count = " + d counts.uv0 + ", numComp = 2, offset = " + d offsets.uv0 + ", },";
       @"      uv1 =          { count = " + d counts.uv1 + ", numComp = 2, offset = " + d offsets.uv1 + ", },";
       @"   },";
       @"   subMeshes = { " ]
     @ (subMeshIndices meshes (offsets.uv1 + counts.uv1) []) @ [ @"   },"; @"} end" ])
    |> String.concat Environment.NewLine

let blobGen mesh = 
    let indices = 
        [ 0..(mesh.nbFace * 3 - 1) ]
        |> List.map (BitConverter.GetBytes >> Array.toList)
        |> flatten
    mesh.vertices @ mesh.uv0 @ mesh.normals @ mesh.tangents @ mesh.uv1 @ indices@ indices@ indices@ indices @ indices |> List.toArray

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
