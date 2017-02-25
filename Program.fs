open Assimp
open System

type t = 
    { normals : byte list
      vertices : byte list
      tangents : byte list
      uv0 : byte list
      indices : byte list }

let rec meshParent (meshIndex : int) (root : Node) = 
    match (root.MeshIndices |> Seq.contains meshIndex) with
    | true -> (Some root)
    | false -> 
        root.Children
        |> Seq.toList
        |> List.map (meshParent meshIndex)
        |> List.filter (function 
               | None -> false
               | _ -> true)
        |> (function 
        | [] -> None
        | fst :: rest -> fst)

let flatten = List.fold (@) []

let toByte (values : float32 list) = 
    values
    |> List.map (BitConverter.GetBytes >> Array.toList)
    |> flatten

let toBytes f vectors = 
    vectors
    |> List.map (f >> toByte)
    |> flatten

let convertMesh (mesh : Mesh, meshName) = 
    let indices = mesh.GetUnsignedIndices() |> Seq.toList
    let normals = mesh.Normals |> Seq.toList
    let vertices = mesh.Vertices |> Seq.toList
    let tangents = mesh.Tangents |> Seq.toList
    normals |> List.iter (fun n -> n.Normalize())
    let uvCoords = 
        mesh.TextureCoordinateChannels
        |> Seq.item 0
        |> Seq.toList
    
    let r = 
        { normals = normals |> toBytes (fun n -> [ n.X; n.Y; n.Z ])
          vertices = vertices |> toBytes (fun n -> [ n.X; n.Y; n.Z ])
          tangents = tangents |> toBytes (fun n -> [ n.X; n.Y; n.Z; 1.0f ])
          uv0 = uvCoords |> toBytes (fun n -> [ n.X; n.Y ])
          indices = 
              indices
              |> List.map (BitConverter.GetBytes >> Array.toList)
              |> flatten }
    
    (meshName, r)

let meshGen (r : t) = 
    let d = sprintf "%d"
    let oNormals = 0
    let oVertices = oNormals + r.normals.Length
    let oTangents = oVertices + r.vertices.Length
    let oUv0 = oTangents + r.tangents.Length
    let oIndices = oUv0 + r.uv0.Length
    [| @"function data() return {"
       @"  animations = {"
       @"  },"
       @"  matConfigs = { { 0, }, },"
       @"  subMeshes = { {"
       @"      indices = {"
       @"          normal =   { count = " + d r.indices.Length + ", offset = " + d oIndices + ", },"
       @"          position = { count = " + d r.indices.Length + ", offset = " + d oIndices + ", },"
       @"          tangent =  { count = " + d r.indices.Length + ", offset = " + d oIndices + ", },"
       @"          uv0 =      { count = " + d r.indices.Length + ", offset = " + d oIndices + ", },"
       @"      },"
       @"      materials = { },"
       @"  }, },"
       @"  vertexAttr ="
       @"   {"
       @"      normal =       { count = " + d r.normals.Length + ", numComp = 3, offset = " + d oNormals + ", },"
       @"      position =     { count = " + d r.vertices.Length + ", numComp = 3, offset = " + d oVertices + ", },"
       @"      tangent =      { count = " + d r.tangents.Length + ", numComp = 4, offset = " + d oTangents + ", },"
       @"      uv0 =          { count = " + d r.uv0.Length + ", numComp = 2, offset = " + d oUv0 + ", },"
       @"  },"
       @"} end" |]
    |> String.concat Environment.NewLine

let exportMesh dir (meshName, r) = 
    let mshPath = dir + "/" + meshName + ".msh"
    let blobPath = mshPath + ".blob"
    printfn "Generating mesh binary:  %s" blobPath
    IO.File.WriteAllBytes(blobPath, r.normals @ r.vertices @ r.tangents @ r.uv0 @ r.indices |> List.toArray)
    printfn "Generating mesh:         %s" mshPath
    IO.File.WriteAllText(mshPath, meshGen r)
    ()

let convert (inputFile : string) = 
    let dir = IO.Path.GetDirectoryName(inputFile)
    let assimpImporter = new AssimpContext()
    let scene = 
        assimpImporter.ImportFile
            (inputFile, 
             PostProcessSteps.CalculateTangentSpace ||| PostProcessSteps.Triangulate 
             ||| PostProcessSteps.GenerateNormals)
    printfn "%s contains %d mesh(es). %s" inputFile scene.MeshCount |> ignore
    scene.Meshes
    |> Seq.mapi (fun i m -> 
           (m, 
            match meshParent i scene.RootNode with
            | None -> sprintf "%d" i
            | Some p -> p.Name))
    |> Seq.map convertMesh
    |> Seq.iter (exportMesh dir)
    assimpImporter.Dispose()

[<EntryPoint>]
let main argv = 
    match argv with
    | [| filename |] -> 
        (match (IO.File.Exists filename) with
         | false -> printfn "%s doesn't exists!" filename
         | true -> convert filename)
    | _ -> printfn "Usage: tpfmc [filename]"
    printfn "Press any key to close..."
    Console.ReadKey() |> ignore
    0 // return an integer exit code
