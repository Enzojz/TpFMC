// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open Assimp
open System

type t = 
    { normals : int
      vertices : int
      tangents : int
      uv0 : int
      indices : int }

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
    
    let bNor = normals |> List.map (fun n -> toByte [ n.X; n.Y; n.Z ])
    let bVer = vertices |> List.map (fun n -> toByte [ n.X; n.Y; n.Z ])
    let bTan = tangents |> List.map (fun n -> toByte [ n.X; n.Y; n.Z; 1.0f ])
    let bUv0 = uvCoords |> List.map (fun n -> toByte [ n.X; n.Y ])
    let bIndices = indices |> List.map (BitConverter.GetBytes >> Array.toList)
    
    let byte = 
        [ bNor; bVer; bTan; bUv0; bIndices ]
        |> List.map flatten
        |> flatten
        |> List.toArray
    
    let length = 
        { normals = List.length normals * 3 * 4
          vertices = List.length vertices * 3 * 4
          tangents = List.length tangents * 4 * 4
          uv0 = List.length uvCoords * 2 * 4
          indices = List.length indices * 4 }
    
    let offset = 
        { normals = 0
          vertices = length.normals
          tangents = length.normals + length.vertices
          uv0 = length.normals + length.vertices + length.tangents
          indices = length.normals + length.vertices + length.tangents + length.uv0 }
    
    (meshName, byte, length, offset)

let meshGen length offset = 
    let d = sprintf "%d"
    [| @"function data() return {"
       @"  animations = {"
       @"  },"
       @"  matConfigs = { { 0, }, },"
       @"  subMeshes = { {"
       @"      indices = {"
       @"          normal =   { count = " + d length.indices + ", offset = " + d offset.indices + ", },"
       @"          position = { count = " + d length.indices + ", offset = " + d offset.indices + ", },"
       @"          tangent =  { count = " + d length.indices + ", offset = " + d offset.indices + ", },"
       @"          uv0 =      { count = " + d length.indices + ", offset = " + d offset.indices + ", },"
       @"      },"
       @"      materials = { },"
       @"  }, },"
       @"  vertexAttr ="
       @"   {"
       @"      normal =       { count = " + d length.normals + ", numComp = 3, offset = " + d offset.normals + ", },"
       @"      position =     { count = " + d length.vertices + ", numComp = 3, offset = " + d offset.vertices + ", },"
       @"      tangent =      { count = " + d length.tangents + ", numComp = 4, offset = " + d offset.tangents + ", },"
       @"      uv0 =          { count = " + d length.uv0 + ", numComp = 2, offset = " + d offset.uv0 + ", },"
       @"  },"
       @"} end" |]
    |> String.concat Environment.NewLine

let exportMesh (meshName, byte, length, offset) = 
    let mshPath = meshName + ".msh"
    let blobPath = mshPath + ".blob"
    printfn "Generating mesh binary:  %s" blobPath
    IO.File.WriteAllBytes(blobPath, byte)
    printfn "Generating mesh:         %s" mshPath
    IO.File.WriteAllText(mshPath, meshGen length offset)

()

let convert inputFile = 
    let assimpImporter = new AssimpContext()
    let scene = 
        assimpImporter.ImportFile
            (inputFile, 
             PostProcessSteps.CalculateTangentSpace ||| PostProcessSteps.Triangulate 
             ||| PostProcessSteps.GenerateNormals)
    printfn "%s contains %d mesh(es). %s" inputFile scene.MeshCount |> ignore
    scene.Meshes
    |> Seq.mapi (fun i m -> (m, match meshParent i scene.RootNode with None -> sprintf "%d" i | Some p -> p.Name))
    |> Seq.map convertMesh
    |> Seq.iter exportMesh
    |> ignore
    ()

[<EntryPoint>]
let main argv = 
    match argv with
    | [| filename |] -> 
        match (System.IO.File.Exists filename) with
        | false -> invalidArg "_" (filename + " doesn't exists!")
        | true -> convert filename
    | _ -> printfn "Usage: tpfmc [filename]"
    printfn "Press any key to close..." |> ignore
    Console.ReadKey() |> ignore
    0 // return an integer exit code
