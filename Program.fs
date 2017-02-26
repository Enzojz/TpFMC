﻿open Assimp
open System

type t = 
    { normals : byte list
      vertices : byte list
      tangents : byte list
      uv0 : byte list
      nbFace : int }

type c = 
    { normals : int
      vertices : int
      tangents : int
      uv0 : int }

let rec namedNodes (node : Node) = 
    match (node.Name.Trim().Length > 0, node.HasMeshes, node.HasChildren) with
    | (true, true, _) -> [ node ]
    | (false, _, false) | (true, false, false) -> []
    | (false, _, true) | (true, false, true) -> 
        node.Children
        |> Seq.map namedNodes
        |> Seq.concat
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

let convertMesh (mesh : Mesh) = 
    let normals = mesh.Normals |> Seq.toList
    let vertices = mesh.Vertices |> Seq.toList
    let tangents = mesh.Tangents |> Seq.toList
    let uvCoords = (Seq.item 0 mesh.TextureCoordinateChannels) |> Seq.toList
    normals |> List.iter (fun n -> n.Normalize())
    { normals = normals |> toBytes (fun n -> [ n.X; n.Y; n.Z ])
      vertices = vertices |> toBytes (fun n -> [ n.X; n.Y; n.Z ])
      tangents = tangents |> toBytes (fun n -> [ n.X; n.Y; n.Z; 1.0f ])
      uv0 = uvCoords |> toBytes (fun n -> [ n.X; n.Y ])
      nbFace = mesh.FaceCount }

let meshGen (meshes : (Material * t) list) = 
    let d = sprintf "%d"
    
    let counts = 
        { normals = meshes |> List.sumBy (fun (_, m) -> m.normals.Length)
          vertices = meshes |> List.sumBy (fun (_, m) -> m.vertices.Length)
          tangents = meshes |> List.sumBy (fun (_, r) -> r.tangents.Length)
          uv0 = meshes |> List.sumBy (fun (_, r) -> r.uv0.Length) }
    
    let offsets = 
        { normals = 0
          vertices = counts.normals
          tangents = counts.normals + counts.vertices
          uv0 = counts.normals + counts.vertices + counts.tangents }
    
    let rec subMeshIndices (meshes : (Material * t) list) offset result = 
        match meshes with
        | [] -> result
        | (m, r) :: rest -> 
            let indiceLength = r.nbFace * 3 * sizeof<int>
            result @ [ @"       {"
                       @"           indices = {"
                       @"               normal =   { count = " + d indiceLength + ", offset = " + d offset + ", },"
                       @"               position = { count = " + d indiceLength + ", offset = " + d offset + ", },"
                       @"               tangent =  { count = " + d indiceLength + ", offset = " + d offset + ", },"
                       @"               uv0 =      { count = " + d indiceLength + ", offset = " + d offset + ", },"
                       @"           },"
                       @"           materials = { """ + m.Name + @".mtl"" },"
                       @"       }," ]
            |> subMeshIndices rest (offset + indiceLength)
    
    [ @"function data() return {"
      @"   animations = {"
      @"   },"
      @"   matConfigs = { { 0, }, },"
      @"   vertexAttr ="
      @"   {"
      @"      normal =       { count = " + d counts.normals + ", numComp = 3, offset = " + d offsets.normals + ", },"
      @"      position =     { count = " + d counts.vertices + ", numComp = 3, offset = " + d offsets.vertices + ", },"
      @"      tangent =      { count = " + d counts.tangents + ", numComp = 4, offset = " + d offsets.tangents + ", },"
      @"      uv0 =          { count = " + d counts.uv0 + ", numComp = 2, offset = " + d offsets.uv0 + ", },"
      @"   },"
      @"   subMeshes = { " ] @ (subMeshIndices meshes (offsets.uv0 + counts.uv0) []) @ [ @"   },"; @"} end" ] 
    |> String.concat Environment.NewLine

let blobGen mesh = 
    let indices = 
        [ 0..(mesh.nbFace * 3 - 1) ]
        |> List.map (BitConverter.GetBytes >> Array.toList)
        |> flatten
    mesh.normals @ mesh.vertices @ mesh.tangents @ mesh.uv0 @ indices |> List.toArray

let exportMesh dir meshName (meshes : (Material * t list) list) = 
    let mergeMeshes (meshes : t list) = 
        { normals = 
              meshes
              |> List.map (fun r -> r.normals)
              |> List.concat
          vertices = 
              meshes
              |> List.map (fun r -> r.vertices)
              |> List.concat
          tangents = 
              meshes
              |> List.map (fun r -> r.tangents)
              |> List.concat
          uv0 = 
              meshes
              |> List.map (fun r -> r.uv0)
              |> List.concat
          nbFace = meshes |> List.sumBy (fun r -> r.nbFace) }
    
    let mshPath = dir + "/" + meshName + ".msh"
    let blobPath = mshPath + ".blob"
    printfn "Generating mesh binary:  %s" blobPath
    IO.File.WriteAllBytes(blobPath, 
                          meshes
                          |> List.map (fun (_, meshes) -> mergeMeshes meshes)
                          |> mergeMeshes
                          |> blobGen)
    printfn "Generating mesh:         %s" mshPath
    IO.File.WriteAllText(mshPath, 
                         meshes
                         |> List.map (fun (material, meshes) -> (material, mergeMeshes meshes))
                         |> meshGen)

let convertNode dir (scene : Scene) (node : Node) = 
    let rec allMeshes (node : Node) = 
        (node.MeshIndices
         |> Seq.map (fun i -> scene.Meshes.Item(i))
         |> Seq.toList)
        @ (node.Children
           |> Seq.map allMeshes
           |> Seq.concat
           |> Seq.toList)
    node
    |> allMeshes
    |> List.groupBy (fun (m : Mesh) -> scene.Materials.Item(m.MaterialIndex))
    |> List.map (fun (material, meshes) -> (material, List.map convertMesh meshes))
    |> (exportMesh dir node.Name)

let convert (inputFile : string) = 
    let dir = IO.Path.GetDirectoryName(inputFile)
    let assimpImporter = new AssimpContext()
    let scene = 
        assimpImporter.ImportFile
            (inputFile, 
             PostProcessSteps.CalculateTangentSpace ||| PostProcessSteps.Triangulate 
             ||| PostProcessSteps.GenerateNormals)
    let nodes = namedNodes scene.RootNode
    printfn "%s contains %d mesh(es)." inputFile nodes.Length |> ignore
    nodes
    |> Seq.iter (convertNode dir scene)
    |> ignore
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
