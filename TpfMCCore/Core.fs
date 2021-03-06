﻿namespace TpfMC

open Assimp
open System

module Core =     
    let aindent = "  "
    type Lua = 
        | V of int
        | B of bool
        | N of float
        | S of string
        | L of (string * Lua)      // local x = 
        | F of (string * Lua)      // function
        | P of (string * Lua) list // { x = .. }
        | X of (int * Lua) list    // [x] = ..
        | A of Lua list            // a table
        | R of Lua list            // Root
        | RT of Lua                // Return
        | VA of string             // Ref
    
    let isSimple nodes = nodes |> List.forall (function V _ | B _ | N _ -> true | _ -> false)

    let rec printLua indent (node : Lua) = 
        let sindent = String.replicate indent aindent
        let ind str = 
            function 
            | true -> (Environment.NewLine + sindent + str)
            | false -> str
        match node with
        | R ls -> ls |> List.map (printLua indent) |> String.concat Environment.NewLine
        | V num -> sprintf "%d" num
        | B bln -> sprintf "%s" (if bln then "true" else "false")
        | N flt -> sprintf "%f" flt
        | S str -> @"""" + str + @""""
        | P ls -> 
          match ls |> List.map (fun (_, n) -> n) |> isSimple with
          | true when ls |> List.length < 5 -> 
            sprintf "{ %s }" 
              (ls 
                |> List.map (
                  fun (name, x) -> 
                    let children = (printLua (indent + 1) x)
                    sprintf "%s = %s" name children
                )
                |> String.concat (", ")
              )
          | _ ->
              sprintf "{\n%s\n%s}" 
                (ls 
                  |> List.map (
                    fun (name, x) -> 
                      let children = (printLua (indent + 1) x)
                      match x with
                      | P z when z |> List.map (fun (_, n) -> n) |> isSimple -> 
                        let maxName = match ls with [] -> 0 | _ -> ls |> List.map (fun (n, _) -> n.Length) |> List.max
                        sprintf "%s%s = %s" sindent (name.PadRight(maxName)) children
                      | _ -> sprintf "%s%s = %s" sindent name children
                  )
                  |> String.concat ("," + Environment.NewLine)
                )
                (String.replicate (indent - 1) aindent)
        | X ls -> printLua indent (P (List.map (fun (name, x) -> sprintf "[%d]" name, x) ls))
        | A ls -> 
          match isSimple ls with
            | true -> 
              sprintf "{ %s }"
                (ls |> List.map (printLua (indent + 1)) |> String.concat ", ")
            | false ->
              sprintf "{\n%s\n%s}" 
                (ls 
                  |> List.map (
                    fun x -> 
                      let children = (printLua (indent + 1) x)
                      sprintf "%s%s" sindent children
                  )
                  |> String.concat ("," + Environment.NewLine)
                )
                (String.replicate (indent - 1) aindent)
        | L (v, x) -> (sprintf "local %s = " v) + (printLua (indent + 1) x)
        | F (f, x) -> (sprintf "function %s() return " f) + (printLua (indent + 1) x) + " end"
        | VA v -> v
        | RT l -> (sprintf "return %s" (printLua (indent + 1) l))
    
    module Input = 
        type MeshData = 
            { normals : Vector3D list;
              vertices : Vector3D list;
              tangents : Vector3D list;
              uv0 : Vector2D list;
              uv1 : Vector2D list;
              transform : Matrix4x4;
              indices : (int * int * int) list }
        
        type ModelInfo = 
            { filename : string;
              scene : Scene;
              output : string;
              nodes : Node list }
        
        type Result = 
            | Succsed of ModelInfo
            | Error of string
        
        type ObjectInfo = 
            { name : string }
        
        let rec namedNodes (node : Node) = 
            match (node.Name.Trim().Length > 0, node.HasMeshes, node.HasChildren) with
            | (true, true, _) -> [ node ]
            | (false, _, false) | (true, false, false) -> []
            | (false, _, true) | (true, false, true) -> 
                node.Children
                |> Seq.collect namedNodes
                |> Seq.toList
        
        let transforms (node : Node) = 
            let rec work (node : Node) children = 
                match node with
                | null -> children |> List.rev
                | _ -> work node.Parent (node.Transform :: children)
            work node []
        
        let transform = transforms >> List.fold (*) Matrix4x4.Identity
        
        let read callback filename = 
            match (IO.File.Exists filename) with
            | false -> Error(filename + " doesn't exists!")
            | true -> 
                let suffix = IO.Path.GetExtension(filename)
                let assimpImporter = new AssimpContext()
                let log = new Assimp.LogStream(LoggingCallback(fun msg _ -> callback ("Assimp: " + (msg.TrimEnd(Environment.NewLine.ToCharArray())))))
                log.Attach()
                match assimpImporter.IsImportFormatSupported(suffix) with
                | false -> 
                    log.Detach()
                    assimpImporter.Dispose()
                    Error("Format not supported:" + suffix)
                | true -> 
                    let scene = assimpImporter.ImportFile(filename, PostProcessSteps.CalculateTangentSpace ||| PostProcessSteps.Triangulate ||| PostProcessSteps.GenerateNormals)
                    let nodes = namedNodes scene.RootNode |> Seq.toList
                    
                    let ret = 
                        { filename = filename;
                          scene = scene;
                          output = IO.Path.GetDirectoryName(IO.Path.GetFullPath(filename)) + @"\" + IO.Path.GetFileName(filename).Replace(".", "_") + @"\";
                          nodes = nodes }
                    log.Detach()
                    assimpImporter.Dispose()
                    Succsed ret
        
        let objectList model = model.scene.RootNode |> namedNodes
        
        let normalizedVec (n : Vector3D) = 
            n.Normalize()
            n
        
        let decompose (m : Matrix4x4) = 
            let mutable quat = Quaternion()
            let mutable trans = Vector3D()
            let mutable scale = Vector3D()
            m.Decompose(&scale, &quat, &trans)
            (scale, quat, trans)
        
        let quaternion2Euler (quat : Assimp.Quaternion) = 
            let (x, y, z, w) = (float (quat.X), float (quat.Y), float (quat.Z), float (quat.W))
            let yaw = Math.Atan2(2.0 * w * z + 2.0 * x * y, 1.0 - 2.0 * y * y - 2.0 * z * z) |> float32
            let pitch = Math.Asin(2.0 * w * y - 2.0 * z * x) |> float32
            let roll = Math.Atan2(2.0 * w * x + 2.0 * y * z, 1.0 - 2.0 * x * x - 2.0 * y * y) |> float32
            Vector3D(roll, yaw, pitch)
        
        let euler2Quatenion (vec : Vector3D) = Quaternion(vec.Z, vec.Y, vec.X)
        
        let convertMesh (mesh : Mesh) = 
            let tuplizeIndice indices = 
                let rec work result = 
                    function 
                    | a :: b :: c :: rest -> work ((a, b, c) :: result) rest
                    | _ -> result |> List.rev
                work [] indices
            
            let normals = 
                mesh.Normals
                |> Seq.toList
                |> List.map normalizedVec
            
            let vertices = mesh.Vertices |> Seq.toList
            let tangents = mesh.Tangents |> Seq.toList
            
            let uv0 = 
                (Seq.item 0 mesh.TextureCoordinateChannels)
                |> Seq.toList
                |> List.map (fun n -> Vector2D(n.X, 1.0f - n.Y))
            
            let uv1 = 
                (Seq.item 1 mesh.TextureCoordinateChannels)
                |> Seq.toList
                |> List.map (fun n -> Vector2D(n.X, 1.0f - n.Y))
            
            { normals = normals;
              vertices = vertices;
              tangents = tangents;
              uv0 = uv0;
              uv1 = uv1;
              transform = Matrix4x4.Identity;
              indices = 
                  mesh.GetIndices()
                  |> Seq.toList
                  |> tuplizeIndice }
        
        let normalizeMesh (mesh : MeshData) = 
            let scale, quat, trans = decompose mesh.transform
            let rotM = Matrix4x4.FromScaling(scale) * (quat.GetMatrix() |> Matrix4x4)
            let verM = rotM * Matrix4x4.FromTranslation(trans)
            rotM.Inverse()
            rotM.Transpose()
            { mesh with vertices = List.map ((*) verM) mesh.vertices;
                        normals = mesh.normals |> List.map (((*) rotM) >> normalizedVec);
                        indices = 
                            if (scale.X * scale.Y * scale.Z < 0.0f) then List.map (fun (a, b, c) -> (c, b, a)) mesh.indices
                            else mesh.indices;
                        transform = Matrix4x4.Identity }
        
        let mergeIndices indices = 
            let rec merge offset = 
                function 
                | fst :: rest -> (List.map (fun (a, b, c) -> (a + offset, b + offset, c + offset)) fst) @ (merge (offset + fst.Length * 3) rest)
                | [] -> []
            merge 0 indices
        
        let mergeMeshes uv02uv1 (meshes : MeshData list) = 
            let normalizedMeshes = meshes |> List.map normalizeMesh
            let merge f = normalizedMeshes |> List.collect f
            { normals = merge (fun r -> r.normals);
              vertices = merge (fun r -> r.vertices);
              tangents = merge (fun r -> r.tangents);
              uv0 = merge (fun r -> r.uv0);
              uv1 = merge (fun r -> if uv02uv1 then r.uv0 else r.uv1);
              transform = Matrix4x4.Identity;
              indices = 
                  normalizedMeshes
                  |> List.map (fun m -> m.indices)
                  |> mergeIndices }
        
        let meshList uv02uv1 (scene : Scene) (m : Assimp.Matrix4x4) (node : Node) = 
            let rec allMeshes (node : Node) = 
                (node.MeshIndices
                 |> Seq.map (fun i -> scene.Meshes.[i])
                 |> Seq.toList)
                @ (node.Children
                   |> Seq.collect allMeshes
                   |> Seq.toList)
            node
            |> allMeshes
            |> List.groupBy (fun m -> scene.Materials.[m.MaterialIndex])
            |> List.map (fun (material, meshes) -> 
                   (material, 
                    meshes
                    |> List.map convertMesh
                    |> List.map (fun mesh -> { mesh with transform = mesh.transform * m })
                    |> mergeMeshes uv02uv1))
    
    module Output = 
        type ByteData = 
            { normals : byte list;
              vertices : byte list;
              tangents : byte list;
              uv0 : byte list;
              uv1 : byte list;
              indices : (int * int * int) list }
        
        type NumData = 
            { normals : int;
              vertices : int;
              tangents : int;
              uv0 : int;
              uv1 : int }
        
        let toBytes f vectors = 
            let toByte (values : float32 list) = values |> List.collect (BitConverter.GetBytes >> Array.toList)
            vectors |> List.collect (f >> toByte)
        
        let convertToBytes (mesh : Input.MeshData) = 
            let nMesh = Input.normalizeMesh mesh
            { normals = nMesh.normals |> toBytes (fun n -> [ n.X; n.Y; n.Z ]);
              vertices = nMesh.vertices |> toBytes (fun n -> [ n.X; n.Y; n.Z ]);
              tangents = nMesh.tangents |> toBytes (fun n -> [ n.X; n.Y; n.Z; 1.0f ]);
              uv0 = nMesh.uv0 |> toBytes (fun n -> [ n.X; n.Y ]);
              uv1 = nMesh.uv1 |> toBytes (fun n -> [ n.X; n.Y ]);
              indices = nMesh.indices }
        
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
                P [ ("count", V count);
                    ("offset", V offset) ]
            
            let subMeshIndices meshes offset = 
                let rec work (meshes : (Material * ByteData) list) offset result = 
                    match meshes with
                    | [] -> A result
                    | (m, r) :: rest -> 
                        let indiceLength = r.indices.Length * 3 * sizeof<int>
                        result @ [ P [ ("indices", 
                                         P (
                                          [ ("normal", indiceAttr indiceLength offset);
                                            ("position", indiceAttr indiceLength offset);
                                            ("tangent", indiceAttr indiceLength offset);
                                            ("uv0", indiceAttr indiceLength offset)
                                          ] @
                                          if (r.uv1.Length > 0) then [("uv1", indiceAttr indiceLength offset)] else []
                                         )
                                       );
                                       ("materials", A [ S (m.Name + @".mtl") ]) ] ]
                        |> work rest (offset + indiceLength)
                work meshes offset []
            
            let vertexAttr count numComp offset = P [("count", V count); ("numComp", V numComp); ("offset", V offset) ]
            F("data", 
              P [ ("animations", A []);
                  ("matConfigs", A [ A(List.init meshes.Length (fun _ -> V 0)) ]);
                  ("subMeshes", subMeshIndices meshes (offsets.uv1 + counts.uv1));
                  ("vertexAttr", 
                    P (
                      [ ("normal", vertexAttr counts.normals 3 offsets.normals);
                        ("position", vertexAttr counts.vertices 3 offsets.vertices);
                        ("tangent", vertexAttr counts.tangents 4 offsets.tangents);
                        ("uv0", vertexAttr counts.uv0 2 offsets.uv0)
                      ] @
                      if (counts.uv1 > 0) then [("uv1", vertexAttr counts.uv1 2 offsets.uv1)] else []
                  )
                  )
              ])
            |> printLua 0
        
        let blobGen (mesh : ByteData) = 
            let indices = 
                mesh.indices
                |> List.collect (fun (a, b, c) -> [ a; b; c ])
                |> List.collect (BitConverter.GetBytes >> Array.toList)
            mesh.vertices @ mesh.uv0 @ mesh.normals @ mesh.tangents @ mesh.uv1 @ indices |> List.toArray
        
        let prepareMesh (meshes : (Material * ByteData) list) = 
            let mergeMeshesBlob (meshes : ByteData list) = 
                let merge f = List.collect f meshes
                { normals = merge (fun r -> r.normals);
                  vertices = merge (fun r -> r.vertices);
                  tangents = merge (fun r -> r.tangents);
                  uv0 = merge (fun r -> r.uv0);
                  uv1 = merge (fun r -> r.uv1);
                  indices = 
                      meshes
                      |> List.map (fun m -> m.indices)
                      |> Input.mergeIndices }
            (meshes
             |> List.map (fun (_, meshes) -> meshes)
             |> mergeMeshesBlob
             |> blobGen, meshes |> meshGen)
        
        let exportMesh dir (meshName, blob, mesh) = 
            let mshPath = dir + "/" + meshName + ".msh"
            if not (IO.Directory.Exists dir) then (IO.Directory.CreateDirectory dir) |> ignore
            let blobPath = mshPath + ".blob"
            IO.File.WriteAllBytes(blobPath, blob)
            IO.File.WriteAllText(mshPath, mesh)
            (meshName + ".msh.blob", meshName + ".msh")
        
        let generateMeshes uv0uv1 (model : Input.ModelInfo) (m : Assimp.Matrix4x4) (node : Node) = 
            node
            |> (Input.meshList uv0uv1 model.scene m)
            |> List.map (fun (material, meshes) -> (material, convertToBytes meshes))
            |> prepareMesh
            |> fun (blob, mesh) -> (node.Name, blob, mesh)
            |> exportMesh model.output
