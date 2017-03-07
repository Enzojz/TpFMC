namespace TpfMC

open Assimp
open System

module Core = 
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
            if c then (printLua indent x)
            else ""
        |> fun rs -> 
            match node with
            | C _ | F _ | V _ -> rs
            | _ -> ind rs (rs.Length > 30)
    
    let rec namedNodes (node : Node) = 
        match (node.Name.Trim().Length > 0, node.HasMeshes, node.HasChildren) with
        | (true, true, _) -> [ node ]
        | (false, _, false) | (true, false, false) -> []
        | (false, _, true) | (true, false, true) -> 
            node.Children
            |> Seq.collect namedNodes
            |> Seq.toList
    
    let transform (node : Node) = 
        let rec work (node : Node) (tf : Matrix4x4) = 
            match node with
            | null -> tf
            | _ -> tf * (work node.Parent node.Transform)
        work node Matrix4x4.Identity
    
    module Input = 
        type MeshData = 
            { normals : Vector3D list;
              vertices : Vector3D list;
              tangents : Vector3D list;
              uv0 : Vector2D list;
              uv1 : Vector2D list;
              transform : Matrix4x4;
              nbFace : int }
        
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
        
        let read filename = 
            match (IO.File.Exists filename) with
            | false -> Error(filename + " doesn't exists!")
            | true -> 
                let suffix = IO.Path.GetExtension(filename)
                let assimpImporter = new AssimpContext()
                match assimpImporter.IsImportFormatSupported(suffix) with
                | false -> 
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
                    assimpImporter.Dispose()
                    Succsed ret
        
        let objectList model = model.scene.RootNode |> namedNodes
        
        let convertMesh (mesh : Mesh) = 
            let normals = mesh.Normals |> Seq.toList
            let vertices = mesh.Vertices |> Seq.toList
            let tangents = mesh.Tangents |> Seq.toList
            
            let uv0 = 
                (Seq.item 0 mesh.TextureCoordinateChannels)
                |> Seq.toList
                |> List.map (fun n -> Vector2D(n.X, n.Y))
            
            let uv1 = 
                (Seq.item 1 mesh.TextureCoordinateChannels)
                |> Seq.toList
                |> List.map (fun n -> Vector2D(n.X, n.Y))
            
            normals |> List.iter (fun n -> n.Normalize())
            { normals = normals;
              vertices = vertices;
              tangents = tangents;
              uv0 = uv0;
              uv1 = uv1;
              transform = Matrix4x4.Identity;
              nbFace = mesh.FaceCount }
        
        let normalizeMesh (mesh : MeshData) =
            let rotM = mesh.transform |> Matrix3x3
            rotM.Inverse()
            rotM.Transpose()
            { mesh with vertices = mesh.vertices |> List.map (fun v -> mesh.transform * v);
                        normals = mesh.normals |> List.map (fun v -> let n = rotM * v in n.Normalize(); n);
                        transform = Matrix4x4.Identity }
        
        let mergeMeshes (meshes : MeshData list) = 
            let normalizedMeshes = meshes |> List.map normalizeMesh
            let merge f = normalizedMeshes |> List.collect f
            { normals = merge (fun r -> r.normals);
              vertices = merge (fun r -> r.vertices);
              tangents = merge (fun r -> r.tangents);
              uv0 = merge (fun r -> r.uv0);
              uv1 = merge (fun r -> r.uv1);
              transform = Matrix4x4.Identity;
              nbFace = meshes |> List.sumBy (fun r -> r.nbFace) }
        
        let meshList (scene : Scene) (m : Assimp.Matrix4x4) (node : Node) = 
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
                    |> mergeMeshes))
    
    module Output = 
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
        
        let flatten = List.fold (@) []
        
        let toBytes f vectors = 
            let toByte (values : float32 list) = 
                values
                |> List.map (BitConverter.GetBytes >> Array.toList)
                |> flatten
            vectors
            |> List.map (f >> toByte)
            |> flatten
        
        let convertToBytes (mesh : Input.MeshData) = 
            let nMesh = Input.normalizeMesh mesh
            { normals = nMesh.normals |> toBytes (fun n -> [ n.X; n.Y; n.Z ]);
              vertices = nMesh.vertices |> toBytes (fun n -> [ n.X; n.Y; n.Z ]);
              tangents = nMesh.tangents |> toBytes (fun n -> [ n.X; n.Y; n.Z; 1.0f ]);
              uv0 = nMesh.uv0 |> toBytes (fun n -> [ n.X; n.Y ]);
              uv1 = nMesh.uv1 |> toBytes (fun n -> [ n.X; n.Y ]);
              nbFace = nMesh.nbFace }
        
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
            
            let subMeshIndices meshes offset = 
                let rec work (meshes : (Material * ByteData) list) offset result = 
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
                        |> work rest (offset + indiceLength)
                work meshes offset []
            
            let vertexAttr count numComp offset = A(P("count", V count) :: P("numComp", V numComp) :: [ P("offset", V offset) ])
            F("data", 
              A [ P("animations", A []);
                  P("matConfigs", A [ A(List.init meshes.Length (fun _ -> V 0)) ]);
                  P("subMeshes", subMeshIndices meshes (offsets.uv1 + counts.uv1));
                  P("vertexAttr", 
                    A [ P("normal", vertexAttr counts.normals 3 offsets.normals);
                        P("position", vertexAttr counts.vertices 3 offsets.vertices);
                        P("tangent", vertexAttr counts.tangents 4 offsets.tangents);
                        P("uv0", vertexAttr counts.uv0 2 offsets.uv0);
                        C(counts.uv1 > 0, P("uv1", vertexAttr counts.uv1 2 offsets.uv1)) ]) ])
            |> printLua 0
        
        let blobGen mesh = 
            let indices = 
                [ 0..(mesh.nbFace * 3 - 1) ]
                |> List.map (BitConverter.GetBytes >> Array.toList)
                |> flatten
            mesh.vertices @ mesh.uv0 @ mesh.normals @ mesh.tangents @ mesh.uv1 @ indices |> List.toArray
        
        let prepareMesh (meshes : (Material * ByteData) list) = 
            let mergeMeshesBlob (meshes : ByteData list) = 
                let merge f = List.collect f meshes
                { normals = merge (fun r -> r.normals);
                  vertices = merge (fun r -> r.vertices);
                  tangents = merge (fun r -> r.tangents);
                  uv0 = merge (fun r -> r.uv0);
                  uv1 = merge (fun r -> r.uv1);
                  nbFace = meshes |> List.sumBy (fun r -> r.nbFace) }
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
        
        let generateMeshes (model : Input.ModelInfo) (m : Assimp.Matrix4x4) (node : Node) = 
            node
            |> (Input.meshList model.scene m)
            |> List.map (fun (material, meshes) -> (material, convertToBytes meshes))
            |> prepareMesh
            |> fun (blob, mesh) -> (node.Name, blob, mesh)
            |> exportMesh model.output
