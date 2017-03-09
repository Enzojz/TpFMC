namespace TpfMC

open System
open System.Windows
open System.Windows.Input
open System.ComponentModel
open System.Linq
open Microsoft.Win32
open System.Windows.Media.Media3D
open System.Windows.Media

type RelayCommand(action : obj -> unit) = 
    let event = DelegateEvent<EventHandler>()
    interface ICommand with
        
        [<CLIEvent>]
        member x.CanExecuteChanged = event.Publish
        
        member x.CanExecute _ = true
        member x.Execute arg = action arg

type Vector3DViewModel(v : Assimp.Vector3D) = 
    let mutable vector = v
    
    member this.X 
        with get () = vector.X
        and set (va) = vector.X <- va
    
    member this.Y 
        with get () = vector.Y
        and set (va) = vector.Y <- va
    
    member this.Z 
        with get () = vector.Z
        and set (va) = vector.Z <- va
    
    member this.Vector = vector

type TransformViewModel(t : Assimp.Matrix4x4) = 
    let mutable t = (t, true)
    let mutable scale = Core.Input.decompose (fst t) |> fun (s, _, _) -> s |> Vector3DViewModel
    let mutable translation = Core.Input.decompose (fst t) |> fun (_, _, s) -> s |> Vector3DViewModel
    
    let mutable rotation = 
        Core.Input.decompose (fst t)
        |> (fun (_, q, _) -> q)
        |> Core.Input.quaternion2Euler
        |> Vector3DViewModel
    
    member this.Transform = 
        Assimp.Matrix4x4.FromScaling(scale.Vector) * ((rotation.Vector |> Core.Input.euler2Quatenion).GetMatrix() |> Assimp.Matrix4x4) 
        * Assimp.Matrix4x4.FromTranslation(translation.Vector)
    
    member this.IsApplied 
        with get () = snd t
        and set (value) = t <- (fst t, value)
    
    member this.Scale = scale
    member this.Translation = translation
    member this.Rotation = rotation
    member this.Description = 
        let rad2deg (v : float32) = v / float32(Math.PI) * 180.0f
        [ (this.Scale.Vector |> (fun v -> 
           if (v.X = 1.0f && v.Y = 1.0f && v.Z = 1.0f) then None
           else Some(Printf.sprintf "S: (% 6.2f, % 6.2f, % 6.2f)" v.X v.Y v.Z)));
          (this.Rotation.Vector |> (fun v -> 
           if (v.X = 0.0f && v.Y = 0.0f && v.Z = 0.0f) then None
           else Some(Printf.sprintf "R: (% 6.2f, % 6.2f, % 6.2f)" (rad2deg v.X) (rad2deg v.Y) (rad2deg v.Z))));
          (this.Translation.Vector |> (fun v -> 
           if (v.X = 0.0f && v.Y = 0.0f && v.Z = 0.0f) then None
           else Some(Printf.sprintf "T: (% 6.2f, % 6.2f, % 6.2f)" v.X v.Y v.Z))) ]
        |> List.filter (function 
               | None -> false
               | Some _ -> true)
        |> List.map (fun (Some s) -> s)
        

type MeshViewModel(n : Assimp.Node) = 
    let mutable node = n
    let mutable isSelected = true
    let mutable transforms = ((Core.Input.transforms n) |> List.filter (fun m -> not m.IsIdentity)) @ [ Assimp.Matrix4x4.Identity ] |> List.map TransformViewModel
    member this.Name = n.Name
    member this.Transforms = transforms
    member this.Node = n
    
    member this.IsSelected 
        with get () = isSelected
        and set (v) = isSelected <- v
    
    member this.UserTransform = List.last transforms

type ModelViewModel(e : Core.Input.ModelInfo) = 
    let mutable entry = e
    let mutable nodes = e.nodes |> List.map MeshViewModel
    member this.Nodes = nodes |> List.map (fun n -> n.Node)
    member this.Entry = e
    member this.Meshes = nodes

type MainWindowViewModel() = 
    let mutable entryList : ModelViewModel list = []
    let mutable selectedEntry : ModelViewModel option = None
    let mutable selectedMesh : MeshViewModel option = None
    let mutable defaultTransform = Assimp.Quaternion(Assimp.Vector3D(1.0f, 0.0f, 0.0f), float32 (Math.PI * 0.5)).GetMatrix() |> Assimp.Matrix4x4
    let mutable console : string list = []
    let propertyChangedEvent = DelegateEvent<PropertyChangedEventHandler>()
    
    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member x.PropertyChanged = propertyChangedEvent.Publish
    
    member x.OnPropertyChanged propertyName = 
        propertyChangedEvent.Trigger([| x;
                                        PropertyChangedEventArgs(propertyName) |])
    
    member this.Console = 
        console
        |> List.rev
        |> (String.concat Environment.NewLine)
    
    member private this.Out str = 
        console <- str :: console
        this.OnPropertyChanged("Console")
    
    member this.EntryList = entryList
    
    member this.SelectedEntry 
        with set (value : obj) = 
            match value with
            | :? ModelViewModel as s -> selectedEntry <- Some s
            | _ -> selectedEntry <- None
            this.OnPropertyChanged("Geometries")
    
    member this.SelectedMesh 
        with get () = 
            match selectedMesh with
            | None -> null
            | Some m -> m :> obj
        and set (value : obj) = 
            match value with
            | :? MeshViewModel as s -> selectedMesh <- Some s
            | _ -> selectedMesh <- None
            this.OnPropertyChanged("SelectedMesh")
    
    member this.Geometries = 
        match selectedEntry with
        | None -> Model3DGroup()
        | Some e -> Model3DGroup(Children = this.GeoGen e (e.Meshes |> List.filter (fun n -> n.IsSelected)))
    
    member private this.Transform(node : MeshViewModel) = 
        (node.Transforms
         |> List.filter (fun t -> t.IsApplied)
         |> List.map (fun t -> t.Transform)
         |> List.fold (*) Assimp.Matrix4x4.Identity)
        * defaultTransform
    
    member private this.GeoGen(model : ModelViewModel) = 
        List.collect (fun n -> Core.Input.meshList model.Entry.scene (n |> this.Transform) n.Node)
        >> List.map (fun (_, mesh) -> 
               let g = 
                   MeshGeometry3D(Positions = (mesh.vertices
                                               |> List.map (fun n -> Point3D(float (n.X), float (n.Y), float (n.Z)))
                                               |> Point3DCollection), 
                                  Normals = (mesh.normals
                                             |> List.map (fun n -> Vector3D(float (n.X), float (n.Y), float (n.Z)))
                                             |> Vector3DCollection), 
                                  TriangleIndices = (mesh.indices
                                                     |> List.collect (fun (a, b, c) -> [ a; b; c ])
                                                     |> Int32Collection))
               GeometryModel3D(geometry = g, material = DiffuseMaterial(Brushes.Gold)) :> Model3D)
        >> Model3DCollection
    
    member this.UpdateGeometries = RelayCommand(fun _ -> this.OnPropertyChanged("Geometries"))
    
    member this.Convert = 
        RelayCommand(fun _ -> 
            entryList |> List.iter (fun e -> 
                             e.Meshes
                             |> List.map (fun n -> Core.Output.generateMeshes e.Entry (n |> this.Transform) n.Node)
                             |> List.iter (fun (blob, mesh) -> 
                                    this.Out(e.Entry.output + blob)
                                    this.Out(e.Entry.output + mesh))))
    
    member private this.LoadFiles filenames = 
        entryList <- filenames
                     |> List.map Core.Input.read
                     |> List.partition (function 
                            | Core.Input.Result.Error _ -> false
                            | Core.Input.Result.Succsed _ -> true)
                     |> (fun (suc, err) -> 
                     err |> List.iter (fun (Core.Input.Result.Error e) -> this.Out(e))
                     suc |> List.map (fun (Core.Input.Result.Succsed s) -> ModelViewModel s))
        this.OnPropertyChanged("EntryList")
    
    member this.DropCommand = 
        RelayCommand(fun args -> 
            let e = args :?> DragEventArgs
            e.Data.GetData(DataFormats.FileDrop) :?> string []
            |> Seq.toList
            |> this.LoadFiles)
    
    member this.ClickCommand = 
        RelayCommand(fun _ -> 
            let diag = OpenFileDialog(CheckFileExists = true, CheckPathExists = true, Multiselect = true)
            diag.ShowDialog() |> ignore
            diag.FileNames
            |> Seq.toList
            |> this.LoadFiles)
