namespace TpfMC

open System
open System.Windows
open System.Windows.Input
open System.ComponentModel
open Microsoft.Win32
open System.Windows.Media.Media3D
open System.Windows.Media
open System.Windows.Media.Imaging
open Pfim
open System.Diagnostics

module Calculates = 
    let hsv2Rgb h s v = 
        if (h < 0) || (h > 360) || s > 1.0 || s < 0.0 || v > 1.0 || v < 0.0 then failwith "Parameters out of range!"
        let hi = (h / 60) % 6
        let f = float (h) / 60.0 - float (hi)
        let p = v * (1.0 - s)
        let q = v * (1.0 - f * s)
        let t = v * (1.0 - (1.0 - f) * s)
        
        let (r, g, b) = 
            match hi with
            | 0 -> (v, t, p)
            | 1 -> (q, v, p)
            | 2 -> (p, v, t)
            | 3 -> (p, q, v)
            | 4 -> (t, p, v)
            | _ -> (v, p, q)
        (int (r * 256.0), int (g * 256.0), int (b * 256.0))
    
    let rad2Deg (v : float32) = v / float32 (Math.PI) * 180.0f
    let deg2Rad (v : float32) = v / 180.0f * float32 (Math.PI)

type RelayCommand(action : obj -> unit) = 
    let event = DelegateEvent<EventHandler>()
    interface ICommand with
        
        [<CLIEvent>]
        member x.CanExecuteChanged = event.Publish
        
        member x.CanExecute _ = true
        member x.Execute arg = action arg

[<AbstractClass>]
type ViewModelBase() = 
    let propertyChangedEvent = DelegateEvent<PropertyChangedEventHandler>()
    
    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member x.PropertyChanged = propertyChangedEvent.Publish
    
    member x.OnPropertyChanged propertyName = 
        propertyChangedEvent.Trigger([| x;
                                        PropertyChangedEventArgs(propertyName) |])

type VectorRad3DViewModel(v : Assimp.Vector3D) = 
    let mutable vector = v
    
    member this.X 
        with get () = Calculates.rad2Deg vector.X
        and set (va) = vector.X <- Calculates.deg2Rad va
    
    member this.Y 
        with get () = Calculates.rad2Deg vector.Y
        and set (va) = vector.Y <- Calculates.deg2Rad va
    
    member this.Z 
        with get () = Calculates.rad2Deg vector.Z
        and set (va) = vector.Z <- Calculates.deg2Rad va
    
    member this.Vector = vector

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
        |> VectorRad3DViewModel
    
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
        [ (this.Scale.Vector |> (fun v -> 
           if (v.X = 1.0f && v.Y = 1.0f && v.Z = 1.0f) then None
           else Some(Printf.sprintf "S: (% 6.2f, % 6.2f, % 6.2f)" v.X v.Y v.Z)));
          (this.Rotation.Vector |> (fun v -> 
           if (v.X = 0.0f && v.Y = 0.0f && v.Z = 0.0f) then None
           else Some(Printf.sprintf "R: (% 6.2f, % 6.2f, % 6.2f)" (Calculates.rad2Deg v.X) (Calculates.rad2Deg v.Y) (Calculates.rad2Deg v.Z))));
          (this.Translation.Vector |> (fun v -> 
           if (v.X = 0.0f && v.Y = 0.0f && v.Z = 0.0f) then None
           else Some(Printf.sprintf "T: (% 6.2f, % 6.2f, % 6.2f)" v.X v.Y v.Z))) ]
        |> List.filter (function 
               | None -> false
               | Some _ -> true)
        |> List.map (fun (Some s) -> s)

type MeshViewModel(n : Assimp.Node) = 
    inherit ViewModelBase()
    let mutable node = n
    let mutable hue = 40
    let mutable isSelected = true
    let mutable transforms = ((Core.Input.transforms n) |> List.filter (fun m -> not m.IsIdentity)) @ [ Assimp.Matrix4x4.Identity ] |> List.map TransformViewModel
    let mutable isConverted = false
    member this.Name = n.Name
    member this.Transforms = transforms
    member this.Node = n
    
    member this.IsSelected 
        with get () = isSelected
        and set (v) = 
            isSelected <- v
            this.OnPropertyChanged("IsSelected")
    
    member this.UserTransform = List.last transforms
    
    member this.Hue 
        with get () = hue
        and set (v) = hue <- v
    
    member this.Color = 
        let (r, g, b) = Calculates.hsv2Rgb hue 0.75 0.75
        Color.FromRgb(BitConverter.GetBytes(r).[0], BitConverter.GetBytes(g).[0], BitConverter.GetBytes(b).[0])
    
    member this.IsConverted 
        with get () = isConverted
        and set (v) = 
            isConverted <- v
            this.OnPropertyChanged("IsConverted")

type ModelViewModel(e : Core.Input.ModelInfo) = 
    let mutable entry = e
    let mutable nodes = e.nodes |> List.mapi (fun i m -> MeshViewModel(m, Hue = i * 360 / (e.nodes.Length + 1)))
    member this.Nodes = nodes |> List.map (fun n -> n.Node)
    member this.Entry = e
    member this.Meshes = nodes
    member this.Output = e.output

type MainWindowViewModel() as this = 
    inherit ViewModelBase()
    let mutable entryList : ModelViewModel list = []
    let mutable selectedEntry : ModelViewModel option = None
    let mutable selectedMesh : MeshViewModel option = None
    let mutable defaultTransform = Assimp.Quaternion(Assimp.Vector3D(1.0f, 0.0f, 0.0f), float32 (Math.PI * 0.5)).GetMatrix() |> Assimp.Matrix4x4
    let mutable console : string list = []
    let propertyChangedEvent = DelegateEvent<PropertyChangedEventHandler>()
    
    do 
        Process.GetCurrentProcess().StartInfo.RedirectStandardOutput <- true
        Process.GetCurrentProcess().OutputDataReceived.Add(fun (obj : DataReceivedEventArgs) -> (this.Out obj.Data))
        Console.Write "ok"
    
    member this.Console = console |> List.rev
    
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
        List.collect (fun n -> (Core.Input.meshList model.Entry.scene (n |> this.Transform) n.Node) |> List.map (fun d -> (d, n.Hue)))
        >> List.map (fun ((mat, mesh), o) -> 
               let rnd = System.Random(Guid.NewGuid().GetHashCode())
               let (r, g, b) = Calculates.hsv2Rgb o (rnd.NextDouble() * 0.25 + 0.75) (rnd.NextDouble() * 0.25 + 0.75)
               let color = Color.FromRgb(BitConverter.GetBytes(r).[0], BitConverter.GetBytes(g).[0], BitConverter.GetBytes(b).[0])
               let defaultMat = DiffuseMaterial(SolidColorBrush(color))
               
               let g = 
                   MeshGeometry3D(Positions = (mesh.vertices
                                               |> List.map (fun n -> Point3D(float (n.X), float (n.Y), float (n.Z)))
                                               |> Point3DCollection), 
                                  Normals = (mesh.normals
                                             |> List.map (fun n -> Vector3D(float (n.X), float (n.Y), float (n.Z)))
                                             |> Vector3DCollection), 
                                  TextureCoordinates = (mesh.uv0
                                                        |> List.map (fun n -> Point(float (n.X), float (n.Y)))
                                                        |> PointCollection), 
                                  TriangleIndices = (mesh.indices
                                                     |> List.collect (fun (a, b, c) -> [ a; b; c ])
                                                     |> Int32Collection))
               
               let toDiffuseMaterial (i : IImage) (format : PixelFormat) = 
                   BitmapSource.Create(i.Width, i.Height, 96.0, 96.0, format, null, i.Data, i.Stride)
                   |> (fun b -> ImageBrush(b, ViewportUnits = BrushMappingMode.Absolute))
                   |> DiffuseMaterial
               
               let m = 
                   match mat.TextureDiffuse.FilePath with
                   | null -> defaultMat
                   | path -> 
                       match IO.File.Exists(path) with
                       | true -> Some(Pfim.FromFile path)
                       | false -> 
                           let p = 
                               IO.Path.Combine((model.Entry.filename
                                                |> IO.Path.GetFullPath
                                                |> IO.Path.GetDirectoryName), path)
                           match IO.File.Exists(p) with
                           | true -> Some(Pfim.FromFile p)
                           | false -> None
                       |> function 
                       | None -> defaultMat
                       | Some i -> 
                           match i.Format with
                           | ImageFormat.Rgb24 -> toDiffuseMaterial i PixelFormats.Bgr24
                           | ImageFormat.Rgba32 -> toDiffuseMaterial i PixelFormats.Bgr32
                           | _ -> defaultMat
               
               GeometryModel3D(geometry = g, material = m) :> Model3D)
        >> Model3DCollection
    
    member this.UpdateGeometries = RelayCommand(fun _ -> this.OnPropertyChanged("Geometries"))
    
    member this.Convert = 
        RelayCommand(fun _ -> 
            entryList |> List.iter (fun e -> 
                             e.Meshes
                             |> List.map (fun n -> 
                                    async { 
                                        n.IsConverted <- false
                                        let result = Core.Output.generateMeshes e.Entry (n |> this.Transform) n.Node
                                        n.IsConverted <- true
                                        return result
                                    })
                             |> Async.Parallel
                             |> Async.RunSynchronously
                             |> Seq.iter (fun (blob, mesh) -> 
                                        this.Out(e.Entry.output + blob)
                                        this.Out(e.Entry.output + mesh))
                             ))
    member private this.LoadFiles filenames = 
        entryList <- filenames
                     |> List.map (Core.Input.read this.Out)
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
            if (diag.FileNames.Length <> 0) then 
                diag.FileNames
                |> Seq.toList
                |> this.LoadFiles
            ())
    
    member this.SelectAll = 
        RelayCommand(fun _ -> 
            match selectedEntry with
            | None -> ()
            | Some e -> e.Meshes |> List.iter (fun m -> m.IsSelected <- true)
            this.OnPropertyChanged("Geometries"))
    
    member this.SelectNone = 
        RelayCommand(fun _ -> 
            match selectedEntry with
            | None -> ()
            | Some e -> e.Meshes |> List.iter (fun m -> m.IsSelected <- false)
            this.OnPropertyChanged("Geometries"))
    
    member this.SelectOnlyItem = 
        RelayCommand(fun (a : obj) -> 
            match selectedEntry with
            | None -> ()
            | Some e -> e.Meshes |> List.iter (fun m -> m.IsSelected <- false)
            let m = a :?> MeshViewModel
            m.IsSelected <- true
            this.OnPropertyChanged("Geometries"))

    member this.OpenOutput = 
        RelayCommand(fun _ -> 
            match selectedEntry with
            | None -> ()
            | Some e -> Process.Start(e.Output) |> ignore
            )
