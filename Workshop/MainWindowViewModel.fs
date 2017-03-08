namespace TpfMC

open System
open System.Windows
open System.Windows.Input
open System.ComponentModel
open System.Linq
open Microsoft.Win32
open System.Windows.Media.Media3D
open System.Windows.Media

type RelayCommand(canExecute : obj -> bool, action : obj -> unit) = 
    let event = new DelegateEvent<EventHandler>()
    interface ICommand with
        
        [<CLIEvent>]
        member x.CanExecuteChanged = event.Publish
        
        member x.CanExecute arg = canExecute (arg)
        member x.Execute arg = action (arg)

type MainWindowViewModel() = 
    let mutable entryList : Core.Input.ModelInfo list = []
    let mutable selectedEntry : Core.Input.ModelInfo option = None
    let mutable selectedMeshes : Assimp.Node list = []
    let mutable meshesTransform : (Assimp.Node * Assimp.Matrix4x4) list = []
    let mutable defaultTransform : Assimp.Matrix4x4 = 
            new Assimp.Matrix4x4(
                1.0f, 0.0f, 0.0f, 0.0f,
                0.0f, 0.0f, -1.0f, 0.0f,
                0.0f, 1.0f, 0.0f, 0.0f,
                0.0f, 0.0f, 0.0f, 1.0f
                )
    let mutable console : string list = []
    let propertyChangedEvent = new DelegateEvent<PropertyChangedEventHandler>()

    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member x.PropertyChanged = propertyChangedEvent.Publish
    
    member x.OnPropertyChanged propertyName = 
        propertyChangedEvent.Trigger([| x;
                                        new PropertyChangedEventArgs(propertyName) |])
    
    member this.Console = 
        console
        |> List.rev
        |> (String.concat Environment.NewLine)
    
    member this.EntryList = entryList
    
    member this.M0 with get() = defaultTransform.A1 and set(v) = defaultTransform.A1 <- v; this.OnPropertyChanged("Geometries")
    member this.M1 with get() = defaultTransform.A2 and set(v) = defaultTransform.A2 <- v; this.OnPropertyChanged("Geometries")
    member this.M2 with get() = defaultTransform.A3 and set(v) = defaultTransform.A3 <- v; this.OnPropertyChanged("Geometries")
    member this.M3 with get() = defaultTransform.A4 and set(v) = defaultTransform.A4 <- v; this.OnPropertyChanged("Geometries")
    member this.M4 with get() = defaultTransform.B1 and set(v) = defaultTransform.B1 <- v; this.OnPropertyChanged("Geometries")
    member this.M5 with get() = defaultTransform.B2 and set(v) = defaultTransform.B2 <- v; this.OnPropertyChanged("Geometries")
    member this.M6 with get() = defaultTransform.B3 and set(v) = defaultTransform.B3 <- v; this.OnPropertyChanged("Geometries")
    member this.M7 with get() = defaultTransform.B4 and set(v) = defaultTransform.B4 <- v; this.OnPropertyChanged("Geometries")
    member this.M8 with get() = defaultTransform.C1 and set(v) = defaultTransform.C1 <- v; this.OnPropertyChanged("Geometries")
    member this.M9 with get() = defaultTransform.C2 and set(v) = defaultTransform.C2 <- v; this.OnPropertyChanged("Geometries")
    member this.M10 with get() = defaultTransform.C3 and set(v) = defaultTransform.C3 <- v; this.OnPropertyChanged("Geometries")
    member this.M11 with get() = defaultTransform.C4 and set(v) = defaultTransform.C4 <- v; this.OnPropertyChanged("Geometries")
    member this.M12 with get() = defaultTransform.D1 and set(v) = defaultTransform.D1 <- v; this.OnPropertyChanged("Geometries")
    member this.M13 with get() = defaultTransform.D2 and set(v) = defaultTransform.D2 <- v; this.OnPropertyChanged("Geometries")
    member this.M14 with get() = defaultTransform.D3 and set(v) = defaultTransform.D3 <- v; this.OnPropertyChanged("Geometries")
    member this.M15 with get() = defaultTransform.D4 and set(v) = defaultTransform.D4 <- v; this.OnPropertyChanged("Geometries")

    member private this.Out str = 
        console <- str :: console
        this.OnPropertyChanged("Console")
    
    member this.SelectedEntry 
        with set (value : obj) = 
            match value with
            | :? Core.Input.ModelInfo as s -> selectedEntry <- Some s
            | _ -> selectedEntry <- None
    
    member private this.Transform(node : Assimp.Node) = 
        match List.filter (fst >> ((=) node)) meshesTransform with
        | [ (_, tr) ] -> tr * defaultTransform
        | _ -> defaultTransform
    
    member this.GeoGen (model : Core.Input.ModelInfo) (nodes : Assimp.Node list) = 
        nodes
        |> List.collect (fun n -> Core.Input.meshList model.scene ((Core.transform n) * (n |> this.Transform)) n)
        |> List.map (fun (_, mesh) -> 
               let g = 
                   new MeshGeometry3D(Positions = (mesh.vertices
                                                   |> List.map (fun n -> new Point3D(float (n.X), float (n.Y), float (n.Z)))
                                                   |> Point3DCollection), 
                                      Normals = (mesh.normals
                                                 |> List.map (fun n -> new Vector3D(float (n.X), float (n.Y), float (n.Z)))
                                                 |> Vector3DCollection), 
                                      TriangleIndices = ( mesh.indices |> List.collect (fun (a, b, c) -> [a; b; c]) |> Int32Collection))
               new GeometryModel3D(geometry = g, material = new DiffuseMaterial(Brushes.Gold)) :> Model3D)
        |> Model3DCollection
    
    member this.Geometries = 
        match selectedEntry with
        | None -> new Model3DGroup()
        | Some e -> new Model3DGroup(Children = this.GeoGen e selectedMeshes)
    
    member private this.LoadFiles filenames = 
        entryList <- filenames
                     |> List.map Core.Input.read
                     |> List.partition (function 
                            | Core.Input.Result.Error _ -> false
                            | Core.Input.Result.Succsed _ -> true)
                     |> (fun (suc, err) -> 
                     err |> List.iter (fun (Core.Input.Result.Error e) -> this.Out(e))
                     suc |> List.map (fun (Core.Input.Result.Succsed s) -> s))
        this.OnPropertyChanged("EntryList")
    
    member this.DropCommand = 
        new RelayCommand((fun _ -> true), 
                         (fun args -> 
                         let e = args :?> DragEventArgs
                         e.Data.GetData(DataFormats.FileDrop) :?> string []
                         |> Seq.toList
                         |> this.LoadFiles))
    
    member this.ClickCommand = 
        new RelayCommand((fun _ -> true), 
                         (fun _ -> 
                         let diag = OpenFileDialog(CheckFileExists = true, CheckPathExists = true, Multiselect = true)
                         diag.ShowDialog() |> ignore
                         diag.FileNames
                         |> Seq.toList
                         |> this.LoadFiles))
    
    member this.MeshesSelected = 
        new RelayCommand((fun _ -> true), 
                         (fun selectedItems -> 
                         selectedMeshes <- ((selectedItems :?> System.Collections.IList).Cast<Assimp.Node>() |> Seq.toList)
                         this.OnPropertyChanged("Geometries")))
    
    member this.Convert = 
        new RelayCommand((fun _ -> true), 
                         (fun _ -> 
                         entryList |> List.iter (fun e -> 
                                          e.nodes
                                          |> List.map (fun n -> Core.Output.generateMeshes e (n |> this.Transform) n)
                                          |> List.iter (fun (blob, mesh) -> 
                                                 this.Out(e.output + blob)
                                                 this.Out(e.output + mesh)))))
