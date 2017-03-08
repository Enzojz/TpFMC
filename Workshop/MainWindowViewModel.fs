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

type MainWindowViewModel() = 
    let mutable entryList : Core.Input.ModelInfo list = []
    let mutable selectedEntry : Core.Input.ModelInfo option = None
    let mutable selectedMeshes : Assimp.Node list = []
    let mutable meshesTransform : (Assimp.Node * Assimp.Matrix4x4) list = []
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
            | :? Core.Input.ModelInfo as s -> selectedEntry <- Some s
            | _ -> selectedEntry <- None
    
    member this.Geometries = 
        match selectedEntry with
        | None -> Model3DGroup()
        | Some e -> Model3DGroup(Children = this.GeoGen e selectedMeshes)

    member private this.Transform(node : Assimp.Node) = 
        match List.filter (fst >> ((=) node)) meshesTransform with
        | [ (_, tr) ] -> tr * defaultTransform
        | _ -> defaultTransform
    
    member private this.GeoGen(model : Core.Input.ModelInfo) = 
        List.collect (fun n -> Core.Input.meshList model.scene ((Core.Input.transform n) * (n |> this.Transform)) n)
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
    
    member this.MeshesSelected = 
        RelayCommand(fun selectedItems -> 
            selectedMeshes <- ((selectedItems :?> System.Collections.IList).Cast<Assimp.Node>() |> Seq.toList)
            this.OnPropertyChanged("Geometries"))
    
    member this.Convert = 
        RelayCommand(fun _ -> 
            entryList |> List.iter (fun e -> 
                             e.nodes
                             |> List.map (fun n -> Core.Output.generateMeshes e ((Core.Input.transform n) * (n |> this.Transform)) n)
                             |> List.iter (fun (blob, mesh) -> 
                                    this.Out(e.output + blob)
                                    this.Out(e.output + mesh))))

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
    
