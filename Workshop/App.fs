//module App

//open System  
//open FsXaml  
//open System.Windows
//open TpfMC


//// Create the View and bind it to the View Model
//let mainWindowViewModel = Application.LoadComponent(
//                             new System.Uri("/App;component/mainwindow.xaml", UriKind.Relative)) :?> Window
//mainWindowViewModel.DataContext <- new MainWindowViewModel() 

//// Application Entry point
//[<STAThread>]
//[<EntryPoint>]
//let main(_) = (new Application()).Run(mainWindowViewModel)

module App  
  
open System  
open FsXaml  
open TpfMC
  
type App = XAML<"App.xaml">  
  
[<EntryPoint;STAThread>]  
let main argv = 
    let app = App()
    let vm = new MainWindowViewModel()
    app.MainWindow <- new MainWindow()
    app.MainWindow.DataContext <- vm
    app.Startup.AddHandler(fun s a -> 
      app.MainWindow.Show()
    )
    app.Run()  