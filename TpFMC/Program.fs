
open Assimp
open System
open TpfMC

let convert (model : Core.Input.ModelInfo) = 
    printfn ""
    printfn "======== %s ========" model.filename
    printfn "%d mesh(es) found." model.nodes.Length
    printfn "Output path: %s" model.output
    printfn "Converting..."

    model.nodes
    |> List.map (Core.Output.generateMeshes model Matrix4x4.Identity) 
    |> List.iter (fun (b, m) -> 
           printfn "%-15s >>>>> Blob: %s" (IO.Path.GetFileName(model.filename)) b
           printfn "%-15s >>>>> Mesh: %s" (IO.Path.GetFileName(model.filename)) m)
    |> ignore
    printfn "======== Finished ========"

[<EntryPoint>]
let main argv = 
    let mutable start = true
    
    let files = 
        match argv with
        | [||] -> 
            printfn "Usage: tpfmc [filename]"
            printfn "or drag/drop the file in the console window."
            [ Console.ReadLine() ]
        | _ -> argv |> Array.toList 
    while start do
        files
        |> List.map (fun f -> f.TrimStart([|'"'|]).TrimEnd([|'"'|]))
        |> List.map (fun filename -> 
               async { 
                   match (Core.Input.read Console.WriteLine filename) with
                   | Core.Input.Error err -> printfn "%s" err
                   | Core.Input.Succsed info -> convert info 
               })
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore
        printfn @"Press ""R"" to repeat, any other key to close..."
        start <- Console.ReadKey().Key = ConsoleKey.R
    0 // return an integer exit code
