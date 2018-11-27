
open Assimp
open System
open TpfMC
open System.Text.RegularExpressions

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let convert uv0uv1 (model : Core.Input.ModelInfo) = 
    printfn ""
    printfn "======== %s ========" model.filename
    printfn "%d mesh(es) found." model.nodes.Length
    printfn "Output path: %s" model.output
    printfn "Converting..."

    model.nodes
    |> List.map (Core.Output.generateMeshes uv0uv1 model Matrix4x4.Identity) 
    |> List.iter (fun (b, m) -> 
           printfn "%-15s >>>>> Blob: %s" (IO.Path.GetFileName(model.filename)) b
           printfn "%-15s >>>>> Mesh: %s" (IO.Path.GetFileName(model.filename)) m)
    |> ignore
    printfn "======== Finished ========"

[<EntryPoint>]
let main argv = 
    let mutable start = true
    
    let filematcher r = function
      | ParseRegex @"^[A-Za-z0-9.\/]+$" [filename] -> (filename, false) :: r
      | ParseRegex @"^[A-Za-z0-9.\/]+ /uv1$" [filename] -> (filename, true) :: r
      | ParseRegex @"^""[A-Za-z0-9.\/ ]+""$" [filename] -> (filename, false) :: r
      | ParseRegex @"^""[A-Za-z0-9.\/ ]+"" /uv1$" [filename] -> (filename, true) :: r
      | _ -> r

    let files = 
        match argv with
        | [||] -> 
            printfn "Usage: tpfmc [filename]"
            printfn "or drag/drop the file in the console window."
            (Console.ReadLine() |> filematcher [])
        | _ -> argv 
            |> Array.fold filematcher []
    while start do
        files
        |> List.map (fun (filename, uv0uv1) -> 
               async { 
                   match (Core.Input.read Console.WriteLine filename) with
                   | Core.Input.Error err -> printfn "%s" err
                   | Core.Input.Succsed info -> convert uv0uv1 info 
               })
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore
        printfn @"Press ""R"" to repeat, any other key to close..."
        start <- Console.ReadKey().Key = ConsoleKey.R
    0 // return an integer exit code
