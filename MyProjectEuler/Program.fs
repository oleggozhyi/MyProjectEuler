open System.Diagnostics

[<EntryPoint>]
let main argv = 
   let sw = Stopwatch.StartNew()
   Problems11to20.problem14()
   sw.Stop()
   printfn "%A" sw.Elapsed

   sw.Restart()
   Problems11to20.problem14_optimized()
   sw.Stop()
   printfn "%A" sw.Elapsed
   0 
