open System.Diagnostics

[<EntryPoint>]
let main argv = 
  let x = System.Diagnostics.Stopwatch.StartNew()
  Problems61to70.problem67()
  x.Stop()
  printfn "%A" x.Elapsed
  0 
