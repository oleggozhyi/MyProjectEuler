module Problems21to30

open System
open System.IO

let problem21() =
    let isAmicable n = 
        let rec divisorsSum n sum i = 
            if i>(float>>sqrt>>int) n then sum
            else divisorsSum n (if n%i<>0 then sum else sum+i+n/i) (i+1)
        let m = 1+divisorsSum n 0 2 in m<>n && 1+divisorsSum m 0 2 = n
    [1..9999] 
    |> List.filter isAmicable
    |> List.sum
    |> printfn "Problem 21 = %A" //31626

let problem22() = 
    let names = File.ReadAllText("./ProblemsData/Problem22.txt")
                    .Split([|',';'"'|], StringSplitOptions.RemoveEmptyEntries)
    let score i (s:string) = (i+1)*Seq.sumBy (fun c->1+(int c)-(int 'A')) s
    names |> Array.sort |> Array.mapi score |> Array.sum
    |> printfn "Problem 22 = %A" //871198282
    