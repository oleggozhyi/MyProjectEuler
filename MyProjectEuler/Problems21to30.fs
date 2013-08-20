module Problems21to30

open System
open System.IO
open System.Collections.Generic

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
    
let problem23() = 
    let isAbundant= 
        let cache = Dictionary<_,_>()
        let rec divisorsSum n sum i = 
            if i>(float>>sqrt>>int) n then sum
            else divisorsSum n (if n%i<>0 then sum elif i*i=n then sum+i else sum+i+n/i) (i+1)
        fun n-> let cached, value =  cache.TryGetValue n 
                if cached then value else cache.[n]<-(1+divisorsSum n 0 2 >n); cache.[n]
    let isNotAbundantSum n = [1..n/2] |> List.tryFind (fun i-> isAbundant i && isAbundant (n-i)) |> (=) None
    [1..28123] |> List.filter isNotAbundantSum |> List.sum
    |> printfn "Problem 23 = %A"


let problem23_optimized() = 
    let isAbundant n = 
        let rec divisorsSum n sum i = 
            if i>(float>>sqrt>>int) n then sum
            else divisorsSum n (if n%i<>0 then sum elif i*i=n then sum+i else sum+i+n/i) (i+1)
        1+divisorsSum n 0 2 >n
    [1..28123] |> List.filter isAbundant
    let isNotAbundantSum n = [12..n/2] |> List.tryFind (fun i-> isAbundant i && isAbundant (n-i)) |> (=) None
    [1..28123] |> List.filter isNotAbundantSum |> List.sum
    |> printfn "Problem 23 = %A"
    