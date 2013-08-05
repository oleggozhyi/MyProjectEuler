module Problems61to70

open System
open System.IO

let problem67() = 
    let split (c:char[]) (s:string) = s.Split(c, StringSplitOptions.RemoveEmptyEntries) 
    let inputLines = File.ReadAllText("./ProblemsData/Problem67.txt") |> split [|'\n'|]
    let reduceLines accLst arr = 
        let rec reduce acc = function
        | (x::[], y::[])      -> List.rev ((x+y)::acc)
        | (x1::x2::xs, y::ys) -> reduce ((max (x1+y) (x2+y))::acc) (x2::xs,ys)
        reduce [] (0::accLst, List.ofArray arr)
    inputLines |> Array.map  (fun l-> split [|' '|] l |> Array.map Int32.Parse)   
               |> Array.fold reduceLines []
               |> List.max
               |> printfn "Problem 67 = %A" //1074
    0