module Problems11to20

open System
open System.IO
open System.Linq
open Microsoft.FSharp.Collections
open System.Collections.Generic

let problem11() = 
   let grid = array2D [ [ 08; 02; 22; 97; 38; 15; 00; 40; 00; 75; 04; 05; 07; 78; 52; 12; 50; 77; 91; 08 ]
                        [ 49; 49; 99; 40; 17; 81; 18; 57; 60; 87; 17; 40; 98; 43; 69; 48; 04; 56; 62; 00 ]
                        [ 81; 49; 31; 73; 55; 79; 14; 29; 93; 71; 40; 67; 53; 88; 30; 03; 49; 13; 36; 65 ]
                        [ 52; 70; 95; 23; 04; 60; 11; 42; 69; 24; 68; 56; 01; 32; 56; 71; 37; 02; 36; 91 ]
                        [ 22; 31; 16; 71; 51; 67; 63; 89; 41; 92; 36; 54; 22; 40; 40; 28; 66; 33; 13; 80 ]
                        [ 24; 47; 32; 60; 99; 03; 45; 02; 44; 75; 33; 53; 78; 36; 84; 20; 35; 17; 12; 50 ]
                        [ 32; 98; 81; 28; 64; 23; 67; 10; 26; 38; 40; 67; 59; 54; 70; 66; 18; 38; 64; 70 ]
                        [ 67; 26; 20; 68; 02; 62; 12; 20; 95; 63; 94; 39; 63; 08; 40; 91; 66; 49; 94; 21 ]
                        [ 24; 55; 58; 05; 66; 73; 99; 26; 97; 17; 78; 78; 96; 83; 14; 88; 34; 89; 63; 72 ]
                        [ 21; 36; 23; 09; 75; 00; 76; 44; 20; 45; 35; 14; 00; 61; 33; 97; 34; 31; 33; 95 ]
                        [ 78; 17; 53; 28; 22; 75; 31; 67; 15; 94; 03; 80; 04; 62; 16; 14; 09; 53; 56; 92 ]
                        [ 16; 39; 05; 42; 96; 35; 31; 47; 55; 58; 88; 24; 00; 17; 54; 24; 36; 29; 85; 57 ]
                        [ 86; 56; 00; 48; 35; 71; 89; 07; 05; 44; 44; 37; 44; 60; 21; 58; 51; 54; 17; 58 ]
                        [ 19; 80; 81; 68; 05; 94; 47; 69; 28; 73; 92; 13; 86; 52; 17; 77; 04; 89; 55; 40 ]
                        [ 04; 52; 08; 83; 97; 35; 99; 16; 07; 97; 57; 32; 16; 26; 26; 79; 33; 27; 98; 66 ]
                        [ 88; 36; 68; 87; 57; 62; 20; 72; 03; 46; 33; 67; 46; 55; 12; 32; 63; 93; 53; 69 ]
                        [ 04; 42; 16; 73; 38; 25; 39; 11; 24; 94; 72; 18; 08; 46; 29; 32; 40; 62; 76; 36 ]
                        [ 20; 69; 36; 41; 72; 30; 23; 88; 34; 62; 99; 69; 82; 67; 59; 85; 74; 04; 36; 16 ]
                        [ 20; 73; 35; 29; 78; 31; 90; 01; 74; 31; 49; 71; 48; 86; 81; 16; 23; 57; 05; 54 ]
                        [ 01; 70; 54; 71; 83; 51; 54; 69; 16; 92; 33; 48; 61; 43; 52; 01; 89; 19; 67; 48 ] ]
   let products (squareArr: int[,]) = Seq.map (Seq.reduce (*)) (seq {
       let horizontalSlice = Seq.cast<int> squareArr.[0..3,0..0] 
       let verticalSlice = Seq.cast<int> squareArr.[0..0,0..3]
       let diagonalDownSlice = seq {for i in 0..3 do yield squareArr.[i,i] }
       let diagonalUpSlice = seq {for i in 0..3 do yield  squareArr.[3-i,i] }
       yield! [horizontalSlice; verticalSlice; diagonalDownSlice; diagonalUpSlice] })
   seq {for x in 0..16 do for y in 0..16 do yield! products grid.[x..x+3,y..y+3] }
    |> Seq.max
    |> printfn "Problem 11 = %A" //70600674
   ()



let problem12() =
    let rec devisorCount n sqrtn acc index = 
            if index > sqrtn then acc 
            else devisorCount n sqrtn  (if n % index = 0 then acc+2 else acc) (index+1)
    let triangleDivisorsCount num = 
        (devisorCount num ((float>>sqrt>>int) num) 2 2) * (devisorCount (num+1) ((float>>sqrt>>int) (num+1)) 2 2) 
    {1..999999999} |> Seq.map triangleDivisorsCount |> Seq.find (fun count->count>=500)
                                                    |> printfn "Problem 12 =%A"  //76576500 
let problem12_recurcive() =
    let rec devisorNumber' num upTo count = function 
        | x when x > upTo -> count
        | _ as x -> devisorNumber' num upTo  (if num % x = 0 then count+2 else count) (x+1) 
    let rec loop num inc = function
        | count when count >= 500 -> num-inc-1 
        | _ ->  loop (num+inc) (inc+1) (devisorNumber' num ((float>>sqrt>>int) num) 0 2)
    loop 1 2 1

let problem12_imperative() =
    let mutable num  = 0
    let mutable i = 1
    let mutable found = false
    while not found do  
        num <- num + i
        i <- i + 1
        let mutable divisors = 2
        for x in 2..(float>>sqrt>>int) num do
              if num % x = 0 then divisors <- divisors + 2
        found <- divisors >= 500
    printfn "Problem 12 =%A" num //76576500 /0.6sec
    ()

let problem13() = 
     let nums =  File.ReadAllText("./ProblemsData/Problem13.txt")
     (nums .Split([|'\n'; ' '|], System.StringSplitOptions.RemoveEmptyEntries) 
            |> Array.map (fun s-> bigint.Parse s) 
            |> Array.sum).ToString().Substring(0,10)
            |> printfn "Problem 13 = %A" //5537376230

let problem14() =
    let collatz n = n |> Seq.unfold (function
                                  | x when x=1L -> None
                                  | x when x%2L=0L -> Some(x, x/2L)
                                  | _ as x -> Some(x, 3L*x+1L)) 
    [2L..1000000L] |> List.maxBy (collatz>>Seq.length) |> printfn "Problem 14 = %A" //837799L

let problem14_optimized() =
    let cache = new Dictionary<_, _>(1000000)
    let memoize n len = cache.Add(n, len); len
    let collatzSeqLen n = 
        let rec collatzSeqLen' totalLen = function
            | 1L -> totalLen
            | _ as n -> match cache.TryGetValue(n) with
                        | (true, len) -> len + totalLen
                        | (false, _) -> let nextValue = if n%2L=0L then n/2L else 3L*n+1L
                                        collatzSeqLen' (totalLen+1L) nextValue               
        collatzSeqLen' 0L n |> memoize n
    {2L..1000000L} |> PSeq.maxBy collatzSeqLen |> printfn "Problem 14 = %A"  //837799

let problem15() = 
   // 40! (20!*20!)
   let (!) n = [1I..n] |> List.reduce (*)
   !40I  / (!20I * !20I ) |> printfn "Problem 15 = %A" //137846528820

let problem16()= 
    let rec digitsSum acc n = if n<10I then acc+n else digitsSum (acc+n%10I) (n/10I)
    2I<<<999 |>  digitsSum 0I |> printfn  "Problem 16 = %A" //1366

let problem17() = 
    let m1to9 = Map.ofList [1, "one"; 2, "two"; 3, "three"; 4, "four"; 5, "five";  
                            6, "six"; 7, "seven"; 8, "eight"; 9, "nine"]
    let m11to19 = Map.ofList [10, "ten"; 11, "eleven"; 12, "twelve"; 13, "thirteen"; 14, "fourteen"; 
                                15, "fifteen"; 16, "sixteen"; 17, "seventeen"; 18, "eighteen"; 19, "nineteen"]
    let m20to90 = Map.ofList [20, "twenty"; 30, "thirty"; 40, "forty"; 50, "fifty"; 60, "sixty";
                                70, "seventy"; 80, "eighty"; 90, "ninety"] 
    let rec num2str strAcc = function
        | n when n<10   -> strAcc + " " +  m1to9.[n]
        | n when n<20   -> strAcc + " " + m11to19.[n]
        | n when n<100  -> if n%10=0 then strAcc + " " +  m20to90.[n/10*10]
                            else num2str (strAcc + " " + m20to90.[n/10*10]) (n%10)
        | n when n<1000 -> if n%100=0 then m1to9.[n/100] + " hundred"
                            else num2str (m1to9.[n/100] + " hundred and") (n%100)
        | 1000          -> "one thousand"
        | _             -> failwith "numbers  > 1000 aren't supported"
    [1..1000] |> List.sumBy ( fun n-> (num2str "" n).Replace(" ", "").Length )
              |> printfn "Problem 17 = %A"  //21124

let problem18() = 
    let input = File.ReadAllText("./ProblemsData/Problem18.txt")
    let split (symbols:char[]) (s:string) =  
        s.Split(symbols, StringSplitOptions.RemoveEmptyEntries) 
        |> List.ofArray
    let reduceLines xs ys = 
        let rec reduce acc = function
        | (x::[], y::[])      -> List.rev ((x+y)::acc)
        | (x1::x2::xs, y::ys) -> let maxBranch = max (x1+y) (x2+y)
                                 reduce (maxBranch::acc) (x2::xs,ys)
        reduce [] (0::xs, ys)
    input 
    |> split [|'\n'|]
    |> List.map  (fun l-> split [|' '|] l |> List.map Int32.Parse)   
    |> List.reduce reduceLines
    |> List.max
    |> printfn "Problem 18 = %A" //1074
    0

let problem19() =
    let month = [|31;28;31;30;31;30;31;31;30;31;30;31|]
    let daysInMonth y m = if m<>2 || not (y%4=0 && y<>1900) then month.[m-1] else 29
    let isSunday y m d = 
        let daysInFullYearsSince1900 = 365+(y-1901)*(365*4+1)/4
        let daysSinceJan1st = d + List.sumBy (daysInMonth y) [1..m-1]
        (daysSinceJan1st+daysInFullYearsSince1900) % 7 = 5
    seq { for y in 1901..2000 do for m in 1..12 do if isSunday y m 1 then yield 1} 
        |> Seq.length 
        |> printfn "Problem 19 = %A"  //171
        
 