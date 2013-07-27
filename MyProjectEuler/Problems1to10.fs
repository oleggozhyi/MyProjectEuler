module Problems1to10

open System
open System.Net.Http
open System.Threading.Tasks
open System.Text.RegularExpressions

let problem1() = 
    {1..999} |> Seq.filter(fun x -> x % 5 = 0 || x % 3 = 0) 
             |> Seq.fold (+) 0
             |> printfn "Problem 1 = %i" // 233168 

let problem2() = 
     // problem 2
    Seq.unfold (fun (current, next) -> Some(current, (next, current + next))) (0, 1) 
            |> Seq.takeWhile (fun x -> x < 4000000)
            |> Seq.sumBy (fun x -> if x%2 = 0 then x else 0) 
            |> printfn "Problem 2 = %i" // 4613732

let problem3() = 
    let rec findLargestPrimeFactor largestPrimeFactor = function
        | n when n = 1I -> largestPrimeFactor
        | _ as n ->  let primeFactor = seq {2I..n} |> Seq.find (fun i-> n % i = 0I)
                     findLargestPrimeFactor (max primeFactor largestPrimeFactor) (n / primeFactor) 
    findLargestPrimeFactor 1I 600851475143I |> printfn "Problem 3 = %A" // 6857

let problem4() = 
    let rec reverseNum part = function
        | 0 -> part
        | n -> reverseNum (part * 10 + n % 10) (n / 10)
    seq {for x in 100..999 do for y in 100..999 -> x * y} 
        |> Seq.filter (fun x -> x = (reverseNum 0 x))  
        |> Seq.max  
        |> printfn "Problem 4 = %i" // 906609 

let problem5() =
    printfn "Problem 5 = %i" (19 * 17 * 13 * 11 * 7 * 5 * (3 * 3) * (2 * 2 * 2 *2)) // 232792560    
         
let problem5_generalized() =
    // TODO: too slow 
    let findLeastCommonMultiple n =
        let isPrime n = [2..(float >> sqrt >> int) n] |> List.forall (fun x -> n % x <> 0)
        let primesProduct = {2..n} |> Seq.filter isPrime |> Seq.fold (*) 1
        let isDividable x = [2..n] |> List.forall (fun i-> x % i = 0)
        seq {for i in primesProduct..System.Int32.MaxValue do yield i } |> Seq.find isDividable
    findLeastCommonMultiple 20  |> printfn "Problem 5 (II) = %A" // 232792560 

let problem6() =
    [for x in 1..100 do for y in 1..100 do if x <> y then yield x * y] |> List.sum  |> printfn "Problem 6 = %A" // 25164150 

let problem7() =
    // see Primes.fs       
    Primes.generate 1000000 |> Seq.skip 10000 |> Seq.head

let problem7_joke() =
    let waitAndReturn (t : Task<'a>) = t.Wait(); t.Result   
    let txtWithPrimes = (HttpClient().GetAsync("http://primes.utm.edu/lists/small/100000.txt") |> waitAndReturn)
                                     .Content.ReadAsStringAsync()                              |> waitAndReturn
    let primesWithSomeBullshit = Regex.Split(txtWithPrimes, "\s+")
    primesWithSomeBullshit |> Array.findIndex  (fun x -> x = "2") 
                           |> (+) 10000 
                           |> Array.get primesWithSomeBullshit 
                           |> printfn "Problem 7 = %A" //104743

let problem8() =
    let digits =  Array.ofSeq <| Seq.map (fun c -> int c - int '0') "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
    let maxProductOf5 = digits.[0..digits.Length-5]
                                |> Array.mapi (fun i _ -> digits.[i..i+4] |> Array.reduce (*)) 
                                |> Array.max  
    maxProductOf5 |> printfn "Problem 8  = %i" //40824

let problem9() =
    let getA b = (500000-1000*b)/(1000-b) 
    let getC b = 1000-b-getA b
    let isTriplet b = let a = getA b in let c = getC b in (a*a+b*b=c*c)
    [499..-1..200] |> List.find isTriplet |> fun b->(getA b)*b*(getC b) |> printfn "Problem 9 = %i" //31875000

let problem10() = 
   // see Primes.fs       
   Primes.generate 2000000 |> Seq.sumBy int64 |> printfn "Problem 10 = %A" //142913828922L