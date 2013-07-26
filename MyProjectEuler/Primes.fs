module Primes

///<summary>
/// Sieve of Eratosthenes
///</summary>
let generate n =
    let sqrtn = (float>>sqrt>>int) n
    let primesSieve = Array.create (n+1) true
    let isPrime x = if primesSieve.[x] && x<=sqrtn  then
                        for i in x*x..2*x..n do primesSieve.[i]<-false
                    primesSieve.[x]
    seq {yield 2; for i in 3..2..n do if isPrime i then yield i}

