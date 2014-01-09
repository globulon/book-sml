(* 3.2*)
fun dec x = x  - 1

fun last ([]) = raise List.Empty
  | last (x::[]) = x 
  | last (x::rest) = last rest
  
fun nth (x::_, 0) = x
  | nth (x::rest, i) = nth (rest, dec i)
  | nth ([], _) = raise List.Empty
  
fun reverseA xs = 
    let 
        fun loop (acc, x::xs) = loop(x::acc, xs)
          | loop (acc, []) = acc
    in loop ([], xs) end

(*3.5 tail rec solution ~ n, n being the bigger list*)    
infix 5 ++
fun (xs ++ []) = xs
  | ([] ++ ys) = ys
  | (xs ++ ys) = 
    let
        fun loop (acc, x::xs) = loop(x::acc, xs)
          | loop (acc, []) = acc
    in loop ([], loop(loop([], xs), ys)) end

fun change (coins, 0) = []
  | change (c::coins, amount) = 
    if c > amount 
    then change (coins, amount)
    else c::change(c::coins, amount - c)
    
fun change2 (coins, available, 0) = [coins]
  | change2 (coins, [], amount) = []
  | change2 (coins, c::available, amount) = 
    if c < amount
    then change2(c::coins, c::available, amount - c) @ change2(coins, available, amount)
    else []
    
val gb_coins = [50, 20, 10, 5, 2, 1]
val us_coins = [25, 10, 5, 1]    

(*binary calculus*)
signature BINARY = 
sig
type binary
val make: int list -> binary
val sum: binary*binary -> binary
val prod: binary*binary -> binary
val toString: binary -> string
end;

structure Binary :> BINARY = 
struct
type binary = int list

fun bincarry (0, ps) = ps
  | bincarry (1, []) = [1]
  | bincarry (1, p::ps) = (1 - p)::bincarry(p, ps)

fun binsum (c, [], qs) = bincarry (c, qs)
  | binsum (c, ps, []) = bincarry (c, ps)
  | binsum (c, p::ps, q::qs) = 
        ((c + p + q) mod 2)::binsum ((c + p +q) div 2, ps, qs) 

fun binprod ([], _) = []
  | binprod (0::ps, qs) = 0::binprod (ps, qs)
  | binprod (1::ps, qs) = binsum (0, ps, 0::binsum(0, ps, qs))

fun make ps = ps
  
fun sum (ps, qs) = binsum (0, ps, qs)

val prod = binprod 

val toString = ((List.foldl String.^ "") o List.rev o (List.map Int.toString))
 
end;

(* dumb definition for matrix*)

signature MATRIX = 
sig
type 'a definition
type 'a matrix

val make: 'a definition -> 'a matrix
val transpose: 'a matrix -> 'a matrix
end;

structure Matrix :> MATRIX = 
struct
type 'a definition = 'a list list
type 'a matrix = 'a list list

fun make xss = xss

fun headcol ([], acc) = List.rev acc
  | headcol ((x::xs)::rows, acc) = headcol (rows, x::acc)  
  | headcol ([]::_, acc) = raise List.Empty
  
fun tailcols ([], acc) = List.rev acc
  | tailcols ((_::row)::rows, acc) = tailcols (rows, row::acc)
  | tailcols ([]::rows, acc) = raise List.Empty

fun transpose rows = headcol(rows, []) :: transpose (tailcols (rows, []))

end

fun pivotrow [row] = row : real list
  | pivotrow (rw1::rw2::rs) =
        if ((abs (hd rw1)) >= (abs (hd rw2)))
        then pivotrow (rw1::rs)
        else pivotrow (rw2::rs)
        
fun scalarprod (k: real) =  List.map (fn x => k*x)
    
fun delrow p = List.filter (fn x => not (Real.==(p, hd x)))
        
val sumrow: real list * real list -> real list = 
    (List.map (fn (x,y) => x + y)) o ListPair.zip

fun gausselim [row] = [row]
  | gausselim ms = 
    let 
        val p::prow = pivotrow ms
        fun normalize ([]) = []
          | normalize ((x::xs)::rest) = 
                sumrow (xs, (scalarprod (~x/p) prow))::
                    (normalize rest)
     in
        (p::prow)::gausselim(normalize (delrow p ms))
     end
     
        
        
        
     
    
