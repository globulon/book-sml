(* sets *)

infix mem;

fun (x mem []     ) = false
  | (x mem (y::ys)) = (x = y) orelse (x mem ys)

  
fun newmem x [] = x::[]
  | newmem x xs = 
    if (x mem xs) then xs else x::xs
    
fun setof []      = []
  | setof (x::xs) = newmem x (setof xs)

fun union xs      [] = xs
  | union []      ys = ys
  | union (x::xs) ys = newmem x (union xs ys)

fun inters xs [] = []
  | inters [] xs = []
  | inters (x::xs) ys = 
    if (x mem ys) 
    then x::(inters xs ys) 
    else (inters xs ys)
    
infix subs 

fun ([] subs ys) = true
  | (xs subs []) = false
  | ((x::xs) subs ys) = (x mem ys) andalso (xs subs ys)
  
fun seq xs ys = (xs subs ys) andalso (ys subs xs)

fun powset [] base = [base]
  | powset (x::xs) base = (powset xs  base) @ (powset xs (x::base))

fun cartesian [] ys = []
  | cartesian (x::xs) ys = 
    let 
        val rest = cartesian xs ys
        fun make_pairs [] = rest
          | make_pairs (y::ys) = (x,y)::(make_pairs ys)
    in 
        make_pairs ys
    end

(* maps *)

fun assoc [] _ = NONE
  | assoc ((x,y)::rest) k = if x=k then (SOME y) else (assoc rest k)
  
(*graphs*)

val graph1 = [
    ("a","b"), ("a","c"), ("a", "d"),
    ("b","e"), ("c","f"), ("d", "e"),
    ("e","f"), ("e","g")]

fun nexts [] k = []
  | nexts ((m,n)::graph) k = 
        if k=m 
        then n::(nexts graph k)
        else (nexts graph k)

fun depthf [] graph visited = rev visited
  | depthf (n::ns) graph visited = 
    if (n mem visited) 
    then (depthf ns graph visited)
    else (depthf ((nexts graph n) @ ns) graph (n::visited))
 
fun depth [] graph visited = rev visited
  | depth (x::xs) graph visited = 
        depth xs 
              graph 
              (if (x mem visited) 
              then visited
              else (depth (nexts graph x) graph (x::visited)))
                
fun topsort graph =
    let 
        fun sort [] visited = visited
          | sort (x::xs) visited = 
            sort xs 
                 (if (x mem visited)
                 then visited 
                 else x::(sort (nexts graph x) visited))
        val (starts,_) = ListPair.unzip graph
     in
        sort starts []
     end
       