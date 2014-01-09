local 
    val a = 16807.0
    val m = 2147483647.0
in 
    fun nextrand seed = 
        let val t = a * seed
        in t - m * real(floor (t/m)) end
end 

fun randlist 0 seed tail = (seed, tail)
  | randlist n seed tail = randlist (n - 1) (nextrand seed) (seed::tail)   
  

fun insert_sort [] = []
  | insert_sort (x::xs) =
    let 
        fun ins x ([]: real list)    = [x]
          | ins x (y::ys) = if x < y then x::y::ys else y::(ins x ys)
    in 
        ins x (insert_sort xs)
    end
    
fun quick []         = []
  | quick [x]        = [x]
  (*cheap optimization*)
  | quick (x::y::[]) = if x < y then (x::y::[]) else (y::x::[]) 
  | quick (x::xs)    = 
        let
            fun partition left right ([]: real list) = 
                    (quick left) @ x::(quick right)
              | partition left right (y::ys) = 
                    if y < x 
                    then partition (y::left) right ys
                    else partition left (y::right) ys
        in 
            partition [] [] xs
        end

val (seed, rs) = randlist 10000 1.0 []

fun merge   []      ys      = ys : real list
  | merge   xs      []      = xs
  | merge (x::xs) (y::ys)   = 
    if x < y 
    then x::(merge xs (y::ys))
    else y::(merge (x::xs) ys)
    
fun tmerge [] = []
  | tmerge [x] = [x]
  (*cheap optimization*)
  | tmerge (x::y::[]) = if x < y then (x::y::[]) else (y::x::[]) 
  | tmerge xs = 
    let val sz = (length xs) div 2
    in merge (tmerge (List.take(xs, sz))) (tmerge (List.drop(xs, sz)))
    end
        


  

        