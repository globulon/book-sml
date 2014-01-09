infix memb

fun x memb [] = false
  | x memb (y::ys) = (x=y) orelse (x memb ys)

fun new_member x xs = if (x memb xs) then xs else x::xs 

fun setof [] = []
  | setof (x::xs) = new_member x (setof xs)
  
infix union

fun [] union ys = ys
  | (x::xs) union ys = new_member x (xs union ys)
  
infix inter

fun [] inter ys = []
  | (x::xs) inter ys = 
    if (x memb ys) then (x::(xs inter ys)) else (xs inter ys) 
    
infix subs
fun [] subs ys = true
  | (x::xs) subs ys = (x memb ys) andalso (xs subs ys)
  
infix seq
fun xs seq ys = (xs subs ys) andalso (ys subs xs)

fun powset xs = 
    let 
        fun loop ([], xs) = [xs]
          | loop (x::xs, ys) = 
                loop(xs, x::ys) @ loop (xs, ys)
    in 
        loop (xs, [])
    end

infix X
fun [] X ys = []
  | (x::xs) X ys = 
    let 
        val others = xs X ys
        fun make_pairs [] =  others
          | make_pairs (y::ys) = (x,y)::(make_pairs ys) 
    in
        make_pairs ys
    end

fun assoc [] a = []
  | assoc ((x,y)::xs) a = if x=a then [y] else (assoc xs a)
  
