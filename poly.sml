structure Poly = 
    struct 
        type coeffs = (int*real) list
        val zero = []
        
        fun sum [] xs = xs
          | sum xs [] = xs
          | sum ((m,a: real)::ts) ((n,b)::us) = 
                if m > n 
                then (m,a)::(sum ts ((n,b)::us)) 
                else 
                    if m < n
                    then (n,b)::(sum ((m,a)::ts) us)
                    else 
                        if Real.==(a,~b) 
                        then (sum ts us) 
                        else ((m, a + b)::(sum ts us))
                        
        fun scalar a [] = []
          | scalar a ((m, x: real)::xs) = (m, a * x)::(scalar a xs)
          
        fun subs xs ys = sum xs (scalar ~1.0 ys)
                 
        fun termprod _ [] = []
          | termprod (m,a: real) ((n,b)::ts) =  
                (m + n, a * b)::(termprod (m,a) ts)
 
        fun nprod [] _ = []
          | nprod ((m,a: real)::ts) us = 
            sum (termprod (m,a) us) (nprod ts us)

        fun prod [] _ = []
          | prod [(m,a)] us = termprod (m,a) us
          | prod ts us = 
            let val size = (List.length ts) div 2
            in sum (prod (List.take(ts, size)) us) (prod (List.drop(ts, size)) us) end
        
        fun power [] _ = []
          | power xs p = 
                let fun iter acc 0 = acc
                      | iter acc p = iter (prod xs acc) (p - 1)
                in iter [(0,1.0)] p end 
                
        
        infix dividedBy
        
        exception DivideByZero
        
        fun ns dividedBy [] = raise DivideByZero
          | [] dividedBy ds = ([], [])
          | ns dividedBy ((d,p)::ds) = 
            let fun divide q [] = (q, []) 
                  | divide q ((r, a)::rs) = 
                    if r >= d
                    then 
                        let val res = [(r - d, p / a)]
                        in 
                            divide (q @ res) (subs ((r, a)::rs) (prod res ((d,p)::ds)))
                        end
                    else (q, ((r, a)::rs))
            in divide [] ns end
     end;   
        
 (* x + 1*)
 val p1: Poly.coeffs = [(1,1.0), (0, 1.0)]
 (* x^2 - 1*)
 and p2: Poly.coeffs = [(2,1.0), (0, ~2.0)]
 (* x - 1*)
 and p3: Poly.coeffs = [(1,1.0), (0, ~1.0)]
 
 
 
 
 
 