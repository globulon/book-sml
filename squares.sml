fun squares r = 
    let 
        val start = floor (Math.sqrt (real r))
        fun between x y acc =
            let
                val delta = r - x*x  
                val y2 = y*y
            in
                if x < y then acc
                else if y2 < delta then (between x (y + 1) acc)
                else if y2 = delta then (between (x - 1) (y + 1) ((x, y)::acc))
                else (between (x -1) y acc)
            end
    in
        between start 0 []
    end