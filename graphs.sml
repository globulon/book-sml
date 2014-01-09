fun nexts a [] = []
  | nexts a xs = 
        let 
            fun loop [] acc = acc
              | loop ((x,y)::tail) acc = 
                 if x=a 
                 then loop tail (y::acc)
                 else loop tail acc
        in
            loop xs []
        end