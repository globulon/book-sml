structure Disjunction =
    struct 
        datatype ('a,'b)\/  = -\/ of 'a | \/- of 'b

        fun mapR (-\/ a) f = -\/(a)
          | mapR (\/- a) f = \/-(f a)
          
        fun mapL (-\/ a) f = -\/(f a)
          | mapL (\/- a) f = \/-(a)
    end;
    
structure Validation = 
    struct
        
        type 'a Validation = (exn list, 'a) Disjunction.\/
        
        fun safely f = (fn x => Disjunction.\/-(f x) handle e => Disjunction.-\/([e]))
        
        val map = Disjunction.mapR

        fun flatmap va f = case (map va f) of 
                                Disjunction.-\/(r) => Disjunction.-\/(r)
                              | Disjunction.\/-(Disjunction.-\/(r)) => Disjunction.-\/(r)
                              | Disjunction.\/-(Disjunction.\/-(r)) => Disjunction.\/-(r)
           
    end;

    