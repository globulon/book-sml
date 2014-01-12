signature TREE=
    sig
        datatype 'a Tree = Leaf | Node of 'a Tree * 'a * 'a Tree
        
        (* topology *)
        val depth: 'a Tree -> int
        val size: 'a Tree -> int
        
        val comptree: int -> int -> int Tree
        
        val reflect: 'a Tree -> 'a Tree
        
        val preorder: 'a Tree -> 'a list
        val inorder: 'a Tree -> 'a list
        val postorder: 'a Tree -> 'a list
        
        val balpre: 'a list -> 'a Tree
        val balin: 'a list -> 'a Tree
    end;

structure Tree :> TREE = 
    struct
        datatype 'a Tree = Leaf | Node of 'a Tree * 'a * 'a Tree
 
        (* topology *)
        fun depth (Node(l, a, r)) = 1 + Int.max(depth l, depth r)
          | depth _ = 0
                  
        fun size (Node(l, a, r)) = 1 + (size l) + (size r) 
          | size _ = 0
          
        fun comptree k n = 
            if n=0 
            then Leaf
            else Node((comptree (2*k) (n - 1)), k , (comptree (2*k + 1) (n - 1)) )
            
        fun reflect Leaf = Leaf
          | reflect (Node(l, a, r)) = Node(r, a, l)
          
        fun preorder t = 
            let 
                fun iter Leaf acc = acc
                  | iter (Node(l, a, r)) acc = a::(iter l (iter r acc))
            in
                iter t []
            end
          
        fun inorder t = 
            let fun iter Leaf acc = acc
                  | iter (Node(l, a, r)) acc = (iter l (a::(iter r acc)))
            in 
                iter t []
            end
          
        fun postorder t = 
            let fun iter Leaf acc = acc
                  | iter (Node(l, a, r)) acc = (iter l (iter r  (a::acc)))
            in 
                iter t []
            end

        fun balpre [] = Leaf
          | balpre (x::xs) =
                let 
                    val size = List.length xs div 2
                in 
                    Node (balpre (List.take(xs, size)), x, balpre (List.drop(xs, size)))
                end
                
        fun balin [] = Leaf
          | balin xs =
                let 
                    val size = List.length xs div 2
                    val (y::ys) = List.drop(xs, size)
                in 
                    Node (balin (List.take(xs, size)), y, balin ys)
                end
    end;

val birnam = Tree.Node(Tree.Node(Tree.Leaf, 
                                 "wood",
                                 Tree.Node(Tree.Node(Tree.Leaf, "Birnam", Tree.Leaf),
                                           "of",
                                           Tree.Leaf)),
                       "The",
                       Tree.Leaf)    
