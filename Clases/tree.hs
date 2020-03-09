module Tree where

data BST a = Void 
           | Node {root :: a, 
                   left :: BST a, 
                   right :: BST a} deriving(Show)

{-@ type BSTL a X = BST {v:a | v < X}  @-}
{-@ type BSTR a X = BST {v:a | X < v}  @-}

{-@ data BST a = Void 
               | Node {root :: a, 
                       left :: BSTL a root, 
                       right :: BSTR a root} 
@-}

tree :: BST Int
tree = Node 3 (Node 2 Void Void) (Node 4 Void Void)

