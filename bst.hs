-- Define a binary search tree with a find and insert function

module BST where

data BST = BSTCons Int BST BST | Empty deriving Show

tree :: BST
tree = BSTCons 10
          (BSTCons 7
            (BSTCons 5 Empty Empty)
            (BSTCons 8 Empty Empty))
          (BSTCons 12
            (BSTCons 11 Empty Empty)
            (BSTCons 14 Empty Empty))


checkbst :: BST -> Bool
checkbst Empty = True
checkbst (BSTCons _node Empty Empty) = True
checkbst (BSTCons _node Empty right) = checkbst right
checkbst (BSTCons _node left Empty) = checkbst left
checkbst (BSTCons node larm@(BSTCons node' _ _) rarm@(BSTCons node'' _ _)) =
  if node' > node || node > node'' then False
    else checkbst larm && checkbst rarm



find :: Int -> BST -> Bool
find n (BSTCons node left right)
    | n == node = True
    | n > node = find n right
    | otherwise = find n left
find _n Empty = False



insert :: Int -> BST -> BST
insert n Empty = BSTCons n Empty Empty -- start condition
insert n (BSTCons node left right) = if n < node then BSTCons node (insert n left) right
                                     else BSTCons node left (insert n right)



size :: BST -> Int
size Empty = 0
size (BSTCons _node left right) = 1 + size left + size right



maxdepth :: BST -> Int
maxdepth Empty = 0
maxdepth (BSTCons _node left right) = 1 + max (maxdepth left) (maxdepth right)



delete :: Int -> BST -> BST

delete _n Empty = Empty

delete n (BSTCons node larm rarm@(BSTCons node'' left'' right'')) =
 if n == node then BSTCons node'' (insertbst larm left'') right''
 else if n < node then BSTCons node (delete n larm) rarm
      else BSTCons node larm (delete n rarm)

delete n (BSTCons node larm Empty) =
  if n == node then larm
    else if n < node then BSTCons node (delete n larm) Empty
         else BSTCons node larm Empty

insertbst :: BST -> BST -> BST
insertbst bstx Empty = bstx
insertbst Empty bsty = bsty
insertbst (BSTCons nodex leftx rightx) (BSTCons nodea lefta righta) =
  if nodex < nodea
    then BSTCons nodea (insertbst lefta (BSTCons nodex leftx rightx)) righta
    else BSTCons nodex (insertbst leftx (BSTCons nodea lefta righta)) rightx
