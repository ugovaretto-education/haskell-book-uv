module Ch7 where

tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = div x 10
        d = mod xLast 10


foldBool :: a -> a -> Bool -> a
foldBool t f c  =  case c of 
                    True -> t
                    False -> f


foldBool2 :: a -> a -> Bool -> a
foldBool2 t f c 
  | c = t
  | otherwise = f


g: (a->b)->(a,c)->b
g f (x,y) =  (f x, y)
