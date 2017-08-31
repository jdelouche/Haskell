import Prelude
type Algebra f a = f a -> a
 
newtype Fix f = Fx (f (Fix f)) 
 
unFix :: Fix f -> f (Fix f)
unFix (Fx x) = x 
 
cata :: Functor f => Algebra f a -> Fix f -> a
cata alg = alg . fmap (cata alg) . unFix
 
data NatF a = ZeroF | SuccF a deriving (Show)
 
instance Functor NatF where
         fmap f ZeroF = ZeroF
         fmap f (SuccF x) = (SuccF (f x)) 
 
fib :: NatF (Int,Int) -> (Int,Int)
fib (ZeroF) = (1,1)
fib (SuccF (m,n)) = (n,m+n)

fibOnString :: NatF (String,String) -> (String,String)
fibOnString (ZeroF) = ("*","*")
fibOnString (SuccF (m,n)) = (n,m++n)

fibOnPair :: NatF ((Int,Int),(String,String)) -> ((Int,Int),(String,String))
fibOnPair (ZeroF) = ((1,1),("*","*"))
fibOnPair (SuccF ((mi,ni),(ms,ns))) = ((ni,mi+ni),(ns,ms++ns))

evalOnPair::Fix NatF -> ((Int,Int),(String,String))
evalOnPair = cata fibOnPair
 
evalOnString::Fix NatF -> (String,String)
evalOnString = cata fibOnString
 
eval::Fix NatF -> (Int,Int)
eval = cata fib 
 
main = print $ evalOnPair  $ Fx $ SuccF 
                       $ Fx $ SuccF 
                       $ Fx $ SuccF 
                       $ Fx $ SuccF 
                       $ Fx $ SuccF 
                       $ Fx $ SuccF 
                       $ Fx $ SuccF 
                       $ Fx $ SuccF 
                       $ Fx $ ZeroF

