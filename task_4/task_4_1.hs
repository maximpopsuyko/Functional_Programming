data FunMonad a = FunMonad { fun :: () -> a }    

instance Functor FunMonad where
    fmap f (FunMonad x) = FunMonad (\() -> f (x ()) )

instance Applicative FunMonad where
    pure x                    = FunMonad $ \() -> x
    FunMonad a <*> FunMonad b = FunMonad $ \() -> (a ()) (b ())

instance Monad FunMonad where
    return a  = FunMonad ( \() -> a )
    m >>= k   = k (fun m ())  
    fail      = error
