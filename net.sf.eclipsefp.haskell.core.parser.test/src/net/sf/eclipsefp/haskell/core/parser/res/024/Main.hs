module ClassDeclTypeSigLineCrash where
class A a where
    dummy :: a -> Int
    dummy _ = undefined