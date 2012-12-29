{-# LANGUAGE TemplateHaskell,CPP #-}
-- a comment
module Main where 

main :: IO (Int) -- ^ haddock
main = do 
        putStr ('h':"ello Prefs!")
        return (2 + 2)

#if USE_TH
$( derive makeTypeable ''Extension )
#endif
