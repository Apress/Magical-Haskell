module Main (main) where
import Init (initConfig)

main :: IO ()
main = do 
    settings <- initConfig
    print settings
