import System.Exit         ( exitWith )
import System.Process      ( system )

main :: IO ()
main = system "t/test" >>= exitWith
