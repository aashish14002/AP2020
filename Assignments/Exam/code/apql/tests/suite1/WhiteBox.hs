-- This is a sample white-box skeleton

import Types
import ParserImpl
-- import PreprocessorImpl
-- import EngineImpl
import Data.List (intercalate)
import System.Exit (exitSuccess, exitFailure)  -- for when running stand-alone

tests :: [(String, Bool)]
tests = [
        parse1, parse2, parse3, parse4, parse5, parse5, parse6, parse7, parse8, parse9
    ] where
    parse1 = ("parse1", parseString "myquery(x) if sibling(\"Frank\", x) and"
                            ++ "not ancestor(x, \"Rupert\")." 
                            == Right [Rule (Atom "myquery" [TVar "x"]) 
                                (CAnd (CAtom (Atom "sibling" [TData "Frank",TVar "x"]))
                                     (CNot (CAtom (Atom "ancestor" 
                                        [TVar "x",TData "Rupert"]))))]),
    parse2 = ("parse2", parseString "p(x,y) if q(x) and r(y). q(\"a\")."
                            == Right [Rule (Atom "p" [TVar "x", TVar "y"])
                                (CAnd (CAtom (Atom "q" [TVar "x"]))
                                        (CAtom (Atom "r" [TVar "y"]))),
                                Rule (Atom "q" [TData "a"])
                                CTrue])
main :: IO ()
main =
  let failed = [name | (name, ok) <- tests, not ok]
  in case failed of
       [] -> do putStrLn "All tests passed!"
                exitSuccess
       _ -> do putStrLn $ "Failed tests: " ++ intercalate ", " failed
               exitFailure

