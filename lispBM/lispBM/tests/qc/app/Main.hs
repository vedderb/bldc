{-# LANGUAGE CPP #-}

module Main where

import Data.IORef
import Data.List
import SExpGen
import Test.QuickCheck
import Test.QuickCheck.Random
import System.Environment
import System.IO
import Options.Applicative
import qualified Data.Map as M

check = quickCheckWithResult stdArgs

properties = M.fromList
    [ ("prop_add", check (withMaxSuccess 100 prop_add)),
      ("prop_sub", check (withMaxSuccess 100 prop_sub)),
      ("prop_num_eq", check (withMaxSuccess 100 prop_num_eq)),
      ("prop_nil_program", check (withMaxSuccess 100 prop_nil_program)),
      ("prop_env", check (withMaxSuccess 100 prop_env)),
      ("prop_unflatten_flatten", check (withMaxSuccess 100 prop_unflatten_flatten)),
      ("prop_gc_progn", check (withMaxSuccess 100 prop_gc_progn)),
      ("prop_if_true",  check (withMaxSuccess 100 prop_if_true)),
      ("prop_if_false", check (withMaxSuccess 100 prop_if_false)),
      ("prop_cond",  check (withMaxSuccess 100 prop_cond)),
      ("prop_eval_quote", check (withMaxSuccess 100 prop_eval_quote)),
      ("prop_gc_2",check (withMaxSuccess 100 prop_gc_2)),
      ("prop_progn_step", check (withMaxSuccess 100 prop_progn_step)),
      ("prop_add_single", check (withMaxSuccess 100 prop_add_single)),
      ("prop_add_inductive", check (verboseShrinking $ withMaxSuccess 1000 prop_add_inductive)),
      ("prop_define", check (withMaxSuccess 1000 prop_define)),
      ("prop_lambda", check (withMaxSuccess 1000 prop_lambda)),
      ("prop_closure", check (withMaxSuccess 1000 prop_closure)),
      ("prop_progn", check (withMaxSuccess 1000 prop_progn)),
      ("prop_var", check (withMaxSuccess 1000 prop_var)),
      ("prop_let", check (withMaxSuccess 1000 prop_let)),
      ("prop_app", check (withMaxSuccess 1000 prop_app))
    ]

generators = M.fromList
  [ ("arb-ctx", putStrLn =<< (prettyCtx <$> generate genCtx)),
    ("arb-type", putStrLn =<< (prettyType <$> generate (arbSizedType 15))),
    ("arb-exp", putStrLn =<< (do t <- generate $ arbSizedType 15
                                 s <- generate $ chooseInt (1,10)
                                 (_, e) <- generate $ genExp newCtx s t
                                 return $ prettyExp e))
  ]

plotSuccess :: Result -> String
plotSuccess s = "Num tests: " ++ (show (numTests s)) ++ "\n" ++
                "Num Discarded: " ++ (show (numDiscarded s)) ++ "\n" ++
                "Output: " ++ (output s) ++ "\n\n" ++
                "SUCCESS"

plotFailure :: Result -> String
plotFailure f = "Num tests: " ++ show (numTests f) ++ "\n" ++
                "Num Discarded: " ++ show (numDiscarded f) ++ "\n" ++
                "Seed: " ++ show (usedSeed f) ++ "\n" ++
                "Reason: " ++ (reason f) ++ "\n" ++
                "Output: " ++ (output f) ++ "\n\n" ++
                -- Show counterexample information
                concat (intersperse "\n" (failingTestCase f)) ++ "\n\n" ++
                "FAILURE"

plotResult :: Result -> String
plotResult s@Test.QuickCheck.Success{} = plotSuccess s
plotResult f@Test.QuickCheck.Failure{} = plotFailure f

runProp :: String -> Handle -> IO ()
runProp p_str logHandle = do
      let prop = M.lookup p_str properties
      case prop of
        (Just a) -> do
          putStrLn $ "Running property: " ++ p_str
          res <- a
          hPutStrLn logHandle "------------------------------------------------------------"
          hPutStrLn logHandle $ "Property: " ++ p_str
          hPutStrLn logHandle $ plotResult res
          hPutStrLn logHandle "------------------------------------------------------------"
          hFlush logHandle
        Nothing -> do hPutStrLn logHandle $ "Property not found: " ++ p_str


data Options = Options --ordering here needs to match order in parser (I think)
   { logfile :: String
   , prop :: String
   , allTests :: Bool
   }
   deriving Show

optParser :: Parser Options
optParser = Options
      <$> strOption
          ( long "logfile"
            <> value ""
            <> short 'l'
            <> metavar "FILENAME"
            <> help "Log results into this file" ) <*>
          strOption
          ( long "property"
            <> short 'p'
            <> value ""
            <> metavar "PROPNAME"
            <> help "Run this specific property") <*>
          switch
          ( long "all"
            <> short 'a'
            <> help "Run all tests")

  
main :: IO ()
main = do  
  args <- execParser (info ( optParser <**> helper) fullDesc)
  rawArgs <- getArgs
  logHandle <- if ((logfile args) == "") then return stdout else openFile (logfile args) WriteMode
  if (allTests args) 
    then mapM_ (\s -> runProp s logHandle) (M.keys properties)
    else if (prop args == "")
         then putStrLn "Some arguments are needed"
         else runProp (prop args) logHandle
