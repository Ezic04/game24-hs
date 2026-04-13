module Main (main) where

import Control.Category ((>>>))
import Control.Monad (replicateM)
import Control.Monad.Reader (ReaderT, ask, lift, runReaderT)
import Data.Bifunctor (Bifunctor (first))
import Data.Enum (enumerate)
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.MultiSet (MultiSet, (\\), fromList)
import Data.Text (pack)
import Expr (BinOpType, ExprNat, UnrOpType, eval)
import ExprParser (pExprEof)
import Numeric.Natural (Natural)
import System.Console.Haskeline (defaultSettings, getInputLine, runInputT)
import System.Random (randomRIO)
import Text.Megaparsec (errorBundlePretty, parse)
import Data.Char (toLower)

data Command
  = ExprStr String
  | Quit
  | Reroll

data RoundResult
  = Won
  | Rerolled
  | Aborted

data AttemptResult 
  = Success 
  | Failure

type RoundM a = ReaderT (MultiSet Natural) IO a

main :: IO ()
main = gameLoop

gameLoop :: IO ()
gameLoop = do
  clearTerminal
  state <- playRound
  case state of
    Aborted -> return ()
    Rerolled -> gameLoop
    Won -> readPlayAgain
 
readPlayAgain :: IO ()
readPlayAgain = runInputT defaultSettings $ do
      minput <- getInputLine "Do you want to play again? [y/n]"
      case minput of
        Just input | map toLower input `elem` ["y", "yes"] -> lift gameLoop
        _ -> return ()
 
playRound :: IO RoundResult
playRound = do
  randsInt <- replicateM 4 $ randomRIO (1, 9 :: Int)
  let randNumsMSet = fromList $ fromIntegral <$> randsInt
      randNumsStr = csShow randNumsMSet
  printBanner randNumsStr
  runReaderT playTurn randNumsMSet

printBanner :: String -> IO ()
printBanner randNumsStr = do
  putStr "\n"
  putStrLn "--------------- GAME 24 ---------------"
  putStrLn $ "Goal: Reach 24 using each number once."
  putStrLn $ "Ops:  [ " ++ binary ++ " ]   [ " ++ unary ++ " ]"
  putStrLn $ "Cmds: [:r] Reroll   [:q] Quit"
  putStrLn "---------------------------------------"
  putStrLn $ "Nums: " ++ randNumsStr
  putStrLn "---------------------------------------"
  where
    binary = csShow (enumerate :: [BinOpType])
    unary = csShow (enumerate :: [UnrOpType])

playTurn :: RoundM RoundResult
playTurn = do
  cmd <- lift readCommand
  case cmd of
    Quit -> return Aborted
    Reroll -> return Rerolled
    ExprStr str -> processExpression str

readCommand :: IO Command
readCommand = runInputT defaultSettings $ do
  minput <- getInputLine "Enter expression: "
  return $ case minput of
    Nothing -> Quit
    Just ":q" -> Quit
    Just ":r" -> Reroll
    Just input -> ExprStr input

processExpression :: String -> RoundM RoundResult
processExpression str = do
  nats <- ask
  case validateExpr nats str of
    Left msg -> do
      lift $ putStrLn msg
      playTurn
    Right expr -> do
      let (msg, result) = checkAttempt expr
      lift $ putStrLn msg
      case result of
        Success -> return Won
        Failure -> playTurn

validateExpr :: MultiSet Natural -> String -> Either String ExprNat
validateExpr availableNats strExpr = do
  expr <- parseExpr strExpr
  let usedNats = fromList $ toList expr
      diff = usedNats \\ availableNats
  if null diff
    then return expr
    else
      let availableStr = csShow availableNats
          usedStr = csShow diff
       in Left $ "Expression can contain only: " ++ availableStr ++ "\nBut contains: " ++ usedStr

checkAttempt :: ExprNat -> (String, AttemptResult)
checkAttempt expr =
  case eval expr of
    Just 24 -> (success, Success)
    Nothing -> (evalFailed, Failure)
    Just r -> (wrongValue r, Failure)
  where
    success = "success!"
    evalFailed = "Expression evaluation failed"
    wrongValue r = "Expression should evaluate to 24\nBut evaluates to: " ++ show r

parseExpr :: String -> Either String ExprNat
parseExpr = pack >>> parse pExprEof "" >>> first bundleFmt
  where
    bundleFmt = errorBundlePretty >>> dropFirstLine
    dropFirstLine = dropWhile (/= '\n') >>> drop 1

clearTerminal :: IO ()
clearTerminal = putStr "\ESC[2J\ESC[H"

csShow :: (Show a, Foldable f) => f a -> String
csShow = toList >>> fmap show >>> intercalate ", "
