import Control.Monad.Trans
import Control.Monad.Trans.State
import System.Console.Haskeline
import Data.List

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

data Player = Human | Machine deriving (Show)
data Cell = Empty | Hidden | Visible | Hit | Miss
type Board = [[Cell]]

instance Show Cell where
  show Empty = "|_"
  show Hidden = "|_"
  show Miss = "|○"
  show Hit = "|✖"
  show Visible = "|◘"

defaultBoard :: Board
defaultBoard = take 5 $ repeat $ take 5 $ repeat Empty

boardToString :: Board -> Int -> String
boardToString board row = foldl (\a c -> a ++ (show c)) "" (board !! row)

humanBoard = defaultBoard
machineBoard = defaultBoard

printBoardRow :: Board -> Int -> String
printBoardRow b row = (show $ row + 1) ++ (boardToString b row) ++ "|"

printRow :: Board -> Board -> Int -> String
printRow hb mb row = (printBoardRow hb row) ++ "               " ++ (printBoardRow mb row)

printBoard :: Board -> Board -> IO ()
printBoard hb mb = do
  putStrLn "    -- Your Board --         -- Enemy's Board --"
  putStrLn "       A B C D E                  A B C D E     "
  putStrLn $ "     " ++ (printRow hb mb 0)
  putStrLn $ "     " ++ (printRow hb mb 1)
  putStrLn $ "     " ++ (printRow hb mb 2)
  putStrLn $ "     " ++ (printRow hb mb 3)
  putStrLn $ "     " ++ (printRow hb mb 4)

replaceItemInArr :: [a] -> Int -> a -> [a]
replaceItemInArr arr i item = (take i arr) ++ [item] ++ (drop (i + 1) arr)

setCell :: Board -> (Int, Int) -> Cell -> Board
setCell board (r, c) cell = let
  upperBoard = take r board
  lowerBoard = drop (r + 1) board
  row = board !! r
  leftRow = take c row
  rightRow = drop (c + 1) row
  in upperBoard ++ [leftRow ++ [cell] ++ rightRow] ++ lowerBoard

getRowByChar :: Char -> Int
getRowByChar '1' = 0
getRowByChar '2' = 1
getRowByChar '3' = 2
getRowByChar '4' = 3
getRowByChar '5' = 4

getColByChar :: Char -> Int
getColByChar 'A' = 0
getColByChar 'B' = 1
getColByChar 'C' = 2
getColByChar 'D' = 3
getColByChar 'E' = 4

getCoords :: Maybe String -> (Int, Int)
getCoords Nothing = (1, 1)
getCoords (Just s) = let h:t = s in (getRowByChar $ t !! 0, getColByChar h)

game :: StateT Board (InputT IO) ()
game = do
  hb <- get
  -- liftIO $ putStrLn "\ESC[2J"
  liftIO $ printBoard hb machineBoard
  liftIO $ putStrLn "\nPlace your battleship (e.g. A1, D3, B5 etc.):"
  s <- lift $ getInputLine "> "
  let coords = getCoords s
  let newHb = setCell hb coords Visible
  put newHb
  game

-- main :: IO ()
main = do
  runInputT defaultSettings (runStateT game humanBoard)
