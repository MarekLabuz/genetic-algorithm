import Control.Monad.Trans
import Control.Monad.Trans.State
import System.Console.Haskeline
import System.Random (randomRIO)
import Data.List

if' :: Bool -> a -> a -> a
if' True x _ = x
if' False _ y = y

data Player = Human | Machine deriving (Show)
data Cell = Empty | Hidden | Visible | Hit | Miss deriving (Eq)
type Board = [[Cell]]
type Coords = (Int, Int)

instance Show Cell where
  show Empty = "|_"
  show Hidden = "|_"
  show Miss = "|○"
  show Hit = "|✖"
  show Visible = "|◘"

startingBoard :: Board
startingBoard = take 5 $ repeat $ take 5 $ repeat Empty

boardToString :: Board -> Int -> String
boardToString board row = foldl (\a c -> a ++ (show c)) "" (board !! row)

printBoardRow :: Board -> Int -> String
printBoardRow b row = (show $ row + 1) ++ (boardToString b row) ++ "|"

printRow :: Board -> Board -> Int -> String
printRow hb mb row = (printBoardRow hb row) ++ "               " ++ (printBoardRow mb row)

printBoard :: Board -> Board -> IO ()
printBoard hb mb = do
  putStrLn "\ESC[2J"
  putStrLn "    -- Your Board --         -- Enemy's Board --"
  putStrLn "       A B C D E                  A B C D E     "
  putStrLn $ "     " ++ (printRow hb mb 0)
  putStrLn $ "     " ++ (printRow hb mb 1)
  putStrLn $ "     " ++ (printRow hb mb 2)
  putStrLn $ "     " ++ (printRow hb mb 3)
  putStrLn $ "     " ++ (printRow hb mb 4)

replaceItemInArr :: [a] -> Int -> a -> [a]
replaceItemInArr arr i item = (take i arr) ++ [item] ++ (drop (i + 1) arr)

setCell :: Board -> Coords -> Cell -> Board
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

getCoords :: Maybe String -> Coords
getCoords Nothing = (1, 1)
getCoords (Just s) = let h:t = s in (getRowByChar $ t !! 0, getColByChar h)

getRandom :: Int -> Int -> IO Int
getRandom from to = randomRIO (from, to)

getRandomCoordL :: Board -> Coords -> [Cell] -> IO Coords
getRandomCoordL b (i, j) c
  | elem (b !! i !! j) c = return (i, j)
  | otherwise = do
    i <- randomRIO (0, 4)
    j <- randomRIO (0, 4)
    getRandomCoordL b (i, j) c

getRandomCoord :: Board -> [Cell] -> IO Coords
getRandomCoord b c = do
  i <- randomRIO (0, 4)
  j <- randomRIO (0, 4)
  getRandomCoordL b (i, j) c

getRandomBoardL :: Board -> Int -> Cell -> IO Board
getRandomBoardL b n c
  | n == 0 = return b
  | otherwise = do
    coord <- getRandomCoord b [Empty]
    getRandomBoardL (setCell b coord c) (n - 1) c

setBoardRandomly :: Board -> Cell -> IO Board
setBoardRandomly b c = getRandomBoardL b 5 c

placeBattleships :: StateT (Board, Int) (InputT IO) Board
placeBattleships = do
  (hb, i) <- get
  if i == 0 then return hb
  else do
    liftIO $ printBoard hb startingBoard
    liftIO $ putStrLn $ "\nPlace your battleship (e.g. A1, D3, B5 etc.): "
    s <- lift $ getInputLine "> "
    let (x, y) = getCoords s
    let j = if' (hb !! x !! y == Empty) (i - 1) i
    put (setCell hb (getCoords s) Visible, j)
    placeBattleships

numberOfHits :: Board -> Int
numberOfHits b = foldl (\a1 row -> foldl (\a2 el -> if' (el == Hit) (a2 + 1) a2) a1 row) 0 b

attackCell :: Cell -> Cell
attackCell cell
  | cell == Empty || cell == Miss = Miss
  | cell == Visible || cell == Hidden || cell == Hit = Hit

attack :: Board -> Coords -> Board
attack b (i, j) = do
  let newCell = attackCell $ b !! i !! j
  setCell b (i, j) newCell

fight :: StateT (Board, Board) (InputT IO) String
fight = do
  (hb, mb) <- get
  liftIO $ printBoard hb mb
  liftIO $ putStrLn $ "\nAttack enemy's battleship (e.g. A1, D3, B5 etc.): "
  s <- lift $ getInputLine "> "
  let newMb = attack mb $ getCoords s
  coord <- liftIO $ getRandomCoord hb [Empty, Visible]
  let newHb = attack hb $ coord
  put (newHb, newMb)
  if numberOfHits newHb == 5 then return "\nYou lost..."
  else if numberOfHits newMb == 5 then return "\nYou won!"
  else fight

game :: IO ()
game = do
  (hb, _) <- runInputT defaultSettings (runStateT placeBattleships (startingBoard, 5))
  -- hb <- setBoardRandomly startingBoard Visible
  mb <- setBoardRandomly startingBoard Hidden
  (winner, (hb, mb)) <- runInputT defaultSettings (runStateT fight (hb, mb))
  printBoard hb mb
  putStrLn winner

main = do
  game
