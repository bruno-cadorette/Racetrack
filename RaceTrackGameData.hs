module RacetrackGameData (serializeGame, Game(..), Point, Cell(..), Player(..), playerIsInValidState, prettyGameOutput) where
import Data.List
import Data.Maybe
import Data.Matrix
import Data.Serialize
import qualified Data.ByteString.Char8 as BStr

type Point = (Int, Int)

data Player = Player { position :: Point, velocity :: Point, playerId :: Int } deriving (Show)

data Cell = Empty | Wall deriving (Show, Eq)

data Game = Game { gameBoard :: Matrix Cell, startPosition :: Point, finishLine :: (Point, Point) } deriving (Show)

instance Serialize Game where
    put = put . serializeGame
    get = fmap deserializeGame get
    
serializeGame :: Game -> BStr.ByteString
serializeGame (Game board start finish) = BStr.unlines $ (BStr.pack $ show start) : (BStr.pack $ show finish) : serializeBoard
    where
        serializeBoard = 
             map (BStr.pack . map serializeCell) $ toLists $ board
        serializeCell Empty = '0'
        serializeCell Wall = '1'
        
readBStr :: (Read a) => BStr.ByteString -> a        
readBStr = read . BStr.unpack

deserializeGame :: BStr.ByteString -> Game
deserializeGame str = Game getBoard (readBStr startPos) (readBStr line) 
    where
        (startPos:line:board) = BStr.lines $ BStr.filter (\x -> x /= '\r') str
        getBoard =  fromLists $ map (map deserializeCell . BStr.unpack) $ board
        deserializeCell '0' = Empty
        deserializeCell '1' = Wall
        deserializeCell e = error $ "The character "++ show e ++ " is invalid"
        
playerIsInValidState :: Player -> Matrix Cell -> Bool    
playerIsInValidState player board = 
    let (x, y) = position player
    in getElem x y board == Empty        
    
prettyGameOutput :: [Player] -> Game -> String
prettyGameOutput players (Game board _ line)  = 
    unlines $ toLists $ matrix (nrows board) (ncols board) (\(i,j)->printCell (getElem i j board) (i, j))
        where
        printCell cell pos = 
            getFirstOrDefault ' ' 
                [(const 'F') <$> (elemIsFinishPoint pos),
                (const '+') <$> (whenMaybe (Wall ==) cell),
                (head . show . playerId) <$> (find (\p -> pos == position p) players)]
                
        getFirstOrDefault defValue = fromJust . fromMaybe (Just defValue) . find isJust
        whenMaybe p x = if p x then Just x else Nothing
        elemIsFinishPoint = whenMaybe (\x-> (\(a,b)-> a==x||b==x) $ line)    
    
    