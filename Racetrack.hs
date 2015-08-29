import Data.Serialize
import qualified Data.ByteString.Char8 as BStr
import Text.Read(readMaybe)
import RacetrackGameData

zipT :: (a -> b -> c) -> (a, a) -> (b, b) -> (c, c)
zipT f (x,y) (x', y') = (f x x',f y y')


isCrossingTheLine :: (Point,Point)->Point->Point->Bool            
isCrossingTheLine line origin target = 
    let 
        reduce = uncurry $ zipT (-)
        (dx,dy) = reduce line
        (dx',dy') = reduce (origin, target)
    in 0 /= dx * dy' - dx' * dy 

updatePlayer :: Player -> Point -> Player    
updatePlayer player newVelocity = player { position = newPosition, velocity = newVelocity }
    where newPosition = zipT (+) (position player) newVelocity
         
         
         
playerInput :: Player -> IO Player
playerInput player = do
    velo' <- safeGetLine $ "You must type a tuple like this one " ++ show (velocity player)
    if uncurry (&&) $ zipT verify (velocity player) velo' then
        return $ updatePlayer player velo' 
    else do
        putStrLn "You must specify a valid new velocity"
        playerInput player
    where
        verify x x' = (x' - x) `elem` [-1, 0, 1]
            

  
playGame :: [Player] -> Game -> IO ()  
playGame allPlayers game = play allPlayers
    where
        play [] = error "No players"
        play players@(current:nexts) = do
            print current
            putStrLn $ prettyGameOutput players game
            current' <- playerInput current
            if playerIsInValidState current' game then
                if isCrossingTheLine (finishLine game) (position current) (position current') then
                    putStrLn $ "Player " ++ show (playerId current) ++ " won the game!"
                else
                    play (nexts ++ [current'])
            else
                putStrLn "Game over"

safeGetLine :: (Read a) => String ->  IO a
safeGetLine errorMessage = do 
    pt <- fmap readMaybe getLine
    case pt of
        Just p -> return p
        Nothing -> do
            putStrLn errorMessage
            safeGetLine errorMessage

main :: IO()
main = do
    decodedGame <- fmap decode $ BStr.readFile "game1.txt"
    putStrLn "How many player wants to play?"
    n <- safeGetLine "You must specify a number"
    case decodedGame of
        Left err -> putStrLn err
        Right game -> playGame (map (Player (startPosition game) (0,0)) [0..n]) game