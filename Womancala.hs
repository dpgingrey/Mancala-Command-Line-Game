module Womancala where

import Solving
import Game

import Text.Read (readMaybe)
import Data.List.Split (splitOn)
import System.Environment (getArgs)

import System.Console.GetOpt
import System.Exit (exitFailure, exitSuccess)
import System.IO (hFlush, stdout)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<|>))

--Main file for the Womancala game

------------- Story Five ---------------
--To see correct indentation in ghci, put putStr before the prettyPrint call and pass in the game you want printed
--It should print well, if not, let me (Paige) know
--Template:
-- Current turn: P_
-- -----------------------------------------
-- |    |    |    |    |    |    |    |    |
-- |    |-----------------------------|    |
-- |    |    |    |    |    |    |    |    |
-- -----------------------------------------

prettyPrint :: Game -> String
prettyPrint (turn,board@(sideOne,pitOne,sideTwo,pitTwo)) = "Current turn: "++(show turn)++"\n"++
                            (printLine "\x2554" '\x2550' "\x2564" 7 "\x2557")++"\n"++
                            "\x2551   "++(prettyPrintSide sideTwo (reverse (pits P2)))++"\n"++
                            prettyPrintMiddle [pitOne,pitTwo] ++"\n"++
                            "\x2551   "++(prettyPrintSide sideOne (pits P1))++"\n"++
                            (printLine "\x255A" '\x2550' "\x2567" 7 "\x255D") ++ "\n"
  where --
        prettyPrintSide :: [Pit] -> [Index]-> String --Will print a side minus the first "|"
        prettyPrintSide board [] = " \x2502    \x2551"
        prettyPrintSide board (index:indexes) = " \x2502 "++(spacedLookup index board) ++ prettyPrintSide board indexes
        --
        prettyPrintMiddle :: [Pit] -> String
        prettyPrintMiddle board = "\x2551 " ++
                                  (spacedLookup (store P2) board) ++
                                  (printLine " \x251C" '\x2500' "\x253C" 5 "\x2524 ")++
                                  (spacedLookup (store P1) board) ++
                                  " \x2551"
        --
        spacedLookup :: Int -> [Pit] -> String
        spacedLookup key list = if result>9 then show result else " "++(show result)
            where result = lookUpUnsafe key list
        --
        printLine :: String -> Char -> String -> Int ->String -> String
        printLine leftCorner middleLines middleTs repeatLength rightCorner =
            leftCorner++(take 4 (repeat middleLines))++(aux (middleTs++(take 4 (repeat middleLines))) repeatLength)++rightCorner
            where aux string 1 = string
                  aux string num = string++(aux string (num-1))

----------------------------------------

----------- Story Twelve ---------------

readGame :: String -> Game
readGame str = (turn, board)
      where listOfLines = lines str
            (turnString:pitStrings) = if (length listOfLines)==15 then listOfLines else error "File format is incorrect"
            turn = case (stringToPlayer turnString) of 
                    Just value -> value
                    Nothing -> error "readGame couldn't find the turn"
            pits = makePits pitStrings
            board = makeBoard pits
            --
            makePits :: [String] -> [Pit]
            makePits [] = []
            makePits (str:strs) =
                let stringNums      = splitOn " " str
                    readIndex       = readMaybe (head stringNums)
                    Just index      = if readIndex==Nothing then error "Incorrect index in board" else readIndex
                    readNumMarbles  = readMaybe (last stringNums)
                    Just numMarbles = if readNumMarbles==Nothing then error "Incorrect marbles in board" else readNumMarbles 
                in ((index,numMarbles):(makePits strs))
            --
            makeBoard :: [Pit] -> Board
            makeBoard sourcePits = ((getPits sourcePits [1..6]), (head (getPits sourcePits [7])),(getPits sourcePits [8..13]), (head (getPits sourcePits [0])))
                where getPits :: [Pit] -> [Index] -> [Pit]
                      getPits _ [] = []
                      getPits [] _ = []
                      getPits (pit@(id,nm):ps) indexes = if id `elem` indexes
                                                       then (pit:(getPits ps indexes))
                                                       else (getPits ps indexes)
            --
            stringToPlayer :: String -> Maybe Player
            stringToPlayer str = case str of 
                                "P1" -> Just P1
                                "P2" -> Just P2
                                otherwise -> error ("readGame: Turn isn't in correct format - " ++ str)





----------------------------------------

----------- Story Thirteen -------------

showGame :: Game -> String
showGame game@(turn,board@(pitsOne,storeOne,pitsTwo,storeTwo)) = unlines ((show turn):((aux [storeTwo])++(aux pitsOne)++(aux [storeOne])++(aux pitsTwo)))
    where aux :: [Pit] -> [String]
          aux [] = []
          aux ((index,numMarbles):ps) = (((show index)++" "++(show numMarbles)):aux ps)

----------------------------------------

----------- Story Fourteen -------------

writeGame :: Game -> FilePath -> IO ()
writeGame game filepath = writeFile filepath $ showGame game

loadGame :: FilePath -> IO Game
loadGame filepath = 
    do  game <- readFile filepath
        return $ readGame game 

putBestMove :: Game -> IO ()
putBestMove game = 
        case bestMove game of 
            Just move -> do putStr $ "Best Move: " ++ show move ++ "\n" 
                            let newGame = completeMoveUnsafe game move
                            case checkWinner newGame of 
                                Just (Win player) -> putStr ("Game over, winner is " ++ show player ++ "!\n")
                                Just (Tie)        -> putStr "Game over, tie!\n"
                                Nothing           -> putStr $ prettyPrint $ newGame 
            Nothing   -> do putStr $ "Game is already complete.\n" 
                            putStr $ prettyPrint game

----------------------------------------
------------------MAIN------------------
main :: IO ()
main = do
    (flags, nonOpts) <- parseFlags
    case nonOpts of
        [filepath] -> do
            game <- loadGame filepath
            if Winner `elem` flags then 
                winnerFlag flags game
            else if hasMove flags then
                moveFlag flags game
            else if Help `elem` flags then
                helpFlag flags game
            else if Print `elem` flags then
                printFlag flags game
            else if Interactive `elem` flags then
                interactiveFlag flags game
            else baseCase flags game
        _ -> do
            putStrLn "Please provide a game file."
            putStrLn helpMessage
            exitFailure

hasMove :: [Flag] -> Bool
hasMove [] = False
hasMove (Move _:xs) = True
hasMove(x:xs) = hasMove xs

----------------------------------------

printFlag :: [Flag] -> Game -> IO ()
printFlag flags game = putStrLn $ prettyPrint game

----------- Story Twenty Two -----------
-- Support the  -w, --winner flag. Uses Story 9/Story 10 in the second sprint with no cut-off depth.
winnerFlag :: [Flag] -> Game -> IO ()
winnerFlag flags game@(turn, _) = do
    case bestMove game of
        Nothing -> do
            putStrLn "Game is already complete."
            exitSuccess
        Just mv -> do
            putStrLn (show mv)

            -- Story 26 - Verbose
            if hasFlag (== Verbose) flags
                then do
                    let nextGame = completeMoveUnsafe game mv
                    if hasFlag (== Winner) flags
                        then do
                            let outcome = whoWillWin nextGame
                            putStrLn ("Quality: " ++ resultString turn outcome)
                        else do
                            putStrLn ("Rating: " ++ show (rateGame nextGame))
                else return ()

----------------------------------------

baseCase :: [Flag] -> Game -> IO ()
baseCase flags game@(turn, board) = if checkWinner game /= Nothing then putStrLn "Game is already over." else  
    if Verbose `elem` flags then do
            case checkWinnerFromRating rating of
                Just a -> putStrLn ("Best Move: " ++ (show move) ++ " Outcome: " ++ (show a))
                Nothing -> putStrLn ("Best Move: " ++ (show move) ++ " Rating: " ++ (show rating))
        else 
            putStrLn ("Best Move: " ++ (show move))
    where (rating, move) = case checkDepthInFlags flags of 
            Nothing -> whoMightWin game 8
            Just depth -> whoMightWin game depth

checkWinnerFromRating :: Rating -> Maybe Winner
checkWinnerFromRating rating 
    | rating == 400  = Just $ Win P1
    | rating == -400 = Just $ Win P2
    | rating == 0 = Just Tie
    | otherwise = Nothing
----------- Story Twenty Four ----------
-- Supports the -h and --help 
helpFlag :: [Flag] -> Game -> IO ()
helpFlag flags game@(turn, _) = do
    if hasFlag (== Help) flags
        then do putStrLn helpMessage
                exitSuccess
        else return ()

----------------------------------------

----------- Story Twenty Five ----------
-- Supports the -m <move> and --move <move> 
moveFlag :: [Flag] -> Game -> IO ()
moveFlag flags game@(turn, _) = do
    case firstMoveStr flags of
        Just mvStr ->
            case parseMove mvStr of
                Nothing -> do
                    putStrLn "Invalid input for move."
                    exitFailure
                Just mv ->
                    case completeMove game mv of
                        Nothing -> do
                            putStrLn "Move was invalid."
                            exitFailure
                        Just newGame -> do
                            if hasFlag (== Verbose) flags
                                then putStr (prettyPrint newGame)
                                else putStrLn (showGame newGame)
                            exitSuccess
        Nothing -> return ()

----------------------------------------

---------- Interactive Mode (27) -------
-- Support the -i flag, needs -d and -2p -2c
interactiveFlag :: [Flag] -> Game -> IO ()
interactiveFlag flags game = 
    if TwoPlayer `elem` flags then do
        winner <- twoPlayerGame game
        printWinner winner
    else if TwoComputer `elem` flags then do
        depth <- getDepthFromPlayer
        winner <- noPlayerGame game (fromMaybe 8 (checkDepthInFlags flags)) depth
        printWinner winner
    else do
        playerTurn <- getTurnFromPlayer
        winner <- onePlayerGame playerTurn game $ fromMaybe 8 (checkDepthInFlags flags)
        printWinner winner

printWinner :: Winner -> IO ()
printWinner (Win P1) = putStrLn "P1 wins!"
printWinner (Win P2) = putStrLn "P2 wins!"
printWinner Tie      = putStrLn "It's a tie!"

checkDepthInFlags :: [Flag] -> Maybe Int
checkDepthInFlags [] = Nothing
checkDepthInFlags ((Depth d):xs) = 
    case readMaybe d of
        Just n -> Just n
        Nothing -> error "Invalid value for depth flag."
checkDepthInFlags (x:xs) = checkDepthInFlags xs

getDepthFromPlayer :: IO Int
getDepthFromPlayer = do
    putStrLn "What is the depth for P2?"
    num <- getLine
    case readMaybe num of
        Just n -> return n
        Nothing -> do
            putStrLn"Invalid depth, try again."
            getDepthFromPlayer

twoPlayerGame :: Game -> IO Winner
twoPlayerGame game@(turn, board) =
    case checkWinner game of
        Just w -> return w
        Nothing -> do
            putStrLn $ prettyPrint game 
            move <- getMoveFromPlayer game
            twoPlayerGame $ completeMoveUnsafe game move

getMoveFromPlayer :: Game -> IO Move
getMoveFromPlayer game@(turn, board) = do
    putStrLn $ (show turn) ++ ", what is your move?"
    response <- getLine
    case readMaybe response of 
        Just num -> 
            if isValidMove game num 
            then return num
            else do
                putStrLn "Invalid Move! Try again."
                getMoveFromPlayer game
        Nothing -> do
            putStrLn "Invalid Move! Try again."
            getMoveFromPlayer game

onePlayerGame :: Turn -> Game -> Int -> IO Winner 
onePlayerGame playerTurn game@(turn, board) depth = 
    case checkWinner game of
        Just w -> return w
        Nothing -> do
            if turn == playerTurn then do
                putStrLn $ prettyPrint game
                move <- getMoveFromPlayer game
                onePlayerGame playerTurn (completeMoveUnsafe game move) depth
            else do
                putStrLn $ "Computer's Turn..."
                putStrLn $ prettyPrint game
                onePlayerGame playerTurn (completeMoveUnsafe game (snd (whoMightWin game depth))) depth
    
getTurnFromPlayer :: IO Turn
getTurnFromPlayer = do 
    putStrLn "Are you P1 or P2?"
    response <- getLine
    case response of 
        "P1" -> return P1
        "P2" -> return P2
        otherwise -> getTurnFromPlayer

noPlayerGame :: Game -> Int -> Int -> IO Winner
noPlayerGame game@(turn, board) depth1 depth2 = 
    case checkWinner game of
        Just w -> return w
        Nothing -> do
            if turn == P1 then do
                putStrLn $ prettyPrint game
                noPlayerGame (completeMoveUnsafe game (snd (whoMightWin game depth1))) depth1 depth2
            else do
                putStrLn $ prettyPrint game
                noPlayerGame (completeMoveUnsafe game (snd (whoMightWin game depth2))) depth1 depth2
----------------------------------------

------- Command Line Interface ----------

-- Possible flags for Womancala
data Flag
    = Winner        -- -w, --winner  (story 22)
    | Verbose       -- -v, --verbose (story 26)
    | Depth String  -- -d <number>, --depth <number> (story 23)
    | Move String   -- -m <move>, --move <move> (story 25)
    | Help          -- -h, --help (story 24)
    | Print         -- -p, --print 
    | Interactive   -- -i, --interactive (story 27) 
    | TwoPlayer     -- -2, --twoplayer
    | TwoComputer   -- -c, --twocomputer
    deriving (Eq, Show)

-- Flags recognized by GetOpt
options :: [OptDescr Flag]
options =
    [ Option ['w'] ["winner"] (NoArg Winner) "Print the ultimate best move."
    , Option ['v'] ["verbose"] (NoArg Verbose) "Verbose output / pretty print in -m mode."
    , Option ['d'] ["depth"] (ReqArg Depth "NUMBER") "Specify cutoff depth for best move calculation of the AI, P1 for interactive mode with 2 AI."
    , Option ['m'] ["move"] (ReqArg Move "MOVE") "Make the specified move and print the resulting board."
    , Option ['h'] ["help"] (NoArg Help) "Show help message."
    , Option ['p'] ["print"] (NoArg Print) "Print out the input board"
    , Option ['i'] ["interactive"] (NoArg Interactive) "Play against the computer, specify depth with -d"
    , Option ['2'] ["twoplayer"] (NoArg TwoPlayer) "Play against another human in the interactive mode."
    , Option ['c'] ["twocomputer"] (NoArg TwoComputer) "Have two computers play against each other in interactive mode."
    ]

-- Displayed when -h / --help is used or when invalid input is given
helpMessage :: String
helpMessage = usageInfo ("Board Syntax:\n"++
                        "-----------------------------------------\n"++
                        "|    | 13 | 12 | 11 | 10 |  9 |  8 |    |\n"++
                        "| S2 |-----------------------------| S1 |\n"++
                        "|    |  1 |  2 |  3 |  4 |  5 |  6 |    |\n"++
                        "-----------------------------------------\n\n"++
                        "Womancala Help:\n\nUsage: womancala [options] <gamefile>\n\nOptions:") options

-- Parses flags, returns them with their arguments
parseFlags :: IO ([Flag], [String])
parseFlags = do
    args <- getArgs
    let (flags, nonOpts, errs) = getOpt Permute options args
    if not (null errs)
        then do
            putStrLn (concat errs ++ "\n" ++ helpMessage)
            exitFailure
        else return (flags, nonOpts)

-- Helpers
firstDepth :: [Flag] -> Maybe String
firstDepth [] = Nothing
firstDepth (Depth d : _) = Just d
firstDepth (_ : fs) = firstDepth fs

firstMoveStr :: [Flag] -> Maybe String
firstMoveStr [] = Nothing
firstMoveStr (Move m : _) = Just m
firstMoveStr (_ : fs) = firstMoveStr fs

hasFlag :: (Flag -> Bool) -> [Flag] -> Bool
hasFlag _ [] = False
hasFlag p (f:fs) = p f || hasFlag p fs

parseMove :: String -> Maybe Move
parseMove s =
    case splitOn "," s of
        [_] -> readMaybe s 
        _   -> Nothing 

-- Turns winner result into "win/lose/tie" for the current player.
resultString :: Player -> Winner -> String
resultString turn result =
    case result of
        Tie       -> "tie"
        Win p     -> "win " ++ show (p)