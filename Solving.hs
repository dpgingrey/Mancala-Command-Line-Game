module Solving where

import Game

import Control.Applicative ((<|>))
import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)
import Debug.Trace

type Rating = Int 

------------- Story Nine ---------------

whoWillWin :: Game -> Winner -- Doesn't need to consider no moves in validmoves because that is caught in checkwinner 
whoWillWin game@(turn, board) = case checkWinner game of -- I wonder if we can optimize this by checking if one player has too many marbles to catch up?
        (Just winstate) -> winstate
        Nothing         -> bestOutcome [whoWillWin (completeMoveUnsafe game move) | move <- validMoves game] 
    where   bestOutcome :: [Winner] -> Winner
            bestOutcome winlist
                | (Win turn) `elem` winlist = (Win turn)
                | Tie `elem` winlist = Tie
                | otherwise = Win (opponent turn)

----------------------------------------

------------- Story 18/19 --------------

whoMightWin :: Game -> Int -> (Rating, Move)
whoMightWin game@(turn, board) depth 
    | depth == 0 = error "Move depth can't be 0." 
    | null possibleMoves = (rateGame game, undefined)
    | depth == 1 = tripleToTuple (comparison (comparing (\(x,y,z) -> x)) possibleMoves)
    | otherwise = 
        case hasWinStateTriple possibleMoves of
            Nothing -> case hasWinState nextLayer of 
                            Nothing -> comparison (comparing fst) nextLayer
                            Just moveTuple -> moveTuple 
            Just triple -> tripleToTuple triple

    where possibleMoves = [(rateGame newGame, move, newGame) | move <- validMoves game, let newGame = completeMoveUnsafe game move]
          nextLayer = deepMoves possibleMoves
          comparison = if turn == P1 then (maximumBy) else (minimumBy)

          deepMoves :: [(Rating, Move, Game)] -> [(Rating, Move)]
          deepMoves [] = []
          deepMoves ((rating, possibleMove, newState):xs) = 
            if abs rating >= 400
            then (rating, possibleMove):(deepMoves xs)
            else ((fst (whoMightWin newState (depth-1))), possibleMove):(deepMoves xs)

          hasWinState :: [(Rating, Move)] -> Maybe (Rating, Move)
          hasWinState [] = Nothing
          hasWinState (ratingTuple@(rating, move):xs) = 
            if (rating >= 400 && turn == P1) || (rating <= -400 && turn == P2) 
            then Just ratingTuple 
            else hasWinState xs  

          hasWinStateTriple :: [(Rating, Move, Game)] -> Maybe (Rating, Move, Game)
          hasWinStateTriple [] = Nothing
          hasWinStateTriple (ratingTriple@(rating, move, game):xs) = 
            if (rating >= 400 && turn == P1) || (rating <= -400 && turn == P2) 
            then Just ratingTriple
            else hasWinStateTriple xs  

          tripleToTuple :: (a, b, c) -> (a, b)
          tripleToTuple (f,s,t) = (f,s) 


----------------------------------------

------------- Story Ten ----------------

bestMove :: Game -> Maybe Move -- Doesn't need to consider no moves in validmoves because that is caught in checkwinner 
bestMove game@(turn,board) = case checkWinner game of
    (Just winner) -> Nothing
    Nothing       -> lookup (Win turn) moveTuples
                 <|> lookup Tie moveTuples
                 <|> (Just $ snd $ head moveTuples)

    where moveTuples = [(whoWillWin (completeMoveUnsafe game move), move) | move <- (validMoves game)]

----------------------------------------