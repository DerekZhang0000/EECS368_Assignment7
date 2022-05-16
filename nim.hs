type Board = [Int]  --initializes starting board
initial :: [Int]
initial = [5,4,3,2,1]

--recursively plays the game until a player wins
play :: [Int] -> Int -> IO ()
play board player = do
    printBoard board
    putChar '\n'
    if gameOver board then
        do  -- victory message when a player wins
            putStr "Player "
            putStr (show (player `mod` 2 + 1))
            putStrLn " wins."
    else
        do
            putStr "Player "    -- displays who's turn it is
            putStr (show player)
            putStrLn "'s Turn."
            putStr "Enter row number: "
            row <- getLine
            if (read row :: Int) < 1 || (read row :: Int) > 5 then
                do  -- error message for invalid row index
                    putStrLn "Invalid row index."
                    play board player   -- restarts turn if selected row is out of bounds
            else
                do
                    putStr "Enter stars to remove: "
                    stars <- getLine
                    putChar '\n'
                    if isValid board (read row :: Int) (read stars :: Int) then
                        play (newBoard board (read row :: Int) (read stars :: Int)) (player `mod` 2 + 1)    -- updates board and changes player if move is valid
                    else
                        do  -- error message for invalid number of stars
                            putStrLn "Invalid number of stars."
                            play board player   -- restarts turn if number of stars taken exceeds the number of stars at the selected row

-- prints current game board
printBoard :: [Int] -> IO ()    -- prints 5 lines, with each corresponding to the integers in the array
printBoard board = putStr $ unlines [show row ++ ": " ++ replicate stars '*'| (stars, row) <- zip board [1..length board]]

-- returns the new board after a move is played
newBoard :: [Int] -> Int -> Int -> [Int]    -- subtracts taken stars from the chosen row
newBoard board row takenStars = [if rowIndex == row then stars - takenStars else stars | (stars, rowIndex) <- zip board [1..length board]]

-- returns whether or not a move is valid
isValid :: [Int] -> Int -> Int -> Bool  -- returns whether or not the stars taken is less than or equal to the stars in the row
isValid board row stars = (board !! (row - 1)) >= stars

-- returns whether or not the game is over
gameOver :: [Int] -> Bool   -- if all values in the list are 0, the game is over
gameOver = all (==0)

-- main function, plays a game of nim
nim :: IO ()   -- starts a game with player 1 going first
nim = play initial 1