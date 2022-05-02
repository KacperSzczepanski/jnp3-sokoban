{-# LANGUAGE OverloadedStrings #-}
import System.IO
-------------------------------------------------------------------------
--list functions
elemList :: Eq a => a -> [a] -> Bool
elemList a list = foldList (\x y -> if x == a then True else y) False list

appendList :: [a] -> [a] -> [a]
appendList l1 l2 = foldList (\x y -> x:y) l2 l1

listLength :: [a] -> Integer
listLength list = foldList (\x y -> y + 1) 0 list

filterList :: (a -> Bool) -> [a] -> [a]
filterList func list = foldList (\x y -> if func x then x:y else y) [] list

nth :: [a] -> Integer -> a
data BIPair a = BIP Integer a
nth [] _ = error "out of index"
nth a@(h:t) n
    | n < 0 = error "out of index"
    | n >= listLength a = error "out of index"
    | otherwise = unpack (foldList (\x y@(BIP cnt z) -> if cnt == 0 then BIP (-1) x else BIP (cnt - 1) z) (BIP (listLength a - n - 1) h) (h:t)) where
        unpack (BIP n elem) = elem

mapList :: (a -> b) -> [a] -> [b]
mapList func list = foldList (\x y -> func x:y) [] list

andList :: [Bool] -> Bool
andList list = foldList (\x y -> y && x) True list

allList :: (a-> Bool) -> [a] -> Bool
allList func list = foldList (\x y -> y && (func x)) True list

foldList :: (a -> b -> b) -> b -> [a] -> b
foldList func b [] = b
foldList func b (h:t) = func h (foldList func b t)
-------------------------------------------------------------------------
storage, wall, ground, box, boxOk, player, playerOnStorage :: Char
player = '@'
playerOnStorage = '+'
box = '$'
boxOk = '*'
ground = ' '
wall = '#'
storage = '.'
blankTile = ' '

data Tile = Storage | Wall | Ground | Box | BoxOk | Blank deriving Eq
drawTile :: Bool -> Tile -> Char
drawTile playerOn Storage
  | playerOn == True = playerOnStorage
  | otherwise        = storage
drawTile _ Wall      = wall
drawTile playerOn Ground
  | playerOn == True = player
  | otherwise        = ground
drawTile _ Box       = box
drawTile _ BoxOk     = boxOk
drawTile _ Blank     = blankTile

--Coord, Direction and additional functions declarations
data Direction = R | U | L | D deriving Eq
data Coord = C Integer Integer deriving (Show, Eq)

getBoxesFromList :: (Coord -> Tile) -> [Coord] -> [Coord]
getBoxesFromList maze list = foldList isBox [] list where
  isBox x y
    | maze x == Box   = x:y
    | maze x == BoxOk = x:y
    | otherwise       = y

data Fours = F Integer Integer Integer Integer deriving Show
getFours :: Fours -> Integer -> Integer
getFours (F a b c d) n
  | n == 1 = a
  | n == 2 = b
  | n == 3 = c
  | n == 4 = d
  | otherwise = 0

instance Eq Fours where
  F a b c d == F a' b' c' d' = a == a' && b == b' && c == c' && d == d'

--declaring state
data State = St {
    playerPos :: Coord,
    dir :: Direction,
    boxList :: [Coord],
    mapNr :: Integer,
    corners :: Fours,
    noOfMoves :: Integer
    }

instance Eq State where
  St a b c d e f == St a' b' c' d' e' f' = a == a' && b == b' && c == c' && d == d' && e == e' && f == f'

type Screen = String

--remove and add boxes functions
removeBoxes :: (Coord -> Tile) -> (Coord -> Tile)
removeBoxes func = newFunc
  where
    newFunc :: Coord -> Tile
    newFunc coord
      | func coord == Box   = Ground
      | func coord == BoxOk = Storage
      | otherwise           = func coord

addBoxes :: [Coord] -> (Coord -> Tile) -> (Coord -> Tile)
addBoxes coords func = newFunc
  where
    newFunc :: Coord -> Tile
    newFunc coord
      | elemList coord coords == True && func coord == Storage = BoxOk
      | elemList coord coords == True                          = Box
      | otherwise                                              = func coord
  
getUpdatedTile :: (Coord -> Tile) -> [Coord] -> (Coord -> Tile)
getUpdatedTile func boxList = addBoxes boxList (removeBoxes func)
  
--draw map
draw :: State -> Screen
draw state@(St playerPos dir boxList level corners moves)
  | isWinning state == True = endScreen moves
  | otherwise = (foldList appendList "" [drawRow y | y <- [maxY,(maxY - 1)..minY]])
  where
    minX = getFours corners 1
    maxY = getFours corners 2
    maxX = getFours corners 3
    minY = getFours corners 4
    drawRow y = (foldList appendList "\n" [[drawTile (playerPos == C x y) (getUpdatedTile (getTile (nth mazesList level)) boxList (C x y))] | x <- [minX..maxX]])

--help functions
adjacentCoord :: Coord -> Direction -> Coord
adjacentCoord (C x y) U = C x (y + 1)
adjacentCoord (C x y) R = C (x + 1) y
adjacentCoord (C x y) D = C x (y - 1)
adjacentCoord (C x y) L = C (x - 1) y

isCoordFree :: Maze -> Coord -> Bool
isCoordFree maze c
  | getTile maze c == Ground || getTile maze c == Storage || getTile maze c == Box || getTile maze c == BoxOk = True
  | otherwise                                                                                                 = False
  
isCoordBox :: Coord -> [Coord] -> Bool
isCoordBox coord boxList = elemList coord boxList

removeFromList :: [Coord] -> Coord -> [Coord]
removeFromList [] _ = []
removeFromList (h:t) coord
  | h == coord = removeFromList t coord
  | otherwise  = h : removeFromList t coord

--delete from coord, move to dir
updateBoxList :: [Coord] -> Coord -> Direction -> [Coord]
updateBoxList boxList coord dir = (removeFromList boxList coord) ++ [(adjacentCoord coord dir)]

data Event = KeyPress String

--handle event function
handleEvent :: Event -> State -> State
handleEvent (KeyPress key) state@(St playerPos dirPl boxList level _ moves)
  | key == "N"                   = if level == (listLength mazesList) - 1 then state else mazeToState (level + 1)
  | isWinning state == True      = state
  | key == "Up"    || key == "W" = tryMove U
  | key == "Right" || key == "D" = tryMove R
  | key == "Down"  || key == "S" = tryMove D
  | key == "Left"  || key == "A" = tryMove L
  where
    updated :: Coord -> Tile
    updated = getUpdatedTile (getTile (nth mazesList level)) boxList
    
    tryMove dirN
      | updated adjCoord1 == Ground ||
        updated adjCoord1 == Storage = state {playerPos = adjCoord1, dir = dirN, noOfMoves = moves + 1}
      | updated adjCoord1 == Box || 
        updated adjCoord1 == BoxOk = 
          if updated adjCoord2 == Ground ||
             updated adjCoord2 == Storage then
               state {playerPos = adjacentCoord playerPos dirN, dir = dirN, boxList = updateBoxList boxList adjCoord1 dirN, noOfMoves = moves + 1}
             else
               state {dir = dirN}
      | otherwise = state {dir = dirN}
      where
        adjCoord1, adjCoord2 :: Coord
        adjCoord1 = adjacentCoord playerPos dirN
        adjCoord2 = adjacentCoord adjCoord1 dirN
handleEvent _ state = state

--polytypes
data SSState world = StartScreen | Running world
    
data Activity world = Activity {
    actState :: world,
    actHandle :: (Event -> world -> world),
    actDraw :: (world -> Screen)
    }

resettable :: Activity s -> Activity s
resettable (Activity state0 handle draw)
  = Activity state0 handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s

withStartScreen :: Activity s -> Activity (SSState s)
withStartScreen (Activity state0 handle draw)
  = Activity state0' handle' draw'
  where
    state0' = StartScreen

    handle' (KeyPress key) StartScreen
         | key == " "                  = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s

isWinning :: State -> Bool
isWinning (St playerPos dir boxList level _ _) = allList (\x -> getTile (nth mazesList level) x == Storage) boxList

data WithUndo a = WithUndo a [a]

withUndo :: Activity State -> Activity (WithUndo State)
withUndo (Activity state0 handle draw) = Activity state0' handle' draw' where
    state0' = WithUndo state0 []
    handle' (KeyPress key) (WithUndo s stack) | key == "U"
      = case stack of s':stack' -> WithUndo s' stack'
                      []        -> WithUndo s []
    handle' e              (WithUndo s stack)
       | s' == s = WithUndo s stack
       | otherwise = WithUndo (handle e s) (s:stack)
      where s' = handle e s

    draw' (WithUndo s _) = draw s
-------------------------------------------------------------------------
data Maze = Maze Coord (Coord -> Tile)

getTile :: Maze -> Coord -> Tile
getTile (Maze _ maze) c = maze c

initialState :: State
initialState = mazeToState 0

mazeToState :: Integer -> State
mazeToState n = St initial D (getBoxesFromList maze (getAllReachable initial (neighFunc maze))) n (findExtremeCoords maze initial) 0 where
  nthMaze@(Maze initial maze) = nth mazesList n
  findExtremeCoords maze initial = getExtremes (getAllReachable initial (neighFunc maze)) (F 0 0 0 0) where
    getExtremes :: [Coord] -> Fours -> Fours
    getExtremes [] (F x1 y1 x2 y2) = F (x1 - 1) (y1 + 1) (x2 + 1) (y2 - 1)
    getExtremes ((C x y):t) extremes@(F x1 y1 x2 y2)
      | x < x1 =
        if y > y1 then 
          getExtremes t (F x y x2 y2)
        else if y < y2 then
          getExtremes t (F x y1 x2 y)
        else
          getExtremes t (F x y1 x2 y2)
      | x > x2 =
        if y > y1 then 
          getExtremes t (F x1 y x y2)
        else if y < y2 then
          getExtremes t (F x1 y1 x y)
        else
          getExtremes t (F x1 y1 x y2)
      | y > y1 = getExtremes t (F x1 y x2 y2)
      | y < y2 = getExtremes t (F x1 y1 x2 y)
      | otherwise = getExtremes t extremes


goodMaze1, goodMaze2, goodMaze3 :: Coord -> Tile
badMaze1, badMaze2, badMaze3 :: Coord -> Tile
spawnPointG1, spawnPointG2, spawnPointG3 :: Coord
spawnPointB1, spawnPointB2, spawnPointB3 :: Coord
goodMaze1 (C x y)
  | abs x > 4  || abs y > 4  = Blank  -- blank
  | abs x == 4 || abs y == 4 = Wall  -- wall
  | x ==  2 && y <= 0        = Wall  -- wall
  | x ==  3 && y <= 0        = Storage  -- storage
  | x >= -2 && y == 0        = Box  -- box
  | otherwise                = Ground  -- ground
spawnPointG1 = C 0 2

goodMaze2 (C x y)
  | abs x > 4 || abs y > 2          = Blank
  | abs x == 4 || abs y == 2        = Wall
  | x == 3 && y == -1               = Wall
  | x == -3 || (x == -2 && y == 1)  = Storage
  | x == 0 && y /= -1               = Box
  | x == 1 && y /= 1                = Box
  | otherwise                       = Ground
spawnPointG2 = C 3 1

goodMaze3 (C x y)
  | abs x > 7 || abs y > 7        = Blank
  | abs x == 7 || abs y == 7      = Wall
  | x == 0 && y == 0              = Storage
  | x == 0 && y == -1             = Box
  | x == -2 && y == 0             = Storage
  | x == -4 && y == 5             = Box
  | y == 6 && -6 <= x && x <= -4  = Ground
  | y == 6 && 4 <= x && x <= 5    = Ground
  | y == 5 && -6 <= x && x <= 5   = Ground
  | x == 5 && -5 <= y && y <= 6   = Ground
  | x == 6 && -5 <= y && y <= -4  = Ground
  | y == -5 && -5 <= x && x <= 6  = Ground
  | y == -6 && -5 <= x && x <= -4 = Ground
  | x == -5 && -6 <= y && y <= 2  = Ground
  | x == -6 && 1 <= y && y <= 2   = Ground
  | y == 2 && -6 <= x && x <= 2   = Ground
  | y == 3 && 1 <= x && x <= 2    = Ground
  | x == 2 && -2 <= y && y <= 3   = Ground
  | x == 3 && -2 <= y && y <= -1  = Ground
  | y == -2 && -2 <= x && x <= 3  = Ground
  | y == -3 && -2 <= x && x <= -1 = Ground
  | x == -2 && y == -1            = Ground
  | otherwise                     = Wall
spawnPointG3 = C (-2) (-1)

badMaze1 (C x y)
  | abs x > 2 || abs y > 1   = Blank
  | abs x == 2 || abs y == 1 = Wall
  | x == -1                  = Storage
  | x == 1                   = Box
  | otherwise                = Ground
spawnPointB1 = C 0 0

badMaze2 (C x y)
  | abs x > 4 || y > 2 || y < -3              = Blank
  | abs x == 4 || y == 2 || y == -3 || x == 0 = Wall
  | x == -3 && y == 1                         = Storage
  | x == 1 && y == 0                          = Box
  | otherwise                                 = Ground
spawnPointB2 = C 3 (-2)

badMaze3 (C x y)
  | x > 4 || x < -5 || y > 3 || y < -2     = Blank
  | x == 4 && y == 0                       = Blank
  | x == 4 || x == -5 || y == 3 || y == -2 = Wall
  | x == 3 && y == 0                       = Storage
  | x == -3 && y == 0                      = Box
  | y == 0                                 = Ground
  | y == 1 && x < -2                       = Ground
  | otherwise                              = Wall
spawnPointB3 = C 0 0

mazeG1, mazeG2, mazeG3 :: Maze
mazeB1, mazeB2, mazeB3 :: Maze
mazeG1 = Maze spawnPointG1 goodMaze1
mazeG2 = Maze spawnPointG2 goodMaze2
mazeG3 = Maze spawnPointG3 goodMaze3
mazeB1 = Maze spawnPointB1 badMaze1
mazeB2 = Maze spawnPointB2 badMaze2
mazeB3 = Maze spawnPointB3 badMaze3

mazes, badMazes :: [Maze]
mazes = [mazeG1, mazeG2, mazeG3]
badMazes = [mazeB1, mazeB2, mazeB3]

mazesList = mazes ++ badMazes
-------------------------------------------------------------------------
neighFunc :: (Coord -> Tile) -> Coord -> [Coord]
neighFunc maze node = foldList func11 [] [adjacentCoord node L, adjacentCoord node D, adjacentCoord node R, adjacentCoord node U] where
  func11 x y
    | maze x == Ground                       = x:y
    | maze x == Storage                      = x:y
    | maze x == Box                          = x:y
    | maze x == BoxOk                        = x:y
    | maze node /= Blank && maze x == Blank  = x:y 
    | otherwise                              = y

getAllReachable :: Eq a => a -> (a -> [a]) -> [a]
getAllReachable node neighFunc = dfs node (neighFunc node) [node] where
  dfs node neighbours visited = foldList func1 visited neighbours where
    func1 x y
      | elemList x y == True  = y
      | otherwise             = dfs x (neighFunc x) (x:y)

isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighbours isOk = andList (mapList isOk (getAllReachable initial neighbours))

reachable :: Eq a => a -> a -> (a -> [a]) -> Bool
reachable v initial neighbours = not (isGraphClosed initial neighbours (\x -> x /= v))

allReachable :: Eq a => [a] -> a -> (a -> [a]) -> Bool
allReachable vs initial neighbours = andList (mapList (\x -> reachable x initial neighbours) vs)

isClosed :: Maze -> Bool
isClosed (Maze initial maze) = isGraphClosed initial (neighFunc maze) isOk where
  isOk node
    | maze node == Ground  = True
    | maze node == Storage = True
    | maze node == Box     = True
    | maze node == BoxOk   = True
    | otherwise            = False
  
isSane :: Maze -> Bool
isSane (Maze initial maze) = reachableStorages >= reachableBoxes where
  allReachableTiles = getAllReachable initial (neighFunc maze)
  reachableStorages = foldList (\x y -> if maze x == Storage then y + 1 else y) 0 allReachableTiles
  reachableBoxes = foldList (\x y -> if maze x == Box then y + 1 else y) 0 allReachableTiles

startScreen :: Screen
startScreen = 
  "maze | closed | sane\n" ++
  "————————————————————\n" ++
  (concat [printDescription mazesList 1]) ++
  "\n" ++
  "W - up\n" ++
  "S - down\n" ++
  "D - right\n" ++
  "A - left\n" ++
  "U - undo\n" ++
  "N - next maze\n" ++
  "Esc - reset game\n" ++
  "\nArrow keys also work.\n" ++
  "\nPress space to start a game."
  where
    printDescription [] _ = ""
    printDescription (h:t) i = 
      (printWithNChars (show i) 4) ++ " | " ++ 
      (printWithNChars (boolToString (isClosed h)) 6) ++ " | " ++
      (printWithNChars (boolToString (isSane h)) 4) ++ "\n" ++ (printDescription t (i + 1)) where
        boolToString :: Bool -> String
        boolToString b
          | b == True = "YES"
          | otherwise = "NO"

        printWithNChars :: String -> Integer -> String
        printWithNChars word length = word ++ (replicate (fromIntegral (length - (listLength word))) ' ')

endScreen :: Integer -> Screen
endScreen n = "Poziom ukończony, liczba ruchów: " ++ (show n)

runActivity :: Activity s -> IO ()
runActivity (Activity state handle draw) = do
  let screen = draw state
  putStrLn $  "\ESCc" ++ startScreen
  
  let go state = do
      
      let readKey = reverse <$> func "" where
          func chars = do
            c <- getChar
            notEmpty <- hReady stdin
            (if notEmpty then func else return) (c:chars)

      key <- readKey
      let action = case key of {
        "w" -> "W";
        "s" -> "S";
        "d" -> "D";
        "a" -> "A";
        "n" -> "N";
        "u" -> "U";
        " " -> " ";
        "\ESC" -> "Esc";
        "\ESC[A" -> "Up";
        "\ESC[B" -> "Down";
        "\ESC[C" -> "Right";
        "\ESC[D" -> "Left";
        _ -> "Some command that won't create any action"
      }
      
      let newState = handle (KeyPress action) state
      let newScreen = draw newState
      putStrLn $ "\ESCc" ++ newScreen
      go newState

  go state

getExtremes :: State -> Fours
getExtremes (St _ _ _ _ a _) = a

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  runActivity (resettable (withStartScreen (withUndo (Activity initialState handleEvent draw))))