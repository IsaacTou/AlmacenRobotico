type Coord = (Int, Int) -- (Fila, Columna)
data Move = U | D | L | R deriving (Show, Eq)
type State = (Coord, Coord, [Coord]) -- (Robot, targetBox, CajasDeBloqueo)  

isOut :: Coord -> Bool
isOut (x,y) | x < 0 || y < 0 = True
                | x > 5 || y > 5 = True
                | otherwise = False


initialState :: Coord -> Coord -> [Coord] -> State
initialState robot targetBox lockBoxes
    | robot == targetBox || isOut robot || isOut targetBox = ((-1,-1), (-1,-1), [])
    | robot `elem` lockBoxes || targetBox `elem` lockBoxes = ((-1,-1), (-1,-1), [])
    | invalidCoords lockBoxes  = ((-1,-1), (-1,-1), [])
    | otherwise  = (robot, targetBox, lockBoxes)
    where 
    invalidCoords :: [Coord] -> Bool
    invalidCoords [] = False
    invalidCoords (x:xs) 
        | x `elem` xs = True
        | isOut x = True
        | otherwise = invalidCoords xs       


nextCoord :: Coord -> Move -> Coord
nextCoord (x,y) U = (x-1, y) 
nextCoord (x,y) D = (x+1, y) 
nextCoord (x,y) L = (x, y-1) 
nextCoord (x,y) R = (x, y+1)


isValidMove :: State -> Move -> Bool
isValidMove ((x,y),targetBox,lockBoxes) movement =
    isValid (nextCoord (x,y) movement) targetBox lockBoxes movement
    where
        isValid :: Coord -> Coord -> [Coord] -> Move -> Bool
        isValid (nx,ny) obj bloq mov  

            | isOut (nx,ny) = False
            | (nx,ny) == obj = validPush (nextCoord (nx,ny) mov) obj bloq 
            | (nx,ny) `elem` bloq = validPush (nextCoord (nx,ny) mov) obj bloq
            | otherwise = True

        validPush :: Coord -> Coord -> [Coord] -> Bool 
        validPush (cx,cy) obj bloq
            | isOut (cx,cy) = False
            | (cx,cy) `elem` bloq = False
            | (cx,cy) == obj = False
            | otherwise = True


applyMove :: State -> Move -> State
applyMove ((x,y),targetBox,lockBoxes) movement
    | newPosition `elem` lockBoxes = changePosition newPosition targetBox lockBoxes movement
    | newPosition == targetBox = (newPosition, nextCoord newPosition movement, lockBoxes)
    | otherwise = (newPosition, targetBox, lockBoxes)
    where 
        newPosition = nextCoord(x,y) movement

        changePosition :: Coord -> Coord -> [Coord] -> Move -> State
        changePosition (rx,ry) objBox lockBoxes mov = ((rx,ry),objBox,newLockBoxes)
            where
                changeLockBoxes = filter(/= (rx,ry)) lockBoxes
                newLockBoxes = nextCoord (rx,ry) mov:changeLockBoxes


addNodes :: (State, Int, [State]) -> [(State, Int, [State])]
addNodes (st, level, road) = 
    tryMov U ++ tryMov D ++ tryMov L ++ tryMov R
    where
    tryMov :: Move -> [(State, Int, [State])]
    tryMov mov
        | isValidMove st mov = 
            let nuevoSt = applyMove st mov
            in [(nuevoSt, level + 1, nuevoSt : road)] 
        | otherwise = []    


isFinalState :: State -> Bool
isFinalState (robotCoord, (cx,cy), cajasbloq)
    | cx == 5 && cy == 5 = True 
    | otherwise = False


solveWarehouse :: State -> (Int, [State])
solveWarehouse initialState = bfs [] [(initialState, 0, [initialState])]
    where
        bfs :: [State] -> [(State, Int, [State])] -> (Int, [State])
        bfs _ [] = (0, []) 
        bfs visited ((st, level, road) : residuaryTail)
            | isFinalState st = (level, reverse road) 
            | st `elem` visited = bfs visited residuaryTail
            | otherwise = 
            let newChildrens = addNodes (st, level, road)
                newTail = residuaryTail ++ newChildrens 
                newVisited = st : visited        
            in bfs newVisited newTail