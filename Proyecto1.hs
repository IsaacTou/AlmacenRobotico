type Coord = (Int, Int) -- (Fila, Columna)
data Move = U | D | L | R deriving (Show, Eq)
type State = (Coord, Coord, [Coord]) -- (Robot, CajaObjetivo, CajasDeBloqueo)  
--Parte 1
estaFuera :: Coord -> Bool-- funcion para saber si una coordenada se salio del tablero
estaFuera (x,y) | x < 0 || y < 0 = True
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


siguienteCoord :: Coord -> Move -> Coord--funcion para obtener la siguiente coordenada dependiendodel movimiento que se quiere realizar
siguienteCoord (x,y) U = (x-1, y) 
siguienteCoord (x,y) D = (x+1, y) 
siguienteCoord (x,y) L = (x, y-1) 
siguienteCoord (x,y) R = (x, y+1)
--Parte 2

isValidMove :: State -> Move -> Bool
isValidMove ((x,y),targetBox,lockBoxes) movement =
    isValid (nextCoord (x,y) movement) targetBox lockBoxes movement
    where
        esValido :: Coord -> Coord -> [Coord] -> Move -> Bool-- funcion donde validamos si el movimiento es valido o no
        esValido (nx,ny) obj bloq mov  

            | isOut (nx,ny) = False
            | (nx,ny) == obj = validPush (nextCoord (nx,ny) mov) obj bloq 
            | (nx,ny) `elem` bloq = validPush (nextCoord (nx,ny) mov) obj bloq
            | otherwise = True

        empujeValido :: Coord -> Coord -> [Coord] -> Bool --funcion donde evaluamos si el empuje se puede realizar o no 
        empujeValido (cx,cy) obj bloq
            | estaFuera (cx,cy) = False
            | (cx,cy) `elem` bloq = False
            | (cx,cy) == obj = False
            | otherwise = True

--Parte 3
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
                cajasBloqModificado = filter(/= (rx,ry)) cajasbloq --utlilizamos filter para eliminar la caja de bloqueo que se va a mover y luego agregamos la nueva posiscion de dicha caja ya movida
                nuevaCajasBloq = siguienteCoord (rx,ry) mov:cajasBloqModificado

--Parte 4
agregarNodos :: (State, Int, [State]) -> [(State, Int, [State])]
agregarNodos (st, nivel, camino) = 
    intentar U ++ intentar D ++ intentar L ++ intentar R
    where
    tryMov :: Move -> [(State, Int, [State])]
    tryMov mov
        | isValidMove st mov = 
            let nuevoSt = applyMove st mov--hacemos uso del let e in para hacer uso de variable temporal que nos permite guardar el estado nuevo 
            in [(nuevoSt, nivel + 1, nuevoSt : camino)] 
        | otherwise = []    


esEstadoFinal :: State -> Bool--vemos si llego a la meta o no
esEstadoFinal (robotCoord, (cx,cy), cajasbloq)
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
            let nuevosHijos = agregarNodos (st, nivel, camino)--usamos let e in de neuvo para agg los hijos a la cola
                nuevaCola = colaRestante ++ nuevosHijos 
                nuevosVisitados = st : visitados        
            in bfs nuevosVisitados nuevaCola