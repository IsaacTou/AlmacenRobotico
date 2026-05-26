type Coord = (Int, Int) -- (Fila, Columna)
data Move = U | D | L | R deriving (Show, Eq)
type State = (Coord, Coord, [Coord]) -- (Robot, CajaObjetivo, CajasDeBloqueo)  
--Parte 1
estaFuera :: Coord -> Bool-- funcion para saber si una coordenada se salio del tablero
estaFuera (x,y) | x < 0 || y < 0 = True
                | x > 5 || y > 5 = True
                | otherwise = False


initialState :: Coord -> Coord -> [Coord] -> State
initialState robot cajaObjetivo cajasDeBloqueo
    | robot == cajaObjetivo || estaFuera robot || estaFuera cajaObjetivo = ((-1,-1), (-1,-1), [])
    | robot `elem` cajasDeBloqueo || cajaObjetivo `elem` cajasDeBloqueo = ((-1,-1), (-1,-1), [])
    | invalidos cajasDeBloqueo  = ((-1,-1), (-1,-1), [])
    | otherwise  = (robot, cajaObjetivo, cajasDeBloqueo)
    where 
    invalidos :: [Coord] -> Bool
    invalidos [] = False
    invalidos (x:xs) 
        | x `elem` xs = True
        | estaFuera x = True
        | otherwise = invalidos xs       


siguienteCoord :: Coord -> Move -> Coord--funcion para obtener la siguiente coordenada dependiendodel movimiento que se quiere realizar
siguienteCoord (x,y) U = (x-1, y) 
siguienteCoord (x,y) D = (x+1, y) 
siguienteCoord (x,y) L = (x, y-1) 
siguienteCoord (x,y) R = (x, y+1)
--Parte 2

isValidMove :: State -> Move -> Bool
isValidMove ((x,y),cajaObjetivo,cajasDeBloqueo) movimiento =
    esValido (siguienteCoord (x,y) movimiento) cajaObjetivo cajasDeBloqueo movimiento
    where
        esValido :: Coord -> Coord -> [Coord] -> Move -> Bool-- funcion donde validamos si el movimiento es valido o no
        esValido (nx,ny) obj bloq mov  

            | estaFuera (nx,ny) = False
            | (nx,ny) == obj = empujeValido (siguienteCoord (nx,ny) mov) obj bloq 
            | (nx,ny) `elem` bloq = empujeValido (siguienteCoord (nx,ny) mov) obj bloq
            | otherwise = True

        empujeValido :: Coord -> Coord -> [Coord] -> Bool --funcion donde evaluamos si el empuje se puede realizar o no 
        empujeValido (cx,cy) obj bloq
            | estaFuera (cx,cy) = False
            | (cx,cy) `elem` bloq = False
            | (cx,cy) == obj = False
            | otherwise = True

--Parte 3
applyMove :: State -> Move -> State
applyMove ((x,y),cajaObjetivo,cajasDeBloqueo) movimiento
    | nuevaPos `elem` cajasDeBloqueo = modificarPosicion nuevaPos cajaObjetivo cajasDeBloqueo movimiento
    | nuevaPos == cajaObjetivo = (nuevaPos, siguienteCoord nuevaPos movimiento, cajasDeBloqueo)
    | otherwise = (nuevaPos, cajaObjetivo, cajasDeBloqueo)
    where 
        nuevaPos = siguienteCoord(x,y) movimiento

        modificarPosicion :: Coord -> Coord -> [Coord] -> Move -> State
        modificarPosicion (rx,ry) cajaobj cajasbloq mov = ((rx,ry),cajaobj,nuevaCajasBloq)
            where
                cajasBloqModificado = filter(/= (rx,ry)) cajasbloq --utlilizamos filter para eliminar la caja de bloqueo que se va a mover y luego agregamos la nueva posiscion de dicha caja ya movida
                nuevaCajasBloq = siguienteCoord (rx,ry) mov:cajasBloqModificado

--Parte 4
agregarNodos :: (State, Int, [State]) -> [(State, Int, [State])]
agregarNodos (st, nivel, camino) = 
    intentar U ++ intentar D ++ intentar L ++ intentar R
    where
    intentar :: Move -> [(State, Int, [State])]
    intentar mov
        | isValidMove st mov = 
            let nuevoSt = applyMove st mov--hacemos uso del let e in para hacer uso de variable temporal que nos permite guardar el estado nuevo 
            in [(nuevoSt, nivel + 1, nuevoSt : camino)] 
        | otherwise = []    


esEstadoFinal :: State -> Bool--vemos si llego a la meta o no
esEstadoFinal (robotCoord, (cx,cy), cajasbloq)
    | cx == 5 && cy == 5 = True 
    | otherwise = False


solveWarehouse :: State -> (Int, [State])
solveWarehouse estadoInicial = bfs [] [(estadoInicial, 0, [estadoInicial])]
    where
        bfs :: [State] -> [(State, Int, [State])] -> (Int, [State])
        bfs _ [] = (0, []) 
        bfs visitados ((st, nivel, camino) : colaRestante)
            | esEstadoFinal st = (nivel, reverse camino) 
            | st `elem` visitados = bfs visitados colaRestante
            | otherwise = 
            let nuevosHijos = agregarNodos (st, nivel, camino)--usamos let e in de neuvo para agg los hijos a la cola
                nuevaCola = colaRestante ++ nuevosHijos 
                nuevosVisitados = st : visitados        
            in bfs nuevosVisitados nuevaCola