type Coord = (Int, Int) -- (Fila, Columna)
data Move = U | D | L | R deriving (Show, Eq)
type State = (Coord, Coord, [Coord]) -- (Robot, CajaObjetivo, CajasDeBloqueo)  
data Arbol = Nodo (State,Int,[State]) [Arbol] deriving (Show, Eq)

estaFuera :: Coord -> Bool
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


siguienteCoord :: Coord -> Move -> Coord
siguienteCoord (x,y) U = (x-1, y) 
siguienteCoord (x,y) D = (x+1, y) 
siguienteCoord (x,y) L = (x, y-1) 
siguienteCoord (x,y) R = (x, y+1)


isValidMove :: State -> Move -> Bool
isValidMove ((x,y),cajaObjetivo,cajasDeBloqueo) movimiento =
    esValido (siguienteCoord (x,y) movimiento) cajaObjetivo cajasDeBloqueo movimiento
    where
        esValido :: Coord -> Coord -> [Coord] -> Move -> Bool
        esValido (nx,ny) obj bloq mov  

            | estaFuera (nx,ny) = False
            | (nx,ny) == obj = empujeValido (siguienteCoord (nx,ny) mov) obj bloq 
            | (nx,ny) `elem` bloq = empujeValido (siguienteCoord (nx,ny) mov) obj bloq
            | otherwise = True

        empujeValido :: Coord -> Coord -> [Coord] -> Bool 
        empujeValido (cx,cy) obj bloq
            | estaFuera (cx,cy) = False
            | (cx,cy) `elem` bloq = False
            | (cx,cy) == obj = False
            | otherwise = True


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
                cajasBloqModificado = filter(/= (rx,ry)) cajasbloq
                nuevaCajasBloq = siguienteCoord (rx,ry) mov:cajasBloqModificado

