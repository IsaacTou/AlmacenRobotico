type Coord = (Int, Int) -- (Fila, Columna)
data Move = U | D | L | R deriving (Show, Eq)
type State = (Coord, Coord, [Coord]) -- (Robot, CajaObjetivo, CajasDeBloqueo)


estaFuera :: Coord -> Bool
estaFuera (x,y) | x < 0 || y < 0 = True
                | x > 5 || y > 5 = True
                | otherwise = False


initialState :: Coord -> Coord -> [Coord] -> State
initialState robot objetivo obstaculos
    | robot == objetivo || estaFuera robot || estaFuera objetivo = ((-1,-1), (-1,-1), [])
    | robot `elem` obstaculos || objetivo `elem` obstaculos = ((-1,-1), (-1,-1), [])
    | invalidos obstaculos  = ((-1,-1), (-1,-1), [])
    | otherwise  = (robot, objetivo, obstaculos)
    where 
    invalidos :: [Coord] -> Bool
    invalidos [] = False
    invalidos (x:xs) 
        | x `elem` xs = True
        | estaFuera x = True
        | otherwise = invalidos xs       

