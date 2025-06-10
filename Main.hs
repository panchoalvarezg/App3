import Debug.Trace

type Coordenada = (Int, Int)
type Bosque = [[Int]]

data Estado = Estado {
  pos :: Coordenada,
  energia :: Int,
  camino :: [Coordenada]
} deriving (Show)

valorEn :: Bosque -> Coordenada -> Int
valorEn bosque (i,j) = (bosque !! i) !! j

valida :: Bosque -> Coordenada -> Bool
valida bosque (i,j) = 
  i >= 0 && j >= 0 && i < length bosque && j < length (head bosque)

ajustarEnergia :: Bosque -> Coordenada -> Bool -> Int -> Int
ajustarEnergia bosque c esDiagonal energiaActual =
  let base = energiaActual + valorEn bosque c
      penalizacionTrampa = if valorEn bosque c == 0 then 3 else 0
      penalizacionDiagonal = if esDiagonal then 2 else 0
  in base - penalizacionTrampa - penalizacionDiagonal

-- MOVIMIENTOS LIMITADOS: solo derecha, abajo, y diagonal abajo-derecha
movimientos :: Bosque -> Estado -> [Estado]
movimientos bosque (Estado (i,j) e caminoAnt) = 
  let candidatos = [ ((i, j+1), False), ((i+1, j), False), ((i+1, j+1), True) ]
      filtrados = filter (\(c, _) -> valida bosque c && notElem c caminoAnt) candidatos
      siguientes = [ Estado c (ajustarEnergia bosque c diag e) (caminoAnt ++ [c])
                   | (c, diag) <- filtrados
                   , ajustarEnergia bosque c diag e >= 0 ]
  in siguientes

explorar :: Bosque -> Coordenada -> Coordenada -> [Estado] -> [Estado]
explorar _ _ _ [] = []
explorar bosque fin destino (x:xs)
  | pos x == destino = trace ("Llega a destino: " ++ show (camino x)) $
                       x : explorar bosque fin destino xs
  | otherwise = explorar bosque fin destino (movimientos bosque x ++ xs)

main :: IO ()
main = do
  putStrLn "Iniciando el bosque mágico... (versión optimizada)"
  
  let bosque = [[2, -1, 3],
                [0, 4, -2],
                [1, -3, 5]]
  let energiaInicial = 10

  let inicio = (0,0)
  let energiaInicialReal = ajustarEnergia bosque inicio False energiaInicial
  putStrLn $ "Energía inicial real tras celda (0,0): " ++ show energiaInicialReal

  let inicial = Estado inicio energiaInicialReal [inicio]
  let fin = (length bosque - 1, length (head bosque) - 1)

  let caminosValidos = explorar bosque fin fin [inicial]
  putStrLn $ "Cantidad de caminos válidos encontrados: " ++ show (length caminosValidos)

  let (caminoFinal, energiaFinal) =
        if null caminosValidos
           then ([], 0)
           else let mejor = foldl1 (\a b -> if energia a > energia b then a else b) caminosValidos
                in (camino mejor, energia mejor)

  putStrLn "¡Cálculo completado!"
  putStrLn $ "Camino: " ++ show caminoFinal
  putStrLn $ "Energía final: " ++ show energiaFinal
