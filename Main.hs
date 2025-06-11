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
  putStrLn "Ingrese energía inicial:"
  energiaStr <- getLine
  let energiaInicial = read energiaStr :: Int

  putStrLn "Ingrese la matriz de runas (formato: [[a,b],[c,d],...]):"
  matrizStr <- getLine
  let bosque = read matrizStr :: [[Int]]

  let inicio = (0,0)
  let energiaInicialReal = ajustarEnergia bosque inicio False energiaInicial
  putStrLn $ "Energía inicial tras celda (0,0): " ++ show energiaInicialReal

  let inicial = Estado inicio energiaInicialReal [inicio]
  let fin = (length bosque - 1, length (head bosque) - 1)

  let caminosValidos = explorar bosque fin fin [inicial]
  putStrLn $ "Caminos válidos encontrados: " ++ show (length caminosValidos)

  let (caminoFinal, energiaFinal) =
        if null caminosValidos
           then ([], 0)
           else let mejor = foldl1 (\a b -> if energia a > energia b then a else b) caminosValidos
                in (camino mejor, energia mejor)

  putStrLn $ "Camino óptimo: " ++ show caminoFinal
  putStrLn $ "Energía final: " ++ show energiaFinal
