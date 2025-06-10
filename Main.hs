-- Main.hs

type Coordenada = (Int, Int)
type Bosque = [[Int]]

data Estado = Estado {
  pos :: Coordenada,
  energia :: Int,
  camino :: [Coordenada]
} deriving (Show)

-- Obtiene el valor de una coordenada
valorEn :: Bosque -> Coordenada -> Int
valorEn bosque (i,j) = (bosque !! i) !! j

-- Verifica si está dentro de los límites del bosque
valida :: Bosque -> Coordenada -> Bool
valida bosque (i,j) = 
  i >= 0 && j >= 0 && i < length bosque && j < length (head bosque)

-- Ajusta energía al pasar por una celda
ajustarEnergia :: Bosque -> Coordenada -> Bool -> Int -> Int
ajustarEnergia bosque c esDiagonal energiaActual =
  let base = energiaActual + valorEn bosque c
      penalizacionTrampa = if valorEn bosque c == 0 then 3 else 0
      penalizacionDiagonal = if esDiagonal then 2 else 0
  in base - penalizacionTrampa - penalizacionDiagonal

-- Genera los estados siguientes desde uno dado
movimientos :: Bosque -> Estado -> [Estado]
movimientos bosque (Estado (i,j) e caminoAnt) = 
  let candidatos = [ ((i, j+1), False), ((i+1, j), False), ((i+1, j+1), True),
                     ((i, j-1), False), ((i-1, j), False) ]
      filtrados = filter (\(c, _) -> valida bosque c && notElem c caminoAnt) candidatos
      siguientes = [ Estado c (ajustarEnergia bosque c diag e) (caminoAnt ++ [c]) | (c, diag) <- filtrados, ajustarEnergia bosque c diag e >= 0 ]
  in siguientes

-- Recursión para explorar todos los caminos válidos
explorar :: Bosque -> Coordenada -> Coordenada -> [Estado] -> [Estado]
explorar _ _ _ [] = []
explorar bosque fin destino (x:xs)
  | pos x == destino = x : explorar bosque fin destino xs
  | otherwise = explorar bosque fin destino (movimientos bosque x ++ xs)

-- Encuentra el camino con mayor energía final
resolverBosque :: Bosque -> Int -> ([Coordenada], Int)
resolverBosque bosque energiaInicial =
  let inicio = (0,0)
      fin = (length bosque - 1, length (head bosque) - 1)
      inicial = Estado inicio (ajustarEnergia bosque inicio False energiaInicial) [inicio]
      caminosValidos = explorar bosque fin fin [inicial]
      mejor = foldl1 (\a b -> if energia a > energia b then a else b) caminosValidos
  in (camino mejor, energia mejor)

-- Main para ejecutar la app
main :: IO ()
main = do
  let bosque = [[2,-3,1,0,2,3],[-5,4,-2,1,0,-4],[1,3,0,-3,2,2],[2,-1,4,0,-5,1],[0,2,-3,3,4,-1],[1,0,2,-2,1,5]]
  let energiaInicial = 12
  let (caminoFinal, energiaFinal) = resolverBosque bosque energiaInicial
  putStrLn $ "Camino: " ++ show caminoFinal
  putStrLn $ "Energía final: " ++ show energiaFinal
