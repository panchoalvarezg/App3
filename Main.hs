main :: IO ()
main = do
  putStrLn "Iniciando el bosque mágico..."

  let bosque = [[2,-3,1,0,2,3],
                [-5,4,-2,1,0,-4],
                [1,3,0,-3,2,2],
                [2,-1,4,0,-5,1],
                [0,2,-3,3,4,-1],
                [1,0,2,-2,1,5]]
  let energiaInicial = 12

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
