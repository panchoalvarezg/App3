# App3 – El Bosque de las Runas Mágicas 🌲✨

Este proyecto en Haskell implementa una simulación funcional del recorrido óptimo de un mago por un bosque lleno de runas mágicas, maximizando su energía final.

## Integrante
- Francisco Alvarez Gajardo – franciscoialvarez@alumnos.uai.cl

## Profesores
● María Loreto Arriagada loreto.arriagada.v@edu.uai.cl
● Paulina González paulina.gonzalez.p@edu.uai.cl
● Justo Vargas justo.vargas@edu.uai.cl
## Ayudante
● Diego Duhalde dduhalde@alumnos.uai.cl
  
## Cómo ejecutar

Requisitos: Instala GHC y usar Mingw64 para compilar y ejecutar la App3

1. Compila el proyecto:
   ghc Main.hs -o bosque.exe

2.Para ejecutar la aplicacion en GitBash:
   ./bosque.exe

3.Ingresa los datos en la consola:
Energía inicial: escribe un número entero.

Matriz de runas: ingrésala en formato Haskell, como por ejemplo:
 [[2, -3, 1, 0, 2, 3], [-5, 4, -2, 1, 0, -4], [1, 3, 0, -3, 2, 2], [2, -1, 4, 0, -5, 1], [0, 2, -3, 3, 4, -1], [1, 0, 2, -2, 1, 5]]

4.Salida en la terminal(Ejemplo de como debe verse):
Energía inicial tras celda (0,0): 14
Caminos válidos encontrados: 93
Camino óptimo: [(0,0),(1,0),...(5,5)]
Energía final: 17
