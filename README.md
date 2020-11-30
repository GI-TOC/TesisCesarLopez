# TesisCesarLopez
Este repositorio contiene los codigos en lenguaje R obtenidos de la adaptacion de la metaheuristica Busqueda De Ardillas al problema de programacion de producción en una configuracion Job Shop con operadores calificados.

El archivo " Automatico.R" contiene el código con la adaptación de la metaheuristica al problema estudiado considerando la asignación de operadores aleatoria.

El archivo "Automatico_SSA2.R" contiene el codigo con la adaptación de la metaheuristica al problema estudiado considerando la heurística Lowest First Decreasing (LFD), 
la cual consiste en ordenar las tareas de la más larga a las más corta en duración y se asigna sucesivamente a un operador que pueda realizarla, teniendo en cuenta que esté disponible al momento de realizar esta asignación. 

Por otra parte, se facilitan los datos para cada de los 28 casos analizados en el trabajo. Cada caso consiste de una terna " Grafo, Tp, Maquina", donde cada archivo contiene la matriz de precedencia, los tiempos de procesos en cada máquina y las maquinas a utilizar. 

Los archivos "Parametrizacion_SSA_Asig_Heuristica.xlxs" y "Parametrizacion_SSA2_Asignacion_LFD.xlxs" contiene los resultados para la calibración de los parámetros de los algoritmos propuestos. 

El archivo Optimos.xlxs, contiene las soluciones óptimas reportadas en la literatura. 
