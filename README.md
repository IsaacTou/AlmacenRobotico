# Proyecto #1: Almacén Robótico

### Datos de los Integrantes

- **Nombres:** Maria Elena Medina Diaz, Isaac Perez Touceda
- **Cédulas de Identidad:** 32.061.879, 31.065.844
- **Asignatura:** Lenguajes de Programación
- **Docentes:** Eugenio Scalise, José Yvimas

## Explicación de Funciones y Herramientas utilizadas

Para la resolucion de el proyecto solo utilizamos operaciones del Prelude de Haskell, por lo tanto a continuacion detallare algunas de las funciones mas destacadas que utilizamos debido a que nos eran de gran utilidad:

### `elem`

Esta función toma un objeto y una lista de objetos, y nos indica si ese objeto pertenece o no a la lista. Generalmente se utiliza como una función infija (colocándola en medio de los argumentos) porque así es mucho más fácil de leer en el código. La aplicamos en casi todo el proyecto: en `initialState` e `invalidos` para revisar que los obstáculos no se solapen; en `esValido` y `empujeValido` para chequear si el robot chocó con alguna caja; en `applyMove` para saber si la casilla destino tiene un obstáculo; y finalmente en `solveWarehouse` dentro del BFS para verificar si el estado actual ya pertenece a la lista de `visitados` y evitar ciclos infinitos.

### `let, in`

Esta herramienta nos permite declarar variables temporales que solo existen dentro de una función específica. Con el `let` guardamos los datos que necesitemos y luego, por medio del `in`, podemos utilizar lo que se guardó en esas variables para procesar el resultado.La implementamos en la función `agregarNodos`. Nos sirvió para crear la variable local `nuevoSt` que almacena el resultado de `applyMove st mov`. Luego, en el bloque `in`, usamos esa variable para armar y devolver la tupla correspondiente con el nivel incrementado y el camino actualizado. Ademas en la función `solveWarehouse`utilizamos el let para calcular los nuevosHijos, la actualización de la cola (nuevaCola) y el registro del estado actual en la lista de visitados (nuevosVisitados). Luego, usando el in, le pasamos esos datos temporales a la siguiente llamada recursiva del bfs.

## Expresión `deriving (Show, Eq)`

la cláusula `deriving` le permite a Haskell generar de forma automática el comportamiento de ciertas clases de tipos básicas sin que nosotros tengamos que programar las funciones desde cero.

- ** `Eq` (Equality):** Se utiliza para los tipos de datos cuyos valores pueden ser comparados por igualdad. Al colocarla, el compilador nos proporciona los operadores `(==)` y `(/=)`. En nuestro proyecto es vital, ya que al aplicarla en nuestro tipo `Move`, permite que funciones como `elem` puedan comparar los estados del robot y sus movimientos de forma directa.
- ** `Show`:** Se utiliza para los tipos de datos cuyos valores necesitamos convertir en una cadena de caracteres (`String`). En nuestro código la añadimos al tipo `Move` para poder transformar los constructores `U`, `D`, `L`, `R` en texto real y así mostrar el camino de la solución en pantalla.
