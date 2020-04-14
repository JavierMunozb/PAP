object mainObject
{

	import scala.annotation.tailrec
	import scala.util.Random
	import scala.io.StdIn.readInt
	
	/**
	 * Método que recorre el tablero hasta una posición dada y retorna el valor contenido en ella.
	 *
	 * @param tablero Tablero que se quiere recorrer.
	 * @param index   Índice al que se quiere llegar en el tablero.
	 * @return Valor de la posición indicada del tablero.
	 */
	def recorrerTablero(tablero: List[Char], index: Int): Char =
	{
		@tailrec def recorrerDentro(tablero: List[Char], i: Int, index: Int): Char =
		{
			tablero match
			{
				case head :: tail => if (i == index) head //Si nos encontramos en el índice requerido, devolvemos el valor
				else recorrerDentro(tail, i + 1, index) //Sino, seguimos buscando
				case Nil => 'Z'
			}
		}
		
		recorrerDentro(tablero, 1, index)
	}
	
	/**
	 * Método utilizado para crear y popular el tablero inicial.
	 *
	 * @return Tablero creado de manera aleatoria.
	 */
	@tailrec def crearTablero(tablero: List[Char], numeroDeValores: Int): List[Char] =
	{
		val posAleatoria = 1 + Random.nextInt(81)
		val letrasPosibles = List('A', 'N', 'R', 'V', 'M', 'G') //Lista con las posibles bolas del juego
		val letraAleatoria = recorrerTablero(letrasPosibles, 1 + Random.nextInt(letrasPosibles.length))
		val aux = realizarMovimiento(letraAleatoria, posAleatoria, tablero)
		if (numeroDeValores > 0) crearTablero(aux, numeroDeValores - 1) else aux
	}
	
	/**
	 * Método utilizado para poder imprimir el tablero por la pantalla.
	 *
	 * @param tablero Tablero que queremos enseñar por pantalla.
	 */
	def mostrarTablero(tablero: List[Char]): Unit = //Esta funcion nos sirve para mostrar el tablero por el terminal
	{
		@tailrec def mostrarDentro(tablero: List[Char], i: Int): Unit = //Utilizamos una función interna para conservar el índice
		{
			tablero match
			{
				case head :: tail => if (i % 9 == 0) println(head) //Hacemos la distincion para organizar la salida por terminal
				else print(head)
					mostrarDentro(tail, i + 1)
				case Nil =>
			}
		}
		
		mostrarDentro(tablero, 1)
	}
	
//	/**
//	 *
//	 * Función que comprueba si hay alguna posición con 5 valores en raya y devuelve sus posiciones.
//	 *
//	 * @param tablero Tablero sobre el que se realiza la búsqueda.
//	 * @return Lista con las posiciones de los objetos con 5 en raya.
//	 */
//	def comprobar5EnRaya(tablero: List[Int]): List[Int] =
//	{
//		//TODO: Encontrar manera de comprobar si hay 5 bolas adyacentes del mismo color, teniendo en cuenta todas las diagonales posibles
//	}
	
	/**
	 * Método que comprueba si quedan más movimientos disponibles en en tablero.
	 *
	 * @param tablero Tablero sobre el que queremos comprobar si quedan más movimientos disponibles.
	 * @return Valor booleano que indica si quedan o no más movimientos posibles.
	 */
	def comprobarFin(tablero: List[Char]): Boolean =
	{
		@tailrec def comprobarDentro(tablero: List[Char], i: Int): Boolean =
		{
			tablero match
			{
				case head :: tail => if (head == '-') false else comprobarDentro(tablero, i + 1)
				case Nil => true
			}
		}
		
		comprobarDentro(tablero, 1)
	}
	
	/**
	 * Método simple para poder conseguir el índice compatible con la lista utilizando la fila y columna.
	 *
	 * @param columna Número de la columna.
	 * @param fila    Número de la fila.
	 * @return Índice compatible con la lista.
	 */
	def conseguirIndice(columna: Int, fila: Int): Int =
	{
		9 * (fila - 1) + columna
	}
	
	/**
	 * Método utilizado para calcular el siguiente movimiento válido.
	 *
	 * @param tablero    Tablero sobre el que se realiza la comprobación.
	 * @param posInicial Posición desde la que se parte.
	 * @param posFinal   Posición a la que se quiere llegar.
	 * @return Siguiente movimiento válido posible.
	 */
	def calcularMovimiento(tablero: List[Char], posInicial: Int, posFinal: Int): Int =
	{
		val filaInicial = (posInicial + 9) / 9 //Valor de la fila obtenido de la posición inicial
		val colInicial = posInicial % 9 //Valor de la columna obtenido de la posición inicial
		val filaFinal = (posFinal + 9) / 9 //Valor de la fila obtenido de la posición objetivo
		val colFinal = posFinal % 9 //Valor de la columna obtenido de la posición objetivo
		val filaPosible = if (filaInicial < filaFinal) filaInicial + 1 //Valor de la nueva fila para la posicion nueva
		else if (filaInicial > filaFinal) filaInicial - 1 else filaInicial
		val colPosible = if (colInicial < colFinal) colInicial + 1 //Valor de la nueva columna para la posicion nueva
		else if (colInicial > colFinal) colInicial - 1 else colInicial
		
		if (filaPosible != filaInicial && recorrerTablero(tablero, conseguirIndice(colInicial, filaPosible)) == '-')
		{
			conseguirIndice(colInicial, filaPosible) //Si la fila nueva no es la misma que la anterior y está vacía
		} else if (colPosible != colInicial && recorrerTablero(tablero, conseguirIndice(colPosible, filaInicial)) == '-')
		{
			conseguirIndice(colPosible, filaInicial) //Si la columna nueva no es la misma que la anterior y está vacía
		} else -1 //Si no se puede hacer ninguno de los dos movimientos anteriores
	}
	
	/**
	 * Método que comprueba si un movimiento es válido o no.
	 *
	 * @param tablero    Tablero en el que se efectua la comprobación.
	 * @param posInicial Posición desde la que se parte.
	 * @param posFinal   Posición objetivo.
	 * @return Valor booleano que indica si el movimiento propuesto es válido o no.
	 */
	@tailrec def comprobarMovimiento(tablero: List[Char], posInicial: Int, posFinal: Int): Boolean =
	{
		if (posInicial == posFinal) true //Si la posición final es igual a la posición inicial, el movimiento es válido
		else //Sino, tenemos que hacer el cálculo de la nueva posición
		{
			val nuevoInicial = calcularMovimiento(tablero, posInicial, posFinal)
			if (nuevoInicial == -1) false //Si este valor es -1 es que no quedan movimientos válidos con esta pieza
			else comprobarMovimiento(tablero, nuevoInicial, posFinal)
		}
	}
	
	/**
	 * Método utilizado para conseguir la posicion de una bola en el tablero a partir de sus coordenadas.
	 *
	 * @param tablero Tablero sobre el que se realiza la operacion.
	 * @return El índice en la lista de la bola seleccionada.
	 */
	@tailrec def seleccionarBola(tablero: List[Char]): Int =
	{
		println("¿En qué fila se encuentra la bola deseada?")
		val fila = readInt()
		println("¿En qué columna se encuentra la bola deseada?")
		val columna = readInt()
		val posicionBola = conseguirIndice(columna, fila)
		if (posicionBola >= 0 && posicionBola < tablero.length) posicionBola else
		{
			println("Esa posición no se encuentra dentro del tablero, por favor vuelva a intentarlo")
			seleccionarBola(tablero)
		}
	}
	
	/**
	 * Método utilizado para pedir al usuario el movimiento que quiere realizar.
	 *
	 * @param tablero    Tablero sobre el que se va a realizar el movimiento.
	 * @param posInicial Posición donde se inicia el movimiento.
	 * @return Índice de la posición objetivo del movimiento del usuario.
	 */
	@tailrec def pedirMovimiento(tablero: List[Char], posInicial: Int): Int =
	{
		println("¿En qué fila quieres poner la bola seleccionada?")
		val fila = readInt()
		println("¿En qué columna quieres poner la bola seleccionada?")
		val columna = readInt()
		val posFinal = conseguirIndice(columna, fila) //Usamos este método para calcular el índice mediante columna y fila
		if (comprobarMovimiento(tablero, posInicial, posFinal)) posFinal else
		{
			println("Movimiento no válido, vuelva a intentarlo con otro movimiento")
			pedirMovimiento(tablero, posInicial)
		}
	}
	
	/**
	 * Método que realiza un movimiento dentro del tablero.
	 *
	 * @param bola    Valor que representa el color de la bola que el usuario quiere poner.
	 * @param pos     Posición en la que se colocará la bola.
	 * @param tablero Tablero sobre el que se realizan las operaciones.
	 * @return Tablero actualizado una vez se han realizado los movimientos indicados.
	 */
	def realizarMovimiento(bola: Char, pos: Int, tablero: List[Char]): List[Char] =
	{
		if (tablero.isEmpty) Nil else if (pos == 1) bola :: tablero.tail else tablero.head :: realizarMovimiento(bola, pos - 1, tablero.tail)
	}
	
	/**
	 * Bucle recursivo principal del juego.
	 *
	 * @param tablero Tablero con el que se desea continuar la ejecución del juego.
	 */
	@tailrec def bucleJuego(tablero: List[Char]): Unit =
	{
		mostrarTablero(tablero)
		if (comprobarFin(tablero))
			{
				println("Se acabó el juego")
			} else
			{
				val posicionInicialElegida = seleccionarBola(tablero)
				val posicionFinalElegida = pedirMovimiento(tablero, posicionInicialElegida)
				val aux = realizarMovimiento(recorrerTablero(tablero, posicionInicialElegida), posicionFinalElegida, tablero)
				val actualizado = realizarMovimiento('-', posicionInicialElegida, aux)
				bucleJuego(actualizado)
			}
	}
	
	def main(args: Array[String]): Unit =
	{
		val aux = List.fill(81)('-')
		val tablero = crearTablero(aux, 9)
		bucleJuego(tablero)
	}
}
