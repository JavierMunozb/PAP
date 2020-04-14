object mainObject
{
	
	import scala.io.StdIn.{readLine, readInt}
	
	/**
	 * Método que recorre el tablero hasta una posición dada y retorna el valor contenido en ella
	 *
	 * @param tablero Tablero que se quiere recorrer.
	 * @param index   Índice al que se quiere llegar en el tablero.
	 * @return Valor de la posición indicada del tablero.
	 */
	def recorrerTablero(tablero: List[Int], index: Int): Int =
	{
		def recorrerDentro(tablero: List[Int], i: Int, index: Int): Int =
		{
			tablero match
			{
				case head :: tail => if (i == index) head
				else recorrerDentro(tail, i + 1, index)
				case Nil => -1
			}
		}
		
		recorrerDentro(tablero, 1, index)
	}

//	/**
//	 * Método utilizado para crear y popular el tablero inicial
//	 *
//	 * @return Tablero creado de manera aleatoria
//	 */
//	def crearTablero(): List[Int] =
//	{
//		//TODO: Crear método de generación de tableros aleatorios
//	}
	
	/**
	 * Método utilizado para poder imprimir el tablero por la pantalla.
	 *
	 * @param tablero Tablero que queremos enseñar por pantalla.
	 */
	def enseñarTablero(tablero: List[Int]): Unit = //Esta funcion nos sirve para mostrar el tablero por el terminal
	{
		def enseñarDentro(tablero: List[Int], i: Int): Unit = //Esta función nos permite usar un índice para formatear la salida
		{
			tablero match
			{
				case head :: tail => if (i % 9 == 0) println(head)
				else print(head)
					enseñarDentro(tail, i + 1)
				case Nil =>
			}
		}
		
		enseñarDentro(tablero, 1)
	}
	
//	def comprobar5EnRaya(tablero: List[Int]): List[Int] =
//	{
//		//TODO: Encontrar manera de comprobar si hay 5 bolas adyacentes del mismo color, teniendo en cuenta todas las diagonales posibles
//	}
	
	/**
	 * Método que comprueba si quedan más movimientos disponibles en en tablero.
	 *
	 * @param tablero Tablero sobre el que queremos comprobar si quedan más movimientos disponibles.
	 * @return Retornamos si quedan más movimientos disponibles o no.
	 */
	def comprobarFin(tablero: List[Int]): Boolean =
	{
		//TODO: Implementar un método que compruebe si queda algún movimiento viable más
		println("¿Hemos terminado? 1 (Sí) or 0 (No)") //Este metodo todavia no está implementado, es para testear
		val answer = readInt()
		if (answer == 1) true
		else false
	}
	
	/**
	 * Método simple para poder conseguir el índice compatible con la lista utilizando la fila y columna
	 *
	 * @param columna Número de la columna
	 * @param fila    Número de la fila
	 * @return Índice compatible con la lista
	 */
	def conseguirIndice(columna: Int, fila: Int): Int =
	{
		9 * (fila - 1) + columna
	}
	
	/**
	 * Método utilizado para calcular el siguiente movimiento válido
	 *
	 * @param tablero    Tablero sobre el que se realiza la comprobación
	 * @param posInicial Posición desde la que se parte
	 * @param posFinal   Posición a la que se quiere llegar
	 * @return Siguiente movimiento válido posible
	 */
	def calcularMovimiento(tablero: List[Int], posInicial: Int, posFinal: Int): Int =
	{
		val filaInicial = (posInicial + 9) / 9
		val colInicial = posInicial % 9
		val filaFinal = (posFinal + 9) / 9
		val colFinal = posFinal % 9
		val filaPosible = if (filaInicial < filaFinal) filaInicial + 1
		else if (filaInicial > filaFinal) filaInicial - 1
		else filaInicial
		val colPosible = if (colInicial < colFinal) colInicial + 1
		else if (colInicial > colFinal) colInicial - 1
		else colInicial
		println("El valor de la posición inicial es "+posInicial)
		println("El valor de la columna inicial es "+colInicial)
		println("El valor de la fila inicial es "+filaInicial)
		println("El valor de la columna final es  "+colFinal)
		println("El valor de la fila final es  "+filaFinal)
		println("El valor de la nueva columna posible es "+colPosible)
		println("El valor de la nueva fila posible es "+filaPosible)
		
		if (filaPosible != filaInicial && recorrerTablero(tablero, conseguirIndice(colInicial, filaPosible)) == 0)
		{
			conseguirIndice(colInicial, filaPosible)
		}
		else if (colPosible != colInicial && recorrerTablero(tablero, conseguirIndice(colPosible, filaInicial)) == 0)
		{
			
			conseguirIndice(colPosible, filaInicial)
		}
		else -1
	}
	
	/**
	 * Método que comprueba si un movimiento es válido o no
	 *
	 * @param tablero    Tablero en el que se efectua la comprobación
	 * @param posInicial Posición desde la que se parte
	 * @param posFinal   Posición objetivo
	 * @return
	 */
	def comprobarMovimiento(tablero: List[Int], posInicial: Int, posFinal: Int): Boolean =
	{
		if (posInicial == posFinal) true
		else
		{
			val nuevoInicial = calcularMovimiento(tablero, posInicial, posFinal)
			if (nuevoInicial == -1) false //Si este valor es -1 es que no quedan movimientos válidos con esta pieza
			else comprobarMovimiento(tablero, nuevoInicial, posFinal)
		}
	}
	
	/**
	 * Método utilizado para conseguir la posicion de una bola en el tablero a partir de sus coordenadas
	 *
	 * @param tablero Tablero sobre el que se realiza la operacion
	 * @return El índice en la lista de la bola seleccionada
	 */
	def seleccionarBola(tablero: List[Int]): Int =
	{
		println("En qué fila se encuentra la bola deseada")
		val fila = readInt()
		println("En qué columna se encuentra la bola deseada")
		val columna = readInt()
		val posicionBola = conseguirIndice(columna, fila)
		if (posicionBola >= 0 && posicionBola < tablero.length) posicionBola
		else
		{
			println("Esa posición no se encuentra dentro del tablero, por favor vuelva a intentarlo")
			seleccionarBola(tablero)
		}
	}
	
	/**
	 * Método utilizado para pedir al usuario el movimiento que quiere realizar.
	 *
	 * @param tablero    Tablero sobre el que se va a realizar el movimiento
	 * @param posInicial Posición donde se inicia el movimiento
	 * @return Índice de la posición objetivo del movimiento del usuario
	 */
	def pedirMovimiento(tablero: List[Int], posInicial: Int): Int =
	{
		println("En qué fila quieres poner la bola seleccionada")
		val fila = readInt()
		println("En qué columna quieres poner la bola seleccionada")
		val columna = readInt()
		val posFinal = conseguirIndice(columna, fila) //Usamos este método para calcular el índice mediante columna y fila
		println("Se ha elegido la posición " + posFinal + " que tiene un valor de " + recorrerTablero(tablero, posFinal))
		if (comprobarMovimiento(tablero, posInicial, posFinal)) posFinal
		else
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
	def realizarMovimiento(bola: Int, pos: Int, tablero: List[Int]): List[Int] =
	{
		if (tablero.isEmpty) Nil
		else if (pos == 1) bola :: tablero.tail
		else tablero.head :: realizarMovimiento(bola, pos - 1, tablero.tail)
	}
	
	/**
	 * Bucle recursivo principal del juego.
	 *
	 * @param tablero Tablero con el que se desea continuar la ejecución del juego.
	 */
	def bucleJuego(tablero: List[Int]): Unit =
	{
		enseñarTablero(tablero)
		if (/*comprobarFin(tablero)*/ false)
			{
				println("Se acabó el juego")
			} else
			{
				val posicionInicialElegida = seleccionarBola(tablero)
				println("Se ha elegido una bola en la posición "+posicionInicialElegida+ " con el valor " + recorrerTablero(tablero, posicionInicialElegida))
				val posicionFinalElegida = pedirMovimiento(tablero, posicionInicialElegida)
				val aux = realizarMovimiento(recorrerTablero(tablero, posicionInicialElegida), posicionFinalElegida, tablero)
				val actualizado = realizarMovimiento(0, posicionInicialElegida, aux)
				bucleJuego(actualizado)
			}
	}
	
	def main(args: Array[String]): Unit =
	{
		val tablero = List(0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0)
		bucleJuego(tablero)
	}
}
