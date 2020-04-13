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
	
	/**
	 * Método utilizado para poder imprimir el tablero por la pantalla.
	 *
	 * @param tablero Tablero que queremos enseñar por pantalla.
	 */
	def enseñarTablero(tablero: List[Int]) //Esta funcion nos sirve para mostrar el tablero por el terminal
	{
		def enseñarDentro(tablero: List[Int], i: Int) //Esta función nos permite usar un índice para formatear la salida
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
//		//TODO: Encontrar manera de comprobar si hay 5 bolas adyacentes del mismo color, teniendo en cuenta
//		// todas las diagonales posibles
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
		val response = readInt()
		if (response == 1) true
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
	 * @param tablero Tablero sobre el que se realiza la comprobación
	 * @param posInicial Posición desde la que se parte
	 * @param posFinal   Posición a la que se quiere llegar
	 * @return Siguiente movimiento válido posible
	 */
	def calcularMovimiento(tablero: List[Int], posInicial: Int, posFinal: Int): Int =
	{
		val filaInicial = (posInicial + 9) / 9
		val colInicial = posInicial / 9
		val filaFinal = (posFinal + 9) / 9
		val colFinal = posFinal / 9
		val filaPosible =
			if (filaInicial<filaFinal) filaInicial+1
			else if (filaInicial>filaFinal) filaInicial-1
			else filaInicial
		val colPosible =
			if (colInicial<colFinal) colInicial+1
			else if (colInicial>colFinal) colInicial-1
			else colInicial
		val filaPosibleColInicial = conseguirIndice(colInicial, filaPosible) //El índice de colInicial y la nueva fila
		val filaInicialColPosible = conseguirIndice(colPosible, filaInicial) //El índice de la columna nueva y filaInicial
		if (filaPosible!=filaInicial && recorrerTablero(tablero, filaPosibleColInicial)== 0) filaPosibleColInicial
		else if (colPosible != colInicial && recorrerTablero(tablero, filaInicialColPosible) == 0) filaInicialColPosible
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
		//TODO: Crear un método que compruebe si un movimiento es válido o no
		if (posInicial == posFinal) true
		else
		{
			val nuevoInicial = calcularMovimiento(tablero, posInicial, posFinal)
			if (nuevoInicial == -1) false //Si este valor es -1 es que no quedan movimientos válidos con esta pieza
			else comprobarMovimiento(tablero, nuevoInicial, posFinal)
		}
	}
	
	/**
	 * Metodo utilizado para pedir la posición de la bola que el usuario quiere mover.
	 *
	 * @return Retorna un valor numérico que indica la bola seleccionada por el usuario.
	 */
	def seleccionarBola(tablero: List[Int]): Int =
	{
		println("En qué fila se encuentra la bola deseada")
		val fila = readInt()
		println("En qué columna se encuentra la bola deseada")
		val columna = readInt()
		val aux = conseguirIndice(columna, fila)
		if (aux > 0 && aux <= 81) recorrerTablero(tablero, aux)
		else
		{
			println("Esa posición no se encuentra dentro del tablero, por favor vuelva a intentarlo")
			seleccionarBola(tablero)
		}
	}
	
	/**
	 * Método utilizado para pedir la posición objetivo del movimiento al usuario.
	 *
	 * @return Retorna un valor numérico que indica la posición deseada por el usuario.
	 */
	def pedirMovimiento(tablero: List[Int], posInicial: Int): Int =
	{
		println("En qué fila quieres poner la bola seleccionada")
		val fila = readInt()
		println("En qué columna quieres poner la bola seleccionada")
		val columna = readInt()
		val posFinal = conseguirIndice(columna, fila) //Usamos este método para calcular el índice mediante columna y fila
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
		//comprobar5EnRaya(tablero)
	}
	
	/**
	 * Bucle recursivo principal del juego.
	 *
	 * @param tablero Tablero con el que se desea continuar la ejecución del juego.
	 */
	def bucleJuego(tablero: List[Int])
	{
		enseñarTablero(tablero)
		if (comprobarFin(tablero))
		{
			println("Se acabó el juego")
		}
		else
		{
			val bolaElegida = seleccionarBola(tablero)
			val posicionElegida = pedirMovimiento(tablero, bolaElegida)
			val actualizado = realizarMovimiento(bolaElegida, posicionElegida, tablero)
			bucleJuego(actualizado)
		}
	}
	
	def main(args: Array[String]): Unit =
	{
		val tablero = List.fill(81)(0)
		bucleJuego(tablero)
	}
}
