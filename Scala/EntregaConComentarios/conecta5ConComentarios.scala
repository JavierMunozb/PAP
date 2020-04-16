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
				case Nil => 'Z' //Retornamos un valor fuera de las posibilidades del juego al encontrar una lista vacía
			}
		}
		
		recorrerDentro(tablero, 1, index)
	}
	
	def recorrerLista(lista: List[Int], index: Int): Int =
	{
		@tailrec def ListaDentro(tablero: List[Int], i: Int, index: Int): Int =
		{
			tablero match
			{
				case head :: tail => if (i == index) head //Si nos encontramos en el índice requerido, devolvemos el valor
				else ListaDentro(tail, i + 1, index) //Sino, seguimos buscando
				case Nil => 'Z'
			}
		}
		
		ListaDentro(lista, 1, index)
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
		if (numeroDeValores > 1) crearTablero(aux, numeroDeValores - 1) else aux
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
				case head :: tail => if (head == '-') false else if (head == '-' && i ==81) true else comprobarDentro(tail, i + 1)
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
		if (posicionBola >= 0 && posicionBola < tablero.length && recorrerTablero(tablero, posicionBola) != '-') posicionBola else
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
	 * @param tablero    Tablero con el que se desea continuar la ejecución del juego.
	 * @param puntuacion Puntuación del usuario hasta el momento
	 */
	@tailrec def bucleJuego(tablero: List[Char], puntuacion: Int): Unit =
	{
		mostrarTablero(tablero)
		if (comprobarFin(tablero))
		{
			println("Se acabó el juego")
			println("Escriba 1 si quiere volver a jugar, 2 si quiere guardar su puntuacion, o tres si quiere salir")
			val opcion = readInt()
			opcion match
			{
				case 1 =>
				{ // Jugar de nuevo
					val aux = List.fill(81)('-')
					val tablero = crearTablero(aux, 9)
					bucleJuego(tablero, 0)
				}
				case 2 => puntuacion //Devuelve la puntuación para guardarla
				case 3 => Nil //Sale de la aplicación
			}
		} else
		{
			val posicionInicialElegida = seleccionarBola(tablero)
			val posicionFinalElegida = pedirMovimiento(tablero, posicionInicialElegida)
			val aux = realizarMovimiento(recorrerTablero(tablero, posicionInicialElegida), posicionFinalElegida, tablero)
			val actualizado = realizarMovimiento('-', posicionInicialElegida, aux)
			val seleccion = comprobar5(tablero)
			/*if(seleccion.length != 0){
				val puntuacionActualizada = seleccion.length * 75
				val nuevoTablero = borrar5(seleccion, actualizado)
				bucleJuego(nuevoTablero, puntuacion + puntuacionActualizada)
			}else{
			*/
			val NuevoTablero = crearTablero(actualizado, 3) //Añadimos 3 bolas
			bucleJuego(NuevoTablero, puntuacion)
		}
	}
	
	def comprobarHorizontal(tablero: List[Char], index: Int, seleccion: List[Int]): List[Int] =
	{
		
		val seleccion1 = index :: seleccion
		if (index % 9 == 0)
		{ //Cambio de fila
			comprobarHorizontal(tablero, index + seleccion1.length, List(index + 1)) //Si cambiamos de fila vuelve a empezar
		}
		
		if (recorrerTablero(tablero, index) == recorrerTablero(tablero, index + 1))
		{ //El siguiente elemento es igual
			// Añadimos la posicion de la bola a la lista de seleccionados
			if (seleccion1.length >= 5)
			{//Hemos encontrado 5 iguales juntas
				seleccion1 //Devolvemos la lista
			} else if (tablero.length == index)
			{
				seleccion1 //LLega al final y no ha encontrado coincidencias
			} else comprobarHorizontal(tablero, index + 1, seleccion1)
		} else
		{
			val seleccionnew = List(index + 1) //Volvemos a empezar la lista si no coincide
			if (index == tablero.length)
			{
				seleccionnew //LLega al final y no ha encontrado coincidencias
			} else
			{
				if (index + seleccion1.length - 1 % 9 == 0)
				{ //Cambio de fila
					comprobarHorizontal(tablero, index + seleccion1.length, List(index + 1)) //Si cambiamos de fila vuelve a empezar
				} else comprobarHorizontal(tablero, index + seleccion1.length, seleccionnew)
			}
		}
	}
	
	def comprobarVerticales(tablero: List[Char], index: Int, seleccion: List[Int]): List[Int] =
	{
		
		val seleccion1 = comprobarVertical(tablero, index, List(index))
		if (seleccion1.length >= 5) seleccion1 else
		{
			comprobarVerticales(tablero, index + 1, List(index + 1))
		}
	}
	
	def comprobarVertical(tablero: List[Char], index: Int, seleccion: List[Int]): List[Int] =
	{
		
		if (index == tablero.length)
		{
			Nil
		} else
		{
			if (recorrerTablero(tablero, index) == recorrerTablero(tablero, index + 9))
			{ //El  elemento de la siguiente columna es igual
				if (seleccion.length + 1 >= 4)
				{
					val newSeleccion = index + 9 :: seleccion
					val seleccionDef = recorrerLista(seleccion, seleccion.length) - 9 :: newSeleccion
					seleccionDef
				} else
				{
					comprobarVertical(tablero, index + 9, index + 9 :: seleccion) // Añadimos la posicion de la bola a la lista de seleccionados
				}
			} else
			{
				print(index)
				comprobarVertical(tablero, index + 1, List(index + 1))
			}
		}
	}
	
	def comprobarDiagonalDerecha(tablero: List[Char], index: Int, seleccion: List[Int], anterior: Int): List[Int] =
	{
		
		if (anterior == 1)
		{ //Anterior = 1 significa que el anterior movimiento fue horizontal
			if (recorrerTablero(tablero, index) == recorrerTablero(tablero, index + 9))
			{ //El  elemento de la siguiente columna es igual
				if (seleccion.length + 1 >= 5)
				{//Hemos encontrado 5 iguales juntas
					println(index + 9 :: seleccion)
					index + 9 :: seleccion //Devolvemos la lista
				} else comprobarDiagonalDerecha(tablero, index + 9, index + 9 :: seleccion, 0)//Volvemos a comprobar, ahora con la horizontal
			} else Nil //si no coincide, devuelve lista ejemplo
		} else //Anterior = 0, el anterior movimiento fue vertical
			if (recorrerTablero(tablero, index) == recorrerTablero(tablero, index + 1))
			{ //El  elemento de la siguiente es igual, diagonal a derechas
				if (seleccion.length + 1 >= 5)
				{//Hemos encontrado 5 iguales juntas
					println(index + 1 :: seleccion)
					index + 1 :: seleccion //Devolvemos la lista
				} else comprobarDiagonalDerecha(tablero, index + 1, index + 1 :: seleccion, 1)//Volvemos a comprobar, ahora con la vertical
			} else Nil //si no coincide, devuelve lista ejemplo
	}
	
	def comprobarDiagonalIzquierda(tablero: List[Char], index: Int, seleccion: List[Int], anterior: Int): List[Int] =
	{
		
		if (anterior == 1)
		{ //Anterior = 1 significa que el anterior movimiento fue horizontal
			if (recorrerTablero(tablero, index) == recorrerTablero(tablero, index + 9))
			{ //El  elemento de la siguiente columna es igual
				if (seleccion.length + 1 >= 5)
				{//Hemos encontrado 5 iguales juntas
					index + 9 :: seleccion //Devolvemos la lista
				} else comprobarDiagonalIzquierda(tablero, index + 9, index + 9 :: seleccion, 0)//Volvemos a comprobar, ahora con la horizontal
			} else Nil //si no coincide, devuelve lista ejemplo
		} else //Anterior = 0, el anterior movimiento fue vertical
			if (recorrerTablero(tablero, index) == recorrerTablero(tablero, index - 1))
			{ //El  elemento de la siguiente es igual, diagonal a izquierdas
				if (seleccion.length + 1 >= 5)
				{//Hemos encontrado 5 iguales juntas
					index - 1 :: seleccion //Devolvemos la lista
				} else comprobarDiagonalIzquierda(tablero, index - 1, index - 1 :: seleccion, 1)//Volvemos a comprobar, ahora con la vertical
			} else Nil //si no coincide, devuelve lista ejemplo
	}
	
	
	def comprobarDiagonales(tablero: List[Char], index: Int, seleccion: List[Int]): List[Int] =
	{
		
		
		val diagonalDerecha1 = comprobarDiagonalDerecha(tablero, index, seleccion, 0)
		val diagonalIzquierda1 = comprobarDiagonalIzquierda(tablero, index, seleccion, 0)
		val diagonalDerecha2 = comprobarDiagonalDerecha(tablero, index, seleccion, 1)
		val diagonalIzquierda2 = comprobarDiagonalIzquierda(tablero, index, seleccion, 1)
		
		
		if (diagonalDerecha1.length >= 5)
		{ //Empezamos la busqueda diagonal derecha por el inicio Horizontal
			diagonalDerecha1
		} else if (diagonalDerecha2.length >= 5)
		{ //Empezamos la busqueda diagonal derecha por el inicio Vertical
			diagonalDerecha2
		} else if (diagonalIzquierda1.length >= 5)
		{ //Empezamos la busqueda diagonal izquierda  por el inicio Horizontal
			diagonalIzquierda1
		} else if (diagonalIzquierda2.length >= 5)
		{ //Empezamos la busqueda diagonal izquierda por el inicio Horizontal
			diagonalIzquierda2
		} else if (index < tablero.length) comprobarDiagonales(tablero, index + 1, List(index + 1)) else Nil
	}
	
	def comprobar5(tablero: List[Char]): List[Int] =
	{
		
		//Por como esta implementado (empezando a buscar en la posicion 1 y bajando, solo hay que buscar las diagonales hacia abajo en el tablero
		if (comprobarHorizontal(tablero, 1, Nil).length >= 5)
		{ //Empezamos la busqueda por la horizontal
			comprobarHorizontal(tablero, 1, Nil)
		} else if (comprobarVertical(tablero, 1, Nil).length >= 5)
		{ //Busqueda por la vertical
			comprobarVertical(tablero, 1, Nil)
		} else if (comprobarDiagonales(tablero, 1, Nil).length >= 5)
		{ //Hacemos busqueda Diafonal
			comprobarDiagonales(tablero, 1, Nil)
		} else Nil
	}
	
	def borrar5(lista: List[Int], tablero: List[Char]): List[Char] =
	{
		realizarMovimiento('-', lista.head, tablero)
		borrar5(lista.tail, tablero)
	}
	
	def main(args: Array[String]): Unit =
	{
		val aux = List.fill(81)('-')
		val tablero = crearTablero(aux, 9)
		bucleJuego(tablero, 0)
	}
}