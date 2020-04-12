object mainObject
{
	
	import scala.io.StdIn.{readLine, readInt}
	
	/**
	 * Método utilizado para poder imprimir el tablero por la pantalla
	 *
	 * @param tablero tablero que queremos enseñar por pantalla.
	 */
	def enseñarTablero(tablero: List[Int]) //Esta funcion nos sirve para mostrar el tablero por el terminal
	{
		def enseñarDentro(tablero: List[Int], i : Int) //Esta funcion interna nos permite usar un indice para formatear la salida
		{
			tablero match
			{
				case head :: tail =>
					if (i % 9 == 0) println(head)
					else print(head)
					enseñarDentro(tail, i+1)
				case Nil =>
			}
		}
		enseñarDentro(tablero, 1)
	}
	
	/**
	 * Método que comprueba si quedan más movimientos disponibles en en tablero.
	 *
	 * @param tablero Tablero sobre el que queremos comprobar si quedan más movimientos disponibles.
	 * @return Retornamos si quedan más movimientos disponibles o no.
	 */
	def comprobarFin(tablero: List[Int]): Boolean =
	{
		println("¿Hemos terminado? 1 (Sí) or 0 (No)") //Este metodo todavia no está implementado completamente, es solo para probar
		val response = readInt()
		if (response == 1) true
		else false
	}
	
	/**
	 * Metodo utilizado para pedir la posición de la bola que el usuario quiere mover.
	 *
	 * @return Retorna un valor numérico que indica la bola seleccionada por el usuario.
	 */
	def seleccionarBola(): Int =
	{
		println("Seleccione la bola que quiere mover")
		val aux = readInt()
		aux
	}
	
	/**
	 * Método utilizado para pedir la posición objetivo del movimiento al usuario
	 *
	 * @return Retorna un valor numérico que indica la posición deseada por el usuario.
	 */
	def pedirMovimiento(): Int =
	{
		println("En qué posición quieres poner la bola seleccionada")
		val aux: Int = readInt()
		aux
	}
	
	/**
	 * Este método realiza un movimiento dentro del tablero.
	 *
	 * @param color Valor que representa el color de la bola que el usuario quiere poner.
	 * @param pos Posición en la que se colocará la bola.
	 * @param tablero Tablero sobre el que se realizan las operaciones.
	 * @return Tablero actualizado una vez se han realizado los movimientos indicados.
	 */
	def realizarMovimiento(color: Int, pos: Int, tablero: List[Int]): List[Int] =
	{
		if (tablero.isEmpty) Nil //Si la lista está vacía, no se puede realizar el movimiento
		else if (pos == 1) color :: tablero.tail //Si la posición indicada es la primera, se realiza el movimiento
		else tablero.head :: realizarMovimiento(color, (pos - 1), tablero.tail) //Se continua recursivamente hasta pos = 1
	}
	
	/**
	 * Bucle recursivo principal del juego.
	 *
	 * @param tablero Tablero con el que se desea continuar la ejecución del juego
	 */
	def BucleJuego(tablero: List[Int])
	{
		if (comprobarFin(tablero))
		{
			println("Se acabó el juego")
		} else
		{
			enseñarTablero(tablero)
			val bolaElegida = seleccionarBola()
			val posicionElegida = pedirMovimiento()
			val actualizado: List[Int] = realizarMovimiento(bolaElegida, posicionElegida, tablero)
			BucleJuego(actualizado)
		}
	}
	
	def main(args: Array[String]): Unit =
	{
		val tablero = List.fill(81)(0)
		BucleJuego(tablero)
	}
}
