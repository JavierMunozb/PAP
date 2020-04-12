object mainObject
{
	
	import scala.io.StdIn.{readLine, readInt}
	
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
	
	def comprobarFin(tablero: List[Int]): Boolean =
	{
		println("¿Hemos terminado? 1 (Sí) or 0 (No)")
		val response = readInt()
		if (response == 1) true
		else false
	}
	
	def seleccionarBola(): Int =
	{
		println("Seleccione la bola que quiere mover")
		val aux = readInt()
		aux
	}
	
	def pedirMovimiento(): Int =
	{
		println("En qué posición quieres poner la bola seleccionada")
		val aux: Int = readInt()
		aux
	}
	
	def realizarMovimiento(color: Int, pos: Int, tablero: List[Int]): List[Int] =
	{
		if (tablero.isEmpty) Nil //Si la lista está vacía, no se puede realizar el movimiento
		else if (pos == 1) color :: tablero.tail //Si la posición indicada es la primera, se realiza el movimiento
		else tablero.head :: realizarMovimiento(color, (pos - 1), tablero.tail) //Se continua recursivamente hasta pos = 1
	}
	
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
