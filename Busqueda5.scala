

object Busqueda5 {
  
  def comprobarHorizontal(tablero: List[Char], index: Int, seleccion: List[Int]) : List[Int] = {
    val seleccion1 = InsertarElemento(index+1, seleccion.length + 1, seleccion)
		if (recorrerTablero(tablero, index) == recorrerTablero(tablero, index + 1)){ //El siguiente elemento es igual 
		  val seleccion1 = InsertarElemento(index+1, seleccion.length + 1, seleccion) // Añadimos la posicion de la bola a la lista de seleccionados
			if (seleccion1.length >= 5){//Hemos encontrado 5 iguales juntas
		    seleccion1 //Devolvemos la lista  
		  }
		  else if (tablero.length == index) { seleccion1 //LLega al final y no ha encontrado coincidencias
      }
		  else{	    
  		  if (index + seleccion1.length - 1 % 9 == 0){ //Cambio de fila 
  			  comprobarHorizontal(tablero, index + seleccion1.length, List(index + 1)) //Si cambiamos de fila vuelve a empezar
  		  }
  		  else comprobarHorizontal(tablero, index + 1, seleccion1)
		  }
		}
		else {
		  val seleccionnew = List(index + 1) //Volvemos a empezar la lista si no coincide
		  if (index == tablero.length) { seleccionnew //LLega al final y no ha encontrado coincidencias
      }
		  else{	    
  		  if (index + seleccion1.length - 1 % 9 == 0){ //Cambio de fila 
  			  comprobarHorizontal(tablero, index + seleccion1.length, List(index + 1)) //Si cambiamos de fila vuelve a empezar
  		  }
  		  else comprobarHorizontal(tablero, index + seleccion1.length, seleccionnew)
		  }
		}	
  }
		
	def comprobarVertical(tablero: List[Char], index: Int, seleccion: List[Int]) : List[Int] = {
	  
	  if (recorrerTablero(tablero, index) == recorrerTablero(tablero, index + 9)){ //El  elemento de la siguiente columna es igual 
	    if (seleccion.length + 1 >= 5){
    	   InsertarElemento(index+9, seleccion.length + 1, seleccion)
	    }
		  else if (index == tablero.length){ seleccion //LLega al final y no ha encontrado coincidencias
    	}
		  else {
		 comprobarVertical(tablero, index + 9,InsertarElemento(index + 9, seleccion.length + 1, seleccion)) // Añadimos la posicion de la bola a la lista de seleccionados
	    }
	  }
		else {
		  comprobarVertical(tablero, index + 1, List(index + 1)	)
	  }
	}
	
	
	def comprobarDiagonal(tablero: List[Char], index: Int, seleccion: List[Int] ,anterior: Int) : List[Int] = {
	  
	   if (anterior == 1) { //Anterior = 1 significa que el anterior movimiento fue horizontal
	    if (recorrerTablero(tablero, index) == recorrerTablero(tablero, index + 9)){ //El  elemento de la siguiente columna es igual 
	      if (seleccion.length + 1 >= 5){//Hemos encontrado 5 iguales juntas
		       InsertarElemento(index+9, seleccion.length + 1, seleccion) //Devolvemos la lista  
	      }
	      else comprobarDiagonal(tablero,index + 9, index+9::seleccion,0)//Volvemos a comprobar, ahora con la horizontal
	    }
	    else List(1,2) //si no coincide, devuelve lista ejemplo 
	  }
	   else //Anterior = 0, el anterior movimiento fue vertical
	     if (recorrerTablero(tablero, index) == recorrerTablero(tablero, index + 1)){ //El  elemento de la siguiente es igual, diagonal a derechas
	      if (seleccion.length + 1 >= 5){//Hemos encontrado 5 iguales juntas
		       InsertarElemento(index+1, seleccion.length + 1, seleccion) //Devolvemos la lista  
	      }
	      else comprobarDiagonal(tablero,index + 1, InsertarElemento(index -1, seleccion.length + 1, seleccion),1)//Volvemos a comprobar, ahora con la vertical
	    }	    
	     else if (recorrerTablero(tablero, index) == recorrerTablero(tablero, index - 1)){ //El  elemento de la siguiente es igual, diagonal a izquierdas
	      if (seleccion.length + 1 >= 5){//Hemos encontrado 5 iguales juntas
		      InsertarElemento(index -1, seleccion.length + 1, seleccion) //Devolvemos la lista  
	      }
	      else comprobarDiagonal(tablero,index - 1, InsertarElemento(index -1, seleccion.length + 1, seleccion),1)//Volvemos a comprobar, ahora con la vertical
	    }
	    else List(1,2) //si no coincide, devuelve lista ejemplo 
	}
	
	
	def InsertarElemento(num: Int, pos: Int, lista: List[Int]): List[Int] =
	{
		if (lista.isEmpty) Nil else if (pos == 1) num :: lista.tail else lista.head :: InsertarElemento(num, pos - 1, lista.tail)
	}
	
	
	def comprobarDiagonales(tablero: List[Char], index: Int, seleccion: List[Int]) : List[Int] = {
	  
	  if (comprobarDiagonal(tablero,index, seleccion,0).length >= 5){ //Empezamos la busqueda diagonal por el inicio Horizontal
		     comprobarDiagonal(tablero,index,seleccion,0)
	      }
	  else if (comprobarDiagonal(tablero,index, seleccion,1).length >= 5){ //Empezamos la busqueda daigonal por el inicio Vertical
		     comprobarDiagonal(tablero,index, seleccion,1)
	      }
	  else 
	    if (index < tablero.length) comprobarDiagonales(tablero,index + 1, List(index + 1)) else List(1,2)
	    
	    
	}
	
	
	def comprobar5(tablero: List[Char], index: Int) : List[Int] = {
	  
	  //Por como esta implementado (empezando a buscar en la posicion 1 y bajando, solo hay que buscar las diagonales hacia abajo en el tablero
	  if (comprobarHorizontal(tablero,1, Nil).length >= 5){ //Empezamos la busqueda por la horizontal
		     comprobarHorizontal(tablero,1, Nil)
	      }
	  else if (comprobarVertical(tablero,1, Nil).length >= 5){ //Busqueda por la vertical
		     comprobarVertical(tablero,1, Nil)
	      }
	   else if (comprobarDiagonales(tablero,index, Nil).length >= 5){ //Hacemos busqueda Diafonal
		     comprobarDiagonales(tablero,index, Nil)
	      }
	  else  println ("No hay coincidencias")
	     
  }
}
