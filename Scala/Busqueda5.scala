

object Busqueda5 {
  def comprobarHorizontal(tablero: List[Char], index: Int, seleccion: List[Int]) : List[Int] = {
    
  
		if (recorrerTablero(tablero, index) == recorrerTablero(tablero, index + 1)){ //El siguiente elemento es igual 
			 val seleccion1 = (index + 1)::seleccion   // Añadimos la bola a la lista de seleccionados
		}
		else {
		  val seleccion1 = List(index + 1) //Volvemos a empezar la lista si no coincide
		}
		if (seleccion1.lenght >= 5){//Hemos encontrado 5 iguales juntas
		  seleccion1 //Devolvemos la lista  
		}
		else if (index == 82) { seleccion1 //LLega al final y no ha encontrado coincidencias
    }
		else{	    
  		 if (index + seleccion1.lenght - 1 % 9 == 0){ //Cambio de fila 
  			val seleccionnew = List(index + 1) //Si cambiamos de fila vuelve a empezar
  			comprobarHorizontal(tablero, index + seleccion1.lenght, seleccionnew)
  		 }
  		else comprobarHorizontal(tablero, index + seleccion1.lenght, seleccion1)
		}
  		
		}
		
	def comprobarVertical(tablero: List[Char], index: Int, seleccion: List[Int]) : List[Int] = {
	  
	  if (recorrerTablero(tablero, index) == recorrerTablero(tablero, index + 9)){ //El  elemento de la siguiente columna es igual 
		  
			comprobarVertical(tablero, index + 9, index + 1::seleccion) // Añadimos la bola a la lista de seleccionados
    	
	  }
	  else if (seleccion1.lenght >= 5)
    	  seleccion1
    	else if (index == 82){ seleccion1 //LLega al final y no ha encontrado coincidencias
    	}
		else {
		  val seleccion1 = List(index + 1)	
		}
			  comprobarVertical(tablero, index + 1, seleccion1)
	}
	
	def comprobarDiagonal(tablero: List[Char], index: Int, seleccion: List[Int] ,anterior: Int) : List[Int] = {
	  
	   if (anterior == 1) { //Anterior = 1 significa que el anterior movimiento fue horizontal
	    if (recorrerTablero(tablero, index) == recorrerTablero(tablero, index + 9)){ //El  elemento de la siguiente columna es igual 
	      if (seleccion1.lenght + 1 >= 5){//Hemos encontrado 5 iguales juntas
		      index + 9::seleccion1 //Devolvemos la lista  
	      }
	      else comprobarDiagonal(tablero,index + 9, index+9::seleccion,0)//Volvemos a comprobar, ahora con la horizontal
	    }
	    else List(1,2) //si no coincide, devuelve lista ejemplo 
	  }
	   else //Anterior = 0, el anterior movimiento fue vertical
	     if (recorrerTablero(tablero, index) == recorrerTablero(tablero, index + 1)){ //El  elemento de la siguiente es igual, diagonal a derechas
	      if (seleccion1.lenght + 1 >= 5){//Hemos encontrado 5 iguales juntas
		      index + 1::seleccion1 //Devolvemos la lista  
	      }
	      else comprobarDiagonal(tablero,index + 1, index+1::seleccion,1)//Volvemos a comprobar, ahora con la vertical
	    }	    
	     else if (recorrerTablero(tablero, index) == recorrerTablero(tablero, index - 1)){ //El  elemento de la siguiente es igual, diagonal a izquierdas
	      if (seleccion1.lenght + 1 >= 5){//Hemos encontrado 5 iguales juntas
		      index + 1::seleccion1 //Devolvemos la lista  
	      }
	      else comprobarDiagonal(tablero,index - 1, index-1::seleccion,1)//Volvemos a comprobar, ahora con la vertical
	    }
	    else List(1,2) //si no coincide, devuelve lista ejemplo 
	}

	def comprobar5(tablero: List[Char], index: Int) : List[Int] = {
	  
	  //Por como esta implementado (empezando a buscar en la posicion 1 y bajando, solo hay que buscar las diagonales hacia abajo en el tablero
	  val seleccion  = List[Int]()
	  if (comprobarHorizontal(tablero,1, seleccion).length >= 5){ //Empezamos la busqueda por la horizontal
		     comprobarHorizontal(tablero,1, seleccion)
	      }
	  else if (comprobarVertical(tablero,1, seleccion).length >= 5){ //EBusqueda por la vertical
		     comprobarVertical(tablero,1, seleccion)
	      }
	  else if (comprobarDiagonal(tablero,1, seleccion,0).length >= 5){ //Empezamos la busqueda daigonal por el inicio Horizontal
		     comprobarDiagonal(tablero,1,seleccion,0)
	      }
	  else if (comprobarDiagonal(tablero,1, seleccion,1).length >= 5){ //Empezamos la busqueda daigonal por el inicio Vertical
		     comprobarDiagonal(tablero,1, seleccion,1)
	      }
	  else println ("No hay coincidencias")
	     
  }
}