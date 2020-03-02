#include <stdio.h>
#include <stdlib.h>
#include <cuda_runtime.h>
#include <device_functions.h>

__device__ int comprobarVecinos(int *a, int idCelula, int ladoMatriz, int ladoBloque, int idBloque)
{
	int idHilo = idCelula;
	int bloques = ladoMatriz / ladoBloque; //Numero de bloques en un lado de la matriz
	int contador = 0;
	//Comprobamos si el bloque al que pertenece el hilo que ha llamado al kernel se encuentra en la esquina superior izquierda de la matriz.
	if (idBloque == 0)
	{
		//En este bloque solo hay que comprobarel lado izquierdo y el superior.
		//Comprobamos si el hilo que ha llamado al kernel se encuentra en la esquina superior izquierda del bloque.
		if (idCelula == 0)
		{
			//Creamos array con vecinos de la celula
			int vecinos[3] = { a[idHilo + 1], a[idHilo + ladoMatriz], a[idHilo + ladoMatriz + 1] };
			for (int i = 0; i < 3; i++)
			{
				if (vecinos[i] == 1)
				{
					contador += 1;
				}
			}
		}
		//Comprobamos si el hilo que ha llamado al kernel se encuentra en la esquina superior derecha del bloque.
		else if (idHilo == ladoBloque - 1)
		{
			//Creamos array con vecinos de la celula
			int vecinos[5] = { a[idHilo - 1],a[idHilo + 1], a[idHilo + ladoMatriz], a[idHilo + ladoMatriz - 1], a[idHilo + ladoMatriz + 1] };
			for (int i = 0; i < 5; i++)
			{
				if (vecinos[i] == 1)
				{
					contador += 1;
				}
			}
		}
		//Comprobamos si el hilo que ha llamado al kernel se encuentra en la esquina inferior izquierda del bloque.
		else if (idHilo == (ladoBloque * ladoMatriz - ladoMatriz))
		{
			//Creamos array con vecinos de la celula
			int vecinos[5] = { a[idHilo + 1], a[idHilo - ladoMatriz], a[idHilo - ladoMatriz + 1], a[idHilo + ladoMatriz], a[idHilo + ladoMatriz + 1] };
			for (int i = 0; i < 5; i++)
			{
				if (vecinos[i] == 1)
				{
					contador += 1;
				}
			}
		}
		//Comprobamos si el hilo que ha llamado al kernel se encuentra en el lado izquierdo (pero no en la esquina) del bloque.
		else if (idHilo % ladoMatriz == 0)
		{
			//Creamos array con vecinos de la celula
			int vecinos[5] = { a[idHilo + 1], a[idHilo + ladoMatriz], a[idHilo - ladoMatriz], a[idHilo - ladoMatriz + 1], a[idHilo + ladoMatriz + 1] };
			for (int i = 0; i < 5; i++)
			{
				if (vecinos[i] == 1)
				{
					contador += 1;
				}
			}
		}
		//Comprobamos si el hilo que ha llamado al kernel se encuentra arriba (pero no en la esquina) del bloque.
		else if (idHilo < ladoBloque)
		{
			//Creamos array con vecinos de la celula
			int vecinos[5] = { a[idHilo + 1], a[idHilo - 1], a[idHilo + ladoMatriz], a[idHilo + ladoMatriz + 1], a[idHilo + ladoMatriz - 1] };
			for (int i = 0; i < 5; i++)
			{
				if (vecinos[i] == 1)
				{
					contador += 1;
				}
			}
		}
		//Hilo esta en el medio de la matriz
		else
		{
			//Creamos array con vecinos de la celula
			int vecinos[8] = { a[idHilo + 1], a[idHilo - 1], a[idHilo + ladoMatriz], a[idHilo - ladoMatriz], a[idHilo - ladoMatriz - 1], a[idHilo - ladoMatriz + 1], a[idHilo + ladoMatriz + 1], a[idHilo + ladoMatriz - 1] };
			for (int i = 0; i < 8; i++)
			{
				if (vecinos[i] == 1)
				{
					contador += 1;
				}
			}
		}
	}
	//Comprobamos si el bloque al que pertenece el hilo que ha llamado al kernel se encuentra en la esquina superior derecha de la matriz.
	else if (idBloque == bloques - 1)
	{
		//En este bloque solo tenemos que diferenciar los hilo del lado derecho y superior.
		//Comprobamos si el hilo que ha llamado al kernel se encuentra en la esquina superior izquierda del bloque.
		if (idCelula == ladoMatriz  - (ladoBloque*(bloques - blockIdx.x)))
		{
			//Creamos array con vecinos de la celula
			int vecinos[5] = { a[idHilo + 1], a[idHilo + ladoMatriz], a[idHilo + ladoMatriz + 1], a[idHilo - ladoMatriz], a[idHilo - ladoMatriz - 1] };
			for (int i = 0; i < 5; i++)
			{
				if (vecinos[i] == 1)
				{
					contador += 1;
				}
			}
		}
		//Comprobamos si el hilo que ha llamado al kernel se encuentra en la esquina superior derecha del bloque.
		else if (idHilo == (ladoMatriz - 1) - (ladoBloque*(bloques - (blockIdx.x + 1))))
		{
			//Creamos array con vecinos de la celula
			int vecinos[3] = { a[idHilo - 1], a[idHilo + ladoMatriz], a[idHilo + ladoMatriz - 1] };
			for (int i = 0; i < 5; i++)
			{
				if (vecinos[i] == 1)
				{
					contador += 1;
				}
			}
		}
		//Comprobamos si el hilo que ha llamado al kernel se encuentra en la derecha del bloque. (Si no es esquina superior)
		else if (idHilo == (threadIdx.y + 1)* ladoMatriz - 1)
		{
			//Creamos array con vecinos de la celula
			int vecinos[5] = { a[idHilo - 1], a[idHilo + ladoMatriz], a[idHilo - ladoMatriz], a[idHilo - ladoMatriz - 1], a[idHilo + ladoMatriz - 1] };
			for (int i = 0; i < 8; i++)
			{
				if (vecinos[i] == 1)
				{
					contador += 1;
				}
			}
		}
		//Comprobamos si el hilo que ha llamado al kernel se encuentra arriba (pero no en la esquina) del bloque.
		else if (idHilo > (ladoMatriz - ladoBloque) &&  idHilo < (ladoMatriz - 1)) // La concicion del > es irrelevante en principio, si esta en este bloque la cumple siempre
		{
			//Creamos array con vecinos de la celula
			int vecinos[5] = { a[idHilo + 1], a[idHilo - 1], a[idHilo + ladoMatriz], a[idHilo + ladoMatriz + 1], a[idHilo + ladoMatriz - 1] };
			for (int i = 0; i < 5; i++)
			{
				if (vecinos[i] == 1)
				{
					contador += 1;
				}
			}
		}
		//Hilo esta en el medio de la matriz
		else
		{
			//Creamos array con vecinos de la celula
			int vecinos[8] = { a[idHilo + 1], a[idHilo - 1], a[idHilo + ladoMatriz], a[idHilo - ladoMatriz], a[idHilo - ladoMatriz - 1], a[idHilo - ladoMatriz + 1], a[idHilo + ladoMatriz + 1], a[idHilo + ladoMatriz - 1] };
			for (int i = 0; i < 8; i++)
			{
				if (vecinos[i] == 1)
				{
					contador += 1;
				}
			}
		}
	}
	//Comprobamos si el bloque al que pertenece el hilo que ha llamado al kernel se encuentra en la esquina inferior izquierda de la matriz.
	else if (idBloque == bloques * bloques - bloques) 
	{
		//En este bloque tenemosque comprobar el ladoo izquierdo e inferior.
		//Comprobamos si el hilo que ha llamado al kernel se encuentra en la esquina inferior izquierda del bloque.
		if (idCelula == ladoMatriz*ladoMatriz - ladoMatriz)
		{
			//Creamos array con vecinos de la celula
			int vecinos[3] = { a[idHilo + 1], a[idHilo - ladoMatriz], a[idHilo - ladoMatriz + 1] };
			for (int i = 0; i < 3; i++)
			{
				if (vecinos[i] == 1)
				{
					contador += 1;
				}
			}
		}
	
		//Comprobamos si el hilo que ha llamado al kernel se encuentra en el lado izquierdo (pero no en la esquina inferior) del bloque.
		else if (idHilo % ladoMatriz == 0)
			{
				//Creamos array con vecinos de la celula
				int vecinos[5] = { a[idHilo + 1], a[idHilo + ladoMatriz], a[idHilo - ladoMatriz], a[idHilo - ladoMatriz + 1], a[idHilo + ladoMatriz + 1] };
				for (int i = 0; i < 5; i++)
				{
					if (vecinos[i] == 1)
					{
						contador += 1;
					}
				}
		}	
		//Comprobamos si el hilo que ha llamado al kernel se encuentra abajo (pero no en la esquina) de la matriz.
		else if (idHilo >= ladoMatriz * (ladoMatriz - 1) && idHilo < ladoMatriz * ladoMatriz - (ladoMatriz - ladoBloque))
		{
			//Creamos array con vecinos de la celula
			int vecinos[5] = { a[idHilo + 1], a[idHilo - 1], a[idHilo - ladoMatriz], a[idHilo - ladoMatriz + 1], a[idHilo - ladoMatriz - 1] };
			for (int i = 0; i < 5; i++)
			{
				if (vecinos[i] == 1)
				{
					contador += 1;
				}
			}
		}
		//Hilo esta en el medio de la matriz
		else
		{
			//Creamos array con vecinos de la celula
			int vecinos[8] = { a[idHilo + 1], a[idHilo - 1], a[idHilo + ladoMatriz], a[idHilo - ladoMatriz], a[idHilo - ladoMatriz - 1], a[idHilo - ladoMatriz + 1], a[idHilo + ladoMatriz + 1], a[idHilo + ladoMatriz - 1] };
			for (int i = 0; i < 8; i++)
			{
				if (vecinos[i] == 1)
				{
					contador += 1;
				}
			}
		}
	}
	//Comprobamos si el bloque al que pertenece el hilo que ha llamado al kernel se encuentra en la esquina inferior derecha de la matriz.
	else if (idBloque == bloques * bloques - 1)
	{
	//En este bloque tenemosque comprobar el ladoo derecho e inferior.
		//Comprobamos si el hilo que ha llamado al kernel se encuentra en la esquina inferior derecha del bloque.
		if (idCelula == ladoMatriz * ladoMatriz - 1)
		{
			//Creamos array con vecinos de la celula
			int vecinos[3] = { a[idHilo - 1], a[idHilo - ladoMatriz], a[idHilo - ladoMatriz - 1] };
			for (int i = 0; i < 3; i++)
			{
				if (vecinos[i] == 1)
				{
					contador += 1;
				}
			}
		}
		else if (idHilo % ladoMatriz == ladoMatriz - 1)
		{
			//Comprobamos si el hilo que ha llamado al kernel se encuentra en el lado derecho del bloque.
			//Creamos array con vecinos de la celula
			int vecinos[5] = { a[idHilo - 1], a[idHilo + ladoMatriz], a[idHilo - ladoMatriz], a[idHilo - ladoMatriz - 1], a[idHilo + ladoMatriz - 1] };
			for (int i = 0; i < 5; i++)
			{
				if (vecinos[i] == 1)
				{
					contador += 1;
				}
			}
		}
		else if (idHilo < ladoMatriz * ladoMatriz - 1  && idHilo > ladoMatriz*ladoMatriz - ladoBloque)
		{
			//Comprobamos si el hilo que ha llamado al kernel se encuentra abajo del bloque.
			//Creamos array con vecinos de la celula
			int vecinos[5] = { a[idHilo - 1], a[idHilo + 1], a[idHilo - ladoMatriz], a[idHilo - ladoMatriz - 1], a[idHilo - ladoMatriz + 1] };
			for (int i = 0; i < 5; i++)
			{
				if (vecinos[i] == 1)
				{
					contador += 1;
				}
			}
		}
		//Hilo esta en el medio de la matriz
		else
		{
			//Creamos array con vecinos de la celula
			int vecinos[8] = { a[idHilo + 1], a[idHilo - 1], a[idHilo + ladoMatriz], a[idHilo - ladoMatriz], a[idHilo - ladoMatriz - 1], a[idHilo - ladoMatriz + 1], a[idHilo + ladoMatriz + 1], a[idHilo + ladoMatriz - 1] };
			for (int i = 0; i < 8; i++)
			{
				if (vecinos[i] == 1)
				{
					contador += 1;
				}
			}
		}
	
	}
	//Comprobamos si el bloque al que pertenece el hilo que ha llamado al kernel se encuentra en la parte superior de la matriz.
	else if ((idBloque > 0 )&& (idBloque < bloques - 1 ))
	{
		//Comprobamos si el hilo que ha llamado al kernel se encuentra arriba del bloque. (Los unicos especiales del bloque)
		if ((idCelula >= ladoMatriz - (ladoBloque*(bloques - blockIdx.x))) && (idHilo <= (ladoMatriz - 1) - (ladoBloque*(bloques - (blockIdx.x + 1)))))
		{
			//Creamos array con vecinos de la celula
			int vecinos[5] = { a[idHilo + 1],  a[idHilo - 1], a[idHilo + ladoMatriz], a[idHilo + ladoMatriz + 1], a[idHilo + ladoMatriz - 1] };
			for (int i = 0; i < 5; i++)
			{
				if (vecinos[i] == 1)
				{
					contador += 1;
				}
			}
		}
		//Hilo esta en el medio de la matriz
		else
		{
			//Creamos array con vecinos de la celula
			int vecinos[8] = { a[idHilo + 1], a[idHilo - 1], a[idHilo + ladoMatriz], a[idHilo - ladoMatriz], a[idHilo - ladoMatriz - 1], a[idHilo - ladoMatriz + 1], a[idHilo + ladoMatriz + 1], a[idHilo + ladoMatriz - 1] };
			for (int i = 0; i < 8; i++)
			{
				if (vecinos[i] == 1)
				{
					contador += 1;
				}
			}
		}
	}
	//Comprobamos si el bloque al que pertenece el hilo que ha llamado al kernel se encuentra en la parte inferior de la matriz.
	else if ((idBloque > (bloques *(bloques -1))) && (idBloque < (bloques * bloques) - 1))
	{
		//Solo tenemos que comprobar abajo del bloque.
		//Comprobamos si el hilo que ha llamado al kernel se encuentra abajo del bloque. (Los unicos especiales del bloque)
		if ((idCelula >= ladoMatriz* ladoMatriz - (ladoBloque*(bloques - blockIdx.x))) && (idHilo <= ((ladoMatriz * ladoMatriz) - 1) - (ladoBloque*(bloques - (blockIdx.x + 1)))))
		{
			//Creamos array con vecinos de la celula
			int vecinos[5] = { a[idHilo + 1], a[idHilo - 1] , a[idHilo - ladoMatriz], a[idHilo - ladoMatriz - 1], a[idHilo - ladoMatriz + 1] };
			for (int i = 0; i < 5; i++)
			{
				if (vecinos[i] == 1)
				{
					contador += 1;
				}
			}
		}
		//Hilo esta en el medio de la matriz
		else
		{
			//Creamos array con vecinos de la celula
			int vecinos[8] = { a[idHilo + 1], a[idHilo - 1], a[idHilo + ladoMatriz], a[idHilo - ladoMatriz], a[idHilo - ladoMatriz - 1], a[idHilo - ladoMatriz + 1], a[idHilo + ladoMatriz + 1], a[idHilo + ladoMatriz - 1] };
			for (int i = 0; i < 8; i++)
			{
				if (vecinos[i] == 1)
				{
					contador += 1;
				}
			}
		}
	}
	//Comprobamos si el bloque al que pertenece el hilo que ha llamado al kernel se encuentra en la parte derecha de la matriz.
	else if (idBloque % bloques == bloques - 1)
	{
		//Comprobamos si el hilo que ha llamado al kernel se encuentra en la derecha del bloque. (Los unicos especiales del bloque)
		if (idHilo % ladoMatriz == ladoMatriz - 1)
		{
			//Creamos array con vecinos de la celula
			int vecinos[5] = { a[idHilo - 1], a[idHilo + ladoMatriz], a[idHilo - ladoMatriz], a[idHilo - ladoMatriz - 1], a[idHilo + ladoMatriz - 1] };
			for (int i = 0; i < 5; i++)
			{
				if (vecinos[i] == 1)
				{
					contador += 1;
				}
			}
		}
		//Hilo esta en el medio de la matriz
		else
		{
			//Creamos array con vecinos de la celula
			int vecinos[8] = { a[idHilo + 1], a[idHilo - 1], a[idHilo + ladoMatriz], a[idHilo - ladoMatriz], a[idHilo - ladoMatriz - 1], a[idHilo - ladoMatriz + 1], a[idHilo + ladoMatriz + 1], a[idHilo + ladoMatriz - 1] };
			for (int i = 0; i < 8; i++)
			{
				if (vecinos[i] == 1)
				{
					contador += 1;
				}
			}
		}
	}
	//Comprobamos si el bloque al que pertenece el hilo que ha llamado al kernel se encuentra en la parte iquierda de la matriz.
	else if (idBloque % bloques == bloques - 1)
	{
		//Comprobamos si el hilo que ha llamado al kernel se encuentra en la derecha del bloque. (Los unicos especiales del bloque)
		if (idHilo % ladoMatriz == 0)
		{
			//Creamos array con vecinos de la celula
			int vecinos[5] = { a[idHilo + 1], a[idHilo + ladoMatriz], a[idHilo - ladoMatriz], a[idHilo - ladoMatriz + 1], a[idHilo + ladoMatriz + 1] };
			for (int i = 0; i < 5; i++)
			{
				if (vecinos[i] == 1)
				{
					contador += 1;
				}
			}
		}
		//Hilo esta en el medio de la matriz
		else
		{
			//Creamos array con vecinos de la celula
			int vecinos[8] = { a[idHilo + 1], a[idHilo - 1], a[idHilo + ladoMatriz], a[idHilo - ladoMatriz], a[idHilo - ladoMatriz - 1], a[idHilo - ladoMatriz + 1], a[idHilo + ladoMatriz + 1], a[idHilo + ladoMatriz - 1] };
			for (int i = 0; i < 8; i++)
			{
				if (vecinos[i] == 1)
				{
					contador += 1;
				}
			}
		}
	}
	//Hilo esta en el medio de la matriz
	else
		{
		//Creamos array con vecinos de la celula
		int vecinos[8] = { a[idHilo + 1], a[idHilo - 1], a[idHilo + ladoMatriz], a[idHilo - ladoMatriz], a[idHilo - ladoMatriz - 1], a[idHilo - ladoMatriz + 1], a[idHilo + ladoMatriz + 1], a[idHilo + ladoMatriz - 1] };
		for (int i = 0; i < 8; i++)
		{
			if (vecinos[i] == 1)
			{
				contador += 1;
			}
		}
	}
	
	return contador;
}
__device__ void cambiarEstado(int *a, int *aux, int idCelula, int ladoMatriz, int ladoBloque,int idBloque)
{
	int idHilo = idCelula;
	int contador = comprobarVecinos(a, idHilo, ladoMatriz,ladoBloque, idBloque);
	//La celula esta viva 
	if (a[idHilo] == 1 && (contador < 2 || contador > 3))
	{
		//Hay menos de 2 celulas vivas o mas de 3
		//Matamos la celula
		aux[idHilo] = 0;
		printf("Celula %d pasa a estar muerta\n", idHilo);
	}
	//La celula esta muerta
	else if (a[idHilo] == 0 && contador == 3)
	{
		//Hay 3 celulas vivas alrededor
		//La celula nace
		aux[idHilo] = 1;
		printf("Celula %d pasa a estar viva\n", idHilo);
	}
}
//Función de comprobación a realizar por el kernel
__global__ void llamadaCelula(int *a, int *aux, int ladoMatriz, int ladoBloque)
{
	int idColumna = blockIdx.x*ladoBloque + threadIdx.x;
	int idFila = blockIdx.y*ladoBloque + threadIdx.y;
	int idBloque = blockIdx.x + blockIdx.y*(ladoMatriz/ladoBloque);
	int idHilo = idColumna + idFila *ladoMatriz;
	cambiarEstado(a, aux, idHilo, ladoMatriz,ladoBloque,idBloque);
	__syncthreads();
}

int main(int argc, char** argv)
{
	int ladoMatriz = 0;
	int ladoBloque = 8;
	char caracter = ' ';
	int generacion = 0;
	printf("Introduzca el tamano de la matriz. \n");
	scanf("%d", &ladoMatriz);
	//Declaraciones de variables.
	int *MatrizA, *MatrizA_d;
	int *MatrizAux_d;
	//Reserva de memoria en el host.
	MatrizA = (int*)malloc(ladoMatriz*ladoMatriz * sizeof(int));
	//Reserva de memoria en el device.
	cudaMalloc((void**)&MatrizA_d, ladoMatriz*ladoMatriz * sizeof(int));
	cudaMalloc((void**)&MatrizAux_d, ladoMatriz*ladoMatriz * sizeof(int));
	//Inicialización de matriz.
	int contadorSemillas = 0;
	for (int i = 0; i < ladoMatriz * ladoMatriz; i++)
	{

		if (rand() % 100 < 25 && contadorSemillas < 9) //Solo puede haber un maximo de 9 semillas iniciales. Hay una posibilidad del 25% de que la posicion sea semilla.
		{
			MatrizA[i] = 1;
			contadorSemillas++;
		}
		else
		{
			MatrizA[i] = 0;
		}
	}
	dim3 nBloques(ladoMatriz/ladoBloque, ladoMatriz / ladoBloque);
	dim3 hilosBloque((ladoMatriz + nBloques.x - 1) / nBloques.x, (ladoMatriz + nBloques.y - 1) / nBloques.y);
	caracter = getchar();
	while (caracter != 'p')
	{
		//Representación de los resultados.
		printf("Matriz A en generacion %d:\n", generacion);
		for (int i = 0; i < ladoMatriz; i++)
		{
			for (int j = 0; j < ladoMatriz; j++)
			{
				printf("%d ", MatrizA[j + i * ladoMatriz]);
			}
			printf("\n");
		}
		//Envío de datos al device.
		cudaMemcpy(MatrizA_d, MatrizA, ladoMatriz*ladoMatriz * sizeof(int), cudaMemcpyHostToDevice);
		//Realización de la operación.
		llamadaCelula << <nBloques, hilosBloque >> > (MatrizA_d, MatrizAux_d, ladoMatriz, ladoBloque);
		//Envío de datos al host.
		cudaMemcpy(MatrizA_d, MatrizAux_d, ladoMatriz * ladoMatriz * sizeof(int), cudaMemcpyDeviceToDevice);
		cudaMemcpy(MatrizA, MatrizA_d, ladoMatriz * ladoMatriz * sizeof(int), cudaMemcpyDeviceToHost);
		caracter = getchar();
		generacion += 1;
	}
	//Liberación del espacio usado por los punteros.
	cudaFree(MatrizA_d);
	cudaFree(MatrizAux_d);
	free(MatrizA);
}