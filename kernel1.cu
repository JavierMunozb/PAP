
#include <stdio.h>
#include <stdlib.h>
#include <cuda_runtime.h>
#include <device_functions.h>

__device__ int comprobarVecinos(int *a, int idCelula, int filas, int columnas)
{
	int idHilo = idCelula;
	int contador = 0;
	//Comprobamos si el hilo que ha llamado al kernel se encuentra en la esquina superior izquierda de la matriz.
	if (idCelula == 0)
	{
		//Creamos array con vecinos de la celula
		int vecinos[3] = { a[idHilo + 1], a[idHilo + columnas], a[idHilo + columnas + 1] };
		for (int i = 0; i < 3; i++)
		{
			if (vecinos[i] == 1)
			{
				contador += 1;
			}
		}
	}
	//Comprobamos si el hilo que ha llamado al kernel se encuentra en la esquina superior derecha de la matriz.
	else if (idHilo == columnas - 1)
	{
		//Creamos array con vecinos de la celula
		int vecinos[3] = { a[idHilo - 1], a[idHilo + columnas], a[idHilo + columnas - 1] };
		for (int i = 0; i < 3; i++)
		{
			if (vecinos[i] == 1)
			{
				contador += 1;
			}
		}
	}
	//Comprobamos si el hilo que ha llamado al kernel se encuentra en la esquina inferior izquierda de la matriz.
	else if (idHilo == (columnas * filas - columnas))
	{
		//Creamos array con vecinos de la celula
		int vecinos[3] = { a[idHilo + 1], a[idHilo - columnas], a[idHilo - columnas + 1] };
		for (int i = 0; i < 3; i++)
		{
			if (vecinos[i] == 1)
			{
				contador += 1;
			}
		}
	}
	//Comprobamos si el hilo que ha llamado al kernel se encuentra en la esquina inferior derecha de la matriz.
	else if (idHilo == columnas * filas - 1)
	{
		//Creamos array con vecinos de la celula
		int vecinos[3] = { a[idHilo - 1], a[idHilo - columnas], a[idHilo - columnas - 1] };
		for (int i = 0; i < 3; i++)
		{
			if (vecinos[i] == 1)
			{
				contador += 1;
			}
		}
	}
	//Comprobamos si el hilo que ha llamado al kernel se encuentra en el lado izquierdo (pero no en la esquina) de la matriz.
	else if (idHilo % columnas == 0)
	{
		//Creamos array con vecinos de la celula
		int vecinos[5] = { a[idHilo + 1], a[idHilo + columnas], a[idHilo - columnas], a[idHilo - columnas + 1], a[idHilo + columnas + 1] };
		for (int i = 0; i < 5; i++)
		{
			if (vecinos[i] == 1)
			{
				contador += 1;
			}
		}
	}
	//Comprobamos si el hilo que ha llamado al kernel se encuentra en el lado derecho (pero no en la esquina) de la matriz.
	else if (idHilo % columnas == columnas - 1)
	{
		//Creamos array con vecinos de la celula
		int vecinos[5] = { a[idHilo - 1], a[idHilo + columnas], a[idHilo - columnas], a[idHilo - columnas - 1], a[idHilo + columnas - 1] };
		for (int i = 0; i < 5; i++)
		{
			if (vecinos[i] == 1)
			{
				contador += 1;
			}
		}
	}
	//Comprobamos si el hilo que ha llamado al kernel se encuentra abajo (pero no en la esquina) de la matriz.
	else if (idHilo >= columnas * (filas - 1) && idHilo < columnas * filas)
	{
		//Creamos array con vecinos de la celula
		int vecinos[5] = { a[idHilo + 1], a[idHilo - 1], a[idHilo - columnas], a[idHilo - columnas + 1], a[idHilo - columnas - 1] };
		for (int i = 0; i < 5; i++)
		{
			if (vecinos[i] == 1)
			{
				contador += 1;
			}
		}
	}
	//Comprobamos si el hilo que ha llamado al kernel se encuentra arriba (pero no en la esquina) de la matriz.
	else if (idHilo < columnas)
	{
		//Creamos array con vecinos de la celula
		int vecinos[5] = { a[idHilo + 1], a[idHilo - 1], a[idHilo + columnas], a[idHilo + columnas + 1], a[idHilo + columnas - 1] };
		for (int i = 0; i < 5; i++)
		{
			if (vecinos[i] == 1)
			{
				contador += 1;
			}
		}
	}
	//Hilo esta en el medio
	else
	{
		//Creamos array con vecinos de la celula
		int vecinos[8] = { a[idHilo + 1], a[idHilo - 1], a[idHilo + columnas], a[idHilo - columnas], a[idHilo - columnas - 1], a[idHilo - columnas + 1], a[idHilo + columnas + 1], a[idHilo + columnas - 1] };
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
__device__ void cambiarEstado(int *a, int *aux, int idCelula, int filas, int columnas)
{
	int idHilo = idCelula;
	int contador = comprobarVecinos(a, idHilo, filas, columnas);
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
__global__ void llamadaCelula(int *a, int *aux, int filas, int columnas)
{
	int idFila = threadIdx.x;
	int idColumna = threadIdx.y;
	int idHilo = idColumna + idFila * blockDim.x;
	cambiarEstado(a, aux, idHilo, filas, columnas);
	__syncthreads();
}

int main(int argc, char** argv)
{
	int filas = 0;
	int columnas = 0;
	char caracter = ' ';
	char manual = ' ';
	int generacion = 0;
	printf("Introduzca el numero de filas. \n");
	scanf("%d", &filas);
	printf("Introduzca el numero de columnas. \n");
	scanf("%d", &columnas);
	printf("Introduzca \"m\" si quiere reproducir de forma manual. \n");
	manual = getchar();
	//Declaraciones de variables.
	int *MatrizA, *MatrizA_d;
	int *MatrizAux_d;
	//Reserva de memoria en el host.
	MatrizA = (int*)malloc(filas*columnas * sizeof(int));
	//Reserva de memoria en el device.
	cudaMalloc((void**)&MatrizA_d, filas*columnas * sizeof(int));
	cudaMalloc((void**)&MatrizAux_d, filas*columnas * sizeof(int));
	//Inicialización de matriz.
	int contadorSemillas = 0;
	for (int i = 0; i < filas*columnas; i++)
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
	dim3 nBloques(1, 1);
	dim3 hilosBloque((columnas + nBloques.x - 1) / nBloques.x, (filas + nBloques.y - 1) / nBloques.y);
	caracter = getchar();
	while (caracter != 'p')
	{
		//Representación de los resultados.
		printf("Matriz A en generacion %d:\n", generacion);
		for (int i = 0; i < filas; i++)
		{
			for (int j = 0; j < columnas; j++)
			{
				printf("%d ", MatrizA[j + i * columnas]);
			}
			printf("\n");
		}
		//Envío de datos al device.
		cudaMemcpy(MatrizA_d, MatrizA, filas*columnas * sizeof(int), cudaMemcpyHostToDevice);
		//Realización de la operación.
		llamadaCelula << <nBloques, hilosBloque >> > (MatrizA_d, MatrizAux_d, filas, columnas);
		//Envío de datos al host.
		cudaMemcpy(MatrizA_d, MatrizAux_d, filas*columnas * sizeof(int), cudaMemcpyDeviceToDevice);
		cudaMemcpy(MatrizA, MatrizA_d, filas*columnas * sizeof(int), cudaMemcpyDeviceToHost);
		//Si esta en modo manual se espera a que el usuario pulse alguna tecla para continuar (Si es 'p' se para la ejecucion)
		if (manual == 'm') {
			caracter = getchar();
		}
		generacion += 1;
	}
	//Liberación del espacio usado por los punteros.
	cudaFree(MatrizA_d);
	cudaFree(MatrizAux_d);
	free(MatrizA);

}
