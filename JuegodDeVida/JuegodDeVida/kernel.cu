#include <stdio.h>
#include <stdlib.h>
#include <cuda_runtime.h>

__device__ void comprobarVivo(int *a, int idCelula, int ladoMatriz)
{
	int idHilo = idCelula;
	int contadorVivas = 0;
	if (a[idHilo] == 0)
	{
		a[idHilo] = 1;
	}
	else
	{
		a[idHilo] = 0;
	}
}

//Función de comprobación a realizar por el kernel
__global__ void llamadaCelula(int *a, int ladoMatriz)
{
	int idFila = threadIdx.x;
	int idColumna = threadIdx.y;
	int idHilo = idColumna + idFila * blockDim.x;
	comprobarVivo(a, idHilo, ladoMatriz);
	__syncthreads();
}

int main(int argc, char** argv)
{
	int ladoMatriz = 0;
	char caracter = ' ';
	int generacion = 0;
	printf("Introduzca el tamaño de la matriz. \n");
	scanf("%d", &ladoMatriz);
	//Declaraciones de variables.
	int *MatrizA, *MatrizA_d;
	//Reserva de memoria en el host.
	MatrizA = (int*)malloc(ladoMatriz*ladoMatriz * sizeof(int));
	//Reserva de memoria en el device.
	cudaMalloc((void**)&MatrizA_d, ladoMatriz*ladoMatriz * sizeof(int));
	//Inicialización de matriz.
	for (int i = 0; i < ladoMatriz*ladoMatriz; i++)
	{
		if (rand() % 2)
		{
			MatrizA[i] = 1;
		}
		else
		{
			MatrizA[i] = 0;
		}
	}
	//Mostramos los valores de la matriz una vez inicializada.
	printf("Matriz A al inicializarse: \n");
	for (int i = 0; i < ladoMatriz; i++)
	{
		for (int j = 0; j < ladoMatriz; j++)
		{
			printf("%d ", MatrizA[j + i * ladoMatriz]);
		}
		printf("\n");
	}
	dim3 nBloques(1, 1);
	dim3 hilosBloque((ladoMatriz + nBloques.x - 1) / nBloques.x, (ladoMatriz + nBloques.y - 1) / nBloques.y);
	//Envío de datos al device.
	cudaMemcpy(MatrizA_d, MatrizA, ladoMatriz*ladoMatriz * sizeof(int), cudaMemcpyHostToDevice);
	while (caracter != 'p')
	{
		//Realización de la operación.
		llamadaCelula << <nBloques, hilosBloque >> > (MatrizA_d, ladoMatriz);
		//Envío de datos al host.
		cudaMemcpy(MatrizA, MatrizA_d, ladoMatriz*ladoMatriz * sizeof(int), cudaMemcpyDeviceToHost);
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
		caracter = getchar();
		generacion += 1;
	}
	//Liberación del espacio usado por los punteros.
	cudaFree(MatrizA_d);
	free(MatrizA);
}