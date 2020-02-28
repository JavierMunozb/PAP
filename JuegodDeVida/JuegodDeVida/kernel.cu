#include <stdio.h>
#include <stdlib.h>
#include <cuda_runtime.h>
//Función de shift a realizar por el kernel
__global__ void llamadaCelula (int *a, int ladoMatriz)
{
	int idFila = threadIdx.x;
	int idColumna = threadIdx.y;
	int idHilo = idColumna + idFila * blockDim.x;
	comprobarVivo(a, idHilo, ladoMatriz);
}

int comprobarVivo(int *a, int idCelula, int ladoMatriz)
{
	int idHilo = idCelula;
	__syncthreads();
	//Comprobamos si el hilo que ha llamado al kernel se encuentra en el borde izquierdo de la matriz.
	if (a[idHilo + 1] == 1 && a[idHilo - 1] == 1 && a[idHilo - ladoMatriz] == 1 && a[idHilo + ladoMatriz] == 0) //Derecha, izquierda y arriba vivas.
	{
		a[idHilo] = 1;
	}
	else if (a[idHilo + 1] == 1 && a[idHilo - 1] == 1 && a[idHilo - ladoMatriz] == 1 && a[idHilo + ladoMatriz] == 0) //Arriba, lateral superior derecha, derecha vivas
	{

	}
	else if (a[idHilo + 1] == 1 && a[idHilo - 1] == 1 && a[idHilo + ladoMatriz] == 1 && a[idHilo - ladoMatriz] == 0) //Derecha, izquierda y abajo vivas.
	{
		a[idHilo] = 1;
	}
	else if (a[id)
}

int main(int argc, char** argv)
{
	int ladoMatriz = 0;
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
	printf("Matriz A: \n");
	for (int i = 0; i < ladoMatriz; i++)
	{
		for (int j = 0; j < ladoMatriz; j++)
		{
			printf("%03d ", MatrizA[j + i * ladoMatriz]);
		}
		printf("\n");
	}
	//Realización de la operación.
	dim3 nBloques(1, 1);
	dim3 hilosBloque((ladoMatriz + nBloques.x - 1) / nBloques.x, (ladoMatriz + nBloques.y - 1) / nBloques.y);
	//Envío de datos al device.
	cudaMemcpy(MatrizA_d, MatrizA, ladoMatriz*ladoMatriz * sizeof(int), cudaMemcpyHostToDevice);
	shift_matriz << <nBloques, hilosBloque >> > (MatrizA_d, ladoMatriz);
	cudaDeviceSynchronize();
	//Envío de datos al host.
	cudaMemcpy(MatrizA, MatrizA_d, ladoMatriz*ladoMatriz * sizeof(int), cudaMemcpyDeviceToHost);
	//Representación de los resultados.
	printf("Los valores de la matriz en el paso %d:\n", i + 1);
	for (int i = 0; i < ladoMatriz; i++)
	{
		for (int j = 0; j < ladoMatriz; j++)
		{
			printf("%03d ", MatrizA[j + i * ladoMatriz]);
		}
		printf("\n");
	}
	//Liberación del espacio usado por los punteros.
	cudaFree(MatrizA_d);
	free(MatrizA);
}