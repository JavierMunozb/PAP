#include <stdio.h>
#include <stdlib.h>
#include <windows.h>
#include <cuda_runtime.h>
#include <device_functions.h>

__device__ int comprobarVecinos(int *a, int idCelula, int ladoMatriz, int largoMatriz)
{
	int idHilo = idCelula;
	int contador = 0;
	//Comprobamos si el hilo que ha llamado al kernel se encuentra en la esquina superior izquierda de la matriz.
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
	//Comprobamos si el hilo que ha llamado al kernel se encuentra en la esquina superior derecha de la matriz.
	else if (idHilo == ladoMatriz - 1)
	{
		//Creamos array con vecinos de la celula
		int vecinos[3] = { a[idHilo - 1], a[idHilo + ladoMatriz], a[idHilo + ladoMatriz - 1] };
		for (int i = 0; i < 3; i++)
		{
			if (vecinos[i] == 1)
			{
				contador += 1;
			}
		}
	}
	//Comprobamos si el hilo que ha llamado al kernel se encuentra en la esquina inferior izquierda de la matriz.
	else if (idHilo == (ladoMatriz *  largoMatriz - ladoMatriz))
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
	//Comprobamos si el hilo que ha llamado al kernel se encuentra en la esquina inferior derecha de la matriz.
	else if (idHilo == ladoMatriz * largoMatriz - 1)
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
	//Comprobamos si el hilo que ha llamado al kernel se encuentra en el lado izquierdo (pero no en la esquina) de la matriz.
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
	//Comprobamos si el hilo que ha llamado al kernel se encuentra en el lado derecho (pero no en la esquina) de la matriz.
	else if (idHilo % ladoMatriz == ladoMatriz - 1)
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
	//Comprobamos si el hilo que ha llamado al kernel se encuentra abajo (pero no en la esquina) de la matriz.
	else if (idHilo > ladoMatriz * (largoMatriz - 1) && idHilo < ladoMatriz * largoMatriz)
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
	//Comprobamos si el hilo que ha llamado al kernel se encuentra arriba (pero no en la esquina) de la matriz.
	else if (idHilo < ladoMatriz)
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
	//Hilo esta en el medio
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
__device__ void cambiarEstado(int *a, int *aux, int idCelula, int ladoMatriz, int largoMatriz)
{
	int idHilo = idCelula;
	int contador = comprobarVecinos(a, idHilo, ladoMatriz, largoMatriz);
	//La celula esta viva 
	if (a[idHilo] == 1 && (contador < 2 || contador > 3))
	{
		//Hay menos de 2 celulas vivas o mas de 3
		//Matamos la celula
		aux[idHilo] = 0;
	}
	//La celula esta muerta
	else if (a[idHilo] == 0 && contador == 3)
	{
		//Hay 3 celulas vivas alrededor
		//La celula nace
		aux[idHilo] = 1;
	}
	//La celula esta viva
	else if (a[idHilo] == 1 && (contador == 2 || contador == 3))
	{
		//Hay 2 o 3 células alrededor
		//La célula se mantiene viva
		aux[idHilo] = 1;
	}
	//La celula esta muerta
	else if (a[idHilo] == 0 && (contador < 2 || contador > 3))
	{
		//Hay menos de 2 o más de 3 células vivas alrededor.
		//La célula se mantiene muerta.
		aux[idHilo] = 0;
	}
}
//Función de comprobación a realizar por el kernel.
__global__ void llamadaCelula(int *a, int *aux, int ladoMatriz, int largoMatriz, int ladoMul16, int largoMul16)
{
	int idFila = threadIdx.x + blockIdx.x * blockDim.x;
	int idColumna = threadIdx.y + blockIdx.y * blockDim.y;
	int idHilo = idColumna + idFila * ladoMul16;
	if (idColumna < ladoMatriz && idFila < largoMatriz) //Queremos que solo los hilos que están dentro de la matriz del usuario trabajen.
	{
		cambiarEstado(a, aux, idHilo, ladoMatriz, largoMatriz); //El resto de hilos del kernel que no cumplan la condición, no trabajarán.
	}
	__syncthreads();
}

//Esta funcion nos sirve para pasar cualquier numero al multiplo de 16 mas cercano
//En nuestro caso, es utilizada para encontrar el multiplo de 16 mas cercano de las dimensiones de la matriz
//De esta manera, mantenemos las dimensiones del bloque a 16*16
int pasarAMul16(int numero)
{
	while (numero % 16 != 0)
	{
		numero++;
	}
	return numero;
}

int main(int argc, char** argv)
{
	int ladoMatriz = 0;
	int largoMatriz = 0;
	char modo = ' ';
	char caracterParada = ' ';
	int generacion = 0;
	printf("Introduzca el metodo de ejecucion (m)anual o (a)utomatica. \n");
	modo = getchar();
	printf("Introduzca el ancho de la matriz. \n");
	scanf("%d", &ladoMatriz);
	getchar();
	printf("Introduzca el alto de la matriz. \n");
	scanf("%d", &largoMatriz);
	//Declaraciones de variables.
	int *MatrizA, *MatrizA_d;
	int *MatrizAux, *MatrizAux_d;
	//Reserva de memoria en el host.
	MatrizA = (int*)malloc(ladoMatriz*largoMatriz * sizeof(int));
	MatrizAux = (int*)malloc(ladoMatriz*largoMatriz * sizeof(int));
	//Reserva de memoria en el device.
	cudaMalloc((void**)&MatrizA_d, ladoMatriz*largoMatriz * sizeof(int));
	cudaMalloc((void**)&MatrizAux_d, ladoMatriz*largoMatriz * sizeof(int));
	//Inicialización de matriz.
	for (int i = 0; i < ladoMatriz * largoMatriz; i++)
	{

		if (rand() % 2) //Hay una posibilidad del 50% de que la posicion sea una celula viva.
		{
			MatrizA[i] = 1;
		}
		else
		{
			MatrizA[i] = 0;
		}
	}
	//Inicialización de l matriz auxiliar
	for (int i = 0; i < ladoMatriz * largoMatriz; i++)
	{
		MatrizAux[i] = 0;
	}
	int ladoMultiplo = pasarAMul16(ladoMatriz);
	int largoMultiplo = pasarAMul16(largoMatriz);
	dim3 nBloques(ladoMultiplo / 16, largoMultiplo / 16);
	dim3 hilosBloque(16, 16);
	cudaMemcpy(MatrizAux_d, MatrizAux, ladoMatriz * largoMatriz * sizeof(int), cudaMemcpyHostToDevice);
	caracterParada = getchar();
	while (caracterParada != 'p')
	{
		//Representación de los resultados.
		printf("Matriz A en generacion %d:\n", generacion);
		for (int i = 0; i < largoMatriz; i++)
		{
			for (int j = 0; j < ladoMatriz; j++)
			{
				printf("%d ", MatrizA[j + i * ladoMatriz]);
			}
			printf("\n");
		}
		//Envío de datos al device.
		cudaMemcpy(MatrizA_d, MatrizA, ladoMatriz * largoMatriz * sizeof(int), cudaMemcpyHostToDevice);
		//Realización de la operación.
		llamadaCelula << <nBloques, hilosBloque >> > (MatrizA_d, MatrizAux_d, ladoMatriz, largoMatriz, ladoMultiplo, largoMultiplo);
		//Envío de datos al host.
		MatrizA_d = MatrizAux_d;
		cudaMemcpy(MatrizA, MatrizA_d, ladoMatriz * largoMatriz * sizeof(int), cudaMemcpyDeviceToHost);
		if (modo == 'm')
		{
			caracterParada = getchar();
		}
		else
		{
			Sleep(1000);
		}
		generacion += 1;
	}
	//Liberación del espacio usado por los punteros.
	cudaFree(MatrizA_d);
	cudaFree(MatrizAux_d);
	free(MatrizA);
	free(MatrizAux);
}