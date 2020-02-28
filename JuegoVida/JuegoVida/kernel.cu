
#include <stdio.h>
#include <stdlib.h>
#include <cuda_runtime.h>
//Función de shift a realizar por el kernel
__global__ void llamadaCelula(int *a, int ladoMatriz)
{
	int idFila = threadIdx.x;
	int idColumna = threadIdx.y;
	int idHilo = idColumna + idFila;
	int vecinos_vivos = comprobarVecinos(a, idHilo, ladoMatriz);
	cambiarEstado(a, idHilo, vecinos_vivos);
}
void cambiarEstado(int *a, int idCelula, int contador) {

	int idHilo = idCelula;
	//La celula esta viva 
	if (a[idHilo] = 1) {
		//Hay menos de 2 celulas vivas o mas de 3
		if (contador < 2 || contador > 3) {
			//Matamos la celula
			a[idHilo] = 0;
		}
	}
	//La celula esta viva
	if (a[idHilo] = 0) {
		//Hay 3 celulas vivas alrededor
		if (contador =  3) {
			//La celula nace
			a[idHilo] = 1;
		}
	}
}
int comprobarVecinos(int *a, int idCelula, int ladoMatriz)
{
	int idHilo = idCelula, contador = 0;
	cudaDeviceSynchronize();
	//Comprobamos si el hilo que ha llamado al kernel se encuentra en la esquina superior izquierda de la matriz.
	//Creamos array con vecinos de la celula
	int vecinos[3] = { a[idHilo + 1], a[idHilo + ladoMatriz], a[idHilo + ladoMatriz + 1] };
	for (int i = 0; i < 3; i++) {
		if (vecinos[i] == 1)
		{
			contador = +1;
		}
	}
	//Comprobamos si el hilo que ha llamado al kernel se encuentra en la esquina superior derecha de la matriz.
	//Creamos array con vecinos de la celula
	int vecinos[3] = { a[idHilo - 1], a[idHilo + ladoMatriz], a[idHilo + ladoMatriz - 1] };
	for (int i = 0; i < 3; i++) {
		if (vecinos[i] == 1)
		{
			contador = +1;
		}
	}
	//Comprobamos si el hilo que ha llamado al kernel se encuentra en la esquina inferior izquierda de la matriz.
	//Creamos array con vecinos de la celula
	int vecinos[3] = { a[idHilo + 1], a[idHilo - ladoMatriz], a[idHilo - ladoMatriz + 1] };
	for (int i = 0; i < 3; i++) {
		if (vecinos[i] == 1)
		{
			contador = +1;
		}
	}
	//Comprobamos si el hilo que ha llamado al kernel se encuentra en la esquina inferior derecha de la matriz.
	//Creamos array con vecinos de la celula
	int vecinos[3] = { a[idHilo - 1], a[idHilo - ladoMatriz], a[idHilo - ladoMatriz - 1] };
	for (int i = 0; i < 3; i++) {
		if (vecinos[i] == 1)
		{
			contador = +1;
		}
	}
	//Comprobamos si el hilo que ha llamado al kernel se encuentra en el lado izquierdo (pero no en la esquina) de la matriz.
	//Creamos array con vecinos de la celula
	int vecinos[5] = { a[idHilo + 1], a[idHilo + ladoMatriz], a[idHilo - ladoMatriz], a[idHilo - ladoMatriz + 1], a[idHilo + ladoMatriz + 1] };
	for (int i = 0; i < 5; i++) {
		if (vecinos[i] == 1)
		{
			contador = +1;
		}
	}
	//Comprobamos si el hilo que ha llamado al kernel se encuentra en el lado derecho (pero no en la esquina) de la matriz.
	//Creamos array con vecinos de la celula
	int vecinos[5] = { a[idHilo - 1], a[idHilo + ladoMatriz], a[idHilo - ladoMatriz], a[idHilo - ladoMatriz - 1], a[idHilo + ladoMatriz - 1] };
	for (int i = 0; i < 5; i++) {
		if (vecinos[i] == 1)
		{
			contador = +1;
		}
	}
	//Comprobamos si el hilo que ha llamado al kernel se encuentra abajo (pero no en la esquina) de la matriz.
	//Creamos array con vecinos de la celula
	int vecinos[5] = { a[idHilo + 1], a[idHilo - 1], a[idHilo - ladoMatriz], a[idHilo - ladoMatriz + 1], a[idHilo - ladoMatriz - 1] };
	for (int i = 0; i < 5; i++) {
		if (vecinos[i] == 1)
		{
			contador = +1;
		}
	}
	//Comprobamos si el hilo que ha llamado al kernel se encuentra arriba (pero no en la esquina) de la matriz.
	//Creamos array con vecinos de la celula
	int vecinos[5] = { a[idHilo + 1], a[idHilo - 1], a[idHilo + ladoMatriz], a[idHilo + ladoMatriz + 1], a[idHilo + ladoMatriz - 1] };
	for (int i = 0; i < 5; i++) {
		if (vecinos[i] == 1)
		{
			contador = +1;
		}
	}
	//Hilo esta en el medio
	//Creamos array con vecinos de la celula
	int vecinos[8] = { a[idHilo + 1], a[idHilo - 1], a[idHilo + ladoMatriz], a[idHilo - ladoMatriz], a[idHilo - ladoMatriz - 1], a[idHilo - ladoMatriz + 1], a[idHilo + ladoMatriz + 1], a[idHilo + ladoMatriz - 1] };
	for (int i = 0; i < 8; i++) {
		if (vecinos[i] == 1)
		{
			contador = +1;
		}
		
	}
	return contador;
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
	int iteracion = 0;
	while (true) {
		iteracion = +1;
		cudaMemcpy(MatrizA_d, MatrizA, ladoMatriz*ladoMatriz * sizeof(int), cudaMemcpyHostToDevice);
		llamadaCelula << <nBloques, hilosBloque >> > (MatrizA_d, ladoMatriz);
		cudaDeviceSynchronize();
		//Envío de datos al host.
		cudaMemcpy(MatrizA, MatrizA_d, ladoMatriz*ladoMatriz * sizeof(int), cudaMemcpyDeviceToHost);
		//Representación de los resultados.
		printf("Los valores de la matriz en el paso %d:\n", iteracion);
		for (int i = 0; i < ladoMatriz; i++)
		{
			for (int j = 0; j < ladoMatriz; j++)
			{
				printf("%03d ", MatrizA[j + i * ladoMatriz]);
			}
			printf("\n");
		}
	} 
	//Liberación del espacio usado por los punteros.
	cudaFree(MatrizA_d);
	free(MatrizA);
}