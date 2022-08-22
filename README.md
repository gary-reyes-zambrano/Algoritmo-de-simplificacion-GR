# Algoritmo de simplificacion GR

## Requisitos previos
+ Base de datos PostgreSQL con tablas que incluyan datos de trayectorias vehiculares.
+ Base de datos PostgreSQL con tablas que incluyan datos de redes de carretera de las ciudades a analizar.
+ Los datos deben contener los campos: latitud, longitud, tiempo, id_trayectoria.
+ RStudio.
+ R 4.1+

## Pasos para la ejecución
### Preparación del entorno
+ Se debe establecer las parametrizaciones con las que se desea ejecutar el algoritmo en el script principal (`Algoritmo-GR.R`):
  + Credenciales del repositorio (PostgreSQL)
  + Elección del Conjunto de datos a recuperar (previamente debe estar creadas las sentencias para la realización de la consulta) (`location`)
  + Según el conjunto de datos selecionado, se debe establecer el valor de la muestra (`muestra`) que debe ser calculada, y las trayectorias seleccionadas automaticamente se escogerán de manera sistemática.
  + Los valores del umbral se calculan automaticamente para cada trayectoria.

Nota: El script contiene valores por defecto para estas variables

### Ejecución del algoritmo GR
+ Habiendo establecido las parametrizaciones y teniendo seleccionado un numero de trayectorias válido para la selección de la muestra, se puede realizar la ejecución de todas las sentencias del script (`Algoritmo-GR.R`), las cuales incluirán:
  + Cargado en memoria de las funciones a utilizar
  + Cargado de las parametrizaciones
  + Consulta a la base de datos
  + Creación de la ruta de almacenamiento para los resultados (Por defecto `C:/Compressing/Algorimto-Fecha/Dataset/Hora`)
  + Calculo de los valores umbrales para el análisis de cada trayectoria
  + Simplificación del conjutno de muestras seleciconadas
  + Generación de un plano simple con los puntos de la trayectoria (original y simplificada)
  + Cálculo de la métrica Razón de compresión
  + Cálculo de la métrica Margen de error
  + Exportación de información estadística para cada trayectoria utilizada
  + Exportación de los resultados de las simplificaciones
  + Exportación de los datos de métricas de rendimiento
