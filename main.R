
debugSource('~/UNIR/TFM/programas/seleccion.dinamica.R')

tabla <- read.csv(file.choose(), sep=",", header=TRUE)
descripcion_tabla <- read.csv(file.choose(), sep=",", header=TRUE)
entrada <- read.csv(file.choose(), sep=",", header=TRUE)
parametros <- read.csv(file.choose(), sep=",", header=TRUE)

salida <- seleccion.dinamica(entrada, tabla, descripcion_tabla, parametros, salida)
#print(salida)
