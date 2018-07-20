

debugSource('~/UNIR/TFM/programas/seleccion.dinamica.R')

tabla <- read.csv(file.choose(), sep=",", header=TRUE)
descripcion_tabla <- read.csv(file.choose(), sep=",", header=TRUE)
parametros <- read.csv(file.choose(), sep=",", header=TRUE)

valida.modelo(0, tabla, descripcion_tabla, parametros)
