#Under GNU General Public License v3.0
#Autor: Jesus Perez Garcia

#Biseccion para encontrar el intervalo que respeta la proporcion minima
busca.intervalo <- function(entrada_atributo, tabla_atributo, min_proporcion, cuenta_total)
{
    maximo_total <- max(tabla_atributo)
    minimo_total <- min(tabla_atributo)
    numero_seleccion <- ceiling(cuenta_total * min_proporcion)
    
    incremento_max <- max(
      abs(maximo_total - entrada_atributo), 
      abs(entrada_atributo - minimo_total)
    )
    incremento_min <- 0
    
    cuenta_grande <- length(
      tabla_atributo[
        tabla_atributo <= entrada_atributo + incremento_max & 
          tabla_atributo >= entrada_atributo - incremento_max
        ]
    )
    cuenta_grande_abierta <- length(
      tabla_atributo[
        tabla_atributo < entrada_atributo + incremento_max & 
          tabla_atributo > entrada_atributo - incremento_max
        ]
    )
    cuenta_pequena <- length(
      tabla_atributo[
        tabla_atributo <= entrada_atributo & 
          tabla_atributo >= entrada_atributo
        ]
    )
    
    fin <- FALSE
    i <- 0
    while ( fin != TRUE )
    {
        i <- i + 1
        if ( cuenta_grande == cuenta_pequena ||
          cuenta_grande_abierta <= cuenta_pequena || 
          incremento_min == incremento_max ||
          i > 100 )
        {
            fin <- TRUE
            incremento <- incremento_max
        }
        else if ( cuenta_grande > numero_seleccion )
        {
            if ( cuenta_pequena >= numero_seleccion )
            {
                fin <- TRUE
                incremento <- incremento_min
            }
            else 
            {
                proporcion <- ( numero_seleccion - cuenta_pequena ) / ( cuenta_grande - cuenta_pequena )
                #incremento_nuevo = incremento_min + (( incremento_max - incremento_min ) * proporcion)
                #biseccion no proporcional, la proporcional funciona ligeramente mejor
                incremento_nuevo <- incremento_min + (( incremento_max - incremento_min ) / 2)
                cuenta_nueva <- length(tabla_atributo[
                    tabla_atributo <= entrada_atributo + incremento_nuevo & tabla_atributo >= entrada_atributo - incremento_nuevo
                ])
                
                if ( cuenta_nueva > numero_seleccion )
                {
                    incremento_max <- incremento_nuevo
                    cuenta_grande <- cuenta_nueva
                    cuenta_grande_abierta <- length(tabla_atributo[
                      tabla_atributo < entrada_atributo + incremento_nuevo & tabla_atributo > entrada_atributo - incremento_nuevo
                    ])
                }
                else if ( cuenta_nueva < numero_seleccion )
                {
                    incremento_min <- incremento_nuevo
                    cuenta_pequena <- cuenta_nueva
                }
                else
                {
                    fin <- TRUE
                    incremento <- incremento_nuevo
                }
            }
        }
        else if ( cuenta_grande == numero_seleccion )
        {
            fin <- TRUE
            incremento <- incremento_max
        }
        else
        {
            print(paste(c("ERROR busca.intervalo grande", entrada_atributo), collapse = " "))
            fin <- TRUE
            incremento <- 0
        }
    }
    
    return (incremento)
}



seleccion.dinamica.recursiva <- function ( entrada, tabla, descripcion_tabla, parametros, salida )
{
    p_valor <- 1
    seguir <- FALSE
    
    campo_clase <- as.character(descripcion_tabla[descripcion_tabla$clase, "campo"])
    tipo_clase <- as.character(descripcion_tabla[descripcion_tabla$clase, "tipo"])
    categoria_clase <- as.character(descripcion_tabla[descripcion_tabla$clase, "categoria"])
    
    selecciones_finales <- data.frame()
    inferencia_final <- data.frame()
    
    for ( i in 1:nrow(descripcion_tabla) )
    {
        #Nos quedamos con el nombre del campo
        campo <- as.character(descripcion_tabla[i,"campo"])
        #Obtenemos el nombre del padre
        padre <- descripcion_tabla[descripcion_tabla$siguiente == campo, "campo"]
        
        #Si no se trata del atributo a predecir y no tiene padre (es una componente hija)
        #y el atributo no tiene que descansar
        if ( campo_clase != campo && length(padre) == 0 && descripcion_tabla[i, "turnos_descanso"] <= 0 )
        {
            #Si el campo es compuesto, buscamos los campos componentes para contarlos. 
            #Despues calcularemos la proporcion minima como la raiz enesima de la minima proporcion de entrada para la categoria del atributo.
            #Si tenemos n atributos componentes y min_proporcion = min_proporcion_parametro^(1/n) -> min_proporcion^n = min_proporcion_parametro
            componentes <- 0
            siguiente <- campo
            
            seleccionar <- TRUE
            
            selecciones <- data.frame()
            
            while ( seleccionar == TRUE && siguiente != "NO_NEXT" )
            {
                componentes <- componentes + 1
                
                #La entrada no tiene valor para alguno de los componentes
                if ( is.na(entrada[[siguiente]]) )
                {
                  seleccionar <- FALSE
                }
                sigaux <- siguiente
                siguiente <- as.character(descripcion_tabla[descripcion_tabla$campo == sigaux, "siguiente"])
            }
            
            #Recorremos los componentes del campo
            siguiente <- campo
            
            tabla_temp <- tabla
            
            componente <- 1
            
            while ( siguiente != "NO_NEXT" && seleccionar == TRUE )
            {
                campo <- siguiente
                tipo <- as.character(descripcion_tabla[descripcion_tabla[["campo"]] == campo, "tipo"])
                categoria <- as.character(descripcion_tabla[descripcion_tabla[["campo"]] == campo, "categoria"])
                
                
                #El tamano inicial lo tenemos que calcular para cada componente
                tamano_inicial <- length(tabla_temp[[campo]])
                
                tabla_temp <- tabla_temp[tabla_temp[[campo]] != "", ]
                tabla_temp <- tabla_temp[!apply(as.array(tabla_temp[[campo]]), 1, is.na), ]
                
                if ( categoria == "cualitativa" )
                {
                    #Calculamos la minima proporcion 
                    min_proporcion <- parametros$min_proporcion_cualitativa^(1/componentes)
                    
                    #Calculamos ya si cumplimos el criterio de minima proporcion al haber ya eliminado los registros con el atributo vacio
                    tamano_final <- length(tabla_temp[[campo]])
                    
                    if ( tamano_final / tamano_inicial < min_proporcion )
                    {
                        seleccionar <- FALSE
                    }
                    
                    if ( seleccionar == TRUE ) 
                    {
                        tabla_temp <- tabla_temp[tabla_temp[[campo]] == entrada[[campo]], ]
                    }
                }
                else
                {
                    #Calculamos la minima proporcion 
                    min_proporcion <- parametros$min_proporcion_cuantitativa^(1/componentes)
                    
                    #Calculamos ya si cumplimos el criterio de minima proporcion al haber ya eliminado los registros con el atributo vacio
                    tamano_final <- length(tabla_temp[[campo]])
                    
                    if ( tamano_final / tamano_inicial < min_proporcion )
                    {
                        seleccionar <- FALSE
                    }
                    
                    if ( seleccionar == TRUE ) 
                    {
                        incremento <- busca.intervalo(entrada[[campo]], tabla_temp[[campo]], min_proporcion, tamano_inicial)
                        tabla_temp <- 
                            tabla_temp[tabla_temp[[campo]] >= entrada[[campo]] - incremento & tabla_temp[[campo]] <= entrada[[campo]] + incremento, ]
                    }
                }
                
                tamano_final <- length(tabla_temp[[campo]])
                
                if ( tamano_final / tamano_inicial < min_proporcion )
                {
                    seleccionar <- FALSE
                }
                
                if ( seleccionar == TRUE )
                {
                    if ( categoria == "cuantitativa" )
                    {
                        seleccion <- data.frame(
                          campo=campo, valor=as.character(entrada[[campo]]), incremento=incremento, 
                          categoria=categoria, tamano=tamano_final, componente=componente
                        )
                    }
                    else
                    {
                      seleccion <- data.frame(
                        campo=campo, valor=entrada[[campo]], incremento=NA,
                        categoria=categoria, tamano=tamano_final, componente=componente
                      )
                    }
                    
                    selecciones <- rbind(selecciones, seleccion)
                }
                
                #No tiene sentido la comprobacion total de proporcion porque puede haber compuestos 
                #cuantitativos con cualitativos
                
                sigaux <- siguiente
                #Cogemos la siguiente componente del atributo compuesto si existiese
                siguiente <- as.character(descripcion_tabla[descripcion_tabla$campo == sigaux, "siguiente"])
                
                componente <- componente + 1
            }
            
            #Si cumplimos el criterio de minima proporcion y el de minima muestra realizamos el test
            if ( seleccionar == TRUE && tamano_final > parametros$minima_muestra )
            {
                if ( categoria_clase == "cualitativa" )
                {
                    #test
                    suma <- 0
                    
                    cuentas_clase_total <- length(tabla[[campo_clase]])
                    cuentas_clase <- summary(as.factor(tabla[[campo_clase]]))
                    nombres_clase <- names(cuentas_clase)
                    
                    cuentas_ejemplo_total <- length(tabla_temp[[campo_clase]])
                    cuentas_ejemplo <- summary(as.factor(tabla_temp[[campo_clase]]))
                    nombres_ejemplo <- names(cuentas_ejemplo)
                    
                    #Si solo queda una clase podemos terminar
                    if ( length(cuentas_clase) <= 1 )
                    {
                        seleccionar <- FALSE
                    }
                    
                    if ( seleccionar == TRUE )
                    {
                        for ( j in 1:length(cuentas_clase) )
                        {
                            cuenta_clase <- cuentas_clase[j]
                            p_clase <- cuenta_clase / cuentas_clase_total
                            nombre_clase <- nombres_clase[j]
                            
                            cuenta_esperada <- p_clase * cuentas_ejemplo_total
                            
                            if ( cuenta_esperada < parametros$minima_muestra_clase )
                            {
                                seleccionar <- FALSE
                            }
                            
                            indice <- which(nombres_ejemplo == nombre_clase)
                            if ( length(indice) == 0 )
                            {
                              cuenta_ejemplo <- 0
                            }
                            else
                            {
                              cuenta_ejemplo <- cuentas_ejemplo[indice]
                            }
                            
                            suma <- suma + ((cuenta_ejemplo - cuenta_esperada)^2/cuenta_esperada)
                        }
                    }
                    
                    if ( seleccionar == TRUE )
                    {
                        grados_libertad <- length(nombres_clase) - 1
                        p_valor_temp <- pchisq(q = suma, df = grados_libertad, lower.tail = FALSE)
                        inferencia <- data.frame(
                          p_valor=p_valor_temp
                        )
                        mas_inferencia <- data.frame(as.list(cuentas_ejemplo))
                        inferencia <- cbind(inferencia, mas_inferencia)
                        inferencia[-1] <- inferencia[-1] / cuentas_ejemplo_total
                    }
                }
                else
                {
                    media_clase = mean(tabla[[campo_clase]])
                    media_temp = mean(tabla_temp[[campo_clase]])
                    desviacion = sd(tabla_temp[[campo_clase]])
                    salida_t = t.test(
                      tabla_temp[[campo_clase]], alternative = "two.sided", 
                      mu = media_clase, conf.level = 1 - parametros$nivel_significacion
                    )
                    p_valor_temp = salida_t$p.value
                    
                    inferencia <- data.frame(
                      p_valor=p_valor_temp, media=media_temp, desviacion_estandar=desviacion
                    )
                }
                
                #Si es el test con menor p_valor nos guardamos la tabla nueva para seguir con la recursividad
                if ( seleccionar == TRUE && p_valor_temp < p_valor && 
                  p_valor_temp <= parametros$nivel_significacion )
                {
                    seguir <- TRUE
                    tabla_nueva <- tabla_temp
                    p_valor <- p_valor_temp
                    inferencia_final <- inferencia
                    selecciones_finales <- selecciones
                }
            }
        }
    }
    
    #Si algun atributo (simple o compuesto) cumple los criterios de minima proporcion, minima muestra y mejora del p_valor seguimos
    if ( seguir == TRUE )
    {
        for ( i in 1:nrow(descripcion_tabla) )
        {
            #Solo comprobamos la primera componente, las demas se seleccionan como hijas
            if ( as.character(descripcion_tabla[i, "campo"]) == as.character(selecciones_finales[1, "campo"]) )
            {
                if ( descripcion_tabla[i, "repeticiones_atributo"] > 0 )
                {
                    descripcion_tabla[i, "repeticiones_atributo"] <-
                      descripcion_tabla[i, "repeticiones_atributo"] - 1
                }
                else
                {
                    #Los atributos cualitativos pasan a ser no seleccionables
                    if ( as.character(descripcion_tabla[i, "categoria"]) == "cualitativa" )
                    {
                        descripcion_tabla[i, "turnos_descanso"] <- 1000000000
                    }
                    else
                    {
                        descripcion_tabla[i, "turnos_descanso"] <- parametros$turnos_descanso
                    }
                }
              
            }
            else
            {
                padre <- descripcion_tabla[descripcion_tabla$siguiente == as.character(descripcion_tabla[i,"campo"]), "campo"]
                if ( length(padre) == 0 )
                {
                    if ( descripcion_tabla[i, "turnos_descanso"] > 0 )
                    {
                        descripcion_tabla[i, "turnos_descanso"] <- 
                          descripcion_tabla[i, "turnos_descanso"] - 1
                    }
                    else
                    {
                        if ( as.character(descripcion_tabla[i, "categoria"]) != "cualitativa" )
                        {
                            descripcion_tabla[i, "repeticiones_atributo"] <- 
                              parametros$repeticiones_atributo
                        }
                    }
                }
            }
        }
        
        salida$condiciones <- rbind(salida$condiciones, selecciones_finales)
        
        #anadimos a la inferencia las columnas que no tenga
        columnas <- colnames(salida$inferencias)
        nueva_inferencia <- data.frame(p_valor=1)
        for ( i in 1:length(columnas) )
        {
            columna <- columnas[i]
            
            if ( length(inferencia_final[[columna]])  == 0 )
            {
                inferencia_final[[columna]] <- 0
            }
            
            #regeneramos el dataframe con los campos en el orden original porque rbind 
            #enlaza en orden sin comprobar el nombre de los campos
            nueva_inferencia[[columna]] <- inferencia_final[[columna]]
        }
        
        salida$inferencias <- rbind(salida$inferencias, nueva_inferencia)
        
        if ( parametros$genera_SQL == TRUE )
        {
            for ( i in 1:nrow(selecciones_finales) )
            {
                if ( selecciones_finales[i, "categoria"] == "cuantitativa" )
                {
                    auxSQL <- paste(
                      c(
                        "AND", as.character(selecciones_finales[i, "campo"]), ">=", 
                          as.numeric(as.character(selecciones_finales$valor[i])) - selecciones_finales[i, "incremento"],
                        "AND", as.character(selecciones_finales[i, "campo"]), "<=", 
                          as.numeric(as.character(selecciones_finales$valor[i])) + selecciones_finales[i, "incremento"]
                      ), collapse = " "
                    )
                }
                else
                {
                    auxSQL <- paste(
                      c(
                        "AND ", as.character(selecciones_finales[i, "campo"]), " = \'", 
                          as.character(selecciones_finales[i, "valor"]), "\'"
                      ), collapse = ""
                    )
                }
            
                salida$SQL <- paste(salida$SQL, auxSQL, sep = "\n")
            }
        }
        
        salida <- seleccion.dinamica.recursiva(entrada, tabla_nueva, descripcion_tabla, parametros, salida)
    }
    
    #Devolvemos la prediccion y el SQL
    return(salida)
}

seleccion.dinamica <- function( entrada, tabla, descripcion_tabla, parametros, salida)
{
    tabla_nueva <- tabla
    entrada_nueva <- entrada
    
    descripcion_tabla[["siguiente"]] <- as.character(descripcion_tabla[["siguiente"]])
    descripcion_tabla[is.na(descripcion_tabla$siguiente), "siguiente"] <- "NO_NEXT"
    descripcion_tabla[descripcion_tabla$siguiente == "", "siguiente"] <- "NO_NEXT"
    
    descripcion_tabla$repeticiones_atributo <- 
      rep(parametros$repeticiones_atributo, nrow(descripcion_tabla))
    descripcion_tabla$turnos_descanso <- rep(0, nrow(descripcion_tabla))
    
    #A los cualitativos les ponemos repeticiones a 1
    descripcion_tabla[
      descripcion_tabla$categoria == "cualitativa", "repeticiones_atributo"
    ] <- 0
    
    for ( i in 1:nrow(descripcion_tabla) )
    {
        campo <- as.character(descripcion_tabla[i,"campo"])
        tipo <- as.character(descripcion_tabla[i,"tipo"])
        formato <- as.character(descripcion_tabla[i,"formato"])
        
        if ( tipo == "fecha" | tipo == "hora" )
        {
            #Sobreescribimos el campo con el formato juliano
            tabla_nueva[[campo]] <- lapply(X=as.list(tabla_nueva[[campo]]), FUN=as.POSIXct, format=formato)
            tabla_nueva[[campo]] <- lapply(X=as.list(tabla_nueva[[campo]]), FUN=as.character, format="%s")
            tabla_nueva[[campo]] <- lapply(X=as.list(tabla_nueva[[campo]]), FUN=as.integer)
                
            entrada_nueva[[campo]] <- lapply(X=as.list(entrada_nueva[[campo]]), FUN=as.POSIXct, format=formato)
            entrada_nueva[[campo]] <- lapply(X=as.list(entrada_nueva[[campo]]), FUN=as.character, format="%s")
            entrada_nueva[[campo]] <- lapply(X=as.list(entrada_nueva[[campo]]), FUN=as.integer)
            
        }
        
        if ( is.list(tabla_nueva[[campo]]) )
        {
            tabla_nueva[[campo]] <- unlist(tabla_nueva[[campo]], use.names = FALSE)
        }
        
        if ( is.factor(tabla_nueva[[campo]]) )
        {
            tabla_nueva[[campo]] <- as.character(tabla_nueva[[campo]])
        }
        
        if ( is.list(entrada_nueva[[campo]]) )
        {
            entrada_nueva[[campo]] <- unlist(entrada_nueva[[campo]], use.names = FALSE)
        }
        
        if ( is.factor(entrada_nueva[[campo]]) )
        {
            entrada_nueva[[campo]] <- as.character(entrada_nueva[[campo]])
        }
    }
    
    campo_clase <- as.character(descripcion_tabla[descripcion_tabla[["clase"]], "campo"])
    tipo_clase <- as.character(descripcion_tabla[descripcion_tabla[["clase"]], "tipo"])
    categoria_clase <- as.character(descripcion_tabla[descripcion_tabla[["clase"]], "categoria"])
    
    tabla_nueva <- tabla_nueva[tabla_nueva[[campo_clase]] != "", ]
    tabla_nueva <- tabla_nueva[!apply(as.array(tabla_nueva[[campo_clase]]), 1, is.na), ]
    
    salida <- NULL
    salida$condiciones <- c()
    
    if ( parametros$genera_SQL == TRUE )
    {
        salida$SQL <- paste(c("SELECT * FROM", parametros$nombre_tabla, "WHERE 1=1"), collapse = " ")
    }
    if ( categoria_clase == "cuantitativa" )
    {
        salida$inferencias <- data.frame(
          p_valor=1, media=mean(tabla_nueva[[campo_clase]]), desviacion_estandar=sd(tabla_nueva[[campo_clase]])
        )
    }
    else
    {
        salida$inferencias <- data.frame(p_valor=1)
        cuentas_clase_total <- length(tabla_nueva[[campo_clase]])
        cuentas_clase <- summary(as.factor(tabla_nueva[[campo_clase]]))
        salida$inferencias <- 
          cbind(salida$inferencias, data.frame(as.list(cuentas_clase)))
        salida$inferencias[-1] <- salida$inferencias[-1] / cuentas_clase_total
    }
        
    #Comenzamos la recursividad
    
    salida <- seleccion.dinamica.recursiva(
      entrada_nueva, tabla_nueva, descripcion_tabla, parametros, salida
    )
    
    if ( parametros$imprimir == TRUE )
    {
        print(salida)
        par(pch=16, cex=1, bg="white", mfrow=c(1,1))
        if ( categoria_clase == "cualitativa" )
        {
            barplot(height=as.matrix(salida$inferencias[2:ncol(salida$inferencias)]), ylim=c(0, 1), beside=TRUE)
        }
        else
        {
            medias <- salida$inferencias$media
            dt <- salida$inferencias$desviacion_estandar
            dt_sup <- medias + dt
            dt_inf <- medias - dt
            maximo <- max(dt_sup) + min(dt)
            minimo <- min(dt_inf) - min(dt)
            ylim <- c(minimo, maximo)
            pasos <- seq(1,length(medias))
            
            par(pch=16, cex=1, bg="white")
            plot(pasos, medias, type="l", ylim=ylim, axes=FALSE)
            axis(side=2)
            axis(side=1, at=pasos, labels=pasos)
            grid()
            
            lines(pasos, medias, type="p")
            
            par(pch=3, cex=0.5)
            for ( i in 1:length(medias) )
            {
                lines(x=c(i,i), y=c(dt_inf[i], dt_sup[i]), type="l")
                
            }
            
            par(pch=24, cex=0.5)
            points(x=pasos, y=dt_sup, bg="black")
            par(pch=25, cex=0.5)
            points(x=pasos, y=dt_inf, bg="black")
        }
    }
    
    return(salida)
}


valida.modelo <- function(muestras, tabla, descripcion_tabla, parametros)
{
  categoria_clase <- as.character(descripcion_tabla[descripcion_tabla$clase, "categoria"])
  if ( muestras > 0 )
  {
    indices <- runif(muestras, 1, nrow(tabla) + 1)
    dim(indices) <- length(indices)
    indices <- apply(X=indices, MARGIN=1, FUN=floor)
  }
  else
  {
    indices <- 1:nrow(tabla)
  }
  
  salidas <- data.frame()
  
  if ( categoria_clase == "cuantitativa" )
  {
    for ( i in indices )
    {
      aux <- data.frame(prediccion=0, valor_real=0)
      tabla_nueva <- tabla[-i, ]
      
      entrada <- tabla[i, ]
      salida <- seleccion.dinamica(entrada, tabla_nueva, descripcion_tabla, parametros, salida)
      
      aux$prediccion <- salida$inferencias$media[length(salida$inferencias$media)]
      aux$valor_real <- 
        entrada[[as.character(descripcion_tabla[descripcion_tabla$clase, "campo"])]]
      salidas <- rbind(salidas, aux)
    }
    
    media_real <- salida$inferencias$media[1]
    coef_corr <- cor(salidas$valor_real, salidas$prediccion)
    
    difer <- abs(salidas$prediccion - salidas$valor_real)
    desv <- abs(media_real - salidas$valor_real)
    
    rae <- 100 * sum(difer) / sum(desv)
    mae <- mean(difer)
    
    print("Correlation coefficient")
    print(coef_corr)
    print("Relative absolute error")
    print(paste(c(rae, "%"), collapse = " "))
    print("Mean absolute error")
    print(mae)
    
    par(pch=16, cex=1, bg="white")
    plot(salidas$valor_real, salidas$prediccion, type="p", xlab = "Valor", ylab="Prediccion")
    abline(lm(salidas$prediccion ~ salidas$valor_real))
  }
  else
  {
    primero <- TRUE
    correctas <- 0
    incorrectas <- 0
    
    for ( i in indices )
    {
      aux <- data.frame(prediccion="prediccion", valor_real="real")
      tabla_nueva <- tabla[-i, ]
      
      entrada <- tabla[i, ]
      salida <- seleccion.dinamica(entrada, tabla_nueva, descripcion_tabla, parametros, salida)
      
      aux_inferencia <- salida$inferencias[length(salida$inferencias$p_valor), -1]
      
      nombres_clase <- names(aux_inferencia)
      
      max_indice <- which.max(aux_inferencia)
      
      aux$prediccion <- nombres_clase[max_indice]
      
      aux$valor_real <- as.character(
        entrada[[as.character(descripcion_tabla[descripcion_tabla$clase, "campo"])]]
      )
      
      if ( primero )
      {
        tp <- salida$inferencias[1, -1]
        tp[,] <- 0
        fp <- tp
        tp_rate <- tp
        fp_rate <- fp
        precision <- tp
        p <- tp
        n <- tp
        
        primero <- FALSE
      }
      
      #Desglose de positivos por clase
      p[[aux$valor_real]] <- p[[aux$valor_real]] + 1
      
      #Desglose de negativos por clase, es decir los que son diferentes a la clase
      #real de la instancia
      n[nombres_clase != aux$valor_real] <- n[nombres_clase != aux$valor_real] + 1
      
      if ( aux$prediccion == aux$valor_real )
      {
        tp[[aux$prediccion]] <- tp[[aux$prediccion]] + 1
        correctas <- correctas + 1
      }
      else
      {
        fp[[aux$prediccion]] <- fp[[aux$prediccion]] + 1
        incorrectas <- incorrectas + 1
      }
      
      salidas <- rbind(salidas, aux)
    }
    
    total <- correctas + incorrectas

    
    print("Correctly Classified Instances")
    print(correctas)
    print(paste(c(correctas / total * 100, "%"),collapse = " "))
    print("Incorrectly Classified Instances")
    print(incorrectas)
    print(paste(c(incorrectas / total * 100, "%"),collapse = " "))
    

    tp_rate <- tp / p
    fp_rate <- fp / n
    precision <- tp / ( tp + fp )
    
    print("P")
    print(p)
    print("N")
    print(n)
    print("TP")
    print(tp)
    print("FP")
    print(fp)
    print("TP RATE")
    print(tp_rate)
    print("FP RATE")
    print(fp_rate)
    print("PRECISION")
    print(precision)
    
    par(pch=16, cex=1, bg="white", mfrow=c(1, 2))
    barplot(height=as.matrix(tp_rate), ylim=c(0, 1), main="TP rate", beside=TRUE)
    barplot(height=as.matrix(fp_rate), ylim=c(0,1), main="FP rate", beside=TRUE)
    par(pch=16, cex=1, bg="white", mfrow=c(1, 1))
  }
}