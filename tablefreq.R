tablefreq <- function(file, sel.col = 2:5, col.name = NA, 
                      h.ini = NA, h.fin = NA, 
                      skip.title = 1, del.lastrows = 3){
  
  # Autores: BBSW | release: Nov 2020 | Version: 5.0 
  
  # Descripción:
    # Esta función toma los datos desde un documento de Excel 
    # generado por un data logger (Por defecto el datta logger 
    # HOBO U23-001 Pro v2) que mide ciertas variables ambientales 
    # (Humedad relativa, Punto de rocio y Termperatura), y de acuerdo 
    # al intervalo de tiempo otorgado por el usuario se le entregará los
    # datos máximos, mínimos y la media para cada variable ambiental
    
  # Argumentos:
    # file         = Archivo del tipo ".xlxs" o “.xls” que contenga la 
    #                tabla generada a partir de los datos generados por 
    #                el data logger, por defecto se usó el 
    #                datta logger HOBO U23-001 Pro v2.  
    # 
    # sel.col      = Vector del tipo numerico. Indica la(s) posicion(es)
    #                de las columnas del Excel de las cuales se desea 
    #                extraer los datos. En caso de que se desee hacer un
    #                filtro por horas, debe incluirse la posicion de la
    #                columna que registra el tiempo.
    #
    # col.name     = Vector del tipo caracter. Va a indicar los nombres 
    #                que tendrán las columnas del data frame final.  
    #                En caso de quedar NA el argumento, 
    #                la función brindará nombres aproximados de forma 
    #                automática.
    #
    # h.ini        = Elemento del tipo fecha. Indica la fecha y hora 
    #                correspondiente al momento donde se desea comenzar 
    #                a leer los de la tabla. Su formato óptimo es 
    #                "AAAA-MM-DD hh:mm:ss" o "AAAA-MM-DD hh:mm".
    #
    # h.fin        = Elemento del tipo fecha. Indica la fecha y hora 
    #                correspondiente al momento donde se desea terminar
    #                leer los datos de la tabla. Su formato óptimo es 
    #                "AAAA-MM-DD hh:mm:ss" o "AAAA-MM-DD hh:mm".
    #
    # skip.title   = Elemento del tipo numérico. Preestablecido con el 
    #                valor "1", permite saltar la fila correspondiente 
    #                al título en la lectura de los datos de la tabla.
    #
    # del.lastrows = Elemento del tipo numérico. Con un valor preestablecido 
    #                de “3” elimina las filas finales que quedaron vacías 
    #                en la tabla ingresada en “file” que están predeterminadas 
    #                por el data logger.
    
  # Código
    library(readxl)                                                    # Se activa el paquete necesario
    tabla <- read_excel(file, skip = skip.title)                       # Se necesita extraer tabla del Excel sin la primera fila
    n <- nrow(tabla)
    tabla2 <- tabla[-((n-del.lastrows + 1):n), sel.col]                # Se genera el filtro de las ultimas tres filas y columnas seleccionadas
  
    col.length <- length(sel.col)
    if(is.character(col.name)){                                        # Condicional si se cambia el NA de col.name
      if(ncol(tabla2) - 1 != length(col.name)) stop("La longitud de col.name debe corresponder a la cantidad de columnas con datos numéricos") 
      colna <- col.name                                                # Si se corresponde, se guarda los nombres asignados
    } else colna <- substr(names(tabla2[ , 2:col.length]),             # Se extrae una porcion de los nombres a las columnas correspondientes
                          start = 1, stop = 6)
  
    switch(as.character(sum(is.na(c(h.ini, h.fin)))),                  # Se cuantifica la cantidad de NA para generar cambios 
          '2' = tabla3 <- as.data.frame(tabla2),                       # 2 NA's = no se filtra
          '1' = stop("Si h.ini o h.fin es modificado, debe otorgarse otro tiempo de referencia"), # 1 NA's = solo se cambio un dato, debe parar
          '0' = {                                                      # 0 NA's = se asignaron fechas, generar filtro
            margen <-  (5 * 3600)                                      # Se asigna la diferencia horaria
            hi <- as.POSIXct(h.ini) - margen                         
            hf <- as.POSIXct(h.fin) - margen
            tabla2 <- as.data.frame(tabla2)    
            tabla3 <- tabla2[hi <= tabla2[,1] & hf >= tabla2[,1] , ]   # Se genera filtro conforme las horas
            
            if(nrow(tabla3) == 0) stop("h.ini y h.fin deben tener formato 'AAAA-MM-DD hh:mm'")    # Si no se genero filtro, hubo error en formato
            }
    )
  
    data <- matrix(data = NA, nrow = 3, ncol = length(colna))           # Se crea matriz vacia adecuada
  
    for(col in 2:col.length){                                           # Se genera ciclo conforme cantidad de columnas
      data[ ,col - 1] <- c(range(tabla3[ ,col]), mean(tabla3[ ,col]))   # Como col comienza en 2, es necesario restar uno para llenar desde primera columna
    }
  
    colnames(data) <- colna                                             # Se asignan sea nombres escogidos o autoasignados
    rownames(data) <- c('min', 'max', 'mean') 
    data
  
  # Valor
    # si h.ini = NA y h.fin = NA se entrega una matriz con  el numero 
    # de columnas de excel seleccionadas y 3 filas con el mínimo, 
    # máximo y promedio de los valores hallados en las columnas.
    #
    # si h.ini = AAAA-MM-DD hh:mm y h.fin = AAAA-MM-DD hh:mm se entrega
    # una matriz con el numero de columnas selecionadas y 3 filas con 
    # el mínimo, máximo y promedio de los valores hallados en los rangos
    # seleccionados.
    #
    # si lo ingresado a h.ini y h.fin tiene un formato erroneo, se 
    # entregará el siguiente mensaje error: "h.ini y h.fin deben tener 
    # formato 'AAAA-MM-DD hh:mm'"
    #
    # si h.ini tiene un valor ingresado y h.fin no o viseversa, se 
    # entregará el siguiente mensaje para denotar el error.: 
    # "Si h.ini o h.fin es modificado, debe otorgarse otro tiempo de 
    # referencia"
    #
    # si col.name no coincide con la cantidad de columnas seleccionadas,
    # se entregará el siguiente mensaje error: "La longitud de col.name
    # debe corresponder a la cantidad de columnas con datos numéricos" 
  
  # Ejemplos de uso
    # tablefreq(file = 'sonda_202012915.xlsx')
    #
    # tablefreq(file = 'sonda_202012915.xls',
    #           h.ini = '2020-03-15 01:50', 
    #           h.fin = '2020-03-15 07:00', sel.col = 2:4)
    #
    # tablefreq(file = 'sonda_2020_2_2690.xlsx',
    #           h.ini = '2020-03-14 10:00', 
    #           h.fin = '2020-03-14 11:10', sel.col = c(2, 5), 
    #           col.name = 'Pt. roci')
  
}


