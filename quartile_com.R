quartile_com <- function(datos){
  # version 1.0 | Mar 2021 | Autor: Murillo David
  
  # Descripcion:
    # Modificacion realizada sobre la funcion summary {stats}
    # para que otorgue adicionalmente el valor RI (Region Intercuartilica)
    # de un conjunto de datos de caracter numerico
  
  # Argumentos:
    # datos -> vector de clase numerica con el conjunto de datos 
  
  # Codigo:
    mar <- summary(datos)
    mar[7] <- mar[5] - mar[2]
    names(mar)[7] <- "RI"
    mar
    
  # Valor:
    # Tabla donde se indica el valor minimo, maximo, cuartiles, promedio y RI
    # del conjunto de datos
    
  # Ejemplos de uso:
    # quartile_com(datos = c(21,127,125,27,123,105,112,50,77,106,83,105,28)
    # quartile_com(20,1,12,20,5,22,27,12,12,3,23,21))
}