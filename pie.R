PIE <- function(ni){ 
  # version 2.0| Oct 2020 | Autores: Ramirez Paola, David Camila, Murillo-C David
  
  # Descripcion:
  # Se mide la diversidad de especies de acuerdo a
  # el indice PIE a partir de un vector de 
  # abundancias por especie
  
  # Argumentos:
  # ni <- vector numerico con las abundancias de especies
  
  # Codigo:
  n <- sum(ni)
  k <- length(ni)
  sqpi <- (ni/n) ^ 2 
  suma<- sum(sqpi[1:k])
  (n/(n -1)) * (1 - suma)
  
  # Valor:
  # El resultado del calculo del indice PIE
  
  # Ejemplo de uso:
  # PIE(ni = c(11,62,14,92,11,10,4))
  # PIE(c(11,77,21,158,10,19,6))
}
