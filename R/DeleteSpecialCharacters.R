#' @title Dibuja el histograma de una variable superponiendo la densidad normal ajustada
#' @description Función que dibuja el histograma de una variable x, superponiendo la densidad normal
#' ajustada. Si el usuario lo desea puede superponer también un estimador de núcleo de la densidad.
#' @param x vector de datos cuyo histograma se va a calcular
#' @param dens valor lógico: TRUE=Superponer estimador de núcleo de la densidad
#' @return el histograma con la densidad normal superpuesta
#' @export normalHist
#' @examples




DeleteSpecialCharacters <- function(my_string){

  my_string_mod <- my_string

  # Convert all symbols to ASCII
  my_string_mod <- iconv(my_string_mod, from = 'UTF-8', to = 'ASCII//TRANSLIT')

  # All space to point
  my_string_mod <- gsub(" ","." ,my_string_mod ,ignore.case = TRUE)

  # Only Numbers and Letters
  my_string_mod <- gsub("[^0-9A-Za-z.' ]","" ,my_string_mod ,ignore.case = TRUE)


  return(my_string_mod)
}
