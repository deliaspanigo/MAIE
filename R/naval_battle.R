#' @title Dibuja el histograma de una variable superponiendo la densidad normal ajustada
#' @description Función que dibuja el histograma de una variable x, superponiendo la densidad normal
#' ajustada. Si el usuario lo desea puede superponer también un estimador de núcleo de la densidad.
#' @param x vector de datos cuyo histograma se va a calcular
#' @param dens valor lógico: TRUE=Superponer estimador de núcleo de la densidad
#' @return el histograma con la densidad normal superpuesta
#' @export normalHist
#' @examples


naval_battle <- function(base, col_number = NULL){

  if(is.null(col_number)) col_number <- c(1:ncol(base))
  col_name <- colnames(base)[col_number]
  col_letter <- num2let(col_number)

  order_number <- c(1:length(col_number))

  out <- cbind.data.frame(order_number, col_name, col_number, col_letter)

  return(out)

}
