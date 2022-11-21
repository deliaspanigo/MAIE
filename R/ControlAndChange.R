#' @title Dibuja el histograma de una variable superponiendo la densidad normal ajustada
#' @description Función que dibuja el histograma de una variable x, superponiendo la densidad normal
#' ajustada. Si el usuario lo desea puede superponer también un estimador de núcleo de la densidad.
#' @param x vector de datos cuyo histograma se va a calcular
#' @param dens valor lógico: TRUE=Superponer estimador de núcleo de la densidad
#' @return el histograma con la densidad normal superpuesta
#' @export normalHist
#' @examples





ControlAndChange <- function(base, language, speak = TRUE){

  control01 <- Rscience.Control.Base(base = base, language = languege)
  new_col_name <- control01[[3]]$new_col_name

  # Si hay algun error!
  if(!control01[[5]]) colnames(base) <- new_col_name

  # Speak!
  if(speak) cat(control01[[4]])

  return(base)
}
