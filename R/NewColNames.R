#' @title Dibuja el histograma de una variable superponiendo la densidad normal ajustada
#' @description Función que dibuja el histograma de una variable x, superponiendo la densidad normal
#' ajustada. Si el usuario lo desea puede superponer también un estimador de núcleo de la densidad.
#' @param x vector de datos cuyo histograma se va a calcular
#' @param dens valor lógico: TRUE=Superponer estimador de núcleo de la densidad
#' @return el histograma con la densidad normal superpuesta
#' @export normalHist
#' @examples




NewColNames <- function(col_names){

  detail <- strsplit(col_names, "")

  for(k1 in 1:length(detail)){

    detail[[k1]] <- DeleteSpecialCharacters(my_string = detail[[k1]])
    if(length(detail[[k1]]) == 1) if(detail[[k1]] == "") detail[[k1]] <- paste0("X", k1)
    detail[[k1]] <- paste0(detail[[k1]], collapse = "")
  }

  new_col_name <- unlist(detail)

  return(new_col_name)
}
