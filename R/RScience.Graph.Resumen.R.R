#' @title Dibuja el histograma de una variable superponiendo la densidad normal ajustada
#' @description Función que dibuja el histograma de una variable x, superponiendo la densidad normal
#' ajustada. Si el usuario lo desea puede superponer también un estimador de núcleo de la densidad.
#' @param x vector de datos cuyo histograma se va a calcular
#' @param dens valor lógico: TRUE=Superponer estimador de núcleo de la densidad
#' @return el histograma con la densidad normal superpuesta
#' @export normalHist
#' @examples





RScience.Graph.Resumen.R <- function(base, col_number = NULL){

  armado <- list()
  general_graph_sentence <- list()
  general_graph_sentence[["hist01"]] <- '
      p <- ggplot(base, aes(x=_var_)) +
            geom_histogram(color="black", fill="white", stat = "count")
      print(p)
    '

  general_graph_sentence[["hist02"]] <- '
      hist(base[,"_var_"], col = "red")
    '

  general_graph_sentence[["boxplot"]] <- '
      boxplot(base[,"_var_"], col = "red")
    '

  matrix_sentence <- as.data.frame(lapply(general_graph_sentence, rep, length(col_number)))

  for(k4 in 1:nrow(matrix_sentence)){
    matrix_sentence[k4,]  <- gsub("_var_", colnames(base)[col_number[k4]], matrix_sentence[k4,])
  }



  return(matrix_sentence)
}
