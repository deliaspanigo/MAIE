#' @title Dibuja el histograma de una variable superponiendo la densidad normal ajustada
#' @description Función que dibuja el histograma de una variable x, superponiendo la densidad normal
#' ajustada. Si el usuario lo desea puede superponer también un estimador de núcleo de la densidad.
#' @param x vector de datos cuyo histograma se va a calcular
#' @param dens valor lógico: TRUE=Superponer estimador de núcleo de la densidad
#' @return el histograma con la densidad normal superpuesta
#' @export normalHist
#' @examples







# n_resumen(base, col_number)
RScience.Table.Resumen.R <- function(base, col_number = NULL,
                                     percentage = c(1, 5, 10, 25, 50, 75, 90, 95, 99),
                                     digits = 4){

  # Default value
  if(is.null(col_number)) col_number <- c(1:ncol(base))

  # Detect Numeric Columns
  dt_numeric <- unlist(lapply(base, is.numeric), use.names = FALSE)
  dt_numeric <- dt_numeric[col_number]

  # Select only numeric cols inside col_number vector
  col_number <- col_number[dt_numeric]

  # 01) Internal Table01 - Naval Battle
  table01 <- n_resumen(base = base, col_number = col_number)


  # Resumen
  table02 <- apply(base[col_number], 2, describe, na.rm = T, IQR=T, omit = T)
  table02 <- do.call(rbind.data.frame, table02)
  table02 <- table02[,-c(1)]
  table02 <- round2(table02, n = digits)
  table02 <- cbind.data.frame(table01[,c(1:4)], table02)


  # Percentiles
  proportion <- percentage/100
  table03 <- apply(base[col_number], 2, quantile,
                   probs = proportion, na.rm = T)
  table03 <- t(table03)
  table03 <- round2(table03, n = digits)
  n <- colSums(!is.na(base[col_number]))
  table03 <- cbind.data.frame(table01[,c(1:4)], n, table03)


  # out <- list(
  #   table01, table02, table03
  # )



  out <- list(
    table01, table02, table03, table03, table02
  )

  names(out) <- paste0("Salida ", 1:length(out))

  return(out)
}
