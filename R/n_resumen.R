
#' @title Dibuja el histograma de una variable superponiendo la densidad normal ajustada
#' @description Función que dibuja el histograma de una variable x, superponiendo la densidad normal
#' ajustada. Si el usuario lo desea puede superponer también un estimador de núcleo de la densidad.
#' @param x vector de datos cuyo histograma se va a calcular
#' @param dens valor lógico: TRUE=Superponer estimador de núcleo de la densidad
#' @return el histograma con la densidad normal superpuesta
#' @export normalHist
#' @examples

n_resumen <- function(base, col_number = NULL){


  if(is.null(col_number)) col_number <- c(1:ncol(base))


  out01 <- naval_battle(base, col_number)

  out02 <- apply(base[col_number], 2, function(x){

    out <- c(length(x), sum(is.na(x)), length(na.omit(x)))
    names(out) <- c("total_nrow", "NA_cell", "n_selected_col")
    out
  })

  out02 <- t(out02)
  n_general <- rep(nrow(na.omit(base[col_number])), nrow(out02))
  na_rows <- nrow(base) - n_general

  # out02 <- cbind.data.frame(rownames(out02), out02)
  # colnames(out02)[1] <- c("col_name")
  # out02



  # Forma 02
  # base <- base[col_number]
  base_mod <- base[col_number]
  sum_by_col <- colSums(!is.na(base_mod))
  unique_n <- unique(sum_by_col)

  contador_group <- 1
  n_group <- rep(NA, length(sum_by_col))
  names(n_group) <- names(sum_by_col)

  # Dos columnas son iguales si tienen la misma cantidad y posicion de datos NA.
  # Si la columna 1, 2 y 3 tienen la misma estructura, a las tres columnas
  # le corresponde el numero 1, porque tomara como columna de referencia
  # a la primer columna que aparece de izquierda a derecha.
  # Si las columnas 4, 5 y 6 son iguales, se les asigna el valor 4 ya que
  # la cuarta columna es la primera que los identifica
  for(k1 in 1:(length(sum_by_col)-1)) {

    for(k2 in (k1+1):length(sum_by_col)) {


      if(is.na(n_group[k1])) n_group[k1] <- k1

      if(sum_by_col[k1] == sum_by_col[k2]){



        if(identical(is.na(base_mod[,k1]), is.na(base_mod[,k2]))){

          if(is.na(n_group[k2])) n_group[k2] <- k1

        }
      }

      # The last alone?
      if(k2 == length(sum_by_col)) if(is.na(n_group[k2])) n_group[k2] <- k2
    }
  }

  # Hacemos un cambio... Para que apareazcan los grupos numerados
  # desde en 1 hasta en grupo 'k' de manera consecutiva.
  # Ahora las columnas 1, 2 y 3 tendran el grupo 1 y las columnas 4,5 y 6
  # seran el grupo 2.
  n_group <- as.numeric(as.factor(n_group))
  names(n_group) <- names(sum_by_col)

  # Change!
  data_group <- n_group

  if(identical(out01$col_name, rownames(out02))){
    if(identical(out01$col_name, names(data_group))){
      out <- cbind.data.frame(out01, out02, data_group, n_general,na_rows)

      return(out)
    }
  }


}
