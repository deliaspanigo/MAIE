#' @title Dibuja el histograma de una variable superponiendo la densidad normal ajustada
#' @description Función que dibuja el histograma de una variable x, superponiendo la densidad normal
#' ajustada. Si el usuario lo desea puede superponer también un estimador de núcleo de la densidad.
#' @param x vector de datos cuyo histograma se va a calcular
#' @param dens valor lógico: TRUE=Superponer estimador de núcleo de la densidad
#' @return el histograma con la densidad normal superpuesta
#' @export normalHist
#' @examples






Rscience.Control.Base <- function(base, language = "ENG"){


  # Default Values
  {
    logic_detail01 <- list()
    logic_detail01[["ESP"]] <- c("Cambiar", "Correcto")
    logic_detail01[["ENG"]] <- c("Change",  "Correct")
  }

  # # 01) Intro
  {
    intro_all <- list()
    intro_all[["ESP"]] <- c("Este es un control de sobre la base de datos.",
                            "Se detalla la cantidad de filas, variables, columnas, cantidad de datos, etc.",
                            "Se centra la atencion en el control del nombre de las columnas de la base.")

    intro_all[["ENG"]] <- c("This is a check on the database.",
                            "The number of rows, variables, columns, amount of data, etc. is detailed.",
                            "The attention is focused on the control of the name of the columns of the database.")

    intro  <- intro_all[[languege]]
  }

  # # 01) Table01 -
  {
    vector01 <- c("n_row", "n_col", "total_cells", "total_na", "total_data")
    table01 <- as.data.frame(matrix(NA, 1 , length(vector01)))
    colnames(table01) <- vector01
    table01$n_row <- nrow(base)
    table01$n_col <- ncol(base)
    table01$total_cells <- nrow(base)*ncol(base)
    table01$total_na <- sum(is.na(base))
    table01$total_data <- nrow(base)*ncol(base) - sum(is.na(base))
  }

  # # 02) Table02 - colnames base control
  {
    # Colnames
    col_name <- colnames(base)
    new_col_name <- NewColNames(col_names = colnames(base))


    # Control Table02
    table02 <- naval_battle(base = base)[,c(1,3,2)]
    table02 <- cbind.data.frame(table02, new_col_name)
    dt_change <- c()
    for (k1 in 1:ncol(table02)) dt_change[k1] <- table02$col_name[k1] != table02$new_col_name[k1]

    decision <- rep(logic_detail01[["ESP"]][2], nrow(table02))
    decision[dt_change] <- logic_detail01[["ESP"]][1]
    table02 <- cbind.data.frame(table02, decision)
  }


  # # 03) Text01
  {
    total_change <- sum(dt_change)


    text01_all <- list()

    # Default Values - Without changes
    text01_all[["ESP"]] <- c("El nombre de todas las columnas es valido.",
                             "No es necesario realizar cambios en el nombre de las columnas.")

    text01_all[["ENG"]] <- c("All colnames are correct.",
                             "Change colnames it's not necesary,")


    # If is necesary same change...
    if(total_change > 0){
      text01_all[["ESP"]] <- c("Some columns have invalid names.",
                               "Es necesario realizar _change_ cambios.")


      text01_all[["ENG"]] <- c("All colnames are correct.",
                               "You need to make _change_ changes.")

    }
    text01 <- text01_all[[languege]]
    text01 <- gsub(pattern = "_change_",
                   replacement = total_change,
                   x =  text01)
  }

  # # 04) Logic
  logic04 <- TRUE
  if(total_change > 0) logic04 <- FALSE

  # Out
  out <- list(intro, table01, table02, text01, logic04)
  return(out)
}
