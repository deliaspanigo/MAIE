#' @title Dibuja el histograma de una variable superponiendo la densidad normal ajustada
#' @description Función que dibuja el histograma de una variable x, superponiendo la densidad normal
#' ajustada. Si el usuario lo desea puede superponer también un estimador de núcleo de la densidad.
#' @param x vector de datos cuyo histograma se va a calcular
#' @param dens valor lógico: TRUE=Superponer estimador de núcleo de la densidad
#' @return el histograma con la densidad normal superpuesta
#' @export normalHist
#' @examples

round2 <- function(x, n) {
  posneg <- sign(x)

  z <- abs(x)*10^n
  z <- z + 0.5
  z <- trunc(z)
  z <- z/10^n
  z*posneg
}


num2let <- function(n, lets = LETTERS) {
  base <- length(lets)
  if (length(n) > 1) return(sapply(n, num2let, lets = lets))
  stopifnot(n > 0)
  out <- ""
  repeat {
    if (n > base) {
      rem <- (n-1) %% base
      n <- (n-1) %/% base
      out <- paste0(lets[rem+1], out)
    } else return( paste0(lets[n], out) )
  }
}

let2num <- function(x, lets = LETTERS) {
  base <- length(lets)
  s <- strsplit(x, "")
  sapply(s, function(x) sum((match(x, lets)) * base ^ seq(length(x) - 1, 0)))
}


naval_battle <- function(base, col_number = NULL){

  if(is.null(col_number)) col_number <- c(1:ncol(base))
  col_name <- colnames(base)[col_number]
  col_letter <- num2let(col_number)

  order_number <- c(1:length(col_number))

  out <- cbind.data.frame(order_number, col_name, col_number, col_letter)

  return(out)

}


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

ControlAndChange <- function(base, language, speak = TRUE){

  control01 <- Rscience.Control.Base(base = base, language = languege)
  new_col_name <- control01[[3]]$new_col_name

  # Si hay algun error!
  if(!control01[[5]]) colnames(base) <- new_col_name

  # Speak!
  if(speak) cat(control01[[4]])

  return(base)
}

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


RScience.Table.Resumen.Excel <- function(base, col_number = NULL, file_name = NULL,
                                         open_file = TRUE,
                                         white_space_side = 5,
                                         white_space_bottom = 5){

  count_the_rows <- function(x){
    if(is.data.frame(x)) return(nrow(x)) else
      if(is.vector(x)) return(length(x))
  }

  count_the_cols <- function(x){
    if(is.data.frame(x)) return(ncol(x)) else
      if(is.vector(x)) return(length(x))
  }

  # The Routput Table
  r_output <- RScience.Table.Resumen.R(base, col_number)

  matrix_sentence <- RScience.Graph.Resumen.R(base, col_number)

  # Creacion de order_of_content
  {
    the_variables <- c("Order", "SheetNumber", "RowNumber", "ColNumber")
    order_of_content <- as.data.frame(matrix(NA, length(r_output), length(the_variables)))
    colnames(order_of_content) <- the_variables

    #                         Order   SheetNumber       RowNumber     ColNumber
    order_of_content[1,] <- c(1,       1,               1,                1)
    order_of_content[2,] <- c(2,       2,               1,                1)
    order_of_content[3,] <- c(3,       2,               1,                2)
    order_of_content[4,] <- c(4,       2,               2,                1)
    order_of_content[5,] <- c(5,       2,               2,                2)

    # Sheets
    total_sheets <- max(order_of_content$SheetNumber)
    sheet_name <- paste("Salida ", 1:length(r_output))
    order_of_content$sheet_name <- sheet_name



    row_count <- unlist(lapply(r_output, count_the_rows))
    row_count <- as.vector(row_count)
    order_of_content$row_count <- row_count


    col_count <- unlist(lapply(r_output, count_the_cols))
    col_count <- as.vector(col_count)
    col_count <- rep(max(col_count), length(col_count))
    order_of_content$col_count <- col_count

    # More cols of information
    order_of_content$title_row_pos <- rep(NA, nrow(order_of_content))
    order_of_content$title_col_pos <- rep(NA, nrow(order_of_content))
    order_of_content$the_title <- paste0("Titulo ", 1:nrow(order_of_content))

    # Row and Col for each title
    for(k1 in 1:total_sheets){
      dt_sheet <- order_of_content$SheetNumber == k1
      row_of_sheet <- order_of_content$RowNumber[dt_sheet]
      col_of_sheet <- order_of_content$ColNumber[dt_sheet]
      max_row <- max(row_of_sheet)
      max_col <- max(col_of_sheet)

      #
      for(k2 in 1:max_row){
        dt_row <- order_of_content$RowNumber == k2
        dt_combination01 <- (dt_sheet + dt_row) == 2

        the_tittle_col <- order_of_content$col_count[dt_combination01]
        the_tittle_col <- the_tittle_col + white_space_side
        the_tittle_col <- cumsum(the_tittle_col)
        the_tittle_col <- the_tittle_col + 1
        the_tittle_col <- c(1, the_tittle_col)
        the_tittle_col <- the_tittle_col[-length(the_tittle_col)]

        order_of_content$title_col_pos[dt_combination01] <- the_tittle_col
      }

      for(k3 in 1:max_col){
        dt_col <- order_of_content$ColNumber == k3
        dt_combination02 <- (dt_sheet + dt_col) == 2
        the_tittle_row <- order_of_content$row_count[dt_combination02]
        the_tittle_row <- the_tittle_row + white_space_bottom
        the_tittle_row <- cumsum(the_tittle_row)
        the_tittle_row <- the_tittle_row + 1
        the_tittle_row <- c(1, the_tittle_row)
        the_tittle_row <- the_tittle_row[-length(the_tittle_row)]
        order_of_content$title_row_pos[dt_combination02] <- the_tittle_row
      }
    }

    # Row and Col for each content
    order_of_content$content_row_pos <-  order_of_content$title_row_pos + 1
    order_of_content$content_col_pos <- order_of_content$title_col_pos

  }

  hs1 <- openxlsx::createStyle(fgFill = "#4F81BD", halign = "CENTER", textDecoration = "Bold",
                               border =  c("top", "bottom", "left", "right"),
                               # borderColour = "red",
                               fontColour = "white")

  boldStyle <- openxlsx::createStyle(textDecoration="BOLD")


  # Crea un nuevo Excel
  wb <- openxlsx::createWorkbook()

  # Una hoja mas
  {
    detalle <- as.character(Sys.time()[1])
    metralla <- strsplit(detalle, " ")
    fecha <- metralla[[1]][1]
    hora <- metralla[[1]][2]

    addWorksheet(wb = wb,
                 sheetName = "RScience",
                 gridLines = FALSE)

    writeData(wb = wb,
              sheet = "RScience",
              x = "RScience",
              startCol = 1,
              startRow = 1)

    writeData(wb = wb,
              sheet = "RScience",
              x = fecha,
              startCol = 2,
              startRow = 2)

    writeData(wb = wb,
              sheet = "RScience",
              x = hora,
              startCol = 2,
              startRow = 3)
    # addStyle(wb = wb,
    #          sheet = "RScience",
    #          style= hs1,
    #          rows = order_of_content$title_row_pos[dt_combination03],
    #          cols = order_of_content$title_col_pos[dt_combination03],
    #          gridExpand = FALSE)
  }

  # Crea todas las hojas vacias
  for(k1 in 1:total_sheets) {

    addWorksheet(wb = wb,
                 sheetName = order_of_content$sheet_name[k1],
                 gridLines = FALSE)
  }

  # Otorga el titulo a todo
  for(k1 in 1:total_sheets) {
    dt_sheet <- order_of_content$SheetNumber == k1
    row_of_sheet <- order_of_content$RowNumber[dt_sheet]
    col_of_sheet <- order_of_content$ColNumber[dt_sheet]
    max_row <- max(row_of_sheet)
    max_col <- max(col_of_sheet)

    for(k2 in 1:max_row) for(k3 in 1:max_col){

      dt_row <- order_of_content$RowNumber == k2
      dt_col <- order_of_content$ColNumber == k3
      dt_combination03 <- (dt_sheet + dt_row + dt_col) == 3

      writeData(wb = wb,
                sheet = order_of_content$sheet_name[k1],
                x = order_of_content$the_title[dt_combination03],
                startCol = order_of_content$title_col_pos[dt_combination03],
                startRow = order_of_content$title_row_pos[dt_combination03])

      addStyle(wb = wb,
               sheet = order_of_content$sheet_name[k1],
               style= hs1,
               rows = order_of_content$title_row_pos[dt_combination03],
               cols = order_of_content$title_col_pos[dt_combination03],
               gridExpand = FALSE)


    }
  }

  # El contenido
  for(k1 in 1:total_sheets) {
    dt_sheet <- order_of_content$SheetNumber == k1
    row_of_sheet <- order_of_content$RowNumber[dt_sheet]
    col_of_sheet <- order_of_content$ColNumber[dt_sheet]
    max_row <- max(row_of_sheet)
    max_col <- max(col_of_sheet)

    for(k2 in 1:max_row) for(k3 in 1:max_col){

      dt_row <- order_of_content$RowNumber == k2
      dt_col <- order_of_content$ColNumber == k3
      dt_combination03 <- (dt_sheet + dt_row + dt_col) == 3
      pos_output <- order_of_content$Order[dt_combination03]

      # Agregamos la tabla
      writeDataTable(wb = wb,
                     x = r_output[[pos_output]],
                     sheet = order_of_content$sheet_name[k1],
                     startCol = order_of_content$content_col_pos[dt_combination03],
                     startRow = order_of_content$content_row_pos[dt_combination03],
                     colNames = TRUE, rowNames = FALSE,
                     tableStyle = "TableStyleLight9")




    }
  }


  # Una hoja de graficos
  {

    addWorksheet(wb = wb,
                 sheetName = "Graficos",
                 gridLines = FALSE)

    for (k4 in 1:ncol(matrix_sentence)) {


      for (k5 in 1:nrow(matrix_sentence)){

        eval(parse(text = matrix_sentence[k5, k4]))

        insertPlot(wb = wb,
                   sheet = "Graficos",
                   width = 6,
                   height = 4,
                   xy = NULL,
                   startRow = 1+20*(k5-1),
                   startCol = 1+10*(k4-1),
                   fileType = "png",
                   units = "in",
                   dpi = 300
        )
      }

    }


  }


  openxlsx::saveWorkbook(wb = wb, file = file_name, overwrite = TRUE)  ## save to working directory

  if(open_file) openxlsx::openXL(file_name)




}
