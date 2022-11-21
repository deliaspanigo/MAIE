#' @title Dibuja el histograma de una variable superponiendo la densidad normal ajustada
#' @description Función que dibuja el histograma de una variable x, superponiendo la densidad normal
#' ajustada. Si el usuario lo desea puede superponer también un estimador de núcleo de la densidad.
#' @param x vector de datos cuyo histograma se va a calcular
#' @param dens valor lógico: TRUE=Superponer estimador de núcleo de la densidad
#' @return el histograma con la densidad normal superpuesta
#' @export normalHist
#' @examples







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
