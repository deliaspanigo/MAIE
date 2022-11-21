


# Source
# source(file = "lib.R")

# Bases
base01 <- iris
base01[1,1] <- NA
base01[2,2] <- NA

base02 <- mtcars
base02[1,1] <- NA
base02[2,2] <- NA
base02[3,3] <- NA
base02[3,5] <- NA
base02[4,5] <- NA
# base02$cyl <- as.character(base02$cyl)

# Base
colnames(base02)[2] <- paste0(colnames(base02)[2], "%%%")
colnames(base02)[3] <- paste0(colnames(base02)[3], "<<>>")

# Detalles
base <- base02
languege <- "ESP"
digits <- 4
file_name = "ABER.xlsx"

col_number <- c(1,2, 3,5)


# Control sobre la base de datos
control01 <- Rscience.Control.Base(base = base, language = languege)
control01

# Cambio en la base
base <- ControlAndChange(base = base, language = "ESP", speak = T)

tablas_control <- Rscience.Control.Base(base, language = "ENG")
tablas_control

medidas_resumen <- RScience.Table.Resumen.R(base, col_number)
medidas_resumen




RScience.Table.Resumen.Excel(base, col_number, file_name = "ABER.xlsx",
                             open_file = TRUE)
