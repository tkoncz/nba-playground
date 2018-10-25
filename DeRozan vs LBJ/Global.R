suppressPackageStartupMessages(library(rvest))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(glue))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(extrafont))

invisible(sapply(
    c(  
      list.files(
          path = "DeRozan vs LBJ/Functions", 
          pattern = ".R", all.files = TRUE, full.names = TRUE
      ),
      list.files(
          path = "General Functions",
          pattern = ".R", all.files = TRUE, full.names = TRUE
      )),
      source
))