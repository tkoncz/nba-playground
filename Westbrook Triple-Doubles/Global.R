suppressPackageStartupMessages(library(rvest))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(glue))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(extrafont))

invisible(sapply(
  list.files(path = "Westbrook Triple-Doubles/Functions", pattern = ".R", all.files = TRUE, full.names = TRUE), 
  source
))