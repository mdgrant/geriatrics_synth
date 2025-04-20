# Description: Read data, functions written to facilitate analyses including uniform formatting and reporting and where common tables were used across key questions.

## preliminaries -------------------------------------- (2022-11-16 11:49) @----
library(conflicted)
library(janitor)
library(robvis)
library(meta)
library(netmeta)
library(metasens)
library(skimr)
library(htmltools)
library(reactable)
library(reactablefmtr)
library(gt, quietly = TRUE)
library(gtsummary)
library(gtExtras)
library(glue)
library(tidyverse)
conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)
conflicts_prefer(gt::google_font)
knitr::opts_chunk$set(echo = FALSE)
settings.meta(CIbracket = "(", CIseparator = ", ", fs.hetstat = 10, colgap.forest = "5mm")

# most recent file
read_file_mg <- function(filename){
  data_files |>
    filter(str_detect(value, filename)) |>
    arrange(desc(value)) |>
    slice(1)
}

data_files <- as_tibble(list.files("data/"))

# most recent data file
data_file <- read_file_mg("geri_data")

load(paste0("data/", data_file))

source("code/functions_2023-04-26.R")
source("code/soe_functions.R")
source("code/table_functions.R")
