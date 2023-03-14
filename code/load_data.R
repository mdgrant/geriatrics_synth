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
knitr::opts_chunk$set(echo = FALSE)

read_file_mg <- function(filename){
  data_files |>
    filter(str_detect(value, filename)) |>
    arrange(desc(value)) |>
    slice(1)
}

data_files <- as_tibble(list.files("data/"))

data_file <- read_file_mg("geri_data")

load(paste0("data/", data_file))
