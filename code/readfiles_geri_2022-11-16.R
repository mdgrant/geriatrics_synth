## preliminaries -------------------------------------- (2022-11-16 11:49) @----
library(tidyverse)
library(janitor)
library(gt, quietly = TRUE)
library(gtExtras)
library(glue)
# library(gtsummary)
knitr::opts_chunk$set(echo = FALSE)

## data files ----------------------------------------- (2022-11-16 14:19) @----
data_files <- as_tibble(list.files("data/"))
data_files # for debug

source("code/functions_geri_2022-11-16.R")

study_char_file <- read_file_mg("studyChar")
study_refs_file <- read_file_mg("studyRefs")
study_arm_file  <- read_file_mg("studyArm")
contin_out_file <- read_file_mg("contOutcomes")
dichot_out_file <- read_file_mg("dichotOutcomes")
likert_out_file <- read_file_mg("likertOutcomes")
# rob_file        <- read_file_mg("rob_")

# display file characteristics
a <- as.character(file.mtime(paste0("data/", study_char_file)))
b <- as.character(file.mtime(paste0("data/", study_arm_file)))
c <- as.character(file.mtime(paste0("data/", contin_out_file)))
d <- as.character(file.mtime(paste0("data/", dichot_out_file)))
e <- as.character(file.mtime(paste0("data/", likert_out_file)))
g <- as.character(file.mtime(paste0("data/", study_refs_file)))
# f <- as.character(file.mtime(paste0("data/", rob_file)))

z <- matrix(c(
  paste(a, study_char_file),
  paste(b, study_arm_file),
  paste(c, contin_out_file),
  paste(d, dichot_out_file),
  paste(e, likert_out_file),
  # paste(f, rob_file),
  paste(g, study_refs_file)
  ))
z

# save list of files in current analysis
write_delim(data.frame(z), "used_files_dates.txt", delim = "--", col_names = FALSE)
rm(a, b, c, d, e, g, z)


## study characteristics  ----------------------------- (2022-11-16 14:22) @----
# study_char.dat <- suppressWarnings(read_csv(path_csv(study_char_file))) |>
design_levels <- c(
  "rct",
  "cluster",
  "crossover",
  "nr_trial",
  "quasi_exp",
  "prospect_coh",
  "retrospect_coh",
  "cross_sect",
  "case_control",
  "case_series",
  "other",
  "fully_paired")

design_labels <- c(
  "Randomized Clinical Trial",
  "Cluster Randomized",
  "Crossover Trial",
  "Nonrandomized Trial",
  "Before-After/Time Series",
  "Prospective Cohort",
  "Retrospective Cohort",
  "Cross Sectional",
  "Case-Control",
  "Case Series",
  "Other",
  "Fully Paired")

study_char_dat <- read_csv(path_csv(study_char_file)) |>
  janitor::clean_names() |>
  rename(author_dist = author, author = author_added) |> # author distiller, author entered
  mutate(across(where(is.logical), as.character),
    # design_f = factor(design),
    design_f = factor(design, levels = design_levels),
    design_f = fct_collapse(design_f, other = c("other", "fully_paired")),
    design_f_lab = factor(design, levels = design_levels, labels = design_labels),
    design_f_lab = fct_collapse(design_f_lab, Other = c("Other", "Fully Paired")),
    country = ifelse(grepl("USA", country), "USA", country),
    country = ifelse(grepl("UK", country), "UK", country),
    study = paste(author, year),
    study_l = paste0("[", study, "]", "(", "evidence_tables.html#", refid, ")")
  ) |>
  relocate(c(design_f, design_f_lab), .after = design) |>
  select(-ris_code, -level, -study_char_k) |>
  filter(refid != 1) |> # refid 1 only for column types
  select(refid, starts_with("design"), study, study_l, year, author:comment, linked_references, labels, title)

levels(study_char_dat$design_f_lab)

study_char_dat |> tabyl(design_f)
# rm(design_levels)

# verify correct column types (nb suppressed warnings)
# type_col(study_char_dat) |> arrange(desc(mode)) |> View()

# check for duplicated refids
glue("Note: ", ifelse(sum(duplicated(study_char_dat$refid)) > 0, "1 +", "No"), " duplicate refids identified in study_char_dat.")

## study arm ------------------------------------------ (2022-12-23 14:50) @----
study_arm_dat <- read_csv(path_csv(study_arm_file)) |>
  janitor::clean_names() |>
  mutate(
    across(where(is.logical), as.character)
  ) |>
  select(-ris_code, -level, -study_char_k) |>
  rename(author_dist = author, author = author_added) |> # author distiller, author entered
  group_by(refid) |> # add study arm numbering
    mutate(arm_id = row_number()) |>
  ungroup() |>
  mutate(
    study = paste(author, year),
    study_l = paste0("[", study, "]", "(", "evidence_tables.html#", refid, ")")
  ) |>
  select(refid, study, study_l, year, arm_id, everything()) |>
  select(-c(author, author_dist, title, doi, user)) |>
  filter(refid != 1) |> # refid 1 only for column types
  relocate(linked_references, labels, .after = last_col()) |>
  left_join(study_char_dat |> select(refid, design_f), by = "refid") |> # add design_f
  relocate(design_f, .after = refid) |>
  relocate(study, .after = design_f)

# type_col(study_arm_dat) |> arrange(desc(mode)) |> View()

## continuous outcomes -------------------------------- (2022-12-23 14:50) @----
contin_dat <- read_csv(path_csv(contin_out_file)) |>
  janitor::clean_names() |>
  rename_with(~ gsub("diffpval", "diff_pval", .x)) |>
  mutate(
    across(where(is.logical), as.character)
  ) |>
  select(-ris_code, -level, -study_char_k) |>
  rename(author_dist = author, author = author_added) |> # author distiller, author entered
  group_by(refid) |> # add study arm numbering
  mutate(arm_id = row_number()) |>
  ungroup() |>
  select(refid, author, year, arm_id, everything()) |>
  filter(refid != 1) |> # refid 1 only for column types
  relocate(linked_references, labels, .after = last_col())

# type_col(contin_dat) |> arrange(desc(mode)) |> View()

## dichotomous outcomes ------------------------------- (2022-12-23 14:50) @----
dichot_dat <- read_csv(path_csv(dichot_out_file)) |>
  janitor::clean_names() |>
  mutate(
    across(where(is.logical), as.character)
  ) |>
  select(-ris_code, -level, -study_char_k) |>
  rename(author_dist = author, author = author_added) |> # author distiller, author entered
  group_by(refid) |> # add study arm numbering
  mutate(arm_id = row_number()) |>
  ungroup() |>
  select(refid, author, year, arm_id, everything()) |>
  filter(refid != 1) |> # refid 1 only for column types
  relocate(linked_references, labels, .after = last_col())

# type_col(dichot_dat) |> arrange(desc(mode)) |> View()

## likert outcomes ------------------------------------ (2022-12-23 14:51) @----
likert_dat <- read_csv(path_csv(likert_out_file)) |>
  janitor::clean_names() |>
  mutate(
    across(where(is.logical), as.character)
  ) |>
  select(-ris_code, -level, -study_char_k) |>
  rename(author_dist = author, author = author_added) |> # author distiller, author entered
  group_by(refid) |> # add study arm numbering
  mutate(arm_id = row_number()) |>
  ungroup() |>
  select(refid, author, year, arm_id, everything()) |>
  filter(refid != 1) |> # refid 1 only for column types
  relocate(linked_references, labels, .after = last_col())

# type_col(likert_dat) |> arrange(desc(mode)) |> View()

## study references from full-text screening ---------- (2022-11-18 12:06) @----
study_refs_dat <- read_csv(path_csv(study_refs_file), col_types = str_c(c("n", rep("c", 5), "n", "c", "n", "n", rep("c", 58)), collapse = "")) |>
  clean_names() |>
  remove_empty(which = "cols") |>
  filter(refid %in% study_char_dat$refid) |>
  select(where(function(x) !all(is.na(x)))) |>
  select(refid, year, mean_ge_65:excl_age_init, note)

# check correct column types (nb suppressed warnings)
# type_col(study_refs.dat) |> arrange(desc(mode)) |> View()

## delete temporary files ----------------------------- (2022-12-24 13:23) @----
rm(list = ls(pattern = ".*file"))






