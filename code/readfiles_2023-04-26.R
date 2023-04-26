## preliminaries -------------------------------------- (2022-11-16 11:49) @----
# in .Rprofile
# library(conflicted)
# library(tidyverse)
# conflicts_prefer(dplyr::filter)
rm(list = ls()) # avoid including errant files
library(janitor)
suppressPackageStartupMessages(library(meta))
suppressPackageStartupMessages(library(netmeta))
suppressPackageStartupMessages(library(metasens))
library(skimr)
library(htmltools)
library(reactable)
library(reactablefmtr)
library(gt, quietly = TRUE)
library(gtsummary)
library(gtExtras)
library(glue)
library(robvis)
# knitr::opts_chunk$set(echo = FALSE, out.format = "html")
knitr::opts_chunk$set(echo = FALSE)
set_gtsummary_theme(theme_gtsummary_journal(journal = "jama"))
settings.meta(CIbracket = "(", CIseparator = ", ")

## data files ----------------------------------------- (2022-11-16 14:19) @----
data_files <- as_tibble(list.files("data/"))
# data_files # for debug

# utility functions
source("code/functions_2023-04-26.R")

# functions for tables that are repetitive
source("code/table_functions.R")

study_char_file <- read_file_mg("studyChar")
study_refs_file <- read_file_mg("studyRefs")
study_arm_file  <- read_file_mg("studyArm")
contin_out_file <- read_file_mg("contOutcomes")
dichot_out_file <- read_file_mg("dichotOutcomes")
likert_out_file <- read_file_mg("likertOutcomes")
rob_file        <- read_file_mg("rob_2")
nrsi_file       <- read_file_mg("nrsi")

# display file characteristics
a <- as.character(file.mtime(paste0("data/", study_char_file)))
b <- as.character(file.mtime(paste0("data/", study_arm_file)))
c <- as.character(file.mtime(paste0("data/", contin_out_file)))
d <- as.character(file.mtime(paste0("data/", dichot_out_file)))
e <- as.character(file.mtime(paste0("data/", likert_out_file)))
f <- as.character(file.mtime(paste0("data/", rob_file)))
g <- as.character(file.mtime(paste0("data/", study_refs_file)))

z <- matrix(c(
  paste(a, study_char_file),
  paste(b, study_arm_file),
  paste(c, contin_out_file),
  paste(d, dichot_out_file),
  paste(e, likert_out_file),
  paste(g, study_refs_file),
  paste(f, rob_file)
  ))
# z

# save list of files in current analysis
write_delim(data.frame(z), "used_files_dates.txt", delim = "--", col_names = FALSE)
rm(a, b, c, d, e, f, g, z)

## study characteristics  ----------------------------- (2022-11-16 14:22) @----
# DATA: 2023-04-03 study_char_dat: study characteristics; adds surgeries; YYYY[a-z] as nec; links to evidence tables
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

design_labels_abbrev <- c(
  "RCT",
  "Cluster",
  "Crossover",
  "NR Trial",
  "Quasi-exp",
  "Prosp Coh",
  "Retro Coh",
  "Cross Sect",
  "Case-Cont",
  "Case Series",
  "Other",
  "Paired")

## append letter same author/year --------------------- (2023-02-18 16:39) @----
study_lettered <- read_csv(path_csv(study_char_file)) |>
  janitor::clean_names() |>
  filter(refid != 1) |> # refid 1 only for column types
  rename(author_dist = author, author = author_added) |>
  mutate(study = paste(author, year)) |>
  select(study, refid) |>
  arrange(study, refid) |>
  group_by(study) |>
  mutate(
    n_studies = row_number(),
    n_group = n(),
    study_r = ifelse(n_group > 1, paste0(study, letters[n_studies]), study)) |>
  ungroup() |>
  select(refid, study_r) |>
  rename(study = study_r)

study_char_dat <- read_csv(path_csv(study_char_file)) |>
  janitor::clean_names() |>
  filter(refid != 1) |> # refid 1 only for column types
  rename(author_dist = author, author = author_added) |> # author distiller, author entered
  left_join(study_lettered, by = "refid") |>
  mutate(across(where(is.logical), as.character),
    # design_f = factor(design),
    design_f = factor(design, levels = design_levels),
    design_f = fct_collapse(design_f, other = c("other", "fully_paired")),
    design_f_lab = factor(design, levels = design_levels, labels = design_labels),
    design_f_lab = fct_collapse(design_f_lab, Other = c("Other", "Fully Paired")),
    design_f_abbrev = factor(design, levels = design_levels, labels = design_labels_abbrev),
    design_f_abbrev = fct_collapse(design_f_abbrev, Other = c("Other", "Paired")),
    country = ifelse(grepl("USA", country), "USA", country),
    country = ifelse(grepl("UK", country), "UK", country),
    study_l = paste0("[", study, "]", "(", "evidence_tables.html#", refid, ")"), # , " [@", refid, "]")
    linked_study = ifelse(!is.na(linked_references), "Yes", "No"),
    neuro_threshold = factor(neuro_threshold,
      levels = c("mmselt24", "mmselt26", "mmselt27", "spmsqgt2", "diffge1sd", "diffge2sd", "diffgt20per", "diffgt2pts", "zleneg196", "zlt196", "zge196", "zgt2", "lowscore", "custom", "notspec", "diff075sd", "mincogle2", "mocale25"),
      labels = c("MMSE <24", "MMSE <26", "MMSE <27", "SPMSQ >2", "Difference from baseline ≥1 SD", "Difference from baseline ≥2 SD", "Difference from baseline >20%", "Difference from baseline >2 pts", "Z ≤-1.96", "Z <1.96", "Z ≥1.96", "Z >2", "Low score not specified", "Custom scoring", "Not specified", "Difference from baseline 0.75 SD", "MiniCog ≤2", "MoCA ≤25")
    )
  ) |>
  relocate(c(design_f, design_f_lab), .after = design) |>
  select(-ris_code, -level, -study_char_k) |>
  select(refid, starts_with("design"), study, study_l, year, author:comment, linked_references, labels, title, starts_with("reli"), neuro_threshold) # does not include factorial

## add surgery classifications ------------------------ (2023-03-04 13:56) @----
surgs <- study_char_dat |>
  select(refid, starts_with("design"), study_l, year, n_enroll, n_analyze, centers, country, non_vh_hdi, starts_with("surg")) |>
  rename_with(~ gsub("surg_", "", .x, fixed = TRUE)) |>
  mutate(across(various:other, ~ gsub("surg_", "", .x, fixed = TRUE)),
         ortho_any = ifelse(if_any(contains("ortho"), ~ !is.na(.x)), "ortho", NA),
         opth = ifelse(str_detect(other_desc, "[Cc]ataract") | !is.na(opth), "ophtho", opth),
         gi = ifelse(!is.na(colorectal) | !is.na(gi_other) | !is.na(abdominal), "GI/Abdominal", NA),
         across(c(various, cardiac, gyn, general, headneck, hepatic, neuro, opth, oralmax, ortho_any, ent, plastic, spine, thoracic, urol, vasc, other), ~ firstup(.x)),
  ) |>
  unite("surgs", various, cardiac, gyn, gi, general, headneck, hepatic, neuro, opth, oralmax, ortho_any, ent, plastic, spine, thoracic, urol, vasc, other, sep = "|", remove = FALSE, na.rm = TRUE) |>
  select(-c(various, abdominal, cardiac, colorectal, gyn, gi, general, headneck, hepatic, neuro, opth, oralmax, ortho_any, ent, plastic, spine, thoracic, urol, vasc, other, design_other, gi_other, starts_with("ortho"), list, other_desc, starts_with("hip"))) |>
  mutate(
    surgs = ifelse(str_count(surgs, "\\|") > 3, "Various", surgs),
    surgs_single = ifelse(str_detect(surgs, "\\|"), "Various", surgs),
    surgs_single = ifelse(surgs == "GI/Abdominal", "GI/Abd", surgs_single),
    surgs_single = ifelse(surgs == "GI/Abdominal|Hepatic", "GI/Abd", surgs_single),
    surgs_single = ifelse(surgs == "Hepatic", "GI/Abd", surgs_single),
    surgs_single = ifelse(surgs == "Various", "Various", surgs_single),
    # surgs_single = ifelse(surgs == "Other", "Various", surgs_single),
    surgs_single_f = factor(surgs_single, levels = rev(c("Various", "Spine", "Vasc", "Ent", "Gyn", "Oralmax", "Other", "Headneck", "Neuro", "Ophtho", "Urol", "Thoracic", "GI/Abd", "Cardiac", "Ortho"))),
    # NOTE: surgs_f_lump minimum 4 studies
    surgs_single_f_lump = fct_lump_min(surgs_single_f, min = 4, other_level = "Other")
  ) |>
  select(refid, surgs, surgs_single, surgs_single_f, surgs_single_f_lump)

surgs |> tabyl(surgs_single_f_lump)

study_char_dat <- study_char_dat |>
  left_join(surgs, by = "refid")

## add linked references ------------------------------ (2023-02-18 12:27) @----
# study_l_w_linked includes study with links to both studies
# study_w_linked:
# study_w_linked_date: only date
# study_l_w_linked: adds link to evidence table from linked study added to study_l
# study_l_w_linked_date: adds link to evidence table using date of linked study
# refid_linked, study_link:

linked_refids <- study_char_dat |>
  filter(!is.na(linked_references)) |>
  select(refid, study, study_l, author, year, linked_references) |>
  arrange(linked_references) |>
  mutate(
    linked = str_remove(linked_references, as.character(refid)),
    linked = as.numeric(str_extract(linked, "\\d+"))
  )

targets_linked_refids <- linked_refids |>
  select(refid, year, study, study_l) |>
  mutate(
    linked = refid,
    year_linked = year,
    target = str_extract(study_l, "\\(.*"),
    link_to_linked_date = paste0(" ", "[", year, "]", target),
    link_to_linked = paste0("[", study, "]", target)
  ) |>
  rename(refid_linked = refid, study_linked = study) |>
  select(refid_linked, linked, link_to_linked, link_to_linked_date, year_linked, study_linked) |>
  left_join(linked_refids, by = "linked") |>
  mutate(
    study_l_w_linked = paste0(study_l, " (", link_to_linked, ")"),
    study_l_w_linked_date = paste0(study_l, link_to_linked_date),
    study_w_linked = paste0(study, " [", study_linked, "]"),
    study_w_linked_date = paste0(study, " [", str_extract(study_linked, "\\d{4}"), "]"),
  ) |>
  select(refid, refid_linked, study_w_linked, study_w_linked_date, study_l_w_linked, study_l_w_linked_date, study_linked)

study_char_dat <- study_char_dat |>
  left_join(targets_linked_refids, by = "refid") |>
  mutate(
    study_w_linked = ifelse(is.na(study_w_linked), study, study_w_linked),
    study_l_w_linked = ifelse(is.na(study_l_w_linked), study_l, study_l_w_linked),
    study_l_w_linked_date = ifelse(is.na(study_l_w_linked_date), study_l, study_l_w_linked_date),
    study_w_linked_date = ifelse(is.na(study_w_linked_date), study, study_w_linked_date),
    linked_references_all_refid = ifelse(!is.na(linked_references), linked_references, refid)
  ) |>
  relocate(study_w_linked, study_w_linked_date, study_l_w_linked, study_l_w_linked_date, refid_linked, study_linked, linked_references_all_refid, .after = study_l)

# save for reference during in analysis
linked_refids <- targets_linked_refids |>
  mutate(study = str_extract(study_w_linked, "^\\w*\\s\\d{4}\\w{1}?")) |>
  select(refid, study, refid_linked, study_linked)

rm(targets_linked_refids)

# levels(study_char_dat$design_f_lab)
# study_char_dat |> tabyl(design_f)

# verify correct column types (nb suppressed warnings)
# type_col(study_char_dat) |> arrange(desc(mode)) |> View()

# check for duplicated refids
# glue("Note: ", ifelse(sum(duplicated(study_char_dat$refid)) > 0, "1 +", "No"), " duplicate refids identified in study_char_dat.")

## study arm ------------------------------------------ (2022-12-23 14:50) @----
study_arm_dat <- read_csv(path_csv(study_arm_file)) |>
  janitor::clean_names() |>
  filter(refid != 1) |> # refid 1 only for column types
  mutate(
    across(where(is.logical), as.character)
  ) |>
  select(-ris_code, -level, -study_char_k) |>
  rename(author_dist = author, author = author_added) |> # author distiller, author entered
  left_join(study_lettered, by = "refid") |>
  group_by(refid) |> # add study arm numbering
  mutate(arm_id = row_number()) |>
  ungroup() |>
  mutate(
    arm_id = ifelse(!is.na(arm_id_reorder), arm_id_reorder, arm_id),
    study_l = paste0("[", study, "]", "(", "evidence_tables.html#", refid, ")"),
    study_id = paste0(study, "-", arm_id), # each table row unique for footnote
    refid_c = as.character(refid),
    refid_c = case_when(
      refid == 1419 & arm_id  %in% c(4, 3) ~ "1419-1",
      refid == 1419 & arm_id  %in% c(1, 2) ~ "1419-2",
      refid == 328 & arm_id  %in% c(2, 4) ~ "328-2",
      refid == 328 & arm_id  %in% c(1, 3) ~ "328-1",
      .default = refid_c
    )
    # refid_c = case_when( # append refid_c for factorial designs to indicate comparison
    #   arm_id == factorial_1_1 ~ paste0(refid_c, "-1"),
    #   arm_id == factorial_1_2 ~ paste0(refid_c, "-1"),
    #   arm_id == factorial_2_1 ~ paste0(refid_c, "-2"),
    #   arm_id == factorial_2_2 ~ paste0(refid_c, "-2"),
    #   arm_id == factorial_3_1 ~ paste0(refid_c, "-3"),
    #   arm_id == factorial_3_2 ~ paste0(refid_c, "-3"),
    #   arm_id == factorial_4_1 ~ paste0(refid_c, "-4"),
    #   arm_id == factorial_4_2 ~ paste0(refid_c, "-4"),
    #   .default = refid_c
  ) |>
  select(refid, study, study_l, study_id, year, arm_id, everything()) |>
  select(-c(author, author_dist, title, doi, user, factorial_1_1:factorial_4_2)) |>
  relocate(linked_references, labels, factorial, refid_c, .after = last_col()) |>
  left_join(study_char_dat |> select(refid, design_f, design_f_lab, study_l_w_linked), by = "refid") |> # add design_f
  relocate(c(design_f, design_f_lab), .after = refid) |>
  relocate(study, study_l_w_linked, .after = design_f_lab) |>
  # CODE: anesthetic type
  unite(anesth_type, inhalation:anes_ns, sep = "-", na.rm = TRUE, remove = FALSE) |>
  # NOTE: variables for anesthetic type: anesth_type, volatile, iv, regional, sedation_only
  mutate(
    anesth_type = str_replace(anesth_type, "inhalation", "Volatile"),
    anesth_type = str_replace(anesth_type, "spinal_epidural", "Spinal+epidural"),
    anesth_type = str_replace(anesth_type, "epidural", "Epidural"),
    anesth_type = str_replace(anesth_type, "tiva", "TIVA"),
    anesth_type = str_replace(anesth_type, "spinal", "Spinal"),
    anesth_type = str_replace(anesth_type, "regional_oth", "Other regional"),
    anesth_type = str_replace(anesth_type, "sedation", "Only sedation"),
    anesth_type = str_replace(anesth_type, "anes_ns", "NS"),
    anesth_type = str_replace_all(anesth_type, "-", "/"),
    volatile_tab = ifelse(!is.na(inhalation), "✓", NA),
    tiva_tab = ifelse(!is.na(tiva), "✓", NA),
    regional_tab = ifelse(if_any(spinal:regional_oth, ~ !is.na(.x)), "✓", NA),
    sedation_only_tab = ifelse(!is.na(sedation), "✓", NA)
  ) |>
  relocate(anesth_type, volatile_tab:sedation_only_tab, .before = inhalation)

## for factorial designs add arm ids
study_arm_dat <- study_arm_dat |>
  mutate(
    study = ifelse(study == "Liu 2016" & str_detect(notes_studyarm, "^aMCI"), paste0(study, " (MCI)"), study),
    study = ifelse(study == "Liu 2016" & str_detect(notes_studyarm, "^non-aMCI"), paste0(study, " (no MCI)"), study),
    study_l = ifelse(study == "Liu 2016 (no MCI)", "[Liu 2016 (no MCI)](evidence_tables.html#1419)", study_l),
    study_l = ifelse(study == "Liu 2016 (MCI)" , "[Liu 2016 (MCI)](evidence_tables.html#1419)", study_l),
    study_l_w_linked = ifelse(study == "Liu 2016 (no MCI)" , "[Liu 2016 (no MCI)](evidence_tables.html#1419)", study_l_w_linked),
    study_l_w_linked = ifelse(study == "Liu 2016 (MCI)" , "[Liu 2016 (MCI)](evidence_tables.html#1419)", study_l_w_linked),
    study = ifelse(study == "Zhang 2018b" & str_detect(refid_c, "-1"), paste0(study, " (prop)"), study),
    study = ifelse(study == "Zhang 2018b" & str_detect(refid_c, "-2"), paste0(study, " (sevo)"), study),
    kq6_other_spec = ifelse(study_id == "Lee 2018b-4", "pregabalin", kq6_other_spec))

# type_col(study_arm_dat) |> arrange(desc(mode)) |> View()

## create asa ps variable ----------------------------- (2023-03-03 13:37) @----
asa_combine <- study_arm_dat |>
  select(refid, arm_id, asa_1:asa_123) |>
  mutate(across(asa_1:asa_123, ~ as.numeric(!is.na(.x)))) |>
  unite(asa_all, asa_1:asa_123) |>
  left_join(study_arm_dat |> select(refid, arm_id, asa_1:asa_123), by = c("refid", "arm_id")) |>
  mutate(across(asa_1:asa_123, ~ str_pad(as.character(digit0(.x)), width = 2, "left")),
    asa_all_combine = case_when(
      asa_all == "0_0_1_0_0_0_0_0" ~ paste0("  |  |", asa_3, "| "),
      asa_all == "1_1_0_0_0_0_1_0" ~ paste0(asa_1, "|", asa_2, "| ", asa_34, "  "),
      asa_all == "0_0_1_1_1_0_0_0" ~ paste0(" ", asa_12, "  |", asa_3, "|", asa_4),
      asa_all == "0_0_1_0_1_0_0_0" ~ paste0(" ", asa_12, "  |", asa_3, "|  "),
      asa_all == "0_0_1_1_0_0_0_0" ~ paste0("  |  |", asa_3, "|", asa_4),
      asa_all == "0_0_0_0_0_1_0_0" & asa_23 == "100" ~ paste0("  | ", asa_23, " |  "),
      asa_all == "0_0_0_0_0_1_0_0" ~ paste0("  | ", asa_23, "  |  "),
      asa_all == "0_0_0_0_1_0_1_0" ~ paste0("  ", asa_12, " |  ", asa_34, " "),
      asa_all == "0_1_1_1_0_0_0_0" ~ paste0("  |", asa_2, "|", asa_3, "|", asa_4),
      asa_all == "0_0_0_0_0_0_0_1" ~ paste0("  ", asa_123, "   |  "),
      asa_all == "0_0_0_0_1_0_0_0" & asa_12 == "100" ~ paste0(" ", asa_12, " |  |  "),
      asa_all == "0_0_0_0_1_0_0_0" ~ paste0(" ", asa_12, "  |  |  "),
      asa_all == "1_1_0_0_0_0_0_0" ~ paste0(asa_1, "|", asa_2, "|  |  "),
      asa_all == "1_1_1_1_0_0_0_0" ~ paste0(asa_1, "|", asa_2, "|", asa_3, "|", asa_4),
      asa_all == "0_1_1_0_0_0_0_0" ~ paste0("  |", asa_2, "|", asa_3, "|  "),
      asa_all == "1_1_1_0_0_0_0_0" ~ paste0(asa_1, "|", asa_2, "|", asa_3, "|  "),
      asa_all == "0_0_0_0_0_0_0_0" ~ "NS",
      .default = NA
    ),
    asa_ps_incl = case_when(
      asa_all == "0_0_1_0_0_0_0_0" ~ "  3 ",
      asa_all == "1_1_0_0_0_0_1_0" ~ "1234",
      asa_all == "0_0_1_1_1_0_0_0" ~ "1234",
      asa_all == "0_0_1_0_1_0_0_0" ~ "123 ",
      asa_all == "0_0_1_1_0_0_0_0" ~ "  34",
      asa_all == "0_0_0_0_0_1_0_0" ~ " 23 ",
      asa_all == "0_0_0_0_1_0_1_0" ~ "1234",
      asa_all == "0_1_1_1_0_0_0_0" ~ " 234",
      asa_all == "0_0_0_0_0_0_0_1" ~ "123 ",
      asa_all == "0_0_0_0_1_0_0_0" ~ "12  ",
      asa_all == "1_1_0_0_0_0_0_0" ~ "12  ",
      asa_all == "1_1_1_1_0_0_0_0" ~ "1234",
      asa_all == "0_1_1_0_0_0_0_0" ~ " 23 ",
      asa_all == "1_1_1_0_0_0_0_0" ~ "123 ",
      asa_all == "0_0_0_0_0_0_0_0" ~ "NS",
      .default = NA
    ),
    asa_ps1_incl = str_detect(asa_ps_incl, "1"),
    asa_ps2_incl = str_detect(asa_ps_incl, "2"),
    asa_ps3_incl = str_detect(asa_ps_incl, "3"),
    asa_ps4_incl = str_detect(asa_ps_incl, "4"),
    asa_psns_incl = str_detect(asa_ps_incl, "NS")
  ) |>
  select(refid, arm_id, asa_all_combine:asa_psns_incl)

study_arm_dat <- study_arm_dat |>
  left_join(asa_combine, by = c("refid", "arm_id")) |>
  relocate(asa_all_combine:asa_psns_incl, .before = asa_1)

## continuous outcomes -------------------------------- (2022-12-23 14:50) @----
contin_dat <- read_csv(path_csv(contin_out_file)) |>
  janitor::clean_names() |>
  rename_with(~ gsub("diffpval", "diff_pval", .x)) |>
  mutate(
    across(where(is.logical), as.character)
  ) |>
  select(-ris_code, -level, -study_char_k) |>
  rename(author_dist = author, author = author_added) |> # author distiller, author entered
  left_join(study_lettered, by = "refid") |>
  group_by(refid) |> # add study arm numbering
  mutate(arm_id = row_number()) |>
  ungroup() |>
  mutate(
    arm_id = ifelse(!is.na(arm_id_reorder), arm_id_reorder, arm_id),
    study_l = paste0("[", study, "]", "(", "evidence_tables.html#", refid, ")"),
    study_id = paste0(study, "-", arm_id)
  ) |>
  select(refid, study, study_l, study_id, year, arm_id, everything()) |>
  select(-c(author, author_dist, title, doi, user)) |>
  filter(refid != 1) |> # refid 1 only for column types
  relocate(linked_references, labels, .after = last_col()) |>
  left_join(study_char_dat |> select(refid, design_f, design_f_lab, design_f_abbrev), by = "refid") |> # add design_f
  relocate(c(design_f, design_f_lab, design_f_abbrev), .after = refid) |>
  select(-study, -study_l, -study_id) |>
  left_join(study_arm_dat |> select(refid, arm_id, study:study_id, refid_c), by = c("refid", "arm_id")) |>
  relocate(study:study_id, .after = design_f_abbrev)

# type_col(contin_dat) |> arrange(desc(mode)) |> View()

## dichotomous outcomes ------------------------------- (2022-12-23 14:50) @----
dichot_dat <- read_csv(path_csv(dichot_out_file)) |>
  janitor::clean_names() |>
  mutate(
    across(where(is.logical), as.character)
  ) |>
  select(-ris_code, -level, -study_char_k) |>
  rename(author_dist = author, author = author_added) |> # author distiller, author entered
  left_join(study_lettered, by = "refid") |>
  group_by(refid) |> # add study arm numbering
  mutate(arm_id = row_number()) |>
  ungroup() |>
  mutate(
    arm_id = ifelse(!is.na(arm_id_reorder), arm_id_reorder, arm_id),
    study_l = paste0("[", study, "]", "(", "evidence_tables.html#", refid, ")"),
    study_id = paste0(study, "-", arm_id)
  ) |>
  select(refid, study, study_l, study_id, year, arm_id, everything()) |>
  select(-c(author, author_dist, title, doi, user)) |>
  filter(refid != 1) |> # refid 1 only for column types
  relocate(linked_references, labels, .after = last_col()) |>
  left_join(study_char_dat |> select(refid, design_f, design_f_lab, design_f_abbrev), by = "refid") |> # add design_f
  relocate(c(design_f, design_f_lab), design_f_abbrev, .after = refid) |>
  select(-study, -study_l, -study_id) |>
  left_join(study_arm_dat |> select(refid, arm_id, study:study_id, refid_c), by = c("refid", "arm_id")) |>
  relocate(study:study_id, .after = design_f_abbrev)

# type_col(dichot_dat) |> arrange(desc(mode)) |> View()

## likert outcomes ------------------------------------ (2022-12-23 14:51) @----
likert_dat <- read_csv(path_csv(likert_out_file)) |>
  janitor::clean_names() |>
  mutate(
    across(where(is.logical), as.character)
  ) |>
  select(-ris_code, -level, -study_char_k) |>
  rename(author_dist = author, author = author_added) |> # author distiller, author entered
  left_join(study_lettered, by = "refid") |>
  group_by(refid) |> # add study arm numbering
  mutate(arm_id = row_number()) |>
  ungroup() |>
  mutate(
    arm_id = ifelse(!is.na(arm_id_reorder), arm_id_reorder, arm_id),
    study_l = paste0("[", study, "]", "(", "evidence_tables.html#", refid, ")"),
    study_id = paste0(study, "-", arm_id)
  ) |>
  select(refid, study, study_l, study_id, year, arm_id, everything()) |>
  select(-c(author, author_dist, title, doi, user)) |>
  filter(refid != 1) |> # refid 1 only for column types
  relocate(linked_references, labels, .after = last_col()) |>
  left_join(study_char_dat |> select(refid, design_f, design_f_lab, design_f_abbrev), by = "refid") |> # add design_f
  relocate(c(design_f, design_f_lab, design_f_abbrev), .after = refid) |>
  select(-study, -study_l, -study_id) |>
  left_join(study_arm_dat |> select(refid, arm_id, study:study_id, refid_c), by = c("refid", "arm_id")) |>
  relocate(study:study_id, .after = design_f_abbrev)

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

## risk of bias 2.0 ----------------------------------- (2023-03-07 10:43) @----
# DATA: 2023-04-03 rob_dat: ROB 2.0 all fields
# DATA: 2023-04-03 rob2_conflict_refid: refids w/conflicts in any assessor field
# DATA: 2023-04-03 rob2_dat: assessor fields; D1:Overall as actual ratings for use in robvis
# DATA: 2023-04-03 rob2_meta_day: D1:Overall as +?- for use in forest plots

rob_dat <- read_csv(path_csv(rob_file)) |>
  clean_names()

rob2_conflict_refid <- rob_dat |>
  select(refid, starts_with("assessor")) |>
  distinct() |>
  group_by(refid) |>
  filter(n() > 1) |>
  ungroup() |>
  select(refid) |>
  distinct()

# reconciled only kq6
rob2_dat <- rob_dat |>
  select(refid, user, starts_with("assessor")) |>
  filter(!refid %in% rob2_conflict_refid) |>
  group_by(refid) |>
  slice(1) |>
  ungroup() |>
  left_join(study_char_dat |> select(refid, study), by = "refid") |>
  mutate(year = str_extract(study, "\\d{4}")) |>
  arrange(year, study) |>
  rename_with(~ str_replace(.x, "assessor_domain", "D")) |>
  rename(Overall = assessor_overall, Study = study) |>
  mutate(
    randomization_process = D1,
    deviations_intervention = D2,
    missing_outcome = D3,
    measurement = D4,
    reporting = D5,
    overall = Overall,
  ) |>
  # select(-Study) |>
  mutate(across(
    D1:Overall,
    ~ case_when(
      .x == "Low" ~ "+",
      .x == "Some concerns" ~ "?",
      .x == "High" ~ "–"
      # .x == "Low" ~ "<p style='color:green;'>U2295</p>",
      # .x == "Some concerns" ~ "<p style='color:yellow;'>?</p>",
      # .x == "High" ~ "<p style=='color:red;'>U2296</p>"
    )
  ))

rob2_meta_dat <- rob2_dat |>
  select(refid, D1:Overall)

rob2_dat <- rob2_dat |>
  select(refid, Study, randomization_process:overall) |>
  rename(
    D1 = randomization_process,
    D2 = deviations_intervention,
    D3 = missing_outcome,
    D4 = measurement,
    D5 = reporting,
    Overall = overall
  )


## nsri ----------------------------------------------- (2023-03-28 15:27) @----
# DATA: 2023-04-03 robinsi_all dat: all ratings for use in robvis; DOES NOT EXCLUDE APPRAIAL CONFLICTS
# DATA: 2023-04-03 robinsi_dat: ratings for use in robvis; DOES NOT EXCLUDE APPRAIAL CONFLICTS
# DATA: 2023-04-03 robinsi_meta_dat: ratings for use in forest plots; DOES NOT EXCLUDE APPRAIAL CONFLICTS
robinsi_all_dat <- read_csv(path_csv(nrsi_file)) |>
  clean_names() |>
  select(!c(author:level, ends_with("comment"))) |>
  left_join(study_char_dat |> select(refid, study), by = "refid") |>
  relocate(study, .before = refid) |>
  mutate(Study = study, D1 = clinconfound, D2 = clinselect, D3 = clinclass, D4 = clindev, D5 = clinmiss, D6 = clinmeasure, D7 = clinreport, Overall = clinoverall) |>
  mutate(across(
    D1:Overall,
    ~ case_when(
      .x == "Low" ~ "++",
      .x == "Moderate" ~ "+",
      .x == "Serious" ~ "-",
      .x == "Critical" ~ "- -",
      .x == "No information" ~ "NI",
      .x == "Not applicable" ~ "NA",
      .default = "Missing"
      # .x == "Low" ~ "<p style='color:green;'>U2295</p>",
      # .x == "Some concerns" ~ "<p style='color:yellow;'>?</p>",
      # .x == "High" ~ "<p style=='color:red;'>U2296</p>"
    )
  ))

# conflicts
# robinsi_all_dat |>
#   group_by(refid) |>
#   distinct() |>
#   filter(n() > 1) |>
#   ungroup() |>
#   select(refid) |>
#   distinct()

robinsi_dat <- robinsi_all_dat |>
  select(refid, Study, clinconfound:clinoverall) |>
  rename(D1 = clinconfound, D2 = clinselect, D3 = clinclass, D4 = clindev, D5 = clinmiss, D6 = clinmeasure, D7 = clinreport, Overall = clinoverall) |>
  group_by(Study) |>
  slice(1) |>
  ungroup() |>
  select(refid, Study, D1:Overall) |>
  mutate(across(D1:D7, ~ ifelse(.x == "No information", "No Information", .x)))
# FIXME: 2023-04-03 select 1st assessment robinsi needs updating

robinsi_meta_dat <- robinsi_all_dat |>
  group_by(Study) |>
  slice(1) |>
  ungroup() |>
  select(refid, Study, D1:Overall)
# FIXME: 2023-04-03 select 1st assessment robinsi needs updating

# check files
# rob_summary(
#   data = robinsi_dat |> select(-refid),
#   tool = "ROBINS-I",
#   colour = "colourblind"
# )
#
# rob_traffic_light(
#   data = robinsi_dat,
#   tool = "ROBINS-I",
#   psize = 4,
#   colour = "colourblind"
# )

## delete temporary files ----------------------------- (2022-12-24 13:23) @----
rm(list = ls(pattern = ".*file"))

## refids by kq --------------------------------------- (2023-01-04 14:05) @----
kq1_refid <- kq_refids(kq1_preop_eval)
kq2_refid <- kq_refids(kq2_prehab)
kq3_refid <- kq_refids(kq3_reg_gen)
kq4_refid <- kq_refids(kq4_tiva_inhale)
kq5_refid <- kq_refids(kq5_inapp_meds)
kq6_refid <- kq_refids(kq6_proph_meds)
kq7_refid <- kq_refids(kq7_postop_reg)
kq8_refid <- kq_refids(kq8_pacu_screen)
# studies with kq5 and kq6 arms
kq56_refid <- study_arm_dat |>
  group_by(refid) |>
  filter(any(kq == "kq5") & any(kq == "kq6")) |>
  ungroup() |>
  select(refid) |>
  distinct() |>
  pull(refid)

## delirium outcomes refids; not other ---------------- (2023-02-13 21:16) @----
delirium_dichot_refid <- c(refid_reported_outcome(dichot_dat, c(d_delirium, d_deli_duration))) |>
  sort() |>
  unique()

delirium_contin_refid <- c(refid_reported_outcome(contin_dat, c_delirium_dur)) |>
  sort() |>
  unique()

delirium_likert_refid <- c(refid_reported_outcome(likert_dat, l_delirium)) |>
  sort() |>
  unique()

# any delirium outcome
delirium_refid <- sort(unique(delirium_dichot_refid, delirium_contin_refid, delirium_likert_refid))

## dncr outcomes -------------------------------------- (2023-02-17 16:33) @----
cogfunc_dichot_refid <- c(refid_reported_outcome(dichot_dat, d_cog_delay)) |>
  sort() |>
  unique()

cogfunc_likert_refid <- c(refid_reported_outcome(likert_dat, l_cogfunc)) |>
  sort() |>
  unique()

# any dncr outcome
cogfunc_refid <- sort(unique(c(cogfunc_dichot_refid, cogfunc_likert_refid)))

# formatted means and medians for tables
source("code/summary_mn_med_2023-02-27.R")

# color palate
# color palate
color_1 <- "#0E6655"
color_2 <- "#A93226"
color_3 <- "#104E8B"
color_4 <- "#F39C12"
gray_mg <- "#969696"

# color_1 <- "#FA8B8B"
# color_2 <- "#76d7c4"
# color_3 <- "#7fb3d5"
# color_4 <- "#c39bd3"


## retracted pubmed trials----------------------------- (2023-04-07 12:13) @----
# retracted_pubmed <- read.csv("data/retracted_trials_2023-04-07.csv")

## verify all distinct arms --------------------------- (2023-04-25 12:51) @----
study_arm_dat |>
  group_by(refid) |>
  mutate(count = n(),
         distinct_count = n_distinct(arm_id)) |>
  select(refid, count, distinct_count) |>
  filter(count != distinct_count)


#### save for use ------------------------------------- (2023-03-13 22:53) @----
save.image(paste0("data/geri_data_", str_replace_all(format(Sys.time()), "\\s|:", "-"), ".Rdata"))


