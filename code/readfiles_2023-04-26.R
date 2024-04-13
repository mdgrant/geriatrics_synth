## preliminaries -------------------------------------- (2022-11-16 11:49) @----
# in .Rprofile
library(conflicted)
library(tidyverse)
# conflicts_prefer(dplyr::filter)
rm(list = ls()) # avoid including errant files
library(janitor)
suppressMessages(library(meta))
suppressMessages(library(netmeta))
suppressMessages(library(metasens))
suppressMessages(library(gt))
library(skimr)
library(htmltools)
library(reactable)
library(reactablefmtr)
library(gtsummary)
library(gtExtras)
library(glue)
library(robvis)
# knitr::opts_chunk$set(echo = FALSE, out.format = "html")
knitr::opts_chunk$set(echo = FALSE)
set_gtsummary_theme(theme_gtsummary_journal(journal = "jama"))
conflicts_prefer(dplyr::lag)
conflicts_prefer(dplyr::filter)

theme <- theme_set(theme_minimal())
theme <- theme_update(
  legend.position = "none",
  axis.text = element_text(size = 12),
  axis.title = element_text(size = 12),
  axis.line.y = element_line(linewidth = 0, colour = "white", linetype = "solid"),
  axis.ticks = element_line(colour = "gray72"),
  panel.grid.major = element_line(colour = "gray80"))

# refids to exclude
exclude_refids <- c(1)

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
  filter(!refid %in% exclude_refids) |> # refid 1 only for column types
  rename(author_dist = author, author = author_added) |>
  mutate(study = paste(author, year)) |>
  select(study, refid) |>
  arrange(study, refid) |>
  group_by(study) |>
  mutate(
    n_studies = row_number(),
    n_group = n(),
    study_r = if_else(n_group > 1, paste0(study, letters[n_studies]), study)) |>
  ungroup() |>
  select(refid, study_r) |>
  rename(study = study_r)

study_char_dat <- read_csv(path_csv(study_char_file)) |>
  janitor::clean_names() |>
  filter(!refid %in% exclude_refids) |> # refid 1 only for column types
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
    country = if_else(grepl("USA", country), "USA", country),
    country = if_else(grepl("UK", country), "UK", country),
    study_l = paste0("[", study, "]", "(", "evidence_tables.html#", refid, ")"), # , " [@", refid, "]")
    linked_study = if_else(!is.na(linked_references), "Yes", "No"),
    neuro_threshold = factor(neuro_threshold,
      levels = c("mmselt24", "mmselt26", "mmselt27", "spmsqgt2", "diffge1sd", "diffge2sd", "diffgt20per", "diffgt2pts", "zleneg196", "zlt196", "zge196", "zgt2", "lowscore", "custom", "notspec", "diff075sd", "mincogle2", "mocale25"),
      labels = c("MMSE <24", "MMSE <26", "MMSE <27", "SPMSQ >2", "Difference from baseline ≥1 SD", "Difference from baseline ≥2 SD", "Difference from baseline >20%", "Difference from baseline >2 pts", "Z ≤-1.96", "Z <1.96", "Z ≥1.96", "Z >2", "Threshold not specified", "Custom scoring", "Threshold not specified", "Difference from baseline 0.75 SD", "MiniCog ≤2", "MoCA ≤25")
    )
  ) |>
  relocate(c(design_f, design_f_lab), .after = design) |>
  select(-ris_code, -level, -study_char_k) |>
  select(refid, starts_with("design"), study, study_l, year, author:comment, linked_references, labels, title, starts_with("reli"), neuro_threshold, neuro_thresh_ext, starts_with("kq5")) # does not include factorial

## file with refid_ver to check for cloned
cloned_refid <- read_csv(path_csv(study_char_file)) |>
  janitor::clean_names() |>
  filter(!refid %in% exclude_refids) |> # refid 1 only for column types
  filter(!is.na(clone)) |>
  pull(refid)

## add surgery classifications ------------------------ (2023-03-04 13:56) @----
surgs <- study_char_dat |>
  select(refid, starts_with("design"), study_l, year, n_enroll, n_analyze, centers, country, non_vh_hdi, starts_with("surg")) |>
  rename_with(~ gsub("surg_", "", .x, fixed = TRUE)) |>
  mutate(across(various:other, ~ gsub("surg_", "", .x, fixed = TRUE)),
         ortho_any = if_else(if_any(contains("ortho"), ~ !is.na(.x)), "ortho", NA),
         opth = if_else(str_detect(other_desc, "[Cc]ataract") | !is.na(opth), "ophtho", opth),
         gi =   if_else(!is.na(colorectal) | !is.na(gi_other) | !is.na(abdominal), "GI/Abdominal", NA),
         across(c(various, cardiac, gyn, general, headneck, hepatic, neuro, opth, oralmax, ortho_any, ent, plastic, spine, thoracic, urol, vasc, other), ~ firstup(.x)),
  ) |>
  unite("surgs", various, cardiac, gyn, gi, general, headneck, hepatic, neuro, opth, oralmax, ortho_any, ent, plastic, spine, thoracic, urol, vasc, other, sep = "|", remove = FALSE, na.rm = TRUE) |>
  select(-c(various, abdominal, cardiac, colorectal, gyn, gi, general, headneck, hepatic, neuro, opth, oralmax, ortho_any, ent, plastic, spine, thoracic, urol, vasc, other, design_other, gi_other, starts_with("ortho"), list, other_desc, starts_with("hip"))) |>
  mutate(
    surgs          = if_else(str_count(surgs, "\\|") > 3, "Various", surgs),
    surgs_single   = if_else(str_detect(surgs, "\\|"), "Various", surgs),
    surgs_single   = if_else(surgs == "GI/Abdominal", "GI/Abd", surgs_single),
    surgs_single   = if_else(surgs == "GI/Abdominal|Hepatic", "GI/Abd", surgs_single),
    surgs_single   = if_else(surgs == "Hepatic", "GI/Abd", surgs_single),
    surgs_single   = if_else(surgs == "Various", "Various", surgs_single),
    # surgs_single = if_else(surgs == "Other", "Various", surgs_single),
    surgs_single_f = factor(surgs_single, levels = rev(c("Various", "Spine", "Vasc", "Ent", "Gyn", "Oralmax", "Other", "Headneck", "Neuro", "Ophtho", "Urol", "Thoracic", "GI/Abd", "Cardiac", "Ortho"))),
    surgs_noabbr_f = factor(surgs_single, levels = rev(c("Various", "Spine", "Vasc", "Ent", "Gyn", "Oralmax", "Other", "Headneck", "Neuro", "Ophtho", "Urol", "Thoracic", "GI/Abd", "Cardiac", "Ortho")), labels = rev(c("Various", "Spine", "Vascular", "Otolaryngological", "Gynecologic", "Oral/Maxillofacial", "Other", "Head & Neck", "Neurosurgical", "Ophthalmologic", "Urologic", "Thoracic", "Gastrointestinal/Abdominal", "Cardiac", "Orthopedic"))),
    # NOTE: surgs_f_lump minimum 4
    surgs_single_f_lump = fct_lump_min(surgs_single_f, min = 4, other_level = "Other"),
    across(c(surgs, surgs_single, surgs_single_f), ~ str_replace(.x, "Ent", "ENT"))
  ) |>
  select(refid, surgs, surgs_single, surgs_single_f, surgs_noabbr_f, surgs_single_f_lump)

# surgs |> tabyl(surgs_single_f_lump)
# surgs |> tabyl(surgs_noabbr)

study_char_dat <- study_char_dat |>
  left_join(surgs, by = "refid")

## ortho_proc ----------------------------------------- (2023-05-02 11:29) @----
ortho_proc <- study_char_dat |>
  select(refid, study, surg_ortho_tka, surg_ortho_tha, surg_ortho_hipfx, surg_ortho_other, surg_list) |>
  mutate(across(contains("ortho"), ~ str_replace_all(.x, "surg_ortho_", ""))) |>
  unite(ortho, surg_ortho_tka:surg_ortho_other, sep = "|", remove = FALSE, na.rm = TRUE) |>
  mutate(
    ortho = str_replace(ortho, "tka", "TKA"),
    ortho = str_replace(ortho, "tha", "THA"),
    ortho = str_replace(ortho, "hipfx", "HipFx"),
    ortho = str_replace(ortho, "other", "Other"),
    ortho = if_else(ortho == "", NA, ortho),
    surg_list = firstup(surg_list),
    surg_list = str_replace(surg_list, "\\.$", ""),
    ortho_hip_knee = case_when(
      str_detect(ortho, "THA|TKA|Hip") ~ "Ortho (hip, knee)",
      !is.na(ortho) ~ "Ortho (other)",
      .default = ortho
    )
  ) |>
  relocate(ortho_hip_knee, .before = surg_list)

ortho_hipfx_refid <- ortho_proc |> filter(!is.na(surg_ortho_hipfx)) |> pull(refid)

## add linked references ------------------------------ (2023-02-18 12:27) @----
# study_l_w_linked includes study with links to both studies
# refid_to_link_to is refid of first publication (hopefully primary or initial cloned)

linked_refids <- study_char_dat |>
  filter(!is.na(linked_references)) |>
  # filter(!is.na(refid)) |> # remove linked to/primary refid (a appended or cloned from)
  select(refid, study, study_l, author, year, linked_references) |>
  group_by(linked_references) |>
  arrange(year) |>
  mutate(
    refid_to_link_to = str_extract(linked_references, "^\\d*"),
    refid_to_link_to = as.numeric(ifelse(row_number() == 1, NA, refid_to_link_to)),
    # NOTE: temporary fix because 813 slor is not at level 3
    refid_to_link_to = if_else(refid_to_link_to == 813, 8574, refid_to_link_to)
  ) |>
  ungroup() |>
  arrange(linked_references)

study_l_link_add <- study_char_dat |> # study_l for refid_to_link_to
  filter(refid %in% linked_refids$refid_to_link_to) |>
  select(refid, study_l) |>
  rename(refid_to_link_to = refid, study_l_to_link_to = study_l)

targets_linked_refids <- linked_refids |>
  select(refid, year, study, study_l, refid_to_link_to) |>
  filter(!is.na(refid_to_link_to)) |>
  left_join(study_l_link_add, by = c("refid_to_link_to")) |>
  mutate(
    study_l_w_linked = paste0(study_l, " (", (study_l_to_link_to), ")"),
    study_w_linked = str_remove(study_l_w_linked, "\\(.*\\) \\("),
    study_w_linked = str_remove(study_w_linked, "\\(.*"),
    study_w_linked = str_remove(study_w_linked, "^\\["),
    study_w_linked = str_replace(study_w_linked, "\\]\\[", ", "),
    study_w_linked = str_remove(study_w_linked, "\\]$")
  )

study_char_dat <- study_char_dat |>
  left_join(targets_linked_refids |> select(refid, refid_to_link_to, study_l_w_linked, study_w_linked), by = "refid") |>
  mutate(
    study_l_w_linked = if_else(is.na(study_l_w_linked), study_l, study_l_w_linked),
    linked_references_all_refid = if_else(!is.na(linked_references), linked_references, as.character(refid)),
    refid_to_link_to = if_else(!is.na(refid_to_link_to), refid_to_link_to, refid)
  ) |>
  relocate(study_l_w_linked, study_w_linked, refid_to_link_to, linked_references_all_refid, .after = study_l)

rm(targets_linked_refids, study_l_link_add)

# verify correct column types (nb suppressed warnings)
# type_col(study_char_dat) |> arrange(desc(mode)) |> View()

# check for duplicated refids
# glue("Note: ", if_else(sum(duplicated(study_char_dat$refid)) > 0, "1 +", "No"), " duplicate refids identified in study_char_dat.")

## study arm ------------------------------------------ (2022-12-23 14:50) @----
study_arm_dat <- read_csv(path_csv(study_arm_file)) |>
  janitor::clean_names() |>
  filter(!refid %in% exclude_refids) |> # refid 1 only for column types
  mutate(
    across(where(is.logical), as.character)
  ) |>
  select(-ris_code, -level, -study_char_k) |>
  rename(author_dist = author, author = author_added) |> # author distiller, author entered
  left_join(study_lettered, by = "refid") |>
  mutate(
    study_l = paste0("[", study, "]", "(", "evidence_tables.html#", refid, ")"),
    study_id = paste0(study, "-", arm_id), # each table row unique for footnote
    # VARIABLE: append refid_c for factorial or subgroups designs to indicate comparison groups
    refid_c = as.character(refid),
    refid_c = case_when(
      refid == 1419 & arm_id  %in% c(4, 3) ~ "1419-1",
      refid == 1419 & arm_id  %in% c(1, 2) ~ "1419-2",
      refid == 18678 & arm_id  %in% c(1, 2) ~ "18678-1",
      refid == 18678 & arm_id  %in% c(3, 4) ~ "18678-2",
      refid == 328 & arm_id  %in% c(2, 4) ~ "328-2",
      refid == 328 & arm_id  %in% c(1, 3) ~ "328-1",
      .default = refid_c
    )
    # not used
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
    volatile_tab = if_else(!is.na(inhalation), "✓", NA),
    tiva_tab = if_else(!is.na(tiva), "✓", NA),
    regional_tab = if_else(if_any(spinal:regional_oth, ~ !is.na(.x)), "✓", NA),
    sedation_only_tab = if_else(!is.na(sedation), "✓", NA),
    cog_imp_n = ifelse(cog_imp == 0, 0, cog_imp_n) # cog_imp_n added and not completed when cog_imp == 0
  ) |>
  relocate(anesth_type, volatile_tab:sedation_only_tab, .before = inhalation)

## verify no duplicate arm_id entries; return error message if duplicate arm_id for study
verify_no_duplicate_arm_id <- study_arm_dat |>
  select(refid, study, arm_id) |>
  group_by(refid) |>
  filter(n_distinct(arm_id) != n()) |>
  ungroup() |>
  nrow()

if (verify_no_duplicate_arm_id != 0) {
  cat(crayon::red("STOP", "DUPLICATE ARM_IDs"))
}

## for factorial designs add arm ids
study_arm_dat <- study_arm_dat |>
  # filter(refid %in% c(18678, 1419, 328)) |>
  mutate(
    study = if_else(study == "Liu 2016" & str_detect(notes_studyarm, "^aMCI"), paste0(study, " (MCI)"), study),
    study = if_else(study == "Liu 2016" & str_detect(notes_studyarm, "^non-aMCI"), paste0(study, " (no MCI)"), study),
    study_l = if_else(study == "Liu 2016 (no MCI)", "[Liu 2016 (no MCI)](evidence_tables.html#1419)", study_l),
    study_l = if_else(study == "Liu 2016 (MCI)" , "[Liu 2016 (MCI)](evidence_tables.html#1419)", study_l),
    study_l_w_linked = if_else(study == "Liu 2016 (no MCI)" , "[Liu 2016 (no MCI)](evidence_tables.html#1419)", study_l_w_linked),
    study_l_w_linked = if_else(study == "Liu 2016 (MCI)" , "[Liu 2016 (MCI)](evidence_tables.html#1419)", study_l_w_linked),
    study = if_else(study == "O'Brien 2023" & subgroup == "dementia", paste0(study, " (dementia)"), study),
    study = if_else(study == "O'Brien 2023" & subgroup == "none", paste0(study, " (no dementia)"), study),
    study_l = if_else(study == "O'Brien 2023 (no dementia)", "[O'Brien 2023 (no dementia)](evidence_tables.html#18678)", study_l),
    study_l = if_else(study == "O'Brien 2023 (dementia)", "[O'Brien 2023 (dementia)](evidence_tables.html#18678)", study_l),
    study_l_w_linked = if_else(study == "O'Brien 2023 (no dementia)", "[O'Brien 2023 (no dementia)](evidence_tables.html#18678)", study_l_w_linked),
    study_l_w_linked = if_else(study == "O'Brien 2023 (dementia)", "[O'Brien 2023 (dementia)](evidence_tables.html#18678)", study_l_w_linked),
    study = if_else(study == "Zhang 2018b" & str_detect(refid_c, "-1"), paste0(study, " (prop)"), study),
    study = if_else(study == "Zhang 2018b" & str_detect(refid_c, "-2"), paste0(study, " (sevo)"), study),
    kq6_other_spec = if_else(study_id == "Lee 2018b-4", "pregabalin", kq6_other_spec)
    ) |>
  rename(kq1_staff_other_spec = staff_other_spec, kq1_staff_other_spec_std = staff_other_spec_std)
  # select(refid, starts_with("study"), subgroup)

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
      asa_all == "0_0_0_1_0_0_0_0" ~ paste0("  |  |  |", asa_4),
      asa_all == "0_0_0_0_0_0_0_0" ~ "NR",
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
      asa_all == "0_0_0_1_0_0_0_0" ~ "   4",
      asa_all == "0_0_0_0_0_0_0_0" ~ "NR",
      .default = NA
    ),
    asa_ps1_incl = str_detect(asa_ps_incl, "1"),
    asa_ps2_incl = str_detect(asa_ps_incl, "2"),
    asa_ps3_incl = str_detect(asa_ps_incl, "3"),
    asa_ps4_incl = str_detect(asa_ps_incl, "4"),
    asa_psns_incl = str_detect(asa_ps_incl, "NR")
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
  mutate(
    study_l = paste0("[", study, "]", "(", "evidence_tables.html#", refid, ")"),
    study_id = paste0(study, "-", arm_id)
  ) |>
  select(refid, study, study_l, study_id, year, arm_id, everything()) |>
  select(-c(author, author_dist, title, doi, user)) |>
  filter(!refid %in% exclude_refids) |> # refid 1 only for column types
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
  mutate(
    study_l = paste0("[", study, "]", "(", "evidence_tables.html#", refid, ")"),
    study_id = paste0(study, "-", arm_id)
  ) |>
  select(refid, study, study_l, study_id, year, arm_id, everything()) |>
  select(-c(author, author_dist, title, doi, user)) |>
  filter(!refid %in% exclude_refids) |> # refid 1 only for column types
  relocate(linked_references, labels, .after = last_col()) |>
  left_join(study_char_dat |> select(refid, design_f, design_f_lab, design_f_abbrev), by = "refid") |> # add design_f
  relocate(c(design_f, design_f_lab), design_f_abbrev, .after = refid) |>
  select(-study, -study_l, -study_id) |>
  left_join(study_arm_dat |> select(refid, arm_id, study:study_id, refid_c), by = c("refid", "arm_id")) |>
  relocate(study:study_id, .after = design_f_abbrev)

## delirium tools dichot ------------------------------ (2022-12-23 14:50) @----
delirium_scale_dichot <- dichot_dat |>
  select(refid, study, arm_id, starts_with("deli_scale")) |>
  remove_empty(which = "rows") |>
  mutate(
    across(starts_with("deli_"), ~ str_remove_all(.x, "scale_")),
    across(starts_with("deli_"), ~ str_remove_all(.x, "deli_")),
    across(deli_scale_cam:deli_scale_unspecified, ~ toupper(.x)),
    deli_scale_4at       = ifelse(str_detect(deli_scale_otherspec, "4AT"), "4AT", NA_character_),
    deli_scale_amt       = ifelse(str_detect(deli_scale_otherspec, "AMT"), "AMT", NA_character_), # Abbreviated Mental Test
    deli_scale_chart_del = ifelse(str_detect(deli_scale_otherspec, "CHART-DEL"), "CHART-DEL", NA_character_),
    deli_scale_chinese   = ifelse(str_detect(deli_scale_otherspec, "Chinese Expert Consensus"), "CEC", NA_character_),
    deli_scale_clinical  = ifelse(str_detect(deli_scale_otherspec, "[Cc]linical|consultation"), "Clinical", NA_character_),
    deli_scale_icd9      = ifelse(str_detect(deli_scale_otherspec, "ICD-9"), "ICD-9", NA_character_),
    deli_scale_icd10     = ifelse(str_detect(deli_scale_otherspec, "ICD-10"), "ICD-10", NA_character_),
    deli_scale_nudesc    = ifelse(str_detect(deli_scale_otherspec, "DESC"), "Nu-DESC", NA_character_),
    deli_scale_dos       = ifelse(str_detect(deli_scale_otherspec, "DOS"), "DOS", deli_scale_dos),
    deli_scale_dsm       = ifelse(str_detect(deli_scale_otherspec, "DSM"), "DSM", deli_scale_dsm),
    deli_scale_chart     = ifelse(str_detect(deli_scale_otherspec, "chart"), "Chart review", NA_character_),
    deli_scale_korean    = ifelse(str_detect(deli_scale_otherspec, "Korean"), "KNDSS", NA_character_),
    deli_scale_clinical  = ifelse(deli_scale_otherspec == "Disturbance of consciousness assessment", "Clinical", deli_scale_clinical),
  ) |>
  relocate(deli_scale_other:deli_scale_otherspec, .after = deli_scale_korean) |>
  unite(delirium_scale, deli_scale_cam:deli_scale_korean, remove = FALSE, sep = "|", na.rm = TRUE) |>
  mutate(
    delirium_scale = ifelse(delirium_scale == "UNSPECIFIED", "NS", delirium_scale),
    delirium_scale_primary = ifelse(!str_detect(delirium_scale, "\\|"), delirium_scale, NA_character_),
    delirium_scale_primary = ifelse(refid %in% c(149), "DOS", delirium_scale_primary),
    delirium_scale_primary = ifelse(refid %in% c(1937, 18945, 335, 9616), delirium_scale, delirium_scale_primary), # 1937 Hollinger 2021 any; 18945 Zarour 2023 any CAM|4AT|CHART-DEL; Barreto Chang… 2021 any/and
    delirium_scale_primary = ifelse(refid %in% c(3841), "DSM", delirium_scale_primary), # 3841 Oh 2021 DRS and CAM but used DSM diagnostic criteria
  ) |>
  select(refid, study, arm_id, delirium_scale, delirium_scale_primary)

## neurocognitive tools dichot ------------------------ (2022-12-23 14:50) @----
neurocog_labels <- c(
  "cerad_avlt" =   "Consortium to Establish a Registry for Alzheimer’s Disease Auditory Verbal Learning Test",
  "apa_pocd" =     "American Psychiatric Association postoperative cognitive dysfunction criteria",
  "almt" =         "Associative Learning and Memory Test",
  "avlt" =         "Auditory Verbal Learning Test",
  "block9" =       "Block Test 9 tests",
  "charlson" =     "Charlson comorbidity index (dementia)",
  "cdrs" =         "Clinical Dementia Rating Scale",
  "cst" =          "Concept Shifting Test",
  "cowa" =         "Controlled oral word association",
  "cbt" =          "Corsi Block Tapping",
  "dsm" =          "Diagnostic and Statistical Manual of Mental Disorders",
  "dst" =          "Digit Span Test",
  "dsymt" =        "Digit Symbol Test",
  "tapping" =      "Finger Tapping Test",
  "pegboard" =     "Grooved Pegboard Test",
  "hopkins_vlt" =  "Hopkins Verbal Learning Test ",
  "iqcd" =         "Informant Questionnaire on Cognitive Decline",
  "ldct" =         "Letter Digit Coding Test",
  "minicog" =      "Mini-Cog",
  "mmse" =         "Mini-Mental State Examination",
  "moca" =         "Montreal Cognitive Assessment",
  "pf" =           "Phonemic Fluency",
  "postop_qrs" =   "Postoperative Quality of Recovery Scale (cognitive)",
  "psyche" =       "PsychE",
  "ravl" =         "Rey Auditory Verbal Learning",
  "sft" =          "Semantic Fluency test",
  "spmsq" =        "Short Portable Mental Status Questionnaire",
  "smt" =          "Story Memory Test",
  "stroop" =       "Stroop Color Word Test",
  "sdmt" =         "Symbol Digit Modalities Test",
  "traila" =       "Trail Marking Test A",
  "trailb" =       "Trail Making Test B",
  "ticsm" =        "Telephone Interview for Cognitive Status-Modified",
  "uds_adc" =      "Uniform Data Set of the Alzheimer’s Disease Centers",
  "vft" =          "Verbal Fluency Test",
  "vvlt" =         "Visual Verbal Learning Tests",
  "wms" =          "Weschler Memory Scale",
  "wrt" =          "Words Recall Test",
  "unspec" =       "Not specified"
)
neurocog_scale_dichot <- dichot_dat |>
  select(refid, study, arm_id, starts_with("neurocog_scale")) |>
  remove_empty(which = "rows") |>
  mutate(
    across(starts_with("neurocog_"), ~ str_remove_all(.x, "scale_")),
    across(starts_with("neurocog_"), ~ str_remove_all(.x, "neurocog_"))
  ) |>
  unite(neurocog_scale, neurocog_scale_apa_pocd:neurocog_scale_unspec, remove = FALSE, sep = "|", na.rm = TRUE) |>
  filter(neurocog_scale != "") |>
  relocate(neurocog_scale, .after = study) |>
  filter(arm_id == "1") |>
  mutate(
    neurocog_footnote = str_remove(neurocog_scale, "\\|fail.*$"),
    neurocog_footnote = str_replace_all(neurocog_footnote, pattern = neurocog_labels),
    neurocog_footnote = str_replace_all(neurocog_footnote, pattern = "\\|", replacement = "; "),
    neurocog_footnote = paste0(neurocog_footnote, "."),
    neurocog_scale = case_match(
      neurocog_scale,
      "apa_pocd"    ~ "APA-POCD",
      "mmse"        ~ "MMSE",
      "postop_qrs"  ~ "PostopQRS",
      "moca"        ~ "MoCA",
      "spmsq"       ~ "SPMSQ",
      "ticsm"       ~ "TICS-M",
      "unspec"      ~ "NS",
      .default      = neurocog_scale
    ),
    neurocog_scale = if_else(str_detect(neurocog_scale, "fail1"), "Failed 1", neurocog_scale),
    neurocog_scale = if_else(str_detect(neurocog_scale, "fail2"), "Failed 2", neurocog_scale),
  ) |>
  select(refid, study, neurocog_scale, neurocog_footnote)

## complication defn ---------------------------------- (2023-06-03 11:05) @----
cardiac_compl <- readxl::read_xlsx("data/complications_defs_2023-05-22.xlsx", range = "A1:G64", sheet = "cardiac") |>
  clean_names() |>
  mutate(cardiac_complications = firstlower(cardiac_complications)) |>
  rename(detail_cardiac = cardiac_complications) |>
  relocate(detail_cardiac, .after = complication) |>
  select(-c(study, kq))

pulmonary_compl <- readxl::read_xlsx("data/complications_defs_2023-05-22.xlsx", range = "A1:D17", sheet = "pulmonary") |>
  clean_names() |>
  mutate(pulmonary_complications = firstlower(pulmonary_complications),
         complication = "pulm") |>
  rename(detail_pulmonary = pulmonary_complications) |>
  select(-c(study, kq))

renal_compl <- readxl::read_xlsx("data/complications_defs_2023-05-22.xlsx", range = "A1:D17", sheet = "renal") |>
  clean_names() |>
  mutate(complication = "kidneyinj") |>
  rename(detail_renal = renal_complications) |>
  select(-c(study, kq))

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
  mutate(
    study_l = paste0("[", study, "]", "(", "evidence_tables.html#", refid, ")"),
    study_id = paste0(study, "-", arm_id)
  ) |>
  select(refid, study, study_l, study_id, year, arm_id, everything()) |>
  select(-c(author, author_dist, title, doi, user)) |>
  filter(!refid %in% exclude_refids) |> # refid 1 only for column types
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
  mutate(across(D1:D7, ~ if_else(.x == "No information", "No Information", .x)))
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

## all_rob_meta_dat rct+nrsi--------------------------- (2023-05-31 11:39) @----
all_rob_meta_dat <- bind_rows(rob2_meta_dat, robinsi_meta_dat |> select(-Study)) |>
  relocate(Overall, .after = D7)

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

## formatted means and medians for tables ------------- (2024-03-06 12:55) @----
source("code/summary_mn_med_2023-02-27.R")

## colors --------------------------------------------- (2024-03-06 12:56) @----
# color palate
# color palate
color_1 <- "#0E6655"
color_2 <- "#A93226"
color_3 <- "#104E8B"
color_4 <- "#F39C12"
color_5 <- "#969696"
gray_mg <- "#969696"

# color_1 <- "#FA8B8B"
# color_2 <- "#76d7c4"
# color_3 <- "#7fb3d5"
# color_4 <- "#c39bd3"


## retracted pubmed trials----------------------------- (2023-04-07 12:13) @----
# retracted_pubmed <- read.csv("data/retracted_trials_2023-04-07.csv")

## verify all distinct arms --------------------------- (2023-04-25 12:51) @----
# should be empty tibble
temp <- study_arm_dat |>
  group_by(refid) |>
  mutate(
    count = n(),
    distinct_count = n_distinct(arm_id)
  ) |>
  select(refid, count, distinct_count) |>
  filter(count != distinct_count) |>
  ungroup() |>
  nrow()

# print warning to console in red to console number of rows is not 0
if (temp != 0) {
  cat(crayon::red("STOP", "DUPLICATE ARMs"))
}

if (temp == 0) {
  cat(crayon::green("NO", "DUPLICATE ARMs"))
}

rm(temp)


## save for use --------------------------------------- (2023-03-13 22:53) @----
limit_colors <- c("#AAB7B8", "#D5DBDB", "#F4F6F6")
save.image(paste0("data/geri_data_", str_replace_all(format(Sys.time()), "\\s|:", "-"), ".Rdata"))

# rm(list = ls())
# rmv_yn <- readline("Remove processed file? y, n: ")
