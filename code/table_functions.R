# table functions
## outcome priority tables ---------------------------- (2023-03-02 11:49) @----
# get ranking data and munge to usable
rankings <- function(key_question){
  # key_question "KQ1" "KQ2" ...

  read_by_kq <- function(kq, range) {
    read_outcome_dat <- function(cells) {
      readxl::read_xlsx("data/OutcomeRankingRawData_2023-01-23.xlsx", range = cells, sheet = "data")
    }
    read_outcome_dat(range) |>
      mutate(kq = kq) |>
      relocate(kq, .before = 1) |>
      mutate(
        across(1:18, as.numeric)
      )
  }

  out_kq1 <- read_by_kq(1, "J2:Z12")
  out_kq2 <- read_by_kq(2, "AA2:AQ12")
  out_kq3 <- read_by_kq(3, "AR2:BH12")
  out_kq4 <- read_by_kq(4, "BI2:BY12")
  out_kq5 <- read_by_kq(5, "BZ2:CP12")
  out_kq6 <- read_by_kq(6, "CQ2:DG12")
  out_kq7 <- read_by_kq(7, "DH2:DX12")
  out_kq8 <- read_by_kq(8, "DY2:EO12")

  priority_dat <- bind_rows(out_kq1, out_kq2, out_kq3, out_kq4, out_kq5, out_kq6, out_kq7, out_kq8) |>
    mutate(kq = factor(kq,
      labels = c(
        "KQ1 Preoperative Evaluation",
        "KQ2 Prehabilitation",
        "KQ3 Regional versus General",
        "KQ4 Intravenous versus Inhaled",
        "KQ5 Potentially Inappropriate Medications",
        "KQ6 Pharmacologic Delirium Prophylaxis",
        "KQ7 Postoperative Regional Anesthesia",
        "KQ8 PACU Delirium Screening"
      )
    )) |>
    relocate(kq, .before = 1)

  # priority_dat |>
  #   group_by(kq) |>
  #   summarise(across(1:17, ~ sum(.x == 2, na.rm = TRUE)), .names = "{.col}") |>
  #   mutate(rank = priority)

  rankings <- function(priority) {
    priority_dat |>
      group_by(kq) |>
      summarise(across(1:17, ~ sum(.x == priority, na.rm = TRUE)), .names = "{.col}") |>
      mutate(rank = priority)
  }

  rankings_dat <- bind_rows(rankings(1), rankings(2), rankings(3), rankings(4), rankings(5)) |>
    arrange(kq, rank)

  temp <- rankings_dat |>
    filter(str_detect(kq, key_question))

  transpose_tibble(temp, col_names = rank, id_col = "outcomes") |>
    filter(!outcomes %in% c("kq", ".names")) |>
    rename_with(.fn = ~ paste0("rank", . )) |>
    rename(outcome = rankoutcomes) |>
    mutate(across(rank1:rank5, as.numeric),
           any_top_5 = rank5 + rank4 + rank3 + rank2 + rank1)
}

# outcome priority table by kq
outcome_tab <- function(outcome_dat, responses) {
  outcome_up_dat <- outcome_dat |>
    filter(!outcome %in% c("Depression", "Intraop awareness"))

  reactable(
    outcome_up_dat,
    pagination = FALSE,
    # highlight = TRUE,
    defaultSorted = "any_top_5",
    defaultSortOrder = "desc",
    defaultColDef = colDef(
      cell = data_bars(outcome_dat,
                       box_shadow = TRUE,
                       # force_outside = c(1, 88),
                       bar_height = 12,
                       max_value = responses
                       # fill_color = "#B22215"
      ),
      style = cell_style(outcome_dat,
                         font_size = "13px",
      )
    ),
    columns = list(
      outcome = colDef(name = "Outcome", width = 235),
      rank1 = colDef(name = "Rank 1", width = 80, headerClass = "header", align = "center"),
      rank2 = colDef(name = "Rank 2", width = 80, headerClass = "header", align = "center"),
      rank3 = colDef(name = "Rank 3", width = 80, headerClass = "header", align = "center"),
      rank4 = colDef(name = "Rank 4", width = 80, headerClass = "header", align = "center"),
      rank5 = colDef(name = "Rank 5", width = 80, headerClass = "header", align = "center"),
      any_top_5 = colDef(
        name = "Any", width = 80, headerClass = "header", align = "center",
        cell = data_bars(
          outcome_dat,
          # force_outside = c(1, 88),
          box_shadow = TRUE,
          bar_height = 12,
          max_value = responses,
          fill_color = "#B22215"
        )
      )
    ),
    theme = reactableTheme(
      borderColor = "#dfe2e5",
      stripedColor = "#f6f8fa",
      highlightColor = "#f0f5f9",
      cellPadding = "0px 0px"
    ),
    compact = TRUE,
    class = "priority-tbl"
  )
}

## outcomes reported tables --------------------------- (2023-03-02 11:50) @----
# footnote designs included for tables
foot_out_freq <- function(data) {
  design_select <- data |>
    select(design_f_abbrev) |>
    mutate(design_select = as.character(fct_collapse(design_f_abbrev))) |>
    distinct() |>
    pull(design_select)

  foot_labels <- tribble(
    ~design_factor, ~abbreviation,
    "RCT", "RCT: randomized clinical trial",
    "Cluster", "Cluster: cluster randomized",
    "Crossover", "Crossover: crossover trial",
    "NR Trial", "NR Trial: non-randomized trial",
    "Quasi-exp", "Quasi-exp: before-after or time series",
    "Prosp Coh", "Prosp Coh: prospective cohort",
    "Retro Coh", "Retro Coh: retrospective cohort",
    "Cross Sect", "Cross Sect: cross-sectional",
    "Case-Cont", "Case-Cont: case-control",
    "Case Series", "Case Series",
    "Other", "Other",
    "Paired", "Paired: fully-paired"
  )

  foot_labels <- foot_labels |>
    filter(design_factor %in% design_select) |>
    pull(abbreviation) |>
    str_flatten(collapse = "; ")

  foot_labels
}

# dichot freq outcomes; data and add to footnote abbreviations for study designs included eg "ADL: activities of daily living; "
dichot_freq_fun <- function(data, add_footnote = NULL) {
  # column_last <- length(fct_unique(fct_drop(data |> pull(design_f_abbrev))))
  # dichot_dat |>
  data |>
    select(refid, design_f_abbrev, d_adl:d_satisfaction) |>
    mutate(across(d_adl:d_satisfaction, ~ !is.na(.x))) |>
    group_by(refid) |>
    mutate(
      across(d_adl:d_satisfaction, ~ sum(.x) != 0)
    ) |>
    ungroup() |>
    rename(
      "Complications" = "d_complication",
      "DNCR/POCD" = "d_cog_delay",
      "Delirium duration" = "d_deli_duration",
      "Discharge location" = "d_disch_location",
      "Opioid use" = "d_opioid",
      "QoR" = "d_qor",
      "ADL" = "d_adl",
      "Readmission" = "d_readmit",
      "Mortality" = "d_mortality",
      "Delirium" = "d_delirium",
      "Depression" = "d_depression",
      "Pain" = "d_pain",
      "Satisfaction" = "d_satisfaction"
    ) |>
    select(-Depression) |>
    group_by(refid) |>
    slice(1) |>
    ungroup() |>
    select(-refid) |>
    arrange(design_f_abbrev) |>
    mutate(
      design_f_abbrev = fct_drop(design_f_abbrev)
    ) |>
    group_by(design_f_abbrev) |>
    tbl_summary(
      by = design_f_abbrev,
      missing = "no"
    ) |>
    modify_header(label = "**Outcome**") |>
    modify_footnote(update = everything() ~ NA) |>
    as_gt(id = "one") |>
    gt_theme_mg() |>
    cols_width(
      label ~ "180px",
      matches("stat_[1-9]") ~ "120px") |>
    tab_options(footnotes.marks = "letters") |>
    tab_footnote(md(paste0(add_footnote, foot_out_freq(data), "."))) |>
    sub_values(values = c("0 (0%)"), replacement = "—")
  # opt_footnote_marks(marks = "standard")
}

# contin freq outcomes; data and add to footnote abbreviations for study designs included eg "ADL: activities of daily living; "
contin_freq_fun <- function(data, add_footnote = NULL) {
  # column_last <- length(fct_unique(fct_drop(data |> pull(design_f_abbrev))))
  data |>
    select(refid, design_f_abbrev, c_6mwd:c_pulmonary) |>
    mutate(across(c_6mwd:c_pulmonary, ~ !is.na(.x))) |>
    group_by(refid) |>
    mutate(
      across(c_6mwd:c_pulmonary, ~ sum(.x) != 0)
    ) |>
    ungroup() |>
    select(-c_6mwd, -c_handgrip, -c_pulmonary) |>
    rename(
      # "6-minute walk" = "c_6mwd",
      "Delirium duration" = "c_delirium_dur",
      # "Grip strength" = "c_handgrip",
      "Length of stay" = "c_los",
      "Opioid use" = "c_opioid",
      # "Pulmonary function" = "c_pulmonary"
    ) |>
    group_by(refid) |>
    slice(1) |>
    ungroup() |>
    select(-refid) |>
    arrange(design_f_abbrev) |>
    mutate(
      design_f_abbrev = fct_drop(design_f_abbrev)
    ) |>
    group_by(design_f_abbrev) |>
    tbl_summary(
      by = design_f_abbrev,
      missing = "no"
    ) |>
    modify_header(label = "**Outcome**") |>
    modify_footnote(update = everything() ~ NA) |>
    as_gt(id = "one") |>
    gt_theme_mg() |>
    cols_width(
      label ~ "180px",
      matches("stat_[1-9]") ~ "120px") |>
    tab_options(footnotes.marks = "letters") |>
    tab_footnote(paste0(add_footnote, foot_out_freq(data), ".")) |>
    sub_values(values = c("0 (0%)"), replacement = "—")
  # opt_footnote_marks(marks = "standard")
}

# likert freq outcomes; data and add to footnote abbreviations for study designs included eg "ADL: activities of daily living; "
likert_freq_fun <- function(data, add_footnote = NULL) {
  # column_last <- length(fct_unique(fct_drop(data |> pull(design_f_abbrev))))
  data |>
    select(refid, design_f_abbrev, l_adl:l_sat) |>
    mutate(across(l_adl:l_sat, ~ !is.na(.x))) |>
    group_by(refid) |>
    mutate(
      across(l_adl:l_sat, ~ sum(.x) != 0)
    ) |>
    ungroup() |>
    select(-l_depression) |>
    rename(
      "ADL" = "l_adl",
      "DNCR/POCD" = "l_cogfunc",
      "Delirium"	= "l_delirium",
      "Complications" = "l_complications",
      # "Depression/anxiety" = "l_depression",
      "Pain" = "l_pain",
      "Quality of life" = "l_qol",
      "QoR" = "l_qor",
      "Satisfaction" = "l_sat",
    ) |>
    group_by(refid) |>
    slice(1) |>
    ungroup() |>
    select(-refid) |>
    arrange(design_f_abbrev) |>
    mutate(
      design_f_abbrev = fct_drop(design_f_abbrev)
    ) |>
    group_by(design_f_abbrev) |>
    tbl_summary(
      by = design_f_abbrev,
      missing = "no"
    ) |>
    modify_header(label = "**Outcome**") |>
    modify_footnote(update = everything() ~ NA) |>
    as_gt(id = "one") |>
    gt_theme_mg() |>
    cols_width(
      label ~ "180px",
      matches("stat_[1-9]") ~ "120px") |>
    tab_options(footnotes.marks = "letters") |>
    tab_footnote(paste0(add_footnote, foot_out_freq(data), ".")) |>
    sub_values(values = c("0 (0%)"), replacement = "—")
  # opt_footnote_marks(marks = "standard")
}

## delirium_total_tab_fun(refids); -------------------- (2023-03-02 11:51) @----
# create table data
# delirium_total_tab <- delirium_total_tab_fun(refids)
# delirium_total_tab <- delirium_total_tab_fun(ket_refid)

delirium_total_tab_fun <- function(refids){
  # referent values for calculating rr and ci; rr_ci
  delirium_rr_ref <- dichot_dat |>
    select(refid, refid_c, arm_id, delitotal_n, arm_n) |>
    filter(!is.na(delitotal_n)) |>
    filter(refid %in% refids) |>
    arrange(refid_c, arm_id) |>
    rename(ref_deli_n = delitotal_n, ref_arm_n = arm_n) |>
    group_by(refid_c) |>
    mutate(
      ref_arm_n = ifelse(row_number() > 1, NA, ref_arm_n),
      ref_deli_n = ifelse(row_number() > 1, NA, ref_deli_n)
    ) |>
    fill(ref_arm_n, ref_deli_n) |>
    mutate(
      ref_arm_n = ifelse(row_number() == 1, NA, ref_arm_n),
      ref_deli_n = ifelse(row_number() == 1, NA, ref_deli_n)
    ) |>
    select(-refid)

  dichot_dat |>
    select(refid, refid_c, year, arm_id, design_f_lab, study, study_l, arm_n, delitotal_time:delitotal_95high, deli_scale_cam:deli_scale_otherspec) |>
    filter(!is.na(delitotal_n)) |>
    filter(refid %in% refids) |>
    remove_empty(which = "cols") |>
    left_join(drugs_dat |> select(refid, arm_id, drug_recode_abbr), by = c("refid", "arm_id")) |>
    relocate(drug_recode_abbr, .after = arm_n) |>
    mutate(
      across(starts_with("deli_"), ~ str_remove_all(.x, "scale_")),
      across(starts_with("deli_"), ~ str_remove_all(.x, "deli_")),
      across(deli_scale_cam:deli_scale_icdsc, ~ toupper(.x)),
    ) |>
    rename_with(~ gsub("scale_", "", .x, fixed = TRUE)) |>
    rename_with(~ gsub("deli_", "", .x, fixed = TRUE)) |>
    left_join(delirium_rr_ref, by = c("refid_c", "arm_id")) |>
    mutate(
      other = ifelse(str_detect(otherspec, "AMT"), "AMT", other),
      other = ifelse(str_detect(otherspec, "Psychiatrist consultation"), "Psych", other),
      other = ifelse(str_detect(otherspec, "Chinese"), "Note", other),
      other = ifelse(str_detect(otherspec, "clinical observation of trained nurse"), "Clinical", other),
      other = ifelse(str_detect(otherspec, "Clinical symptoms"), "Clinical", other),
      other = ifelse(str_detect(otherspec, "Clinical"), "Clinical", other), # Clinical symptoms Cheng 2016
      other = ifelse(str_detect(otherspec, "Disturbance of consciousness assessment"), "DOC", other), #	Disturbance of consciousness assessment Hu 2022
      other = ifelse(str_detect(otherspec, "Nu-DESC"), "Nu-DESC", other),
      other = ifelse(str_detect(otherspec, "Nursing delirium screening scale"), "Nu-DESC", other),
      other = ifelse(str_detect(otherspec, "ICD"), "ICD", other),
      other = ifelse(str_detect(study, "Memtsoudis"), "Other", other),
      other = ifelse(study == "Liu 2023b", "Clinical", other), # chart review: psychiatrist diagnosis; use of antipsychotics; >=2 anesthesiologists agreed with trained nurse screening results
      other = ifelse(other == "other", NA_character_ , other),
      calc_percent = delitotal_n/arm_n * 100,
      n_percent = n_per_fun(delitotal_n, arm_n, 1),
      rr_ci = ifelse(!is.na(ref_arm_n), rr_ci_fun(delitotal_n, arm_n, ref_deli_n, ref_arm_n), "—")
    ) |>
    relocate(calc_percent, .after = delitotal_perc) |>
    unite(scale_delirium, cam:other, remove = TRUE, sep = "/", na.rm = TRUE) |>
    mutate(scale_delirium = ifelse(scale_delirium == "unspecified", "NS", scale_delirium)) |>
    select(year, refid, refid_c, design_f_lab, study, study_l, arm_id, arm_n, drug_recode_abbr, scale_delirium, delitotal_time, n_percent, calc_percent, rr_ci) |>
    arrange(year, study, refid_c, arm_id) |>
    left_join(table_mn_med |> select(refid, arm_id, pre_mmse), by = c("refid", "arm_id")) |>
    left_join(table_age_mn_med, by = c("refid", "arm_id")) |>
    relocate(pre_mmse, .before = scale_delirium) |>
    relocate(age_table, .after = arm_n)
}

delirium_total_gt_fun <- function(drug_f_abbr){
  delirium_total_tab |>
    arrange(year, study, arm_id) |>
    group_by(study_l) |>
    mutate(
      delitotal_time = ifelse(delitotal_time == 999, "Stay", as.character(delitotal_time)),
      study_l = ifelse(row_number() > 1, "", study_l),
      delitotal_time = ifelse(row_number() > 1, "", delitotal_time),
      scale_delirium = ifelse(row_number() > 1, "", scale_delirium),
      bar = case_when(
        str_detect(drug_recode_abbr, drug_f_abbr) ~ bar_prop(calc_percent, "#A93226"),
        str_detect(drug_recode_abbr, "Prop|Mid|Preg|Dex/Preg") ~ bar_prop(calc_percent, "#104E8B"),
        .default = bar_prop(calc_percent, "#969696")
      ),
    ) |>
    ungroup() |>
    rename(drug_recode = drug_recode_abbr) |>
    group_by(compare_groups) |>
    gt(id = "one") |>
    cols_hide(c(year, refid, refid_c, arm_id, study, design_f_lab, pre_mmse, age_table, calc_percent)) |>
    cols_label(
      study_l = "Study",
      arm_n = " N",
      # age_table = "  Age",
      drug_recode = md("Arm"),
      # pre_mmse = md("  MMSE<br/>  (preop)"),
      scale_delirium = "Scale",
      delitotal_time = "Day(s)",
      n_percent = "N (%)",
      bar = "0 – 100%",
      rr_ci = "RR (95% CI)"
    ) |>
    fmt_markdown(columns = c(study_l, bar)) |>
    tab_spanner(label = "Incidence Proportion", columns = c(n_percent, bar)) |>
    tab_style(style = cell_text(align = "left"),   locations = cells_column_labels(columns = c(study, drug_recode, scale_delirium))) |>
    tab_style(style = cell_text(align = "center"), locations = cells_column_labels(columns = c(arm_n, delitotal_time, rr_ci))) |>
    tab_style(style = cell_text(align = "left"),   locations = cells_body(columns = c(drug_recode, scale_delirium, bar))) |>
    tab_style(style = cell_text(align = "center"), locations = cells_body(columns = c(delitotal_time, rr_ci))) |>
    tab_style(style = cell_text(align = "right"),  locations = cells_column_labels(columns = c(n_percent))) |>
    tab_style(style = cell_text(align = "right"),  locations = cells_body(columns = c(n_percent))) |>
    tab_style(style = list(cell_text(color = "#A93226")), locations = cells_body(columns = c(arm_n:n_percent), rows = str_detect(drug_recode, drug_f_abbr))) |>
    gt_theme_mg() |>
    cols_width(
      study_l ~ px(165),
      arm_n ~ px(80),
      # age_table ~ px(100),
      drug_recode ~ px(70),
      # pre_mmse ~ px(95),
      scale_delirium ~ px(105),
      delitotal_time ~ px(55),
      n_percent ~ px(90),
      bar ~ px(100),
      rr_ci ~ px(140)
    )
}

## summary rob function ------------------------------- (2023-03-23 11:52) @----
rob_summary_fun <- function(rob_refids) {
  rob_temp_dat <- rob2_dat |>
    filter(refid %in% {{ rob_refids }}) |>
    select(Study, D1:Overall)
    # select(-refid)
  rob_summary(rob_temp_dat, tool = "ROB2", colour = "colourblind", weighted = FALSE)
}

# weighted requires refid in meta object
rob_summary_meta_weighted_fun <- function(meta) {
  add_weights <- tibble(meta$data$refid, meta$w.random / sum(meta$w.random)) |>
    set_names("refid", "weight")
  rob_temp_dat <- rob2_dat |>
    filter(refid %in% meta$data$refid) |>
    select(refid, Study, D1:Overall) |>
    left_join(add_weights, by = "refid") |>
    rename(Weight = weight) |>
    select(-refid)
  rob_summary(rob_temp_dat, tool = "ROB2", colour = "colourblind", weighted = TRUE)
}

rob_summary_meta_weighted_nrsi_fun <- function(meta) {
  add_weights <- tibble(meta$data$refid, meta$w.random / sum(meta$w.random)) |>
    set_names("refid", "weight")
  rob_temp_dat <- robinsi_dat |>
    filter(refid %in% meta$data$refid) |>
    select(refid, Study, D1:Overall) |>
    left_join(add_weights, by = "refid") |>
    rename(Weight = weight) |>
    select(-refid)
  rob_summary(rob_temp_dat, tool = "ROBINS-I", colour = "colourblind", weighted = TRUE)
}

# to use with meta object and subset
rob_summary_meta_weighted_subset_fun <- function(meta) {
  add_weights <- tibble(as.numeric(meta$data$refid[meta$subset]), meta$w.random / sum(meta$w.random)) |>
    set_names("refid", "weight")
  rob_temp_dat <- rob2_dat |>
    filter(refid %in% meta$data$refid[meta$subset]) |>
    select(refid, Study, D1:Overall) |>
    left_join(add_weights, by = "refid") |>
    rename(Weight = weight) |>
    select(-refid)
  rob_summary(rob_temp_dat, tool = "ROB2", colour = "colourblind", weighted = TRUE)
}

robinsi_summary_fun <- function(robinsi_refids) {
  rob_temp_dat <- robinsi_dat |>
    filter(refid %in% {{ robinsi_refids }}) |>
    select(-refid)
  rob_summary(rob_temp_dat, tool = "ROBINS-I", weighted = FALSE)
}

# rob_summary(
#   data = robinsi_dat |> select(-refid),
#   tool = "ROBINS-I",
#   colour = "colourblind"
# )
#
# rob_traffic_light(robinsi_dat |> select(-refid), psize = 4, tool = "ROBINS-I", colour = "colourblind")

## balance table functions ---------------------------- (2023-12-19 11:15) @----

riskdiff_color <- "#888888"

kq1_balance_main <- function(inc_exclude = "exclude") {
  expanded_dat <- readxl::read_excel("data/balance_tables_2023-09-14_mac_mg.xlsx", sheet = "ExpandedPreop", range = "A4:M26") |>
    remove_empty(which = "cols") |>
    clean_names() |>
    filter(exclude %notin% inc_exclude) |> # remove NRSI or risk diff
    rename(est = estimate_95_percent_ci) |>
    filter(!if_all(rct:est, ~ is.na(.x))) |>
    mutate(
      high     = paste0("[", high, "]",     "(soe_gt.html#expanded-preoperative-evaluation)"),
      mod      = paste0("[", mod, "]",      "(soe_gt.html#expanded-preoperative-evaluation)"),
      low      = paste0("[", low, "]",      "(soe_gt.html#expanded-preoperative-evaluation)"),
      vlow     = paste0("[", vlow, "]",     "(soe_gt.html#expanded-preoperative-evaluation)"),
      low_very = paste0("[", low_very, "]", "(soe_gt.html#expanded-preoperative-evaluation)"),
      # group = ifelse(outcome == "Patient satisfaction", "Patient-reported", "Clinical"),
      across(everything(), ~ str_remove(.x, "‡|†|\\*")),
      grade = case_when(
        grade == "Very low" ~ vlow,
        grade == "Low" ~ low,
        grade == "Moderate" ~ mod,
        grade == "High" ~ high,
        grade == "Low/very low" ~ low_very,
        .default = grade
      ),
      event_e = ifelse(!is.na(n_exp), paste0(formattable::comma(events_exp), " (", formattable::comma(n_exp), ")"), NA),
      event_c = ifelse(!is.na(n_std), paste0(formattable::comma(events_std), " (", formattable::comma(n_std), ")"), NA),
      # event_e = ifelse(!is.na(n_exp), paste0(events_exp, " (", n_exp, ")"), NA),
      # event_c = ifelse(!is.na(n_std), paste0(events_std, " (", n_std, ")"), NA),
      across(c(event_e, event_c), ~ str_remove(.x, "NA "))
    ) |>
    relocate(c(event_e, event_c), .after = nrsi) |>
    select(-c(events_exp:n_std, exclude, grade_2, high, mod, low, vlow, low_very))

  expanded_dat |>
    gt(id = "one") |>
    cols_label(
      outcome  = "Outcome",
      rct      = "RCT",
      nrsi     = "NRSI",
      event_e  = "N (Total)",
      event_c  = "N (Total)",
      # n        = "    N",
      grade    = "GRADE",
      measure  = "Effect",
      est      = "Estimate (95% CI)"
    ) |>
    fmt_markdown(columns = c(grade)) |>
    cols_hide(n) |>
    fmt_integer(use_seps = TRUE, sep_mark = ",") |>
    gt_theme_mg() |>
    cols_width(
      outcome ~ px(240),
      rct ~ px(40),
      nrsi ~ px(45),
      # n       ~ px(65),
      event_e ~ px(110),
      event_c ~ px(110),
      grade ~ px(100),
      measure ~ px(70),
      est ~ px(140)
    ) |>
    sub_missing(columns = everything(), missing_text = "") |>
    tab_spanner(label = "Preoperative Evaluation", columns = c(event_e, event_c), level = 2) |>
    tab_spanner(label = "Expanded", columns = c(event_e), level = 1) |>
    tab_spanner(label = "Standard", columns = c(event_c), level = 1) |>
    opt_footnote_marks(marks = "standard") |>
    tab_style(style = cell_text(align = "center"), locations = cells_column_labels(columns = c(event_c, event_e))) |>
    tab_style(style = cell_text(align = "center"), locations = cells_column_labels(columns = c(n, grade, rct, nrsi))) |>
    tab_style(style = cell_text(align = "left"), locations = cells_body(columns = c(est, measure))) |>
    tab_style(style = cell_text(align = "center"), locations = cells_body(columns = c(grade, rct, nrsi))) |>
    tab_style(style = cell_text(size = px(12)), locations = cells_body(columns = c(measure), rows = measure == "RD/1000")) |>
    tab_style(style = list(cell_text(color = riskdiff_color)), locations = cells_body(columns = c(rct:est), rows = str_detect(measure, "RD"))) |>
    tab_style(style = list(cell_text(color = "black")), locations = cells_body(columns = c(rct:est), rows = str_detect(measure, "RD") & str_detect(outcome, "satis"))) |>
    tab_footnote(md("RCT: randomized clinical trial; NRSI: nonrandomized studies of interventions (non-randomized trial, before-after and cohort studies);  GRADE: Grades of Recommendation, Assessment, Development, and Evaluation; RR: risk ratio; SMD: standardized mean difference; RD: risk difference; MD: mean difference.")) |>
    tab_footnote(md(grade_foot), locations = cells_column_labels(columns = grade)) |>
    tab_footnote("Cardiovascular, pulmonary, and renal.", locations = cells_body(columns = c(outcome), rows = outcome == "Complications")) |>
    tab_footnote("High versus lower satisfaction.", locations = cells_body(columns = c(est), rows = outcome == "Patient satisfaction"))
}

kq1_complications <- function() {
  expanded_dat <- readxl::read_excel("data/balance_tables_2023-09-14_mac_mg.xlsx", sheet = "ExpandedPreop", range = "B36:M47") |>
    remove_empty(which = "cols") |>
    clean_names() |>
    rename(est = estimate_95_percent_ci) |>
    filter(!if_all(rct:est, ~ is.na(.x))) |>
    mutate(
      high     = paste0("[", high, "]",     "(soe_gt.html#exp-std-grade)"),
      mod      = paste0("[", mod, "]",      "(soe_gt.html#exp-std-grade)"),
      low      = paste0("[", low, "]",      "(soe_gt.html#exp-std-grade)"),
      vlow     = paste0("[", vlow, "]",     "(soe_gt.html#exp-std-grade)"),
      low_very = paste0("[", low_very, "]", "(soe_gt.html#exp-std-grade)"),
      # group = ifelse(outcome == "Patient satisfaction", "Patient-reported", "Clinical"),
      across(everything(), ~ str_remove(.x, "‡|†|\\*")),
      grade = case_when(
        grade == "Very low" ~ vlow,
        grade == "Low" ~ low,
        grade == "Moderate" ~ mod,
        grade == "High" ~ high,
        grade == "Low/very low" ~ low_very,
        .default = grade
      ),
      event_e = ifelse(!is.na(n_exp), paste0(formattable::comma(events_exp), " (", formattable::comma(n_exp), ")"), NA),
      event_c = ifelse(!is.na(n_std), paste0(formattable::comma(events_std), " (", formattable::comma(n_std), ")"), NA),
      # event_e = ifelse(!is.na(n_exp), paste0(events_exp, " (", n_exp, ")"), NA),
      # event_c = ifelse(!is.na(n_std), paste0(events_std, " (", n_std, ")"), NA),
      across(c(event_e, event_c), ~ str_remove(.x, "NA "))
    ) |>
    relocate(c(event_e, event_c), .after = nrsi) |>
    select(-c(events_exp:n_std, grade_2, high, mod, low, vlow, low_very))

  expanded_dat |>
    gt(id = "one") |>
    cols_label(
      outcome  = "Outcome",
      rct      = "RCT",
      nrsi     = "NRSI",
      event_e  = "N (Total)",
      event_c  = "N (Total)",
      # n        = "    N",
      grade    = "GRADE",
      measure  = "Effect",
      est      = "Estimate (95% CI)"
    ) |>
    fmt_markdown(columns = c(grade)) |>
    cols_hide(n) |>
    fmt_integer(use_seps = TRUE, sep_mark = ",") |>
    gt_theme_mg() |>
    cols_width(
      outcome ~ px(240),
      rct ~ px(40),
      nrsi ~ px(45),
      # n       ~ px(65),
      event_e ~ px(110),
      event_c ~ px(110),
      grade ~ px(100),
      measure ~ px(70),
      est ~ px(140)
    ) |>
    sub_missing(columns = everything(), missing_text = "") |>
    tab_spanner(label = "Preoperative Evaluation", columns = c(event_e, event_c), level = 2) |>
    tab_spanner(label = "Expanded", columns = c(event_e), level = 1) |>
    tab_spanner(label = "Standard", columns = c(event_c), level = 1) |>
    opt_footnote_marks(marks = "standard") |>
    tab_style(style = cell_text(align = "center"), locations = cells_column_labels(columns = c(event_c, event_e))) |>
    tab_style(style = cell_text(align = "center"), locations = cells_column_labels(columns = c(n, grade, rct, nrsi))) |>
    tab_style(style = cell_text(align = "left"), locations = cells_column_labels(columns = c(est, measure))) |>
    tab_style(style = cell_text(align = "center"), locations = cells_body(columns = c(grade, rct, nrsi))) |>
    tab_style(style = cell_text(align = "left"), locations = cells_body(columns = c(est, measure))) |>
    tab_style(style = cell_text(size = px(12)), locations = cells_body(columns = c(measure), rows = measure == "RD/1000")) |>
    tab_style(style = list(cell_text(color = riskdiff_color)), locations = cells_body(columns = c(rct:est), rows = str_detect(measure, "RD"))) |>
    tab_footnote(md("RCT: randomized clinical trial; GRADE: Grades of Recommendation, Assessment, Development, and Evaluation; RR: risk ratio; RD: risk difference.")) |>
    tab_footnote(md(grade_foot), locations = cells_column_labels(columns = grade)) |>
    tab_footnote(md("Comparing higher/highest category or categories with lower ones."), locations = cells_body(columns = c(est), rows = outcome == "Patient satisfaction"), placement = "right") |>
    tab_footnote("Cardiovascular, pulmonary, and acute kidney injury.", locations = cells_body(columns = c(outcome), rows = outcome == "Complications")) |>
    tab_footnote("Complications reported variously across the 13 studies. ", locations = cells_body(columns = c(rct), rows = outcome == "Complications"), placement = "right") |>
    tab_footnote("One study no events; the other two. ", locations = cells_body(columns = c(est), rows = outcome == "cardiac arrest"), placement = "right") |>
    tab_footnote("Unspecified in 2 studies and COPD exacerabation or pneumonia in the other.", locations = cells_body(columns = c(outcome), rows = outcome == "Pulmonary complications"), placement = "right")
}

kq3_balance_main <- function(exclude = "RD") {
  reg_gen_dat <- readxl::read_excel("data/balance_tables_2023-09-14_mac_mg.xlsx", sheet = "RegionalGeneral", range = "B4:L21") |>
  remove_empty(which = "cols") |>
  clean_names() |>
  # filter(!is.na(outcome)) |>
  filter(if(exclude == "RD/1000") !is.na(outcome) else TRUE) |>
  # filter(measure != no_riskdiff) |>
  rename(est = estimate_95_percent_ci) |>
  # filter(!if_all(rct:est, ~ is.na(.x))) |>
    mutate(
      high     = paste0("[", high, "]",     "(soe_gt.html#neuraxial-versus-general-anesthesia)"),
      mod      = paste0("[", mod, "]",      "(soe_gt.html#neuraxial-versus-general-anesthesia)"),
      low      = paste0("[", low, "]",      "(soe_gt.html#neuraxial-versus-general-anesthesia)"),
      vlow     = paste0("[", vlow, "]",     "(soe_gt.html#neuraxial-versus-general-anesthesia)"),
      low_very = paste0("[", low_very, "]", "(soe_gt.html#neuraxial-versus-general-anesthesia)"),
      # group = ifelse(outcome == "Patient satisfaction", "Patient-reported", "Clinical"),
      across(everything(), ~ str_remove(.x, "‡|†|\\*")),
      grade = case_when(
        grade == "Very low" ~ vlow,
        grade == "Low" ~ low,
        grade == "Moderate" ~ mod,
        grade == "High" ~ high,
        grade == "Low/very low" ~ low_very,
        .default = grade
      ),
    event_e = ifelse(!is.na(n_reg), paste0(formattable::comma(events_reg), " (", formattable::comma(n_reg), ")"), NA),
    event_c = ifelse(!is.na(n_gen), paste0(formattable::comma(events_gen), " (", formattable::comma(n_gen), ")"), NA),
    # event_e = ifelse(!is.na(n_reg), paste0(events_reg, " (", n_reg, ")"), NA),
    # event_c = ifelse(!is.na(n_gen), paste0(events_gen, " (", n_gen, ")"), NA),
    across(c(event_e, event_c), ~ str_remove(.x, "NA "))
  ) |>
  relocate(c(event_e, event_c), .after = rct) |>
  select(-c(events_reg:n_gen, grade_2, high, mod, low, vlow, low_very))

reg_gen_dat |>
  gt(id = "one") |>
  cols_label(
    outcome  = "Outcome",
    rct      = "RCT",
    event_e  = "N (Total)",
    event_c  = "N (Total)",
    # n        = "    N",
    grade    = "GRADE",
    measure  = "Effect",
    est      = "Estimate (95% CI)"
  ) |>
  fmt_markdown(columns = c(grade)) |>
  cols_hide(n) |>
  fmt_integer(use_seps = TRUE, sep_mark = ",") |>
  gt_theme_mg() |>
  cols_width(
    outcome ~ px(240),
    rct     ~ px(40),
    # n       ~ px(65),
    event_e ~ px(100),
    event_c ~ px(100),
    grade   ~ px(120),
    measure ~ px(70),
    est     ~ px(160)
  ) |>
  sub_missing(columns = everything(), missing_text = "") |>
  text_replace("NR", "—", locations = cells_body(grade)) |>
  tab_spanner(label = "Neuraxial", columns = c(event_e), level = 1) |>
  tab_spanner(label = "General", columns = c(event_c), level = 1) |>
  opt_footnote_marks(marks = "standard") |>
  tab_style(style = cell_text(align = "center"),      locations = cells_column_labels(columns = c(event_c, event_e, grade))) |>
  tab_style(style = cell_text(align = "right"),       locations = cells_column_labels(columns = c(rct))) |>
  tab_style(style = cell_text(align = "right"),       locations = cells_body(columns = c(rct, event_e, event_c))) |>
  tab_style(style = cell_text(align = "left"),        locations = cells_body(columns = c(measure, est))) |>
  tab_style(style = cell_text(align = "center"),      locations = cells_body(columns = c(grade))) |>
  tab_style(style = cell_text(indent = px(10)),       locations = cells_body(columns = c(outcome), rows = outcome %in% c("All procedures", "Hip fracture", "Other"))) |>
  tab_style(style = list(cell_text(color = riskdiff_color)), locations = cells_body(columns = c(est, measure), rows = str_detect(measure, "RD"))) |>
  # tab_footnote(md("RCT: randomized clinical trial; [GRADE: Grades of Recommendation, Assessment, Development, and Evaluation](soe_gt.html#grade); RR: risk ratio; SMD: standardized mean difference; MD: mean difference; RD/100: risk difference per 100; RD/1000: risk difference per 1000; NR: not rated.")) |>
  tab_footnote(md(grade_foot), locations = cells_column_labels(columns = grade)) |>
  tab_footnote("Studies reported 0 and 2 events.", locations = cells_body(columns = c(est), rows = outcome == "Cardiac arrest"), placement = "right") |>
  # tab_footnote(md("[Comparing higher/highest category or categories with lower ones.](kq3.html#patient-satisfaction)"), locations = cells_body(columns = c(est), rows = outcome == "Patient satisfaction"), placement = "right") |>
  tab_footnote(md("Comparing higher/highest category or categories with lower ones."), locations = cells_body(columns = c(est), rows = outcome == "Patient satisfaction"), placement = "right") |>
  tab_footnote("Complications reported variously across the 13 trials. ", locations = cells_body(columns = c(outcome), rows = outcome == "Complications"), placement = "right") |>
  tab_footnote("Hip fracture trials (n = 6) RR 1.08 (95% CI, 0.87–1.35); other procedures (n = 4) RR 0.74 (95% CI, 0.35–1.57).", locations = cells_body(columns = c(est), rows = outcome == "Delirium"), placement = "right") |>
  tab_footnote("RD per 100 for all surgeries 0.1 (-1.8 to 2.2); in hip fracture trials 1.3 (-1.3 to 3.9); others -2.2 (-9.2 to 4.8).",  locations = cells_body(columns = c(est), rows = outcome == "Delirium"), placement = "right") |>
  tab_footnote("Common effects model (2 trials).", locations = cells_body(columns = c(est), rows = outcome == "Other"), placement = "right") |>
  tab_footnote("Using Neuman 2021 primary result of inability to walk 60 feet without human assistance in a sensitivity analysis including 1644 patients yield a pooled SMD -0.07 (95% CI, -0.25 to 0.12).", locations = cells_body(columns = c(est), rows = outcome == "Physical function"), placement = "right")
}

kq3_complications <- function() {
  reg_gen_dat <- readxl::read_excel("data/balance_tables_2023-09-14_mac_mg.xlsx", sheet = "RegionalGeneral", range = "B33:L52") |>
    remove_empty(which = "cols") |>
    clean_names() |>
    rename(est = estimate_95_percent_ci) |>
    filter(!if_all(rct:est, ~ is.na(.x))) |>
    mutate(
      high     = paste0("[", high, "]",     "(soe_gt.html#reg-gen-grade)"),
      mod      = paste0("[", mod, "]",      "(soe_gt.html#reg-gen-grade)"),
      low      = paste0("[", low, "]",      "(soe_gt.html#reg-gen-grade)"),
      vlow     = paste0("[", vlow, "]",     "(soe_gt.html#reg-gen-grade)"),
      low_very = paste0("[", low_very, "]", "(soe_gt.html#reg-gen-grade)"),
      # group = ifelse(outcome == "Patient satisfaction", "Patient-reported", "Clinical"),
      across(everything(), ~ str_remove(.x, "‡|†|\\*")),
      grade = case_when(
        grade == "Very low" ~ vlow,
        grade == "Low" ~ low,
        grade == "Moderate" ~ mod,
        grade == "High" ~ high,
        grade == "Low/very low" ~ low_very,
        .default = grade
      ),
      event_e = ifelse(!is.na(n_reg), paste0(formattable::comma(events_reg), " (", formattable::comma(n_reg), ")"), NA),
      event_c = ifelse(!is.na(n_gen), paste0(formattable::comma(events_gen), " (", formattable::comma(n_gen), ")"), NA),
      # event_e = ifelse(!is.na(n_reg), paste0(events_reg, " (", n_reg, ")"), NA),
      # event_c = ifelse(!is.na(n_gen), paste0(events_gen, " (", n_gen, ")"), NA),
      across(c(event_e, event_c), ~ str_remove(.x, "NA "))
    ) |>
    relocate(c(event_e, event_c), .after = rct) |>
    select(-c(events_reg:n_gen, grade_2, high, mod, low, vlow, low_very))

  reg_gen_dat |>
    gt(id = "one") |>
    cols_label(
      outcome  = "Outcome",
      rct      = "RCT",
      event_e  = "N (Total)",
      event_c  = "N (Total)",
      # n        = "    N",
      grade    = "GRADE",
      measure  = "Effect",
      est      = "Estimate (95% CI)"
    ) |>
    fmt_markdown(columns = c(grade, est)) |>
    cols_hide(n) |>
    fmt_integer(use_seps = TRUE, sep_mark = ",") |>
    gt_theme_mg() |>
    cols_width(
      outcome ~ px(240),
      rct ~ px(40),
      # n       ~ px(65),
      event_e ~ px(100),
      event_c ~ px(100),
      grade ~ px(120),
      measure ~ px(70),
      est ~ px(160)
    ) |>
    sub_missing(columns = everything(), missing_text = "") |>
    tab_spanner(label = "Neuraxial", columns = c(event_e), level = 1) |>
    tab_spanner(label = "General", columns = c(event_c), level = 1) |>
    opt_footnote_marks(marks = "standard") |>
    tab_style(style = cell_text(align = "center"), locations = cells_column_labels(columns = c(event_c, event_e, grade))) |>
    tab_style(style = cell_text(align = "right"), locations = cells_column_labels(columns = c(rct))) |>
    tab_style(style = cell_text(align = "right"), locations = cells_body(columns = c(rct, event_e, event_c))) |>
    tab_style(style = cell_text(align = "left"), locations = cells_body(columns = c(measure, est))) |>
    tab_style(style = cell_text(align = "center"), locations = cells_body(columns = c(grade))) |>
    tab_style(style = list(cell_text(color = riskdiff_color)), locations = cells_body(columns = c(est, measure), rows = str_detect(measure, "RD"))) |>
    # tab_footnote(md("RCT: randomized clinical trial; [GRADE: Grades of Recommendation, Assessment, Development, and Evaluation](soe_gt.html#grade); RR: risk ratio; RD/1000: risk difference per 1000.")) |>
    tab_footnote(md("RCT: randomized clinical trial; GRADE: Grades of Recommendation, Assessment, Development, and Evaluation; AKI: acute kidney injury; RR: risk ratio; RD/1000: risk difference per 1000.")) |>
    tab_footnote("Fixed/common effects models when 2 studies.", locations = cells_column_labels(columns = measure)) |>
    tab_footnote(md(grade_foot), locations = cells_column_labels(columns = grade)) |>
    # tab_footnote("Complications as reported included bradycardia (4 studies); unspecified (2 studies); tachycardia (1 study); myocardial infarction, heart failure, or new onset arrhythmia (1 study).", locations = cells_body(columns = c(outcome), rows = outcome == "Cardiac complications")) |>
    # tab_footnote("Studies reported 0 and 2 events.", locations = cells_body(columns = c(est), rows = outcome == "Cardiac arrest"), placement = "right") |>
    tab_footnote(md("[Comparing higher/highest category or categories compared with lower ones.](kq3.html#patient-satisfaction)"), locations = cells_body(columns = c(est), rows = outcome == "Patient satisfaction"), placement = "right") |>
    # tab_footnote(md("Comparing higher/highest category or categories compared with lower ones."), locations = cells_body(columns = c(est), rows = outcome == "Patient satisfaction"), placement = "right") |>
    tab_footnote("Cardiovascular, pulmonary, and acute kidney injury.", locations = cells_body(columns = c(outcome), rows = outcome == "Complications")) |>
    tab_footnote("Complications reported variously across the 13 trials. ", locations = cells_body(columns = c(rct), rows = outcome == "Complications"), placement = "right") |>
    tab_footnote("One study no events; the other two. ", locations = cells_body(columns = c(est), rows = outcome == "cardiac arrest"), placement = "right") |>
    tab_footnote("Complications reported included pneumonia, respiratory failure, or unspecified.", locations = cells_body(columns = c(outcome), rows = outcome == "Pulmonary complications"))
}

kq4_balance_main <- function(inc_exclude = "exclude") {
  tiva_inhaled_dat <- readxl::read_excel("data/balance_tables_2023-09-14_mac_mg.xlsx", sheet = "TIVAInhaled", range = "A4:M22") |>
    remove_empty(which = "cols") |>
    clean_names() |>
    filter(exclude %notin% inc_exclude) |> # remove NRSI only; keep for complications
    rename(est = estimate_95_percent_ci) |>
    filter(!if_all(rct:est, ~ is.na(.x))) |>
    mutate(
      high     = paste0("[", high, "]",     "(soe_gt.html#tiva-versus-inhaled-volatile)"),
      mod      = paste0("[", mod, "]",      "(soe_gt.html#tiva-versus-inhaled-volatile)"),
      low      = paste0("[", low, "]",      "(soe_gt.html#tiva-versus-inhaled-volatile)"),
      vlow     = paste0("[", vlow, "]",     "(soe_gt.html#tiva-versus-inhaled-volatile)"),
      low_very = paste0("[", low_very, "]", "(soe_gt.html#tiva-versus-inhaled-volatile)"),
      # group = ifelse(outcome == "Patient satisfaction", "Patient-reported", "Clinical"),
      across(everything(), ~ str_remove(.x, "‡|†|\\*")),
      grade = case_when(
        grade == "Very low" ~ vlow,
        grade == "Low" ~ low,
        grade == "Moderate" ~ mod,
        grade == "High" ~ high,
        grade == "Low/very low" ~ low_very,
        .default = grade
      ),
      event_e = ifelse(!is.na(n_tiva), paste0(formattable::comma(events_tiva), " (", formattable::comma(n_tiva), ")"), NA),
      event_c = ifelse(!is.na(n_inh), paste0(formattable::comma(events_inh), " (", formattable::comma(n_inh), ")"), NA),
      across(c(event_e, event_c), ~ str_remove(.x, "NA "))
    ) |>
    relocate(c(event_e, event_c), .after = nrsi) |>
    select(-c(events_tiva:n_inh, grade_2, exclude, high, mod, low, vlow, low_very))

  tiva_inhaled_dat |>
    gt(id = "one") |>
    cols_label(
      outcome  = "Outcome",
      rct      = "RCT",
      nrsi     = "NRSI",
      event_e  = "N (Total)",
      event_c  = "N (Total)",
      # n        = "    N",
      grade    = "GRADE",
      measure  = "Effect",
      est      = "Estimate (95% CI)"
    ) |>
    fmt_markdown(columns = c(grade)) |>
    cols_hide(n) |>
    fmt_integer(use_seps = TRUE, sep_mark = ",") |>
    gt_theme_mg() |>
    cols_width(
      outcome ~ px(240),
      rct     ~ px(40),
      nrsi    ~ px(45),
      # n       ~ px(65),
      event_e ~ px(110),
      event_c ~ px(110),
      grade   ~ px(100),
      measure ~ px(65),
      est     ~ px(140)
    ) |>
    sub_missing(columns = everything(), missing_text = "") |>
    tab_spanner(label = "TIVA", columns = c(event_e), level = 1) |>
    tab_spanner(label = "Inhaled", columns = c(event_c), level = 1) |>
    opt_footnote_marks(marks = "standard") |>
    tab_style(style = cell_text(align = "center"),      locations = cells_column_labels(columns = c(event_c, event_e))) |>
    tab_style(style = cell_text(align = "center"),      locations = cells_column_labels(columns = c(n, grade, rct, nrsi))) |>
    # tab_style(style = cell_text(align = "right"),       locations = cells_column_labels(columns = c(est))) |>
    tab_style(style = cell_text(align = "left"),        locations = cells_body(columns = c(est, measure))) |>
    tab_style(style = cell_text(align = "center"),      locations = cells_body(columns = c(grade, rct, nrsi))) |>
    tab_style(style = list(cell_text(color = riskdiff_color)), locations = cells_body(columns = c(est, measure), rows = str_detect(measure, "RD"))) |>
    tab_style(style = cell_text(size = px(12)),               locations = cells_body(columns = c(measure), rows = measure == "RD/1000")) |>
    tab_footnote("RCT: randomized clinical trial; NRSI: nonrandomized studies of interventions; GRADE: Grades of Recommendation, Assessment, Development, and Evaluation; RR: risk ratio; MD: mean difference; RD: risk difference.") |>
    tab_footnote("Results from nonrandomized designed shown only when evidence not available from randomized trials.", locations = cells_column_labels(columns = nrsi), placement = "right") |>
    tab_footnote(md(grade_foot), locations = cells_column_labels(columns = grade)) |>
    tab_footnote("Neither study detected a difference.", locations = cells_body(columns = c(est), rows = outcome == "Neurocognitive disorders")) |>
    tab_footnote("0 events in one study; second did not detect a difference.", locations = cells_body(columns = c(est), rows = outcome == "Pulmonary congestion/edema")) |>
    tab_footnote("2 versus 0 events.", locations = cells_body(columns = c(est), rows = outcome == "Cardiac arrest")) |>
    tab_footnote("Approximate owing to pruning in studies using propensity matching.", locations = cells_body(columns = c(est), rows = est %in% c("-0.8 (-6.5 to 10.2)"))) |>
    # tab_footnote(md("[Comparing higher/highest category or categories with lower ones.](kq4.html#patient-satisfaction)"), locations = cells_body(columns = c(est), rows = outcome == "Patient satisfaction"), placement = "right") |>
    tab_footnote(md("Comparing higher/highest category or categories with lower ones."), locations = cells_body(columns = c(est), rows = outcome == "Patient satisfaction"), placement = "right")
}

kq4_complications <- function(){
  tiva_inhaled_dat <- readxl::read_excel("data/balance_tables_2023-09-14_mac_mg.xlsx", sheet = "TIVAInhaled", range = "B25:M44") |>
    remove_empty(which = "cols") |>
    clean_names() |>
    rename(est = estimate_95_percent_ci) |>
    filter(!if_all(rct:est, ~ is.na(.x))) |>
    mutate(
      high     = paste0("[", high, "]",     "(soe_gt.html#tiva-grade)"),
      mod      = paste0("[", mod, "]",      "(soe_gt.html#tiva-grade)"),
      low      = paste0("[", low, "]",      "(soe_gt.html#tiva-grade)"),
      vlow     = paste0("[", vlow, "]",     "(soe_gt.html#tiva-grade)"),
      low_very = paste0("[", low_very, "]", "(soe_gt.html#tiva-grade)"),
      # group = ifelse(outcome == "Patient satisfaction", "Patient-reported", "Clinical"),
      across(everything(), ~ str_remove(.x, "‡|†|\\*")),
      grade = case_when(
        grade == "Very low" ~ vlow,
        grade == "Low" ~ low,
        grade == "Moderate" ~ mod,
        grade == "High" ~ high,
        grade == "Low/very low" ~ low_very,
        .default = grade
      ),
      event_e = ifelse(!is.na(n_tiva), paste0(formattable::comma(events_tiva), " (", formattable::comma(n_tiva), ")"), NA),
      event_c = ifelse(!is.na(n_inh), paste0(formattable::comma(events_inh), " (", formattable::comma(n_inh), ")"), NA),
      across(c(event_e, event_c), ~ str_remove(.x, "NA "))
    ) |>
    relocate(c(event_e, event_c), .after = nrsi) |>
    select(-c(events_tiva:n_inh, grade_2, high, mod, low, vlow, low_very))

  tiva_inhaled_dat |>
    gt(id = "one") |>
    cols_label(
      outcome  = "Outcome",
      rct      = "RCT",
      nrsi     = "NRSI",
      event_e  = "N (Total)",
      event_c  = "N (Total)",
      # n        = "    N",
      grade    = "GRADE",
      measure  = "Effect",
      est      = "Estimate (95% CI)"
    ) |>
    fmt_markdown(columns = c(grade)) |>
    cols_hide(n) |>
    fmt_integer(use_seps = TRUE, sep_mark = ",") |>
    gt_theme_mg() |>
    cols_width(
      outcome ~ px(240),
      rct     ~ px(40),
      nrsi    ~ px(45),
      # n       ~ px(65),
      event_e ~ px(110),
      event_c ~ px(110),
      grade   ~ px(100),
      measure ~ px(65),
      est     ~ px(140)
    ) |>
    sub_missing(columns = everything(), missing_text = "") |>
    tab_spanner(label = "TIVA", columns = c(event_e), level = 1) |>
    tab_spanner(label = "Inhaled", columns = c(event_c), level = 1) |>
    opt_footnote_marks(marks = "standard") |>
    tab_style(style = cell_text(align = "center"), locations = cells_column_labels(columns = c(event_c, event_e))) |>
    tab_style(style = cell_text(align = "center"), locations = cells_column_labels(columns = c(n, grade, rct, nrsi))) |>
    tab_style(style = cell_text(align = "left"), locations = cells_column_labels(columns = c(est, measure))) |>
    # tab_style(style = cell_text(align = "left"), locations = cells_body(columns = c(est))) |>
    tab_style(style = cell_text(align = "center"), locations = cells_body(columns = c(grade, rct, nrsi))) |>
    tab_style(style = cell_text(align = "left"), locations = cells_body(columns = c(est, measure))) |>
    tab_style(style = cell_text(size = px(12)), locations = cells_body(columns = c(measure), rows = str_detect(measure, "RD"))) |>
    tab_style(style = list(cell_text(color = riskdiff_color)), locations = cells_body(columns = c(rct:est), rows = str_detect(measure, "RD"))) |>
    tab_footnote(md("RCT: randomized clinical trial; NRSI: nonrandomized studies of interventions; GRADE: Grades of Recommendation, Assessment, Development, and Evaluation; RR: risk ratio; OR: odds ratio; RD: risk difference.")) |>
    tab_footnote(md(grade_foot), locations = cells_column_labels(columns = grade)) |>
    tab_footnote("Approximate owing to pruning in studies using propensity matching.", locations = cells_body(columns = c(est), rows = dplyr::lag(measure) == "OR" & !is.na(est))) |>
    # tab_footnote(md("[Comparing higher/highest category or categories with lower ones.](kq3.html#patient-satisfaction)"), locations = cells_body(columns = c(est), rows = outcome == "Patient satisfaction"), placement = "right") |>
    tab_footnote(md("Comparing higher/highest category or categories with lower ones."), locations = cells_body(columns = c(est), rows = outcome == "Patient satisfaction"), placement = "right") |>
    tab_footnote("Cardiovascular, pulmonary, and acute kidney injury.", locations = cells_body(columns = c(outcome), rows = outcome == "Complications")) |>
    tab_footnote("Complications reported variously across the 13 trials. ", locations = cells_body(columns = c(rct), rows = outcome == "Complications"), placement = "right") |>
    tab_footnote("One study no events; the other two. ", locations = cells_body(columns = c(est), rows = outcome == "cardiac arrest"), placement = "right") |>
    tab_footnote("Common effects model.", locations = cells_body(columns = c(est), rows = est %in% c("3.47 (0.57–21.2)", "1.7 (-0.7 to 4.1)")), placement = "right") |>
    tab_footnote("No events in 1 study; 3 in the other.", locations = cells_body(columns = c(est), rows = str_detect(est, "—")), placement = "right")
}

kq6_balance_dex_main <- function(inc_exclude = "exclude") {
  dex_plac_dat <- readxl::read_excel("data/balance_tables_2023-09-14_mac_mg.xlsx", sheet = "DeliriumProph", range = "A4:O16", col_types = c(rep("text", 2), rep("numeric", 7), rep("text", 6))) |>
    remove_empty(which = "cols") |>
    filter(exclude %notin% inc_exclude) |>
    clean_names() |>
    rename(est = estimate_95_percent_ci) |>
    filter(!if_all(rct:est, ~ is.na(.x))) |>
    mutate(
      i2 = str_c(i2, "%"),
      high     = paste0("[", high, "]",     "(soe_gt.html#delirium-prophylaxis)"),
      mod      = paste0("[", mod, "]",      "(soe_gt.html#delirium-prophylaxis)"),
      low      = paste0("[", low, "]",      "(soe_gt.html#delirium-prophylaxis)"),
      vlow     = paste0("[", vlow, "]",     "(soe_gt.html#delirium-prophylaxis)"),
      low_very = paste0("[", low_very, "]", "(soe_gt.html#delirium-prophylaxis)"),
      # group = ifelse(outcome == "Patient satisfaction", "Patient-reported", "Clinical"),
      across(everything(), ~ str_remove(.x, "‡|†|\\*")),
      # group = ifelse(outcome == "Patient satisfaction", "Patient-reported", "Clinical"),
      across(everything(), ~ str_remove(.x, "‡|†|\\*")),
      grade = case_when(
        grade == "Very low" ~ vlow,
        grade == "Low" ~ low,
        grade == "Moderate" ~ mod,
        grade == "High" ~ high,
        grade == "Low/very low" ~ low_very,
        .default = grade
      ),
      event_e = ifelse(!is.na(n_dex), paste0(formattable::comma(events_dex), " (", formattable::comma(n_dex), ")"), NA),
      event_c = ifelse(!is.na(n_pla), paste0(formattable::comma(events_pla), " (", formattable::comma(n_pla), ")"), NA),
      across(c(event_e, event_c), ~ str_remove(.x, "NA "))
    ) |>
    relocate(c(event_e, event_c), .after = nrsi) |>
    select(-c(events_dex:n_pla, grade_2, high, mod, low, vlow, low_very))

  dex_plac_dat |>
    gt(id = "one") |>
    cols_label(
      outcome  = "Outcome",
      rct      = "RCT",
      nrsi     = "NRSI",
      event_e  = "N (Total)",
      event_c  = "N (Total)",
      # n        = "    N",
      grade    = "GRADE",
      measure  = "Effect",
      est      = md("    Estimate<br/>      (95% CI)"),
      i2       = md("*I*<sup> 2</sup>"),
      pi       = md("  (95% PI)")
    ) |>
    fmt_markdown(columns = c(grade, i2)) |>
    cols_hide(c(n, exclude)) |>
    fmt_integer(use_seps = TRUE, sep_mark = ",") |>
    gt_theme_mg() |>
    cols_width(
      outcome ~ px(240),
      rct ~ px(40),
      nrsi ~ px(45),
      # n       ~ px(65),
      event_e ~ px(115),
      event_c ~ px(105),
      grade ~ px(100),
      measure ~ px(65),
      est ~ px(120),
      i2 ~ px(40),
      pi ~ px(100)
    ) |>
    sub_missing(columns = everything(), missing_text = "") |>
    sub_values(columns = pi, pattern = "insuff_data", replacement = "      ") |>
    tab_spanner(label = "Dexmedetomidine", columns = c(event_e), level = 1) |>
    tab_spanner(label = "Placebo", columns = c(event_c), level = 1, id = "event_c_spanner") |>
    # tab_spanner(label = "Heterogeneity", columns = c(i2), level = 1) |>
    # tab_style(style = cell_text(size = "11px"), locations = cells_column_spanners(spanners = "Heterogeneity")) |>
    opt_footnote_marks(marks = "standard") |>
    tab_style(style = cell_text(align = "right"), locations = cells_body(columns = c(i2))) |>
    tab_style(style = cell_text(align = "center"), locations = cells_column_labels(columns = c(event_c, event_e, n, grade, rct, nrsi))) |>
    # tab_style(style = cell_text(align = "left"),        locations = cells_body(columns = c(est))) |>
    tab_style(style = cell_text(align = "center"), locations = cells_body(columns = c(grade, measure, rct, nrsi))) |>
    # tab_style(style = list(cell_text(color = riskdiff_color)), locations = cells_body(columns = c(est, measure), rows = str_detect(measure, "RD"))) |>
    tab_style(style = list(cell_text(color = riskdiff_color)), locations = cells_body(columns = c(rct:pi), rows = str_detect(measure, "RD"))) |>
    tab_footnote("RCT: randomized clinical trial; NRSI: nonrandomized studies of interventions; GRADE: Grades of Recommendation, Assessment, Development, and Evaluation; PI: prediction interval; RR: risk ratio; SMD: standardized mean difference; RD: risk difference; MD: mean difference.") |>
    tab_footnote(md(grade_foot), locations = cells_column_labels(columns = grade)) |>
    tab_footnote("Neither study detected a difference.", locations = cells_body(columns = c(est), rows = outcome == "Neurocognitive disorders")) |>
    tab_footnote("Insufficient data to estimate a valid prediction interval.", locations = cells_body(columns = pi, rows = pi == "insuff_data")) |>
    # tab_footnote("Cardiovascular, pulmonary, and acute kidney injury.", locations = cells_body(columns = c(outcome), rows = outcome == "Complications")) |>
    tab_footnote("0 events in one study; second did not detect a difference.", locations = cells_body(columns = c(est), rows = outcome == "Pulmonary congestion/edema")) |>
    tab_footnote("2 versus 0 events.", locations = cells_body(columns = c(est), rows = outcome == "Cardiac arrest")) |>
    # tab_footnote(md("[Comparing higher/highest category or categories with lower ones.](kq4.html#patient-satisfaction)"), locations = cells_body(columns = c(est), rows = outcome == "Patient satisfaction"), placement = "right")
    tab_footnote(md("Comparing higher/highest category or categories with lower ones."), locations = cells_body(columns = c(est), rows = outcome == "Patient satisfaction"), placement = "right") |>
    tab_footnote("In some studies (randomized or not) the control incorporated neither placebo or prophylaxis.", locations = cells_column_spanners(spanners = "event_c_spanner")) |>
    tab_footnote("With Hartung-Knapp adjustment MD -0.8 (-1.5 to -0.08).", locations = cells_body(columns = c(est), rows = outcome == "Length of stay (days)"), placement = "right")
}

kq6_dex_complications <- function(){
  dex_plac_dat <- readxl::read_excel("data/balance_tables_2023-09-14_mac_mg.xlsx", sheet = "DeliriumProph", range = "A23:O39", col_types = c(rep("text", 2), rep("numeric", 7), rep("text", 6))) |>
    # remove_empty(which = "cols") |>
    select(-exclude) |>
    clean_names() |>
    rename(est = estimate_95_percent_ci) |>
    filter(!if_all(rct:est, ~ is.na(.x))) |>
    mutate(
      i2 = str_c(i2, "%"),
      high     = paste0("[", high, "]",     "(soe_gt.html#delirium-prophylaxis)"),
      mod      = paste0("[", mod, "]",      "(soe_gt.html#delirium-prophylaxis)"),
      low      = paste0("[", low, "]",      "(soe_gt.html#delirium-prophylaxis)"),
      vlow     = paste0("[", vlow, "]",     "(soe_gt.html#delirium-prophylaxis)"),
      low_very = paste0("[", low_very, "]", "(soe_gt.html#delirium-prophylaxis)"),
      across(everything(), ~ str_remove(.x, "‡|†|\\*")),
      grade = case_when(
        grade == "Very low" ~ vlow,
        grade == "Low" ~ low,
        grade == "Moderate" ~ mod,
        grade == "High" ~ high,
        grade == "Low/very low" ~ low_very,
        .default = grade
      ),
      event_e = ifelse(!is.na(n_dex), paste0(formattable::comma(events_dex), " (", formattable::comma(n_dex), ")"), NA),
      event_c = ifelse(!is.na(n_pla), paste0(formattable::comma(events_pla), " (", formattable::comma(n_pla), ")"), NA),
      across(c(event_e, event_c), ~ str_remove(.x, "NA "))
    ) |>
    relocate(c(event_e, event_c), .after = nrsi) |>
    select(-c(events_dex:n_pla, grade_2, high, mod, low, vlow, low_very))

  dex_plac_dat |>
    gt(id = "one") |>
    cols_label(
      outcome  = "Outcome",
      rct      = "RCT",
      nrsi     = "NRSI",
      event_e  = "N (Total)",
      event_c  = "N (Total)",
      # n        = "    N",
      grade    = "GRADE",
      measure  = "Effect",
      est      = "Estimate (95% CI)",
      i2       = md("*I*<sup> 2</sup>"),
      pi       = md("  (95% PI)")
    ) |>
    fmt_markdown(columns = c(grade, i2)) |>
    cols_hide(n) |>
    fmt_integer(use_seps = TRUE, sep_mark = ",") |>
    gt_theme_mg() |>
    cols_width(
      outcome ~ px(240),
      rct ~ px(40),
      nrsi ~ px(45),
      # n       ~ px(65),
      event_e ~ px(115),
      event_c ~ px(105),
      grade ~ px(100),
      measure ~ px(65),
      est ~ px(120),
      i2 ~ px(40),
      pi ~ px(100)
    ) |>
    sub_missing(columns = everything(), missing_text = "") |>
    sub_values(columns = pi, pattern = "insuff_data", replacement = "      ") |>
    tab_spanner(label = "Dexmedetomidine", columns = c(event_e), level = 1) |>
    tab_spanner(label = "Placebo", columns = c(event_c), level = 1, id = "event_c_spanner") |>
    opt_footnote_marks(marks = "standard") |>
    tab_style(style = cell_text(align = "center"), locations = cells_column_labels(columns = c(event_c, event_e))) |>
    tab_style(style = cell_text(align = "center"), locations = cells_column_labels(columns = c(n, grade, rct))) |>
    tab_style(style = cell_text(align = "center"), locations = cells_body(columns = c(grade, measure, rct))) |>
    tab_style(style = cell_text(size = px(12)), locations = cells_body(columns = c(measure), rows = measure == "RD/1000")) |>
    tab_style(style = list(cell_text(color = riskdiff_color)), locations = cells_body(columns = c(rct:pi), rows = str_detect(measure, "RD"))) |>
    tab_footnote("Insufficient data to estimate a valid prediction interval.", locations = cells_body(columns = pi, rows = pi == "insuff_data")) |>
    tab_footnote(md("RCT: randomized clinical trial; GRADE: Grades of Recommendation, Assessment, Development, and Evaluation; RR: risk ratio; RD: risk difference.")) |>
    tab_footnote(md(grade_foot), locations = cells_column_labels(columns = grade)) |>
    tab_footnote(md("[Comparing higher/highest category or categories with lower ones.](kq3.html#patient-satisfaction)"), locations = cells_body(columns = c(est), rows = outcome == "Patient satisfaction"), placement = "right") |>
    tab_footnote("Comparing higher/highest category or categories with lower ones", locations = cells_body(columns = c(est), rows = outcome == "Patient satisfaction"), placement = "right") |>
    tab_footnote("Cardiovascular, pulmonary, and acute kidney injury.", locations = cells_body(columns = c(outcome), rows = outcome == "Complications")) |>
    tab_footnote("Complications reported variously across the 13 trials. ", locations = cells_body(columns = c(rct), rows = outcome == "Complications"), placement = "right") |>
    tab_footnote("One study no events; the other two. ", locations = cells_body(columns = c(est), rows = outcome == "cardiac arrest"), placement = "right") |>
    tab_footnote("Complications reported as heart failure (1 study), arrhythmia (1 study), or bradycardia (17 studies).", locations = cells_body(columns = c(outcome), rows = outcome == "Cardiac complications")) |>
    tab_footnote("In some studies (randomized or not) the control incorporated neither placebo or prophylaxis.", locations = cells_column_spanners(spanners = "event_c_spanner"))
}

kq6_ketamine <- function(){
  ket_plac_dat <- readxl::read_excel("data/balance_tables_2023-09-14_mac_mg.xlsx", sheet = "DeliriumProph", range = "B42:M50") |>
    remove_empty(which = "cols") |>
    clean_names() |>
    rename(est = estimate_95_percent_ci) |>
    filter(!if_all(rct:est, ~ is.na(.x))) |>
    mutate(
      high     = paste0("[", high, "]",     "(soe_gt.html#del-prophylaxis-grade)"),
      mod      = paste0("[", mod, "]",      "(soe_gt.html#del-prophylaxis-grade)"),
      low      = paste0("[", low, "]",      "(soe_gt.html#del-prophylaxis-grade)"),
      vlow     = paste0("[", vlow, "]",     "(soe_gt.html#del-prophylaxis-grade)"),
      low_very = paste0("[", low_very, "]", "(soe_gt.html#del-prophylaxis-grade)"),
      # group = ifelse(outcome == "Patient satisfaction", "Patient-reported", "Clinical"),
      across(everything(), ~ str_remove(.x, "‡|†|\\*")),
      # group = ifelse(outcome == "Patient satisfaction", "Patient-reported", "Clinical"),
      across(everything(), ~ str_remove(.x, "‡|†|\\*")),
      grade = case_when(
        grade == "Very low" ~ vlow,
        grade == "Low" ~ low,
        grade == "Moderate" ~ mod,
        grade == "High" ~ high,
        grade == "Low/very low" ~ low_very,
        .default = grade
      ),
      event_e = ifelse(!is.na(n_ket), paste0(formattable::comma(events_ket), " (", formattable::comma(n_ket), ")"), NA),
      event_c = ifelse(!is.na(n_pla), paste0(formattable::comma(events_pla), " (", formattable::comma(n_pla), ")"), NA),
      across(c(event_e, event_c), ~ str_remove(.x, "NA ")),
      event_e = ifelse(event_e == "8,808 (119,621)", "8,808<br/>(119,621)", event_e),
    ) |>
    relocate(c(event_e, event_c), .after = nrsi) |>
    select(-c(events_ket:n_pla, grade_2, high, mod, low, vlow, low_very))

  ket_plac_dat |>
    gt(id = "one") |>
    cols_label(
      outcome  = "Outcome",
      rct      = "RCT",
      nrsi     = "NRSI",
      event_e  = "N (Total)",
      event_c  = "N (Total)",
      # n        = "    N",
      grade    = "GRADE",
      measure  = "Effect",
      est      = "Estimate (95% CI)"
    ) |>
    fmt_markdown(columns = c(grade, event_e)) |>
    cols_hide(n) |>
    fmt_integer(use_seps = TRUE, sep_mark = ",") |>
    gt_theme_mg() |>
    cols_width(
      outcome ~ px(240),
      rct ~ px(40),
      nrsi ~ px(45),
      # n       ~ px(65),
      event_e ~ px(110),
      event_c ~ px(110),
      grade ~ px(100),
      measure ~ px(45),
      est ~ px(140)
    ) |>
    sub_missing(columns = everything(), missing_text = "") |>
    tab_spanner(label = "Ketamine", columns = c(event_e), level = 1) |>
    tab_spanner(label = "Placebo", columns = c(event_c), level = 1) |>
    opt_footnote_marks(marks = "standard") |>
    tab_style(style = cell_text(align = "center"),        locations = cells_column_labels(columns = c(event_c, event_e))) |>
    tab_style(style = cell_text(align = "center"),      locations = cells_column_labels(columns = c(n, grade, rct, nrsi))) |>
    # tab_style(style = cell_text(align = "right"),       locations = cells_column_labels(columns = c(est))) |>
    tab_style(style = cell_text(align = "right"),        locations = cells_body(columns = c(event_e))) |>
    tab_style(style = cell_text(align = "center"),      locations = cells_body(columns = c(grade, measure, rct, nrsi))) |>
    tab_footnote("RCT: randomized clinical trial; NRSI: nonrandomized studies of interventions; GRADE: Grades of Recommendation, Assessment, Development, and Evaluation; RR: risk ratio; OR: odds ratio; MD: mean difference.") |>
    tab_footnote(md(grade_foot), locations = cells_column_labels(columns = grade)) |>
    tab_footnote("Neither study detected a difference.", locations = cells_body(columns = c(est), rows = outcome == "Neurocognitive disorders")) |>
    # tab_footnote("Cardiovascular, pulmonary, and acute kidney injury.", locations = cells_body(columns = c(outcome), rows = outcome == "Complications")) |>
    tab_footnote("0 events in one study; second did not detect a difference.", locations = cells_body(columns = c(est), rows = outcome == "Pulmonary congestion/edema")) |>
    tab_footnote("2 versus 0 events.", locations = cells_body(columns = c(est), rows = outcome == "Cardiac arrest")) |>
    tab_footnote(md("[Comparing higher/highest category or categories with lower ones.](kq4.html#patient-satisfaction)"), locations = cells_body(columns = c(est), rows = outcome == "Patient satisfaction"), placement = "right")
}

kq6_balance_mel_ram_main <- function(){
  mel_ram_plac_dat <- readxl::read_excel("data/balance_tables_2023-09-14_mac_mg.xlsx", sheet = "DeliriumProph", range = "B57:M65") |>
    remove_empty(which = "cols") |>
    clean_names() |>
    rename(est = estimate_95_percent_ci) |>
    filter(!if_all(rct:est, ~ is.na(.x))) |>
    mutate(
      high     = paste0("[", high, "]",     "(soe_gt.html#del-prophylaxis-grade)"),
      mod      = paste0("[", mod, "]",      "(soe_gt.html#del-prophylaxis-grade)"),
      low      = paste0("[", low, "]",      "(soe_gt.html#del-prophylaxis-grade)"),
      vlow     = paste0("[", vlow, "]",     "(soe_gt.html#del-prophylaxis-grade)"),
      low_very = paste0("[", low_very, "]", "(soe_gt.html#del-prophylaxis-grade)"),
      # group = ifelse(outcome == "Patient satisfaction", "Patient-reported", "Clinical"),
      across(everything(), ~ str_remove(.x, "‡|†|\\*")),
      # group = ifelse(outcome == "Patient satisfaction", "Patient-reported", "Clinical"),
      across(everything(), ~ str_remove(.x, "‡|†|\\*")),
      grade = case_when(
        grade == "Very low" ~ vlow,
        grade == "Low" ~ low,
        grade == "Moderate" ~ mod,
        grade == "High" ~ high,
        grade == "Low/very low" ~ low_very,
        .default = grade
      ),
      event_e = ifelse(!is.na(n_mel_ram), paste0(formattable::comma(events_mel_ram), " (", formattable::comma(n_mel_ram), ")"), NA),
      event_c = ifelse(!is.na(n_pla), paste0(formattable::comma(events_pla), " (", formattable::comma(n_pla), ")"), NA),
      across(c(event_e, event_c), ~ str_remove(.x, "NA "))
    ) |>
    relocate(c(event_e, event_c), .after = nrsi) |>
    select(-c(events_mel_ram:n_pla, grade_2, high, mod, low, vlow, low_very))

  mel_ram_plac_dat |>
    gt(id = "one") |>
    cols_label(
      outcome  = "Outcome",
      rct      = "RCT",
      nrsi     = "NRSI",
      event_e  = "N (Total)",
      event_c  = "N (Total)",
      # n        = "    N",
      grade    = "GRADE",
      measure  = "Effect",
      est      = "Estimate (95% CI)"
    ) |>
    fmt_markdown(columns = c(grade, event_e)) |>
    cols_hide(n) |>
    fmt_integer(use_seps = TRUE, sep_mark = ",") |>
    gt_theme_mg() |>
    cols_width(
      outcome ~ px(240),
      rct ~ px(40),
      nrsi ~ px(45),
      # n       ~ px(65),
      event_e ~ px(110),
      event_c ~ px(110),
      grade ~ px(100),
      measure ~ px(45),
      est ~ px(140)
    ) |>
    sub_missing(columns = everything(), missing_text = "") |>
    tab_spanner(label = md("Melatonin<br/>or Ramelteon"), columns = c(event_e), level = 1) |>
    tab_spanner(label = md("<br/>Placebo"), columns = c(event_c), level = 1) |>
    opt_footnote_marks(marks = "standard") |>
    tab_style(style = cell_text(align = "center"),        locations = cells_column_labels(columns = c(event_c, event_e))) |>
    tab_style(style = cell_text(align = "center"),      locations = cells_column_labels(columns = c(n, grade, rct, nrsi))) |>
    # tab_style(style = cell_text(align = "right"),       locations = cells_column_labels(columns = c(est))) |>
    # tab_style(style = cell_text(align = "left"),        locations = cells_body(columns = c(est))) |>
    tab_style(style = cell_text(align = "center"),      locations = cells_body(columns = c(grade, measure, rct, nrsi))) |>
    tab_footnote("RCT: randomized clinical trial; NRSI: nonrandomized studies of interventions; GRADE: Grades of Recommendation, Assessment, Development, and Evaluation; RR: risk ratio; OR: odds ratio; MD: mean difference; SMD: standardized mean difference.") |>
    tab_footnote(md(grade_foot), locations = cells_column_labels(columns = grade)) |>
    tab_footnote("Neither study detected a difference.", locations = cells_body(columns = c(est), rows = outcome == "Neurocognitive disorders")) |>
    # tab_footnote("Cardiovascular, pulmonary, and acute kidney injury.", locations = cells_body(columns = c(outcome), rows = outcome == "Complications")) |>
    tab_footnote("0 events in one study; second did not detect a difference.", locations = cells_body(columns = c(est), rows = outcome == "Pulmonary congestion/edema")) |>
    tab_footnote("2 versus 0 events.", locations = cells_body(columns = c(est), rows = outcome == "Cardiac arrest")) |>
    tab_footnote("Comparing higher/highest category or categories with lower ones", locations = cells_body(columns = c(est), rows = outcome == "Patient satisfaction"), placement = "right")
}

kq6_balance_mel_ram_complications <- function() {
  mel_ram_plac_dat <- readxl::read_excel("data/balance_tables_2023-09-14_mac_mg.xlsx", sheet = "DeliriumProph", range = "B71:M82") |>
    remove_empty(which = "cols") |>
    clean_names() |>
    rename(est = estimate_95_percent_ci) |>
    filter(!if_all(rct:est, ~ is.na(.x))) |>
    mutate(
      high     = paste0("[", high, "]",     "(soe_gt.html#del-prophylaxis-grade)"),
      mod      = paste0("[", mod, "]",      "(soe_gt.html#del-prophylaxis-grade)"),
      low      = paste0("[", low, "]",      "(soe_gt.html#del-prophylaxis-grade)"),
      vlow     = paste0("[", vlow, "]",     "(soe_gt.html#del-prophylaxis-grade)"),
      low_very = paste0("[", low_very, "]", "(soe_gt.html#del-prophylaxis-grade)"),
      # group = ifelse(outcome == "Patient satisfaction", "Patient-reported", "Clinical"),
      across(everything(), ~ str_remove(.x, "‡|†|\\*")),
      # group = ifelse(outcome == "Patient satisfaction", "Patient-reported", "Clinical"),
      across(everything(), ~ str_remove(.x, "‡|†|\\*")),
      grade = case_when(
        grade == "Very low" ~ vlow,
        grade == "Low" ~ low,
        grade == "Moderate" ~ mod,
        grade == "High" ~ high,
        grade == "Low/very low" ~ low_very,
        .default = grade
      ),
      event_e = ifelse(!is.na(n_mel_ram), paste0(formattable::comma(events_mel_ram), " (", formattable::comma(n_mel_ram), ")"), NA),
      event_c = ifelse(!is.na(n_pla), paste0(formattable::comma(events_pla), " (", formattable::comma(n_pla), ")"), NA),
      across(c(event_e, event_c), ~ str_remove(.x, "NA "))
    ) |>
    relocate(c(event_e, event_c), .after = nrsi) |>
    select(-c(events_mel_ram:n_pla, grade_2, high, mod, low, vlow, low_very))

  mel_ram_plac_dat |>
    gt(id = "one") |>
    cols_label(
      outcome  = "Outcome",
      rct      = "RCT",
      nrsi     = "NRSI",
      event_e  = "N (Total)",
      event_c  = "N (Total)",
      # n        = "    N",
      grade    = "GRADE",
      measure  = "Effect",
      est      = "Estimate (95% CI)"
    ) |>
    fmt_markdown(columns = c(grade, event_e)) |>
    cols_hide(n) |>
    fmt_integer(use_seps = TRUE, sep_mark = ",") |>
    gt_theme_mg() |>
    cols_width(
      outcome ~ px(240),
      rct ~ px(40),
      nrsi ~ px(45),
      # n       ~ px(65),
      event_e ~ px(110),
      event_c ~ px(110),
      grade ~ px(100),
      measure ~ px(45),
      est ~ px(140)
    ) |>
    sub_missing(columns = everything(), missing_text = "") |>
    tab_spanner(label = md("Melatonin<br/>or Ramelteon"), columns = c(event_e), level = 1) |>
    tab_spanner(label = md("<br/>Placebo"), columns = c(event_c), level = 1) |>
    opt_footnote_marks(marks = "standard") |>
    tab_style(style = cell_text(align = "center"), locations = cells_column_labels(columns = c(event_c, event_e))) |>
    tab_style(style = cell_text(align = "center"), locations = cells_column_labels(columns = c(n, grade, rct, nrsi))) |>
    # tab_style(style = cell_text(align = "right"),       locations = cells_column_labels(columns = c(est))) |>
    # tab_style(style = cell_text(align = "left"),        locations = cells_body(columns = c(est))) |>
    tab_style(style = cell_text(align = "center"), locations = cells_body(columns = c(grade, measure, rct, nrsi))) |>
    tab_footnote(md("RCT: randomized clinical trial; GRADE: Grades of Recommendation, Assessment, Development, and Evaluation; RR: risk ratio; MD: mean difference.")) |>
    tab_footnote(md(grade_foot), locations = cells_column_labels(columns = grade)) |>
    # tab_footnote("Complications bradycardia (3 studies); atrial fibrillation (1 study); cardiac dysfunction (1 study).", locations = cells_body(columns = c(outcome), rows = outcome == "Cardiac complications")) |>
    tab_footnote(md("[Comparing higher/highest category or categories with lower ones.](kq3.html#patient-satisfaction)"), locations = cells_body(columns = c(est), rows = outcome == "Patient satisfaction"), placement = "right") |>
    tab_footnote("Comparing higher/highest category or categories with lower ones", locations = cells_body(columns = c(est), rows = outcome == "Patient satisfaction"), placement = "right") |>
    tab_footnote("Complications reported variously across the 13 trials. ", locations = cells_body(columns = c(rct), rows = outcome == "Complications"), placement = "right") |>
    tab_footnote("One study no events; the other two. ", locations = cells_body(columns = c(est), rows = outcome == "cardiac arrest"), placement = "right")
}

## summary table functions ---------------------------- (2024-04-28 07:41) @----
summary_study_char_tab_rct <- function(refids) {
  study_char_tab <- study_char_dat |>
  filter(refid %in% refids) |>
  filter(design == "rct") |>
  mutate(
    low_r = if_else(non_vh_hdi == "yes", 1, 0, missing = 0),
    pilot = if_else(pilot == "yes", 1, 0, missing = 0),
    ambulatory = !is.na(ambulatory),
    one_center = centers == 1,
    multi_center = centers > 1,
    general = !is.na(general),
    regional = !is.na(regional),
    sedation = !is.na(sedation),
    non_vh_hdi = ifelse(is.na(non_vh_hdi), 0, 1),
    funding = case_when(
      funding == "industry" ~ "Industry",
      funding == "pub_indus" ~ "Public and industry",
      funding == "public" ~ "Public",
      funding == "NR" ~ "Not reported",
      funding == "none" ~ "None",
    ),
    # funding = ordered(funding, levels = c("Public", "Industry", "Public and industry", "Not reported")),
    # funding = factor(funding, levels = c("public", "industry", "pub_indus", "NR"), labels = c("Public", "Industry", "Public and industry", "Not reported")),
    author_coi = author_coi == "author_coi",
    registered = if_else(registered == "yes", TRUE, FALSE, missing = FALSE),
    across(c(ambulatory, one_center, multi_center, general, regional, sedation, registered, author_coi), ~ .x * 1)
  ) |>
  select(refid, study, country, low_r, non_vh_hdi, pilot, ambulatory, centers, funding, registered, n_enroll, arms, author_coi)

n_studies <- nrow(study_char_tab)

study_char_tab |>
  select(n_enroll, arms, pilot, ambulatory, centers, country, non_vh_hdi, funding, author_coi, registered) |>
  tbl_summary(
    label = list(
      n_enroll = "Patients enrolled",
      arms = "Arms, N (%)",
      pilot = "Pilot study, N (%)",
      ambulatory = "Ambulatory, N (%)",
      centers = "Centers, N (%)",
      country = "Country, N (%)",
      non_vh_hdi = "Low resource country, N (%)",
      funding = "Funding, N (%)",
      author_coi = "Author conflict of interest, N (%)",
      registered = "Registered, N (%)"
    ),
    digits = list(
      n_enroll ~ 0,
      arms = c(0, 1),
      pilot = c(0, 1),
      ambulatory = c(0, 1),
      centers = c(0, 1),
      country = c(0, 1),
      non_vh_hdi = c(0, 1),
      funding = c(0, 1),
      author_coi = c(0, 1),
      registered = c(0, 1)
    ),
    type = list(
      n_enroll ~ "continuous",
      arms ~ "categorical"
    ),
    statistic = list(
    # calculate percentages including missing values in the denominator
      n_enroll ~ "{mean} <u>{median}</u> ({min} - {max})",
      arms ~ "{n} ({p})",
      pilot = "{n} ({p})",
      ambulatory = "{n} ({p})",
      centers = "{n} ({p})",
      country = "{n} ({p})",
      non_vh_hdi = "{n} ({p})",
      funding = "{n} ({p})",
      author_coi = "{n} ({p})",
      registered = "{n} ({p})"
    ),
    missing_text = "Not reported",
    sort = list(everything() ~ "frequency")
  ) |>
  modify_header(stat_0 = "**Mean <u>Med</u> (Range) <br/> or N (%)**") |>
  modify_footnote(update = stat_0 ~ NA) |>
  modify_header(label = paste0("Characteristic", "<br/>[", n_studies, " trials]")) |>
  as_gt(id = "one") |>
  cols_width(
      # 5 ~ px(200),
      6 ~ px(135)
    ) |>
  fmt_markdown(stat_0) |>
  gt_theme_mg()
  # tab_footnote("Mean, median, or range of the mean or median reported in trials.", locations = cells_column_labels(columns = c(stat_0)), placement = "right")
}

summary_study_char_tab_nrsi <- function(refids) {
  study_char_tab <- study_char_dat |>
    filter(refid %in% refids) |>
    filter(!design == "rct") |>
    mutate(
      low_r = if_else(non_vh_hdi == "yes", 1, 0, missing = 0),
      pilot = if_else(pilot == "yes", 1, 0, missing = 0),
      ambulatory = !is.na(ambulatory),
      one_center = centers == 1,
      multi_center = centers > 1,
      general = !is.na(general),
      regional = !is.na(regional),
      sedation = !is.na(sedation),
      non_vh_hdi = ifelse(is.na(non_vh_hdi), 0, 1),
      funding = case_when(
        funding == "industry" ~ "Industry",
        funding == "pub_indus" ~ "Public and industry",
        funding == "public" ~ "Public",
        funding == "NR" ~ "Not reported",
        funding == "none" ~ "None",
      ),
      author_coi = author_coi == "author_coi",
      registered = if_else(registered == "yes", TRUE, FALSE, missing = FALSE),
      across(c(ambulatory, one_center, multi_center, general, regional, sedation, registered, author_coi), ~ .x * 1)
    ) |>
    select(refid, study, design_f_lab, country, low_r, non_vh_hdi, pilot, ambulatory, centers, funding, registered, n_enroll, arms, author_coi)

  n_studies <- nrow(study_char_tab)

  study_char_tab |>
    mutate(design_f_lab = fct_drop(design_f_lab)) |>
    select(n_enroll, design_f_lab, arms, pilot, ambulatory, centers, country, non_vh_hdi, funding, author_coi, registered) |>
    tbl_summary(
      label = list(
        n_enroll = "Patients enrolled",
        design_f_lab = "Design, N (%)",
        arms = "Arms, N (%)",
        pilot = "Pilot study, N (%)",
        ambulatory = "Ambulatory, N (%)",
        centers = "Centers, N (%)",
        country = "Country, N (%)",
        non_vh_hdi = "Low resource country, N (%)",
        funding = "Funding, N (%)",
        author_coi = "Author conflict of interest, N (%)",
        registered = "Registered, N (%)"
      ),
      digits = list(
        n_enroll ~ 0,
        design_f_lab ~ c(0, 1),
        arms = c(0, 1),
        pilot = c(0, 1),
        ambulatory = c(0, 1),
        centers = c(0, 1),
        country = c(0, 1),
        non_vh_hdi = c(0, 1),
        funding = c(0, 1),
        author_coi = c(0, 1),
        registered = c(0, 1)
      ),
      type = list(
        n_enroll ~ "continuous",
        arms ~ "categorical"
      ),
      statistic = list(
        # calculate percentages including missing values in the denominator
        n_enroll ~ "{mean} <u>{median}</u> ({min} - {max})",
        design_f_lab ~ "{n} ({p})",
        arms ~ "{n} ({p})",
        pilot = "{n} ({p})",
        ambulatory = "{n} ({p})",
        centers = "{n} ({p})",
        country = "{n} ({p})",
        non_vh_hdi = "{n} ({p})",
        funding = "{n} ({p})",
        author_coi = "{n} ({p})",
        registered = "{n} ({p})"
      ),
      missing_text = "Not reported",
      sort = list(c(design_f_lab, country, funding) ~ "frequency")
    ) |>
    modify_header(stat_0 = "**Mean <u>Med</u> (Range) <br/> or N (%)**") |>
    modify_footnote(update = stat_0 ~ NA) |>
    modify_header(label = paste0("Characteristic", "<br/>[", n_studies, " studies]")) |>
    as_gt(id = "one") |>
    cols_width(
      # 5 ~ px(200),
      6 ~ px(135)
    ) |>
    fmt_markdown(stat_0) |>
    gt_theme_mg()
}

summary_surg_tab_rct <- function(refids) {
  surg_refids <- study_char_dat |>
    filter(refid %in% refids) |>
    filter(design == "rct") |>
    pull(refid)

  # convenience
  surg_tab <- surgs |>
    filter(refid %in% surg_refids) |>
    select(refid, surgs) |>
    tabyl(surgs) |>
    arrange(desc(percent)) |>
    rename(per = percent) |>
    mutate(
      per = paste0("(", format(round(100 * per, 1), nsmall = 1), ")"),
      per = str_replace(per, " ", ""),
      n = as.character(n),
      n_per = paste(n, per)
    ) |>
    select(surgs, n_per) |>
    rename(result = n_per, characteristic = surgs)

  surgs |>
    filter(refid %in% surg_refids) |>
    select(surgs) |>
    tbl_summary(
      label = list(
        surgs = "Procedure(s), N (%)"
      ),
      statistic = list(
        surgs ~ "{n} ({p})"
      ),
      type = list(
        surgs ~ "categorical"
      ),
      missing_text = "Not reported",
      sort = list(everything() ~ "frequency")
    ) |>
    modify_header(label = "<br/>Surgery") |>
    modify_footnote(update = stat_0 ~ NA) |>
    as_gt(id = "one") |>
    gt_theme_mg() |>
    cols_width(
      5 ~ px(200),
      6 ~ px(80)
    ) |>
    tab_footnote("If reported as various/mixed or included more than 4 types of procedures.", locations = cells_body(columns = label, rows = label == "Various"), placement = "right")
}

summary_surg_tab_nrsi <- function(refids) {
  surg_refids <- study_char_dat |>
    filter(refid %in% refids) |>
    filter(design != "rct") |>
    pull(refid)

  # convenience
  surg_tab <- surgs |>
    filter(refid %in% surg_refids) |>
    select(refid, surgs) |>
    tabyl(surgs) |>
    arrange(desc(percent)) |>
    rename(per = percent) |>
    mutate(
      per = paste0("(", format(round(100 * per, 1), nsmall = 1), ")"),
      per = str_replace(per, " ", ""),
      n = as.character(n),
      n_per = paste(n, per)
    ) |>
    select(surgs, n_per) |>
    rename(result = n_per, characteristic = surgs)

  surgs |>
    filter(refid %in% surg_refids) |>
    select(surgs) |>
    tbl_summary(
      label = list(
        surgs = "Procedure(s), N (%)"
      ),
      statistic = list(
        surgs ~ "{n} ({p})"
      ),
      type = list(
        surgs ~ "categorical"
      ),
      missing_text = "Not reported",
      sort = list(everything() ~ "frequency")
    ) |>
    modify_header(label = "<br/>Surgery") |>
    modify_footnote(update = stat_0 ~ NA) |>
    as_gt(id = "one") |>
    gt_theme_mg() |>
    cols_width(
      5 ~ px(200),
      6 ~ px(80)
    ) |>
    tab_footnote("If reported as various/mixed or included more than 4 types of procedures.", locations = cells_body(columns = label, rows = label == "Various"), placement = "right")
}

pt_char_tab_refids_rct <- function(refids) {
  study_char_dat |>
    filter(refid %in% refids) |>
    filter(design %in% c("rct")) |>
    pull(refid)
}

pt_char_tab_refids_nrsi <- function(refids) {
  study_char_dat |>
    filter(refid %in% refids) |>
    filter(design %in% c("prospect_coh", "retrospect_coh", "nr_trial", "quasi_exp", "crossover", "case_control")) |>
    pull(refid)
}

## next ----------------------------------------------- (2024-04-28 08:25) @----
