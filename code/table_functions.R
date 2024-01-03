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

  tblhelpr::transpose_tibble(temp, col_names = rank, id_col = "outcomes") |>
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

## outcome priority tables ---------------------------- (2023-03-02 11:50) @----
# footnote designs for tables
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
      "DNCR/PND" = "d_cog_delay",
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
    tab_footnote(paste0(add_footnote, foot_out_freq(data), ".")) |>
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
      "DNCR/PND" = "l_cogfunc",
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
    select(refid, refid_c, year, arm_id, design_f_lab, study, study_l, arm_n, delitotal_time:delitotal_95high, deli_cam:deli_scale_otherspec) |>
    filter(!is.na(delitotal_n)) |>
    filter(refid %in% refids) |>
    remove_empty(which = "cols") |>
    left_join(drugs_dat |> select(refid, arm_id, drug_recode_abbr), by = c("refid", "arm_id")) |>
    relocate(drug_recode_abbr, .after = arm_n) |>
    mutate(
      across(starts_with("deli_"), ~ str_remove_all(.x, "scale_")),
      across(starts_with("deli_"), ~ str_remove_all(.x, "deli_")),
      across(deli_cam:deli_scale_icdsc, ~ toupper(.x)),
    ) |>
    rename_with(~ gsub("scale_", "", .x, fixed = TRUE)) |>
    rename_with(~ gsub("deli_", "", .x, fixed = TRUE)) |>
    left_join(delirium_rr_ref, by = c("refid_c", "arm_id")) |>
    mutate(
      other = ifelse(str_detect(otherspec, "AMT"), "AMT", other),
      other = ifelse(str_detect(otherspec, "Psychiatrist consultation"), "Psych", other),
      other = ifelse(str_detect(otherspec, "Chinese"), "Note", other),
      other = ifelse(other == "other", NA_character_ , other),
      calc_percent = delitotal_n/arm_n * 100,
      n_percent = n_per_fun(delitotal_n, arm_n, 1),
      rr_ci = ifelse(!is.na(ref_arm_n), rr_ci_fun(delitotal_n, arm_n, ref_deli_n, ref_arm_n), "—")
    ) |>
    relocate(calc_percent, .after = delitotal_perc) |>
    unite(scale_delirium, cam:other, remove = TRUE, sep = "/", na.rm = TRUE) |>
    mutate(scale_delirium = ifelse(scale_delirium == "unspec", "NS", scale_delirium)) |>
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
      drug_recode = md("Drug"),
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
      arm_n ~ px(45),
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
    select(-refid)
  rob_summary(rob_temp_dat, tool = "ROB2", colour = "colourblind", weighted = TRUE, Weight = weight)
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

# grade footnote
kq3_balance_main <- function() {
  high  <- '<span><span class="quality-sign">⨁⨁⨁⨁</span>'
  mod   <- '<span><span class="quality-sign">⨁⨁⨁◯</span>'
  low   <- '<span><span class="quality-sign">⨁⨁◯◯</span>'
  vlow  <- '<span><span class="quality-sign">⨁◯◯◯</span>'
  low_very  <- paste(low, vlow, sep = "<br/>")
  grade_foot <- paste0("Very low: ", vlow, "; Low: ", low, "; Moderate: ", mod, "; High: ", high, ".")

  reg_gen_dat <- readxl::read_excel("data/balance_tables_2023-09-14_mac_mg.xlsx", sheet = "RegionalGeneral", range = "B4:L21") |>
  remove_empty(which = "cols") |>
  clean_names() |>
  rename(est = estimate_95_percent_ci) |>
  # filter(!if_all(rct:est, ~ is.na(.x))) |>
  mutate(
    across(everything(), ~ str_remove(.x, "‡|†|\\*")),
    grade = case_when(
      grade == "Very low" ~ vlow,
      grade == "Low" ~ low,
      grade == "Moderate" ~ mod,
      grade == "High" ~ high,
      grade == "Low/very low" ~ low_very,
      .default = grade
    ),
    event_e = ifelse(!is.na(n_reg), paste0(events_reg, " (", n_reg, ")"), NA),
    event_c = ifelse(!is.na(n_gen), paste0(events_gen, " (", n_gen, ")"), NA),
    across(c(event_e, event_c), ~ str_remove(.x, "NA "))
  ) |>
  relocate(c(event_e, event_c), .after = rct) |>
  select(-c(events_reg:n_gen, grade_2))

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
  tab_spanner(label = "Regional", columns = c(event_e), level = 1) |>
  tab_spanner(label = "General", columns = c(event_c), level = 1) |>
  opt_footnote_marks(marks = "standard") |>
  tab_style(style = cell_text(align = "center"),      locations = cells_column_labels(columns = c(event_c, event_e, grade))) |>
  tab_style(style = cell_text(align = "right"),       locations = cells_column_labels(columns = c(rct))) |>
  tab_style(style = cell_text(align = "right"),       locations = cells_body(columns = c(rct, event_e, event_c))) |>
  tab_style(style = cell_text(align = "left"),        locations = cells_body(columns = c(measure, est))) |>
  tab_style(style = cell_text(align = "center"),      locations = cells_body(columns = c(grade))) |>
  tab_style(style = cell_text(indent = px(10)),       locations = cells_body(columns = c(outcome), rows = outcome %in% c("All procedures", "Hip fracture", "Other"))) |>
  tab_footnote(md("RCT: randomized clinical trial; [GRADE: Grades of Recommendation, Assessment, Development, and Evaluation](soe_gt.html#grade); RR: risk ratio; RD/1000: risk difference per 1000.")) |>
  # tab_footnote(md("RCT: randomized clinical trial; GRADE: Grades of Recommendation, Assessment, Development, and Evaluation; RR: risk ratio; RD/1000: risk difference per 1000.")) |>
  tab_footnote(md(grade_foot), locations = cells_column_labels(columns = grade)) |>
  tab_footnote("Studies reported 0 and 2 events.", locations = cells_body(columns = c(est), rows = outcome == "Cardiac arrest"), placement = "right") |>
  # tab_footnote(md("[Comparing higher/highest category or categories with lower ones.](kq3.html#patient-satisfaction)"), locations = cells_body(columns = c(est), rows = outcome == "Patient satisfaction"), placement = "right") |>
  tab_footnote(md("Comparing higher/highest category or categories with lower ones."), locations = cells_body(columns = c(est), rows = outcome == "Patient satisfaction"), placement = "right") |>
  tab_footnote("Complications reported variously across the 17 trials. ", locations = cells_body(columns = c(outcome), rows = outcome == "Complications"), placement = "right") |>
  tab_footnote("Hip fracture trials RR 1.03 (0.77–1.32); others RR 0.90 (0.62–1.29).", locations = cells_body(columns = c(est), rows = outcome == "Delirium"), placement = "right")
}

kq3_complications <- function(){
  high  <- '<span><span class="quality-sign">⨁⨁⨁⨁</span>'
  mod   <- '<span><span class="quality-sign">⨁⨁⨁◯</span>'
  low   <- '<span><span class="quality-sign">⨁⨁◯◯</span>'
  vlow  <- '<span><span class="quality-sign">⨁◯◯◯</span>'
  low_very  <- paste(low, vlow, sep = "<br/>")
  grade_foot <- paste0("Very low: ", vlow, "; Low: ", low, "; Moderate: ", mod, "; High: ", high, ".")

reg_gen_dat <- readxl::read_excel("data/balance_tables_2023-09-14_mac_mg.xlsx", sheet = "RegionalGeneral", range = "B33:L50") |>
  remove_empty(which = "cols") |>
  clean_names() |>
  rename(est = estimate_95_percent_ci) |>
  filter(!if_all(rct:est, ~ is.na(.x))) |>
  mutate(
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
    event_e = ifelse(!is.na(n_reg), paste0(events_reg, " (", n_reg, ")"), NA),
    event_c = ifelse(!is.na(n_gen), paste0(events_gen, " (", n_gen, ")"), NA),
    across(c(event_e, event_c), ~ str_remove(.x, "NA "))
  ) |>
  relocate(c(event_e, event_c), .after = rct) |>
  select(-c(events_reg:n_gen, grade_2))

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
    rct     ~ px(40),
    # n       ~ px(65),
    event_e ~ px(100),
    event_c ~ px(100),
    grade   ~ px(120),
    measure ~ px(70),
    est     ~ px(160)
  ) |>
  sub_missing(columns = everything(), missing_text = "") |>
  tab_spanner(label = "Regional", columns = c(event_e), level = 1) |>
  tab_spanner(label = "General", columns = c(event_c), level = 1) |>
  opt_footnote_marks(marks = "standard") |>
  tab_style(style = cell_text(align = "center"),      locations = cells_column_labels(columns = c(event_c, event_e, grade))) |>
  tab_style(style = cell_text(align = "right"),       locations = cells_column_labels(columns = c(rct))) |>
  tab_style(style = cell_text(align = "right"),       locations = cells_body(columns = c(rct, event_e, event_c))) |>
  tab_style(style = cell_text(align = "left"),        locations = cells_body(columns = c(measure, est))) |>
  tab_style(style = cell_text(align = "center"),      locations = cells_body(columns = c(grade))) |>
  tab_footnote(md("RCT: randomized clinical trial; [GRADE: Grades of Recommendation, Assessment, Development, and Evaluation](soe_gt.html#grade); RR: risk ratio; RD/1000: risk difference per 1000.")) |>
  # tab_footnote(md("RCT: randomized clinical trial; GRADE: Grades of Recommendation, Assessment, Development, and Evaluation; RR: risk ratio; RD/1000: risk difference per 1000.")) |>
  tab_footnote("Fixed/common effects models when 2 studies.", locations = cells_column_labels(columns = measure)) |>
  tab_footnote(md(grade_foot), locations = cells_column_labels(columns = grade)) |>
  # tab_footnote("Complications as reported included bradycardia (4 studies); unspecified (2 studies); tachycardia (1 study); myocardial infarction, heart failure, or new onset arrhythmia (1 study).", locations = cells_body(columns = c(outcome), rows = outcome == "Cardiac complications")) |>
  # tab_footnote("Studies reported 0 and 2 events.", locations = cells_body(columns = c(est), rows = outcome == "Cardiac arrest"), placement = "right") |>
  tab_footnote(md("[Comparing higher/highest category or categories compared with lower ones.](kq3.html#patient-satisfaction)"), locations = cells_body(columns = c(est), rows = outcome == "Patient satisfaction"), placement = "right") |>
  # tab_footnote(md("Comparing higher/highest category or categories compared with lower ones."), locations = cells_body(columns = c(est), rows = outcome == "Patient satisfaction"), placement = "right") |>
  tab_footnote("Cardiovascular, pulmonary, and acute kidney injury.", locations = cells_body(columns = c(outcome), rows = outcome == "Complications")) |>
  tab_footnote("Complications reported variously across the 17 trials. ", locations = cells_body(columns = c(rct), rows = outcome == "Complications"), placement = "right") |>
  tab_footnote("One study no events; the other two. ", locations = cells_body(columns = c(est), rows = outcome == "cardiac arrest"), placement = "right") |>
  tab_footnote("Complications reported included pneumonia, respiratory failure, or unspecified.", locations = cells_body(columns = c(outcome), rows = outcome == "Pulmonary complications"))
}

