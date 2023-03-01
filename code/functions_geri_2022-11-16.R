# read files
read_file_mg <- function(filename){
  data_files |>
    filter(str_detect(value, filename)) |>
    arrange(desc(value)) |>
    slice(1)
}

# extract path
path_csv <- function(name_csv){
  sheet <- name_csv
  path <- str_c("data/", sheet)
  return(path)
}

# function to pull refids
kq_refids <- function(kq_id) {
  study_char_dat |>
    filter(!is.na({{kq_id}})) |>
    pull(refid)
}

# count of unique for variable
count_unique <- function(df, var){
  df |>
    select({{var}}) |>
    distinct() |>
    count() |>
    pull(n)
}

# 2 by 2 frequency table
proc_freq <- function(df, a, b){
  df %>%
    group_by({{a}}, {{b}}) %>%
    tally() %>%
    spread({{a}}, n) %>%
    gt::gt(.)
}

# convenience function to view record(s) for refid from dataset
view_rec <- function(data_set, refid_select) {
  data_set |>
    filter(refid == refid_select) |>
    janitor::remove_empty(which = "cols") |>
    t() |>
    as.data.frame() |>
    rownames_to_column(var = "variable") |>
    View()
}

## view_all function to page through all distiller forms
readkey <- function() {
  cat("[press [enter] to continue]")
  number <- scan(n = 1)
}

view_all <- function(refid){
  view_rec(study_char_dat, refid)
  readkey()
  view_rec(study_arm_dat, refid)
  readkey()
  view_rec(dichot_dat, refid)
  readkey()
  view_rec(contin_dat, refid)
  readkey()
  view_rec(likert_dat, refid)
}

# convenience function to view record(s) for refid from dataset
noview_rec <- function(data_set, refid_select) {
  data_set |>
    filter(refid == refid_select) |>
    janitor::remove_empty(which = "cols") |>
    t() |>
    as.data.frame() |>
    rownames_to_column(var = "variable")
}

# replace nonmissing with U00D7
notna_to_x <- function(variable, symbol) {
  # ifelse(!is.na(variable), "\U00D7", NA)
  ifelse(!is.na(variable), symbol, NA)
}

# capitalize 1st letter
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# convert columns guessed as logical to character
type_col <- function(data) {
  data %>%
    summary.default() |>
    as.data.frame() |>
    clean_names() |>
    filter(var2 == "Mode") |>
    rename(mode = freq, column = var1) |>
    select(-2)
    # mutate(
    #   mode = ifelse(mode == "logical", "character", mode),
    #   mode = str_extract(mode, "^\\w{1}")
    # ) |>
    # pull(mode) |>
    # str_c(collapse = "")
}

# gt style file
gt_theme_mg <- function(data) {
  data %>%
    opt_row_striping() |>
    opt_table_lines(extent = "none") |>
    tab_options(
      table.font.color = "black",
      data_row.padding = px(2),
      table.font.size = px(12),
      column_labels.font.size = px(12),
      table.align = "left",
      table_body.border.bottom.width = px(1.7),
      table.border.top.width = px(1.7),
      table_body.border.bottom.color = "#9A9EA1",
      table.border.top.color = "#9A9EA1",
      table_body.border.bottom.style = "solid",
      table.border.top.style = "solid",
      column_labels.border.bottom.color = "#9A9EA1",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = px(1.3),
      column_labels.font.weight = "bold",
      column_labels.padding = px(3),
      heading.align = "left",
      footnotes.padding = px(0),
      source_notes.padding = px(0),
    ) |>
    opt_horizontal_padding(scale = 2) |>
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_row_groups()
    ) |>
    opt_footnote_marks(marks = "letters") |>
    opt_css(
      css = "
      #one .gt_footnote_marks {
      font-style: normal;
      font-weight: normal;
      font-size: 75%;
      vertical-align: 0.4em;
      }
    "
    )
}

# simple figure and table caption labels (flexibility for html)
table_n <- 0
figure_n <- 0

table_ref <- function() {
  table_n <<- table_n + 1
  paste0("Table ", table_n, ". ")
}

figure_ref <- function() {
  figure_n <<- figure_n + 1
  paste0("Figure ", figure_n, ". ")
}

# create excel file of individual study records
library(openxlsx)

# function to create excel with pages for each refid
by_study_xlsx <- function(refids, kq_dat, name) {
  # record for worksheet
  view_xlsx <- function(data_set, refid_select) {
    data_set %>%
      filter(refid == refid_select) %>%
      janitor::remove_empty(which = "cols") %>%
      t() |>
      as.data.frame() %>%
      rownames_to_column(., var = "var_name")
  }

  wb <- createWorkbook(name)

  for (i in 1:length(refids)) {
    temp_sheet <- view_xlsx(kq_dat, refids[i])
    addWorksheet(wb, sheetName = refids[i])
    setColWidths(wb, i, cols = c(1:5), widths = c(rep(40, 5)))
    writeData(wb, sheet = i, temp_sheet)
  }

  path <- glue::glue("/Users/mgrant/Desktop/{name}.xlsx")
  saveWorkbook(wb, path, overwrite = TRUE)
}

# usage
# add design to tibble if not study characteristics file
# temp_dat <- left_join(study_arm_dat, study_char_dat |> select(refid, design_f), by = "refid") |>
#   relocate(design_f, .after = refid) |>
#   arrange(design_f)
# by_study_xlsx(kq5_refid, temp_dat, "kq5")

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
  reactable(
    outcome_dat,
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
        font_size = "11px",
      )
    ),
    columns = list(
      outcome = colDef(name = "Outcome", width = 200),
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

# select records for kq
data_kq <- function(data, refids) {
  data |>
    filter(refid %in% refids) |>
    arrange(design_f)
}

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
      "Delayed NCR" = "d_cog_delay",
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
    sub_values(values = c("0 (0)"), replacement = "—")
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
    rename(
      "6-minute walk" = "c_6mwd",
      "Delirium duration" = "c_delirium_dur",
      "Grip strength" = "c_handgrip",
      "Length of stay" = "c_los",
      "Opioid use" = "c_opioid",
      "Pulmonary function" = "c_pulmonary"
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
    sub_values(values = c("0 (0)"), replacement = "—")
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
    rename(
      "ADL" = "l_adl",
      "Delayed NCR" = "l_cogfunc",
      "Delirium"	= "l_delirium",
      "Complications" = "l_complications",
      "Depression/anxiety" = "l_depression",
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
    sub_values(values = c("0 (0)"), replacement = "—")
  # opt_footnote_marks(marks = "standard")
}

# refid not missing by outcome
refid_reported_outcome <- function(data_dat, vars){
  data_dat |>
    filter(if_any({{vars}}, ~ !is.na(.x))) |>
    select(refid) |>
    distinct() |>
    pull(refid)
}

refid_reported_outcome_other <- function(data_dat, vars, instrument, negate_flag = FALSE){
  data_dat |>
  filter(str_detect({{vars}}, instrument, negate = negate_flag)) |>
  select(refid) |>
  distinct() |>
  pull(refid)
}

# age column "age_table" for tables; column header md("Mean <u>Med</u> (SD) [Range] {IQR}")
# table_age_mn_med <- study_arm_dat |>
#   select(starts_with("age_"), arm_n, refid, arm_id) |>
#   age_for_tables() |>
#   select(refid, arm_id, age_table)

digit1 <- function(x){
  formatC(x, digits = 1, format = "f")
}

digit0 <- function(x){
  formatC(x, digits = 0, format = "f")
}

age_for_tables <- function(x) {
  mutate(x,
    age_sd = ifelse(is.na(age_sd) & !is.na(age_95l + age_95u), (age_95u - age_95l) / (1.96 * 2) * sqrt(arm_n), age_sd),
    age_table =
      case_when(
        !is.na(age_mean + age_sd) ~ paste0(digit1(age_mean), " (", digit1(age_sd), ")"),
        !is.na(age_mean + age_low + age_up) ~ paste0(digit1(age_mean), " [", digit0(age_low), "-", digit0(age_up), "]"),
        !is.na(age_med + age_low + age_up) ~ paste0("<u>",digit1(age_med), "</u>", " [", digit0(age_low), "-", digit0(age_up), "]"),
        !is.na(age_med + age_iqrl + age_iqru) ~ paste0("<u>", digit1(age_med), "</u>", " {", digit0(age_iqrl), "-", digit0(age_iqru), "}"),
        !is.na(age_mean) ~ as.character(digit1(age_mean)),
        !is.na(age_med) ~ paste0("<u>", digit1(age_med), "</u>"),
        .default = ""
      )
  )
}

# mean med uncertainty summary column header md("Mean <u>Med</u> (SD) [Range] {IQR}")
# example mmse1_dat <- mean_med_table(likert_dat, "mmse_", 1, 0)
mean_med_table <- function(data, variable_select, observation_n, digs = 0) {
  data |>
    select(starts_with(variable_select), refid, arm_id, arm_n) |>
    select(matches(paste0(observation_n, "$")), refid, arm_id, arm_n) |>
    select(!matches("diff"), refid, arm_id, arm_n) |>
    rename_with(~ gsub("95", "ci95", .x, fixed = TRUE)) |>
    rename_with(~ gsub(variable_select, "", .x, fixed = TRUE)) |>
    rename_with(~ str_replace(.x, "[1-4]", "")) |>
    mutate(
      sd = ifelse(is.na(sd) & !is.na(ci95l + ci95u), (ci95u - ci95l) / (1.96 * 2) * sqrt(arm_n), sd),
      sd_f = formatC(sd, digits = 1, format = "f"),
      table =
        case_when(
          !is.na(m + sd) ~ paste0(formatC(m, digits = digs, format = "f"), " (", sd_f, ")"),
          !is.na(m + rl + ru) ~ paste0(formatC(m, digits = digs, format = "f"), " [", formatC(rl, digits = digs, format = "f"), "-", formatC(ru, digits = digs, format = "f"), "]"),
          !is.na(med + rl + ru) ~ paste0("<u>", formatC(med, digits = digs, format = "f"), "</u>", " [", formatC(rl, digits = digs, format = "f"), "-", formatC(ru, digits = digs, format = "f"), "]"),
          !is.na(med + iqrl + iqru) ~ paste0("<u>", formatC(med, digits = digs, format = "f"), "</u>", " {", formatC(iqrl, digits = digs, format = "f"), "-", formatC(iqru, digits = digs, format = "f"), "}"),
          !is.na(m) ~ as.character(formatC(m, digits = digs, format = "f")),
          !is.na(med) ~ paste0("<u>", formatC(med, digits = digs, format = "f"), "</u>"),
          .default = ""
        )
    ) |>
    select(refid, arm_id, time, table) |>
    rename(!!paste0(variable_select, "table", observation_n) := table) |>
    rename(!!paste0(variable_select, "time", observation_n) := time)
}

# single time
# mean_med_table_single(study_arm_dat, "pre_mmse_", 0)
mean_med_table_single <- function(data, variable_select, digs = 1) {
  data |>
    select(starts_with(variable_select), refid, arm_id, arm_n) |>
    select(!matches("diff"), refid, arm_id, arm_n) |>
    rename_with(~ gsub("95", "ci95", .x, fixed = TRUE)) |>
    rename_with(~ gsub(variable_select, "", .x, fixed = TRUE)) |>
    rename_with(~ str_replace(.x, "[1-4]", "")) |>
    mutate(
      sd = ifelse(is.na(sd) & !is.na(ci95l + ci95u), (ci95u - ci95l) / (1.96 * 2) * sqrt(arm_n), sd),
      sd_f = formatC(sd, digits = 1, format = "f"),
      table =
        case_when(
          !is.na(m + sd) ~ paste0(formatC(m, digits = digs, format = "f"), " (", sd_f, ")"),
          !is.na(m + rl + ru) ~ paste0(formatC(m, digits = digs, format = "f"), " [", formatC(rl, digits = digs, format = "f"), "-", formatC(ru, digits = digs, format = "f"), "]"),
          !is.na(med + rl + ru) ~ paste0("<u>", formatC(med, digits = digs, format = "f"), "</u>", " [", formatC(rl, digits = digs, format = "f"), "-", formatC(ru, digits = digs, format = "f"), "]"),
          !is.na(med + iqrl + iqru) ~ paste0("<u>", formatC(med, digits = digs, format = "f"), "</u>", " {", formatC(iqrl, digits = digs, format = "f"), "-", formatC(iqru, digits = digs, format = "f"), "}"),
          !is.na(m) ~ as.character(formatC(m, digits = digs, format = "f")),
          !is.na(med) ~ paste0("<u>", formatC(med, digits = digs, format = "f"), "</u>"),
          .default = ""
        )
    ) |>
    select(refid, arm_id, table) |>
    rename(!!paste0(variable_select) := table)
}

# proportion bar for gt
bar_chart <- function(label, height = "14px", fill = "#00bfc4", background = "white") {
  bar <- glue::glue("<div style='background:{fill};width:{label}%;height:{height};'></div>")
  chart <- glue::glue("<div style='flex-grow:1;margin-left:2px;margin-right:2px;background:{background};'>{bar}</div>")
  glue::glue("<div style='display:flex;align-items:left';>{chart}</div>") |>
    gt::html()
}

bar_prop <- function(proportion, fill_color, background_color = "#EAECEE") {
  purrr::map(proportion, ~ bar_chart(label = .x, fill = fill_color, background = background_color))
}

# bar_prop_select <- function(selector, proportion, fill_color, background = "#d2d2d2") {
#   ifelse(selector,
#     purrr::map(proportion, ~ bar_chart(label = .x, fill = fill_color, background = background)),
#     purrr::map(proportion, ~ bar_chart(label = .x, fill = fill_color, background = background))
#   )
# }
