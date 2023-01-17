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

# convenience function to view record(s) for refid from dataset
noview_rec <- function(data_set, refid_select) {
  data_set |>
    filter(refid == refid_select) |>
    janitor::remove_empty(which = "cols") |>
    t() |>
    as.data.frame() |>
    rownames_to_column(var = "variable")
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
      heading.align = "left"
    ) |>
    opt_horizontal_padding(scale = 2) |>
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_row_groups()
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

# get ranking data and munge to usable
rankings <- function(key_question){
  # key_question "KQ1" "KQ2" ...

  read_by_kq <- function(kq, range){
    read_outcome_dat <- function(cells){readxl::read_xlsx("data/OutcomeRankingRawData_2022-09-06.xlsx", range = cells, sheet = "data")}
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

  rankings <- function(priority){
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

# outcome priority table
outcome_tab <- function(outcome_dat) {
  reactable(
    outcome_dat,
    pagination = FALSE,
    highlight = TRUE,
    defaultSorted = "any_top_5",
    defaultSortOrder = "desc",
    defaultColDef = colDef(
      cell = data_bars(outcome_dat,
        box_shadow = TRUE,
        bar_height = 12,
        max_value = 10
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
          box_shadow = TRUE,
          bar_height = 12,
          max_value = 10,
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

  paste0("ADL: activities of daily living; ", foot_labels, ".")
}

# dichot freq outcomes
dichot_freq_fun <- function(data) {
  # for footnote
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
      "Cognitive delay" = "d_cog_delay",
      "Delirium duration" = "d_deli_duration",
      "Discharge location" = "d_disch_location",
      "Opioid use" = "d_opioid",
      "Quality of recovery" = "d_qor",
      "ADL" = "d_adl",
      "Readmission" = "d_readmit",
      "Mortality" = "d_mortality",
      "Delirium" = "d_delirium",
      "Depression" = "d_depression",
      "Comfort" = "d_comfort",
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
    as_gt(id = "one") |>
    cols_width(
      1 ~ "120px",
      everything() ~ "120px") |>
    gt_theme_mg() |>
    tab_options(footnotes.marks = "letters") |>
    tab_source_note(foot_out_freq(data))
  # opt_footnote_marks(marks = "standard")
}

# contin freq outcomes
contin_freq_fun <- function(data) {
  data |>
    select(refid, design_f_abbrev, c_6mwd:c_pulmonary) |>
    mutate(across(c_6mwd:c_pulmonary, ~ !is.na(.x))) |>
    group_by(refid) |>
    mutate(
      across(c_6mwd:c_pulmonary, ~ sum(.x) != 0)
    ) |>
    ungroup() |>
    rename(
      "6-Minute walk" = "c_6mwd",
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
    as_gt(id = "one") |>
    cols_width(
      1 ~ "120px",
      everything() ~ "120px") |>
    gt_theme_mg() |>
    tab_options(footnotes.marks = "letters")|>
    tab_source_note(foot_out_freq(data))
  # opt_footnote_marks(marks = "standard")
}

# likert freq outcomes
likert_freq_fun <- function(data) {
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
      "Cognitive function" = "l_cogfunc",
      "Complications" = "l_complications",
      "Mental status" = "l_mental",
      "Pain" = "l_pain",
      "Quality of life" = "l_qol",
      "Quality of recovery" = "l_qor",
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
    as_gt(id = "one") |>
    cols_width(
      1 ~ "120px",
      everything() ~ "120px") |>
    gt_theme_mg() |>
    tab_options(footnotes.marks = "letters")|>
    tab_source_note(foot_out_freq(data))
  # opt_footnote_marks(marks = "standard")
}
