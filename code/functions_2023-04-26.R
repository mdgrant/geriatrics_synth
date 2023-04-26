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
    filter(!is.na({{ kq_id }})) |>
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

tab_lst <- function(data, vars){
  data %>%
    select({{vars}}) %>%
    map(~ tabyl(.))
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

## view_all function to page through all distiller forms; uses readkey
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

# replace nonmissing with U00D7
notna_to_x <- function(variable, symbol) {
  # ifelse(!is.na(variable), "\U00D7", NA)
  ifelse(!is.na(variable), symbol, NA)
}

# format to n (percent)
# n_per_fun(9, 28, 1)
n_per_fun <- function(events_n, total, n_sig_dig){
  str_c(events_n," (", formatC(events_n/total * 100, digits = n_sig_dig, format = "f"), ")")
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

# simple figure and table caption labels (flexibility for html)
table_n <- 0
figure_n <- 0

table_ref <- function() {
  table_n <<- table_n + 1
  paste0("Table ", table_n, ". ")
}

anchor_table_ref <- function() {
   table_n + 1
}

# `r paste0("tab", knitr::current_input(), anchor_table_ref())`

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

# select refids for variable str_detect
refid_detect_select_fun <- function(data_select, var_select, string_select) {
  data_select |>
    filter(str_detect({{ var_select }}, string_select)) |>
    select(refid) |>
    distinct() |>
    pull(refid)
}

## by_study_xlsx usage -------------------------------- (2023-03-06 22:52) @----
# add design to tibble if not study characteristics file
# temp_dat <- left_join(study_arm_dat, study_char_dat |> select(refid, design_f), by = "refid") |>
#   relocate(design_f, .after = refid) |>
#   arrange(design_f)
# by_study_xlsx(kq5_refid, temp_dat, "kq5")

# select records for kq
data_kq <- function(data, refids) {
  data |>
    filter(refid %in% refids) |>
    arrange(design_f)
}

# refid not missing by outcome
refid_reported_outcome <- function(data_dat, vars) {
  data_dat |>
    filter(if_any({{ vars }}, ~ !is.na(.x))) |>
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

## age column "age_table" for tables ------------------ (2023-03-06 22:52) @----
# age column "age_table" for tables; column header md("Mean <u>Med</u> (SD) [Range] {IQR}")
# table_age_mn_med <- study_arm_dat |>
#   select(starts_with("age_"), arm_n, refid, arm_id) |>
#   age_for_tables() |>
#   select(refid, arm_id, age_table)

digit0 <- function(x){
  formatC(x, digits = 0, format = "f")
}

digit1 <- function(x){
  formatC(x, digits = 1, format = "f")
}

digit2 <- function(x){
  formatC(x, digits = 2, format = "f")
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

los_for_tables <- function(x) {
  mutate(x,
         los_sd = ifelse(is.na(los_sd) & !is.na(los_95l + los_95u), (los_95u - los_95l) / (1.96 * 2) * sqrt(arm_n), los_sd),
         los_table =
           case_when(
             !is.na(los_m + los_sd) ~ paste0(digit1(los_m), " (", digit1(los_sd), ")"),
             !is.na(los_m + los_rl + los_ru) ~ paste0(digit1(los_m), " [", digit0(los_rl), "-", digit0(los_ru), "]"),
             !is.na(los_med + los_rl + los_ru) ~ paste0("<u>",digit1(los_med), "</u>", " [", digit0(los_rl), "-", digit0(los_ru), "]"),
             !is.na(los_med + los_iqrl + los_iqru) ~ paste0("<u>", digit1(los_med), "</u>", " {", digit0(los_iqrl), "-", digit0(los_iqru), "}"),
             !is.na(los_m) ~ as.character(digit1(los_m)),
             !is.na(los_med) ~ paste0("<u>", digit1(los_med), "</u>"),
             .default = ""
           )
  ) |>
    select(los_table)
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
          !is.na(m + sd) ~ paste0(formatC(m, digits = digs + 1, format = "f"), " (", sd_f, ")"),
          !is.na(m + rl + ru) ~ paste0(formatC(m, digits = digs + 1, format = "f"), " [", formatC(rl, digits = digs, format = "f"), "-", formatC(ru, digits = digs, format = "f"), "]"),
          !is.na(med + rl + ru) ~ paste0("<u>", formatC(med, digits = digs, format = "f"), "</u>", " [", formatC(rl, digits = digs, format = "f"), "-", formatC(ru, digits = digs, format = "f"), "]"),
          !is.na(med + iqrl + iqru) ~ paste0("<u>", formatC(med, digits = digs, format = "f"), "</u>", " {", formatC(iqrl, digits = digs, format = "f"), "-", formatC(iqru, digits = digs, format = "f"), "}"),
          !is.na(m) ~ as.character(formatC(m, digits = digs + 1, format = "f")),
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
          !is.na(m + sd) ~ paste0(formatC(m, digits = digs + 1, format = "f"), " (", sd_f, ")"),
          !is.na(m + rl + ru) ~ paste0(formatC(m, digits = digs + 1, format = "f"), " [", formatC(rl, digits = digs, format = "f"), "-", formatC(ru, digits = digs, format = "f"), "]"),
          !is.na(med + rl + ru) ~ paste0("<u>", formatC(med, digits = digs, format = "f"), "</u>", " [", formatC(rl, digits = digs, format = "f"), "-", formatC(ru, digits = digs, format = "f"), "]"),
          !is.na(med + iqrl + iqru) ~ paste0("<u>", formatC(med, digits = digs, format = "f"), "</u>", " {", formatC(iqrl, digits = digs, format = "f"), "-", formatC(iqru, digits = digs, format = "f"), "}"),
          !is.na(m) ~ as.character(formatC(m + 1, digits = digs + 1, format = "f")),
          !is.na(med) ~ paste0("<u>", formatC(med, digits = digs, format = "f"), "</u>"),
          .default = ""
        )
    ) |>
    select(refid, arm_id, table) |>
    rename(!!paste0(variable_select) := table)
}

# proportion bar for gt
bar_chart <- function(label, height = "11px", fill = "#00bfc4", background = "white") {
  bar <- glue::glue("<div style='background:{fill};width:{label}%;height:{height};'></div>")
  chart <- glue::glue("<div style='flex-grow:1;margin-left:2px;margin-right:2px;background:{background};'>{bar}</div>")
  glue::glue("<div style='display:flex;align-items:left';>{chart}</div>") |>
    gt::html()
}

bar_prop <- function(proportion, fill_color, background_color = "#EAECEE") {
  purrr::map(proportion, ~ bar_chart(label = .x, fill = fill_color, background = background_color))
}

## bar_prop_select ------------------------------------ (2023-03-06 22:52) @----
# bar_prop_select <- function(selector, proportion, fill_color, background = "#d2d2d2") {
#   ifelse(selector,
#     purrr::map(proportion, ~ bar_chart(label = .x, fill = fill_color, background = background)),
#     purrr::map(proportion, ~ bar_chart(label = .x, fill = fill_color, background = background))
#   )
# }


## rr_ci_fun ------------------------------------------ (2023-03-06 22:53) @----
# calculate relative risk, ci, and format no refid
rr_ci_fun <- function(event1, n1, event2, n2, digits = 2) {
  a <- meta::metabin(event1, n1, event2, n2, sm = "RR")
  with(a, paste0(
    sprintf(paste0("%.", digits, "f"), round(exp(TE), digits)), " (",
    sprintf(paste0("%.", digits, "f"), round(exp(lower), digits)), "-",
    sprintf(paste0("%.", digits, "f"), round(exp(upper), digits)), ")"))
}

## rd_ci_fun ------------------------------------------ (2023-03-06 22:53) @----
# calculate risk difference, ci, and format no refid
rd_per_ci_fun <- function(event1, n1, event2, n2, digits = 2) {
  a <- meta::metabin(event1, n1 , event2, n2, sm = "RD")
  with(a, paste0(
    sprintf(paste0("%.", digits, "f"), round(TE * 100, digits)), "% (",
    sprintf(paste0("%.", digits, "f"), round(lower * 100, digits)), ", ",
    sprintf(paste0("%.", digits, "f"), round(upper * 100 , digits)), ")"))
}

## collapse arms -------------------------------------- (2023-03-09 07:19) @----
# collapse_arms_dichot(dichot_dat, "Lee 2018a", c(2, 3), delitotal_n, arm_n, "den")
# collapse_dichot(dichot_dat, "Lee 2018a", c(2, 3), delitotal_n)
# collapse_dichot(dichot_dat, "Lee 2018a", c(2, 3), arm_n)
collapse_dichot <- function(data_dat, study, arms, variable) {
  data_dat |>
    filter(study == {{ study }} & arm_id %in% {{ arms }}) |>
    summarize(variable_n = sum({{ variable }})) |>
    pull(variable_n)
}

# make base graphs pretty
par(bty = "n", xaxt = "l", yaxt = "l")


## proportion primary or secondary outcome ------------ (2023-04-01 09:58) @----
prim_sec_out_fun <- function(outcome, refids) {
  temp <- study_char_dat |>
    filter(refid %in% refids) |>
    mutate(percent = digit1((mean(!is.na({{ outcome }})) * 100))) |>
    select(percent) |>
    distinct() |>
    pull()
  paste0(temp, "%")
}


## total for soe table from pariwise ------------------ (2023-04-10 10:03) @----
total_meta <- function(data_meta) {
  data_meta |> summarize(total = sum(n1 + n2))
}

## refids from meta to clipboard ---------------------- (2023-04-12 09:35) @----
refid_meta_fun <- function(data_meta){
  temp <- data_meta |>
    select(refid) |>
    distinct() |>
    pull(refid) |>
    toString()

  clipr::write_clip(temp)
  return(temp)
}

## gt style file --------------------------------------- (2023-04-10 10:03) @----
gt_theme_mg <- function(data) {
  data %>%
    opt_row_striping() |>
    opt_table_lines(extent = "none") |>
    # opt_table_font(
    #   font = list(
    #     google_font(name = "Source Sans Pro")
    #   )
    # ) |>
    # opt_table_font(stack = "Source Sans Pro") |>
    tab_options(
      table.font.color = "black",
      table.font.names = "Source Sans Pro",
      data_row.padding = px(2),
      table.font.size = px(13), # ?12
      column_labels.font.size = px(13),  # ?12
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
      footnotes.font.size = px(12),
      source_notes.padding = px(0),
    ) |>
    opt_horizontal_padding(scale = 2) |>
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_row_groups()
    ) |>
    opt_footnote_marks(marks = "letters") |>
    opt_footnote_spec(spec_ref = "^x", spec_ftr = "^x") |>
  opt_css(
    css = "
    #one .gt_footnote_marks {
    font-style: normal;
    font-weight: normal;
    font-size: 85%;
    vertical-align: 0px;
    }
  "
  )
}
