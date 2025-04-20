## NOTE
# These functions are either used in the analyses or allow including results externally
# with minimal manual steps (eg, storing a formatted result in the clipboard that can be pasted).

## DATA MANIPULATION !!----
## replace nonmissing with U00D7 ---------------------- (2024-10-31 10:19) @----
notna_to_x <- function(variable, symbol = "\U00D7") {
  ifelse(!is.na(variable), symbol, NA)
}

## replace missing with U00D7 ------------------------- (2024-10-31 10:19) @----
na_to_x <- function(variable, symbol = "\U00D7") {
  ifelse(is.na(variable), symbol, NA)
}

## %notin% operator ----------------------------------- (2024-10-31 10:19) @----
`%notin%` <- Negate(`%in%`)

## format to n (percent) ------------------------------ (2024-10-31 10:21) @----
# n_per_fun(9, 28, 1)
n_per_fun <- function(events_n, total, n_sig_dig = 1){
  temp <- str_c(format(events_n, big.mark = ",")," (", formatC(events_n/total * 100, digits = n_sig_dig, format = "f"), ")")
  temp <- str_replace(temp, "100.0", "100")
  temp <- str_replace(temp, "\\(0.0\\)", "(0)")
}

## capitalize 1st letter ------------------------------ (2024-10-31 10:23) @----
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

## lower case 1st letter ------------------------------ (2024-10-31 10:23) @----
firstlower <- function(x) {
  substr(x, 1, 1) <- tolower(substr(x, 1, 1))
  x
}

## convert columns guessed as logical to character ---- (2024-10-31 10:24) @----
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

## select refids for variable str_detect -------------- (2024-10-31 07:47) @----
# ket_mort_refid <- refid_detect_select_fun(mortality_dat, drug_recode_abbr, "Ket")
refid_detect_select_fun <- function(data_select, var_select, string_select) {
  data_select |>
    filter(str_detect({{ var_select }}, string_select)) |>
    select(refid) |>
    distinct() |>
    pull(refid)
}

## cochrane function to combine arms ------------------ (2024-10-31 07:46) @----
combine_contin <- function(n_1, n_2, x_1, x_2, sd_1, sd_2){
  n_comb <- n_1 + n_2
  x_comb <-  (n_1 * x_1 + n_2 * x_2)/(n_comb)
  sd_comb <- sqrt(((n_1 - 1) * sd_1^2 + (n_2 - 1) * sd_2^2 + (n_1 * n_2)/(n_comb) * (x_1 - x_2)^2) / (n_comb - 1))
  c(n_comb, x_comb, sd_comb)
}

## calculate SD for groups from p value, assuming equal (2024-10-31 07:46) @----
sd_bwgrp_fun <- function(m1, m2, n1, n2, pVal) {
  sd2 <- abs((m2 - m1) / qt(pVal / 2, n1 + n2 - 2)) * sqrt(n2 * n1 / (n2 + n1))
  sd1 <- sd2
  print(c(n1, m1, sd1, n2, m2, sd2))
}

# output only sd
sd_bwgrp_sdonly_fun <- function(m1, m2, n1, n2, pVal) {
  sd2 <- abs((m2 - m1) / qt(pVal / 2, n1 + n2 - 2)) * sqrt(n2 * n1 / (n2 + n1))
  sd2
}

## select records for kq ------------------------------ (2024-11-01 08:46) @----
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

## rr_ci_fun ------------------------------------------ (2023-03-06 22:53) @----
# calculate relative risk, ci, and format no refid
rr_ci_fun <- function(event1, n1, event2, n2, digits = 2) {
  a <- meta::metabin(event1, n1, event2, n2, sm = "RR", method = "MH", method.random.ci = "classic")
  rr_ci <- with(a, paste0(
    sprintf(paste0("%.", digits, "f"), round(exp(TE), digits)), " (",
    sprintf(paste0("%.", digits, "f"), round(exp(lower), digits)), "-",
    sprintf(paste0("%.", digits, "f"), round(exp(upper), digits)), ")"))
  ifelse(event1 == 0 | event2 == 0, "Not estimated", rr_ci)
}

## or_ci_fun ------------------------------------------ (2023-03-06 22:53) @----
# calculate relative risk, ci, and format no refid
or_ci_fun <- function(event1, n1, event2, n2, digits = 2) {
  a <- meta::metabin(event1, n1, event2, n2, sm = "OR", method = "MH", method.random.ci = "classic")
  with(a, paste0(
    sprintf(paste0("%.", digits, "f"), round(exp(TE), digits)), " (",
    sprintf(paste0("%.", digits, "f"), round(exp(lower), digits)), "-",
    sprintf(paste0("%.", digits, "f"), round(exp(upper), digits)), ")"))
}

or_ln_te_fun <- function(event1, n1, event2, n2) {
  meta::metabin(event1, n1, event2, n2, sm = "OR", method = "MH", method.random.ci = "classic")$TE
}

or_ln_se_fun <- function(event1, n1, event2, n2, digits = 2) {
  meta::metabin(event1, n1, event2, n2, sm = "OR", method = "MH", method.random.ci = "classic")$seTE
}

## se from confint of est
se_ln_ci_fun <- function(low, high) {
  se <- (abs(log(high) - log(low)) / 3.92)
  se
}

## se from confint of est
se_ci_fun <- function(low, high) {
  se <- (abs(high - low) / 3.92)
  se
}

## smd_ci_function ------------------------------------ (2023-07-13 13:34) @----
smd_ci <- function(n1, mean1, sd1, n2, mean2, sd2, digits = 2){
  a <- meta::metacont(n.e = n1, mean.e = mean1, sd.e = sd1, n.c = n2, mean.c = mean2, sd.c = sd2, sm = "SMD")
  a <- with(a, paste0(
    sprintf(paste0("%.", digits, "f"), round(TE, digits)), " (",
    sprintf(paste0("%.", digits, "f"), round(lower, digits)), " to ",
    sprintf(paste0("%.", digits, "f"), round(upper, digits)), ")"))
  ifelse(!grepl("^-", a), paste0(" ", a), a)
}

## format_est_ci_fun ---------------------------------- (2023-05-13 10:20) @----
# format an est with 95% CI
format_est_ci_fun <- function(est, low, high, digits = 2) {
  est_ci <- paste0(
    sprintf(paste0("%.", digits, "f"), round(est, digits)), " (",
    sprintf(paste0("%.", digits, "f"), round(low, digits)), "-",
    sprintf(paste0("%.", digits, "f"), round(high, digits)), ")"
  )
  # est_ci <- ifelse(print_95_per == TRUE, str_replace(est_ci, "\\(", "\\(95% CI, "), est_ci)
  est_ci
}

## rd_ci_fun ------------------------------------------ (2023-03-06 22:53) @----
# calculate risk difference, ci, and format no refid
rd_per_ci_fun <- function(event1, n1, event2, n2, digits = 2) {
  a <- meta::metabin(event1, n1 , event2, n2, sm = "RD", method = "MH", method.random.ci = "classic")
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

collapse_dichot_study_compl <- function(data_dat, study_compl, arms, variable) {
  data_dat |>
    filter(study_compl == {{ study_compl }} & arm_id %in% {{ arms }}) |>
    summarize(variable_n = sum({{ variable }})) |>
    pull(variable_n)
}

# make base graphs pretty
par(bty = "n", xaxt = "l", yaxt = "l")



## proportion primary or secondary outcome ------------ (2023-04-01 09:58) @----
# used selectively in figure footnotes
prim_sec_out_fun <- function(outcome, refids) {
  temp <- study_char_dat |>
    filter(refid %in% refids) |>
    mutate(percent = digit1((mean(!is.na({{ outcome }})) * 100))) |>
    select(percent) |>
    distinct() |>
    pull()
  paste0(temp, "%")
}

## from tblhelper ------------------------------------- (2023-12-27 10:50) @----
tibble_to_matrix <- function(tbl, ..., row_names = NULL) {
  cols <- rlang::enquos(...)
  mat <- as.matrix(dplyr::select(tbl, !!!cols))
  if (!is.null(row_names)) {
    if (length(row_names) == 1 & row_names[1] %in% colnames(tbl)) {
      row_names <- tbl[[row_names]]
    }
    rownames(mat) <- row_names
  }
  return(mat)
}

transpose_tibble <- function(tbl, col_names, id_col = "columns") {
  col_names <- rlang::enquo(col_names)
  tibble_to_matrix(tbl, -!!col_names,
                   row_names = dplyr::pull(tbl, !!col_names)
  ) %>%
    t() %>%
    dplyr::as_tibble(rownames = id_col) %>%
    return()
}

## FOR TABLES !!----
## gt style file -------------------------------------- (2023-04-10 10:03) @----
gt_theme_mg <- function(data) {
  data %>%
    # opt_row_striping() |>
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
      # row.striping.background_color = NULL,
      # row.striping.include_stub = NULL,
      row.striping.include_table_body = TRUE,
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

vert_lab_fun <- function(label){
  paste0("<vertical-text>", label, "</vertical-text>")
}

## first row by refid only ---------------------------- (2024-10-31 07:46) @----
# shortens code to only list study in table in 1st row only
# in place of "study_l = ifelse(row_number() > 1, "", study_l)"
# used only in kq5 (later part of project)
first_row <- function(variable){
  ifelse(row_number() > 1, "", as.character(variable))
}

## los for tables ------------------------------------- (2024-11-01 08:46) @----
# pull los mean or median and sd, iqr, or range and formate for tables as Mean <u>Med</u> (SD) [Range] {IQR}
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

## mean_med_table ------------------------------------- (2024-11-01 16:14) @----
# pull variable mean or median and sd, iqr, or range and formate for tables as Mean <u>Med</u> (SD) [Range] {IQR}
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

## mean_med_table_adl --------------------------------- (2024-11-01 16:15) @----
# adl and time mean or median and sd, iqr, or range and formate for tables as Mean <u>Med</u> (SD) [Range] {IQR}
mean_med_table_adl <- function(data, variable_select, observation_n = NULL, digs = 0) {
  data |>
    select(starts_with(variable_select), refid, arm_id, arm_n) |>
    select(matches(paste0(observation_n, "$")), refid, arm_id, arm_n) |>
    select(!matches("diff"), refid, arm_id, arm_n) |>
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
    select(refid, arm_id, scale, time, table) |>
    rename(!!paste0(variable_select, "table") := table) |>
    rename(!!paste0(variable_select, "time") := time)
}

## mean_med_table_single ------------------------------ (2024-11-01 16:10) @----
# pull study arm mean or median with sd, range, iqr and format for tables as Mean <u>Med</u> (SD) [Range] {IQR}
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

## footnote convenience ------------------------------- (2023-05-31 11:39) @----
footnote_study <- function(data, study_select, footnote) {
  data |> filter(study == study_select) |> pull(footnote)
}

## traffic light plots by refid
rob2_traffic_light_refid <- function(refid_select) {
  rob_temp_dat <- rob2_dat |>
    filter(!is.na(Study) & refid %in% refid_select) |>
    select(-refid)

  rob_traffic_light(rob_temp_dat, psize = 4, tool = "ROB2", colour = "colourblind")
}

robinsi_traffic_light_refid <- function(refid_select) {
  robinsi_temp_dat <- robinsi_dat |>
    filter(!is.na(Study) & refid %in% refid_select) |>
    select(-refid)

  rob_traffic_light(robinsi_temp_dat, psize = 4, tool = "ROBINS-I", colour = "colourblind")
}

## summary weighted for pt characteristics ------------ (2023-12-27 10:50) @----
pt_sum <- function(variable, wgt){sum(variable * wgt, na.rm = FALSE)}
## FOR FIGURES !! ----
## for baseline risk plot for RR or OR ---------------- (2023-12-27 10:50) @----
bubble_plot_data <- function(meta_object) {
  yi_meta <- meta_object$TE
  sei_meta <- meta_object$seTE
  control_event_rate <- meta_object$event.c / meta_object$n.c
  slab <- meta_object$studlab
  tibble(yi_meta, sei_meta, control_event_rate, slab)
}

# usage
# temp <- bubble_plot_data(gen_reg_delirium_meta)
# fitted_meta <- metafor::rma(yi_meta, vi_meta, mods = ~ control_event_rate, slab = slab, method = "FE", data = temp)
# metafor::regplot(fitted_meta, xlab = "Baseline risk (event rate)", ylab = "Risk Ratio", refline = 0, atransf = exp, at = log(c(0.2, 0.4, 0.7, 1, 2, 4, 8)), labsize = 0.5, xlim = c(-0.02, 0.3), las = 1, label = TRUE, offset = c(0.4), lwd = 0.5)
# bubble_plot(gen_reg_delirium_meta)

## trim white space from png -------------------------- (2023-12-27 10:50) @----
trim_image <- function(file_png) {
  file_png <- paste0("assets/", file_png)
  image <- magick::image_read(file_png)
  image_trimmed <- magick::image_trim(image)  # Adjust fuzz value if needed
  magick::image_write(image_trimmed, path = file_png)
}

## CAPTIONING !!----
## initialize fig tab n labels (flexibility for html) - (2024-10-31 10:25) @----
table_n <- 0
figure_n <- 0

## table caption counter ------------------------------ (2024-10-31 07:48) @----
table_ref <- function() {
  table_n <<- table_n + 1
  paste0("Table ", table_n, ". ")
}

## figure caption counter ----------------------------- (2024-10-31 07:48) @----
figure_ref <- function() {
  figure_n <<- figure_n + 1
  paste0("Figure ", figure_n, ". ")
}

## TO INCLUDE RESULTS IN EXTERNAL FILES (clipboard) !!----
## total for soe table from pairwise ------------------ (2023-04-10 10:03) @----
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

## soe functions placed in separate file -------------- (2023-07-07 13:43) @----

# RESUME: 2024-11-06
#### ---- !!!! ----
## OTHER IN SENSITIVITY ANALYSES OR CONVENIENCE !!-----
## total in meta -------------------------------------- (2023-07-07 13:43) @----
total_meta <- function(data_meta) {
  data_meta |> summarize(total = sum(n1 + n2))
}

## traffic light plots by refid rct/nrsi from meta ---- (2024-02-22 08:43) @----
meta_rob_traffic_light_refid <- function(refid_select) {
  rob2 <- rob2_traffic_light_refid(temp_meta$data$refid)
  robinsi <- robinsi_traffic_light_refid(temp_meta$data$refid)
  return(list(rob2, robinsi))
}

## grade levels --------------------------------------- (2023-05-31 11:39) @----
# not a function but need for other functions
high  <- "<span><span class='quality-sign'>⨁⨁⨁⨁</span>"
mod   <- "<span><span class='quality-sign'>⨁⨁⨁◯</span>"
low   <- "<span><span class='quality-sign'>⨁⨁◯◯</span>"
vlow  <- "<span><span class='quality-sign'>⨁◯◯◯</span>"
low_very  <- paste(low, vlow, sep = "<br/>")
# grade_foot <- paste0("Very low: ", vlow, "; Low: ", low, "; Moderate: ", mod, "; High: ", high, ".")
grade_foot <- paste0("[", "Very low: ", vlow, "; Low: ", low, "; Moderate: ", mod, "; High: ", high, ".", "]", "(", "soe_gt.html#grade", ")")

## from skim all files for overview ------------------- (2023-12-27 10:50) @----
skim_report <- function(data_dat){
  name_txt <- paste0(as.character(substitute(data_dat)), "_skim.txt")
  path <- file.path(getwd(), "dictionaries", name_txt)
  temp <- data_dat |> remove_empty(which = "cols")
  temp <- skim(temp)
  sink(path)
  print(temp)
  sink()
}

all_skimmed <- function() {
  skim_report(study_char_dat)
  skim_report(study_arm_dat)
  skim_report(dichot_dat)
  skim_report(contin_dat)
  skim_report(likert_dat)
}

## analysis of credibility for meta-analysis ---------- (2023-08-07 15:05) @----
# for sensitivity analysis only http://dx.doi.org/10.1098/rsos.171047
meta_ancred <- function(meta_fit) {
  # ancred calculation functions
  cpi_sig_or <- function(lower, upper) {
    upper <- log(upper)
    lower <- log(lower)
    sl <- (upper - lower)^2 / (4 * sqrt(upper * lower))
    exp(sl)
  }

  cpi_nsig_or <- function(lower, upper) {
    al <- exp((log(upper * lower) * (log(upper / lower))^2) / (2 * log(upper) * log(lower)))
    al
  }

  # flag to use inverse for OR < 1
  odds_ratio <- exp(update(meta_fit, sm = "OR")$TE.random)
  lt_1 <- ifelse(odds_ratio < 1, TRUE, FALSE)
  p_lt_05 <- meta_fit$pval.random < 0.05

  # # upper and lower bounds
  odds_ratio_low <- exp(update(meta_fit, sm = "OR")$lower.random)
  odds_ratio_up <- exp(update(meta_fit, sm = "OR")$upper.random)

  list(odds_ratio_low, odds_ratio_up)

  # critical prior interval
  cpi <- ifelse(p_lt_05, cpi_sig_or(odds_ratio_low, odds_ratio_up), cpi_nsig_or(odds_ratio_low, odds_ratio_up))

  # convert to relative risk
  event_rate <- gtools::inv.logit(metaprop(meta_fit$event.c, meta_fit$n.c)$TE.random)
  cpi <- ifelse(lt_1 == TRUE, 1 / cpi, cpi)
  cpi_rr <- DescTools::ORToRelRisk(cpi, event_rate)

  case_when(
    lt_1 == TRUE & cpi_rr < 1 & p_lt_05 == TRUE ~ paste("Convincing to a skeptic if plausible RR <", round(cpi_rr, 2)),
    lt_1 == TRUE & cpi_rr < 1 & p_lt_05 == FALSE ~ paste("Convincing to an advocate if prior evidence supports a RR between 1 and", round(cpi_rr, 2)),
    lt_1 == TRUE & cpi_rr > 1 & p_lt_05 == TRUE ~ paste("Convincing to a skeptic if plausible RR >", round(cpi_rr, 2)),
    lt_1 == FALSE & p_lt_05 == FALSE ~ paste("Convincing to an advocate if prior evidence supports a RR between 1 and ", round(cpi_rr, 2)),
  )
}

## count of unique for variable NOT USED -------------- (2024-10-31 07:45) @----
count_unique <- function(df, var){
  df |>
    select({{var}}) |>
    distinct() |>
    count() |>
    pull(n)
}

## 2 by 2 frequency table NOT USED -------------------- (2024-10-31 10:15) @----
proc_freq <- function(df, a, b){
  df %>%
    group_by({{a}}, {{b}}) %>%
    tally() %>%
    spread({{a}}, n) %>%
    gt::gt(.)
}

## tabyl for selected variables NOT USED -------------- (2024-10-31 10:17) @----
tab_lst <- function(data, vars){
  data %>%
    select({{vars}}) %>%
    map(~ tabyl(.))
}

## view record(s) for refid from dataset NOT USED ----- (2024-10-31 10:17) @----
view_rec <- function(data_set, refid_select) {
  data_set |>
    filter(refid == refid_select) |>
    janitor::remove_empty(which = "cols") |>
    t() |>
    as.data.frame() |>
    rownames_to_column(var = "variable") |>
    View()
}

## page through distiller forms; uses readkey NOT USED- (2024-10-31 10:18) @----
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


## for summary tables NOT USED ------------------------ (2024-10-31 13:27) @----
n_per_tf <- function(var_name, n_dig = 1){
  paste0(sum(var_name == TRUE), " (", format(round(100*(mean(var_name)), n_dig), nsmall = 1), ")")
}

## excel with pages for each refid NOT USED ----------- (2024-10-31 07:47) @----
library(openxlsx)
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

# by_study_xlsx usage
# add design to tibble if not study characteristics file
# temp_dat <- left_join(study_arm_dat, study_char_dat |> select(refid, design_f), by = "refid") |>
#   relocate(design_f, .after = refid) |>
#   arrange(design_f)
# by_study_xlsx(kq5_refid, temp_dat, "kq5")


refid_reported_outcome_other <- function(data_dat, vars, instrument, negate_flag = FALSE){
  data_dat |>
    filter(str_detect({{vars}}, instrument, negate = negate_flag)) |>
    select(refid) |>
    distinct() |>
    pull(refid)
}

## adl_transpose_fun NOT USED ------------------------- (2023-07-07 13:43) @----
adl_transpose_fun <- function(data, obs_number){
  obs_number <- as.character(obs_number)
  data |>
    select(refid:adl_scale, matches(obs_number)) |>
    rename_with(~ str_replace(.x, obs_number, ""))
}

## new png NOT USED ----------------------------------- (2023-12-27 10:50) @----
png_fun <- function(name, width, height){
  png(paste0("assets/", name, ".png"), width = 14, height = 5, units = "in", res = 300)
  fig_temp()
  dev.off()
}






## calculate figure width NOT USED -------------------- (2023-12-27 10:50) @----
# greater than 564 measured in plot pane entire RStudio screen
calc_width_display_gt_564 <- function(width_px_from_rstudio) {
  width_png_inches <- width_px_from_rstudio / 71.4
  percent_png <- 0.58 * (width_px_from_rstudio / 564)
  paste("width: ", round(width_png_inches, 2), " inches; ", 100 * round(percent_png, 2), "%", sep = "")
}

# calc_width_display_gt_564(696)
