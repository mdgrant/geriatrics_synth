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

# count of unique for variable
qinu <- function(df, var){
  df %>%
    select({{var}}) %>%
    distinct() %>%
    count() %>%
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
  data_set %>%
    filter(refid == refid_select) %>%
    janitor::remove_empty(which = "cols") %>%
    t() %>%
    View()
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
    ) |>
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_row_groups()
    )
}

# simple figure and table caption labels (greater flexibility for html)
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

