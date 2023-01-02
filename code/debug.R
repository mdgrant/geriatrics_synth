# create excel file of individual study records
library(openxlsx)

# record for worksheet
view_xlsx <- function(data_set, refid_select) {
  data_set %>%
    filter(refid == refid_select) %>%
    janitor::remove_empty(which = "cols") %>%
    t() |>
    as.data.frame() %>%
    rownames_to_column(., var = "var_name")
}

temp <- view_xlsx(study_arm_dat, 246)

## here ----------------------------------------------- (2022-12-29 14:12) @----
style <- createStyle(
  fontSize = 10,
  fontName = "Arial",
  wrapText = TRUE
)

refids <- c(246, 742)

wb <- createWorkbook("Kq5_trials")
for (i in 1:length(refids)) {
  temp_sheet <- view_xlsx(study_arm_dat, refids[i])
  addWorksheet(wb, sheetName = refids[i])
  setColWidths(wb, i, cols = c(1:5), widths = c(rep(40, 5)))
  writeDataTable(wb, sheet = i, temp_sheet)
}

saveWorkbook(wb, "/Users/mgrant/Desktop/kq5_trials.xlsx", overwrite = TRUE)
