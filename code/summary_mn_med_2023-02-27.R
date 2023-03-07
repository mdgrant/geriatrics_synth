## summaries mean and median reported for tables ------ (2023-02-26 08:10) @----

# table of mean/medians
table_mn_med <- mean_med_table_single(study_arm_dat, "pre_mmse_", 0) |>
  left_join(mean_med_table(likert_dat, "mmse_", 1, 0), by = c("refid", "arm_id")) |>
  left_join(mean_med_table(likert_dat, "mmse_", 2, 0), by = c("refid", "arm_id")) |>
  left_join(mean_med_table(likert_dat, "mmse_", 3, 0), by = c("refid", "arm_id")) |>
  left_join(mean_med_table(likert_dat, "moca_", 1, 0), by = c("refid", "arm_id")) |>
  left_join(mean_med_table_single(likert_dat, "mmselast_", 0), by = c("refid", "arm_id")) |>
  rename_with(~ str_remove(.x, "_$"))

# age
table_age_mn_med <- study_arm_dat |>
  select(starts_with("age_"), arm_n, refid, arm_id) |>
  age_for_tables() |>
  select(refid, arm_id, age_table)
