# table opioid mean med time
opioid_dat <- contin_dat |>
  filter(if_any(matches("opioid"), ~ !is.na(.x))) |>
  select(refid, study, arm_id, arm_n, opioid_m1:opioid_iqru1, opioid_time1, opioid_m2:opioid_iqru2, opioid_time2, opioid_m3:opioid_iqru3, opioid_time3, opioid_m_all:opioid_iqru_all, opioid_time_all) |>
select(refid:arm_n, ends_with("all")) |>
  rename_with(~ str_replace(.x, "_all", "1"))

opioid_times_dat <- mean_med_table(opioid_dat, "opioid_", 1, 0) |>
  left_join(mean_med_table(opioid_dat, "opioid_", 2, 0), by = c("refid", "arm_id")) |>
  left_join(mean_med_table(opioid_dat, "opioid_", 3, 0), by = c("refid", "arm_id")) |>
  left_join(mean_med_table(opioid_dat |> select(refid:arm_n, ends_with("all")) |>
    rename_with(~ str_replace(.x, "_all", "1")), "opioid_", 1, 0), by = c("refid", "arm_id")) |>
  left_join(study_arm_dat |>
    select(refid, study, arm, arm_id), by = c("refid", "arm_id")) |>
  relocate(study:arm, .after = refid)
