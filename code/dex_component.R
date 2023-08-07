```{r delirium_comp_meta_data}
# NOTE: component meta data
# VARIABLE: postop_only <lgl> from study arm only post-op administration [postop_only_dat]
# study arm postop only
timing_dat <- study_arm_dat |>
  select(refid, arm_id, proph_med_pre, proph_med_induct, proph_med_intra, proph_med_postop) |>
  rename_with(~ str_replace(.x, "proph_med", "time")) |>
  mutate(
    time_pre = ifelse(!is.na(time_pre), "Preop", NA),
    time_induct = ifelse(!is.na(time_induct), "Induction", NA),
    time_intra = ifelse(!is.na(time_intra), "Intraop", NA),
    time_postop = ifelse(!is.na(time_postop), "Postop", NA),
  ) |>
    unite(arm_time, time_pre, time_induct, time_intra, time_postop, na.rm = TRUE, sep = "|")


delirium_dex_comp_meta_dat <- dichot_dat |>
  # only dexmedetomidine studies
  filter(refid %in% dex_refid) |>
  filter(!is.na(delitotal_n)) |>
  left_join(drugs_dat |> select(refid, arm_id, drug_recode_abbr), by = c("refid", "arm_id")) |>
  left_join(study_char_dat |> select(refid, country), by = "refid") |>
  left_join(surgs |> select(refid, surgs_single_f), by = c("refid")) |>
  left_join(timing_dat, by = c("refid", "arm_id")) |>
  rename(arm = drug_recode_abbr, surg_f = surgs_single_f) |>
  select(refid, refid_c, arm_id, year, study, design_f_lab, country, surg_f, arm_time, arm, delitotal_n, arm_n, delitotal_perc, delirium_inc_prop) |>
  arrange(refid_c, arm_id) |>
  # collapse arms
  # group_by(refid) |>  filter(n() > 2) |>  ungroup() |> # verify collapsed arms; verified 2023-03-09
  mutate(
    arm = fct_collapse(arm, "Plac" = c("Plac", "None")),
    arm_time = ifelse(arm_time == "", as.character(arm), arm_time),
    # arm_time = ifelse(arm == "None", "Place", arm_time),
    delirium_inc_prop = ifelse(delirium_inc_prop == TRUE, "Incidence Proportion", "Maximum Daily"),
    across(c(arm, design_f_lab, surg_f), ~ fct_drop(.x)),
    delitotal_n = case_when(
      # study == "Lee 2018a" & arm_id == 2 ~ collapse_dichot(dichot_dat, "Lee 2018a", c(2, 3), delitotal_n), # for nma keep all arms as is
      study == "Zhao 2020" & arm_id == 2 ~ collapse_dichot(dichot_dat, "Zhao 2020", c(2, 3, 4), delitotal_n), # nb collapses all doses
      .default = delitotal_n
    ),
    arm_n = case_when(
      # study == "Lee 2018a" & arm_id == 2 ~ collapse_dichot(dichot_dat, "Lee 2018a", c(2, 3), arm_n),
      study == "Zhao 2020" & arm_id == 2 ~ collapse_dichot(dichot_dat, "Zhao 2020", c(2, 3, 4), arm_n),
      .default = arm_n
    ),
    arm_time = ifelse(study == "Lee 2018b" & arm_id == 4, "Induction|Intraop|Preg", arm_time)
  ) |>
  filter(!(study %in% c("Zhao 2020") & arm_id %in% c(3, 4)))
  # filter(!(study == "He 2018" & arm_id == 2)) |>
  # filter(!(study %in% c("Lee 2018a", "Avidan 2017") & arm_id == 3)) |>
  # filter(!(study %in% c("Lee 2018b") & arm_id %in% c(2, 4))) |> # remove pregabalin, pregabaline/dex arm

## calculated vs reported percentages; 2023-04-08 ----- (2023-04-24 18:21) @----
# calculated vs reported percentages; verified 2023-04-08
# temp <- delirium_meta_dat |>
#   mutate(
#     calc_percent = delitotal_n / arm_n * 100,
#     diff = calc_percent - delitotal_perc
#   ) |>
#   select(refid, study, arm_id, arm, arm_n, delitotal_n, calc_percent, delitotal_perc, diff) |>
#   # exclude collapsed arms
#   filter(!study %in% c("Lee 2018a", "Avidan 2017", "Zhao 2020")) |>
#   filter(abs(diff) >= 0.05)
#
# writexl::write_xlsx(temp, "/Users/mgrant/Desktop/temp.xlsx")

```

```{r delirium_comp_meta}
library(netmeta)
component_nma_dat <- delirium_dex_comp_meta_dat

trts <- unique(component_nma_dat$arm_time)
# trts <- unique(component_nma_rct_dat$arm)
tabyl(component_nma_dat$arm_time)

pairwise_comp_dat <- pairwise(
  treat = arm_time,
  event = delitotal_n,
  n = arm_n,
  studlab = study,
  data = component_nma_dat,
  # data = component_nma_rct_dat,
  sm = "RR") |>
  select(studlab:seTE)

# net_disc <- discomb(TE, seTE, treat1, treat2, studlab,
#   data = pairwise_comp_dat,
#   ref = "std",
#   sm = "OR",
#   trts = trts
# )

net1 <- netmeta(TE, seTE, treat1, treat2, studlab,
  data = pairwise_comp_dat,
  ref = "Plac",
  sm = "OR",
  trts = trts
)

net_comb <- netcomb(net1, inactive = "Plac")

# netleague(net1,
#   digits = 2,
#   common =  FALSE,
#   # direct = FALSE,
#   seq = trts
# )

# forest(net_disc)
netgraph(net1,
  lwd = 4,
  plastic = FALSE,
  thickness = TRUE,
  points = TRUE,
  alpha.transparency = 2,
  col = "azure3",
  col.points = "#F39C12",
  number.of.studies = TRUE,
  # seq = trts_melram,
  rescale.pointsize = TRUE,
  # cex.points = c(1, 2, 3, 4, 5),
  # cex.points = node_size$n/40,
  # cex.points = netmeta_melram_nma$n.trts,
  # labels = paste0(trts, "\n(n=", round(n.trts), ")"),
  col.number.of.studies = "black",
  cex.number.of.studies = 1.2,
  col.highlight = "white",
  bg.number.of.studies = c("#FFFFFF"),
  offset = .02
)

```
# ```{r component_nma_forest, fig.width = 12, fig.height = 4.55, fig.align = "left"}
forest(net_comb, xlab = "Odds Ratio")
# ```
