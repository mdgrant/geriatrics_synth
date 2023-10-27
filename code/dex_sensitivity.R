library(PublicationBias)
library(phacking)
library(multibiasmeta)
library(metafor)

dex_sens_dat <- read_csv(file = "data/dex_sens_dat.csv")

rma(yi, vi, data = dex_sens_dat, method = "FE")

pubbias_dex_1 <- pubbias_meta(
  dex_sens_dat$yi,
  dex_sens_dat$vi,
  model_type = "fixed",
  selection_ratio = 1
)

pubbias_dex_1$stats

pubbias_dex_4 <- pubbias_meta(
  dex_sens_dat$yi,
  dex_sens_dat$vi,
  model_type = "random",
  selection_ratio = 4
)

pubbias_dex_4$stats

pubbias_dex_s <- pubbias_svalue(
  dex_sens_dat$yi,
  dex_sens_dat$vi
)

pubbias_dex_s$stats

significance_funnel(dex_sens_dat$yi, dex_sens_dat$vi)

phacking_dex <- phacking_meta(
  yi = dex_sens_dat$yi,
  vi = dex_sens_dat$vi,
  parallelize = FALSE)

phacking_dex$stats

rtma_qqplot(phacking_dex)

z_density(yi = dex_sens_dat$yi, vi = dex_sens_dat$vi)


::: {.callout-note collapse="true" appearance="minimal" icon="false"}
#### <caption_mg> `r figure_ref()` Dexmedetomidine compared with placebo or no intervention --- **China versus other countries**. </caption_mg>

```{r delirium_incidence_fig_country, out.width = "70%", out.height = "40%", fig.align = "left"}
#| warning: false
par(mar = c(1, 1, 1, 1))
set.seed(1234)
# total to add
dex_country <- delirium_meta_dat |>
  filter(design_f_lab == "Randomized Clinical Trial" & refid %in% dex_refid) |>
  filter(any(arm == "Plac/None"), .by = refid_c) |>
  mutate(
    deli_prop = delitotal_n / arm_n,
    china = ifelse(country == "China", "China", "Other"),
  ) |>
  select(refid_c, year, study, country, arm, china, surg_f, deli_prop)

# separate geom_point by arm
dex_country |>
  # filter(china == FALSE) |>
  ggplot(aes(x = deli_prop, y = china, fill = arm)) +
  geom_boxplot(width = 0.2, outlier.size = 1.3, position = position_dodge(.5)) +
  geom_point(position = position_jitterdodge(jitter.width = 1, dodge.width = .5), alpha = .7, aes(color = arm)) +
  # scale_fill_manual(values = c("#8B0000", "#377eb8")) +
  scale_fill_brewer(palette="BuPu", breaks = c("Plac/None","Dex"), name = "arm") +
  # scale_fill_discrete(breaks = c("Plac/None","Dex")) +
  labs(y = "", x = "Incidence Proportion") +
  scale_x_continuous(limits = c(0, 1), labels = scales::percent, name = "arm") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme(
    legend.position = "right",
    legend.title = element_blank()
  )

```
:::

## cumulative by sample size -------------------------- (2023-10-27 04:13) @----

# dex_plac_meta_cum <- metabin(event2, n2, event1, n1,
#   data = dex_meta_dat,
#   studlab = study,
#   sm = "RR",
#   method = "MH",
#   method.tau = "REML",
#   hakn = TRUE,
#   prediction = TRUE,
#   allstudies = TRUE
#   )
#
# dex_meta_cum_dat <- pairwise_dat |>
#   filter(refid %in% dex_plac_refid) |>
#   select(study, year, event2, n2, event1, n1, china_vs_other) |>
#   mutate(
#     total_samp = n1 + n2,
#     study = ifelse(china_vs_other == "China", paste0(study, "*"), study),
#     study = paste0(study, " (n=", total_samp, ") "),
#   )
#
# # toString(sort(dex_plac_refid))
# # total_meta(dex_meta_dat)
#
# dex_plac_cum_meta <- metabin(event2, n2, event1, n1,
#   data = dex_meta_cum_dat,
#   studlab = study,
#   sm = "RR",
#   method = "MH",
#   method.tau = "REML",
#   hakn = TRUE,
#   prediction = TRUE,
#   allstudies = TRUE
#   )
#
# temp <- metacum(dex_plac_cum_meta, pooled = "common", sortvar = 1/total_samp)
#
# forest(temp,
#   rightcols = c("effect", "ci", "pval"),
#   rightlabs = c("RR", "(95% CI)", "P-value"),
#   just.addcols.right = "right",
#   xlim = c(0.4, 3))
