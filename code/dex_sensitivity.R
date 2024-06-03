library(PublicationBias)
library(phacking)
library(multibiasmeta)
library(metafor)

dex_sens_dat <- read_csv("data/dex_meta_dat_sens.csv")

dat <- escalc(measure = "RR", ai = event2, n1i = n2, ci = event1, n2i = n1, data = dex_sens_dat)

dat <- escalc(measure = "RR", ai = event2, n1i = n2, ci = event1, n2i = n1, data = dex_sens_nonchina_dat)

dat <- escalc(measure = "RR", ai = event2, n1i = n2, ci = event1, n2i = n1, data = mel_ram_sens_dat)

significance_funnel(yi = dat$yi, vi = dat$vi, favor_positive = FALSE, alpha_select = 0.05)

rma(yi = yi, vi = vi, data = dat, measure = "RR")

# selection ratio to achieve a null result
meta <- pubbias_meta(
  yi = dat$yi,
  vi = dat$vi,
  selection_ratio = 50,
  model_type = "fixed",
  favor_positive = FALSE
)

summary(meta)

meta <- pubbias_svalue(yi = -dat$yi, vi = dat$vi)

summary(meta)

dex_sens_dat <- tibble(
  yi = -dex_plac_meta$TE,
  vi = dex_plac_meta$seTE^2,
  study = dex_plac_meta$studlab
)

# dex_sens <- escalc(yi = yi, sei = sei, data = dex_sens_dat)

temp <- rma(yi = yi, vi = vi, slab = study, data = dex_sens_dat, measure = "OR")

forest(temp)

pubbias_dex_1 <- pubbias_meta(
  dex_sens_dat$yi,
  dex_sens_dat$vi,
  model_type = "robust",
  selection_ratio = 1
)

pubbias_dex_1$stats

pubbias_dex_4 <- pubbias_meta(
  dex_sens_dat$yi,
  dex_sens_dat$vi,
  model_type = "robust",
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
  sei = dex_sens_dat$vi,
  parallelize = TRUE)

phacking_dex$stats

rtma_qqplot(phacking_dex)

z_density(yi = dex_sens_dat$yi, sei = dex_sens_dat$sei)


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
