library(PublicationBias)
library(phacking)
library(multibiasmeta)
library(metafor)

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
