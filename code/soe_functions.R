## risk difference from meta -------------------------- (2024-01-22 16:53) @----
# checked 2024-11-07
# convert RR or OR meta to RD and format for footnote; includes baseline event rate for control arms from common effects model
# note OR meta must be from event data, not effect measures

rd_ci_from_meta <- function(meta_object, pscale = 100, digits = 2) {
  meta_object <- update(meta_object, sm = "RD")
  if (!is.null(meta_object$subset)) {stop("Use rd_ci_from_meta_sub to account for subsetted meta-analysis")}
  temp <- c(meta_object$TE.random, meta_object$lower.random, meta_object$upper.random) * pscale
  temp <- formatC(temp, digits = digits, format = "f")
  temp_control <- metaprop(meta_object$event.c, meta_object$n.c)
  control_arm <- boot::inv.logit(temp_control$TE.common) * pscale
  control_arm <- formatC(control_arm, digits = digits, format = "f")
  paste0("Risk difference (random effects): ", temp[1], " per ", pscale, " (95% CI, ", temp[2], " to ", temp[3], ")", ";", " control arm event rate ", control_arm, " per ", pscale, ".")
}

# > rd_ci_from_meta(dex_plac_meta)  # example with RR
# [1] "Risk difference: -8.69 per 100 (95% CI, -11.81 to -5.57); control arm event rate 17.81 per 100."
# > rd_ci_from_meta(temp_meta)  # same from OR meta
# [1] "Risk difference: -8.69 per 100 (95% CI, -11.81 to -5.57); control arm event rate 17.81 per 100."

## rd_ci_from_meta_sub -------------------------------- (2024-11-07 10:49) @----
# checked 2024-11-07
# convert RR or OR meta to RD and format for footnote; includes baseline event rate for control arms from common effects model; use when data subseted
# note OR meta must be from event data, not effect measures

rd_ci_from_meta_sub <- function(meta_object, pscale = 100, digits = 2) {
  meta_object <- update(meta_object, sm = "RD")
  if (is.null(meta_object$subset)) {stop("Use rd_ci_from_meta; not a subset in meta-analysis")}
  temp <- c(meta_object$TE.random, meta_object$lower.random, meta_object$upper.random) * pscale
  temp <- formatC(temp, digits = digits, format = "f")
  temp_control <- metaprop(meta_object$data$.event.c[meta_object$subset], meta_object$data$.n.c[meta_object$subset]) # subset data
  control_arm <- boot::inv.logit(temp_control$TE.common) * pscale
  control_arm <- formatC(control_arm, digits = digits, format = "f")
  paste0("Risk difference (random effects): ", temp[1], " per ", pscale, " (95% CI, ", temp[2], " to ", temp[3], ")", ";", " control arm event rate ", control_arm, " per ", pscale, ".")
}

# > rd_ci_from_meta_sub(dex_plac_china_meta)
# [1] "Risk difference: -9.75 per 100 (95% CI, -13.45 to -6.06); control arm event rate 18.67 per 100."
# > rd_ci_from_meta_sub(temp_china_meta) # same from OR meta
# [1] "Risk difference: -9.75 per 100 (95% CI, -13.45 to -6.06); control arm event rate 18.67 per 100."


## rd_ci_from_meta_soe -------------------------------- (2024-11-04 14:50) @----
# convert RR meta to RD and format for SOE; includes baseline event rate for control arms from common effects model; result saved in clipboard for pasting
# NOTE: to format easily in grey when pasting use keyboard maestro gray macro  in excel to paste from clipboard (<p style="color:gray;">%SystemClipboard%</p>)

rd_ci_from_meta_soe <- function(meta_object, pscale = 100, digits = 2) {
  meta_object <- update(meta_object, sm = "RD")
  if (!is.null(meta_object$subset)) {stop("Use rd_ci_from_meta_sub_rr_soe to account for subsetted meta-analysis")}
  temp <- c(meta_object$TE.random, meta_object$lower.random, meta_object$upper.random) * pscale
  temp <- formatC(temp, digits = digits, format = "f")
  temp_control <- metaprop(meta_object$event.c, meta_object$n.c)
  control_arm <- boot::inv.logit(temp_control$TE.common) * pscale
  control_arm <- formatC(control_arm, digits = digits, format = "f")
  (clipr::write_clip(paste0("RD ", temp[1], " per ", pscale, " (95% CI, ", temp[2], " to ", temp[3], ")", "<br/>[", "control arm event rate ", control_arm, " per ", pscale, "]")))
}

# > rd_ci_from_meta_soe(dex_plac_meta)
# [1] "RD -8.69 per 100 (95% CI, -11.81 to -5.57)<br/>[control arm event rate 17.81 per 100]"
# paste result from clipboard: "RD -8.69 per 100 (95% CI, -11.81 to -5.57)<br/>[control arm event rate 17.81 per 100]"

## rd_ci_from_meta_sub_soe ---------------------------- (2024-11-07 11:38) @----
# use when data are subseted
rd_ci_from_meta_sub_soe <- function(meta_object, pscale = 100, digits = 2) {
  meta_object <- update(meta_object, sm = "RD")
  if (is.null(meta_object$subset)) {stop("No subsets. Use rd_ci_from_meta_rr_soe")}
  temp <- c(meta_object$TE.random, meta_object$lower.random, meta_object$upper.random) * pscale
  temp <- formatC(temp, digits = digits, format = "f")
  temp_control <- metaprop(meta_object$data$.event.c[meta_object$subset], meta_object$data$.n.c[meta_object$subset]) # subset data
  control_arm <- boot::inv.logit(temp_control$TE.common) * pscale
  control_arm <- formatC(control_arm, digits = digits, format = "f")
  (clipr::write_clip(paste0("RD ", temp[1], " per ", pscale, " (95% CI, ", temp[2], " to ", temp[3], ")", "<br/>[", "control arm event rate ", control_arm, " per ", pscale, "]")))
}

# > rd_ci_from_meta_sub_rr_soe(dex_plac_china_meta, digits = 1)
# [1] "RD -9.8 per 100 (95% CI, -13.4 to -6.1)<br/>[control arm event rate 18.7 per 100]"
# paste result from clipboard: "RD -9.8 per 100 (95% CI, -13.4 to -6.1)<br/>[control arm event rate 18.7 per 100]"


## soe result from meta to clipboard ------------------ (2024-01-22 16:53) @----
# example "0.86 (95% CI, 0.44–1.66; PI 0.24–3.10)"
soe_meta_result_rr_or <- function(meta_object, effect = "RR", digits = 2) {
  temp_rr <- exp(c(meta_object$TE.random, meta_object$lower.random, meta_object$upper.random))
  temp_rr <- formatC(temp_rr, digits = digits, format = "f")
  temp_rr <- paste0(effect, " ", temp_rr[1], " (95% CI, ", temp_rr[2], "–", temp_rr[3], ";")
  temp_pi <- exp(c(meta_object$lower.predict, meta_object$upper.predict))
  temp_pi <- formatC(temp_pi, digits = digits, format = "f")
  temp_pi <- paste0(" PI ", temp_pi[1], "–", temp_pi[2], ")")
  # to add add i2 w/CI
  # temp_i2 <- paste0(
  # "*I*<sup> 2</sup> ",
  # formatC(meta_object$I2 * 100, 0, format = "f"), "%", " (95% CI, ",
  # formatC(meta_object$lower.I2 * 100, 0, format = "f"), "%", "-",
  # formatC(meta_object$upper.I2 * 100, 0, format = "f"), "%", ")")
  # (clipr::write_clip(paste0(temp_rr, temp_pi, "<br/>&emsp;", temp_i2)))
  (clipr::write_clip(paste0(temp_rr, temp_pi)))
}

# for kq4 — use data from meta_object to estimate risk difference when pooling uses metagen
rd_ci_from_meta_or_kq4 <- function(meta_object, pscale = 1, digits = 2) {
  temp <- metabin(meta_object$data$n_TIVA, meta_object$data$arm_n_TIVA, meta_object$data$n_Inhaled, meta_object$data$arm_n_Inhaled, sm = "RD")
  temp_control <- metaprop(temp$event.c, temp$n.c)
  temp <- c(temp$TE.random, temp$lower.random, temp$upper.random) * pscale
  temp <- formatC(temp, digits = digits, format = "f")
  control_arm <- boot::inv.logit(temp_control$TE.common) * pscale
  control_arm <- formatC(control_arm, digits = digits, format = "f")
  paste0("Approximate pooled risk difference ", temp[1], " per ", pscale, " (95% CI, ", temp[2], " to ", temp[3], ")", ";", " control arm event rate ", control_arm, " per ", pscale, ".")
  # paste0(temp[1], " per ", pscale, " (95% CI, ", temp[2], " to ", temp[3], ")")
}

# RESUME: 2025-04-10 edit as above; note also xlim argument for forest plot
rd_ci_from_meta_rr_kq4 <- function(meta_object, pscale = 100, digits = 2) {
  meta_object <- update(meta_object, sm = "RD")
  temp <- c(meta_object$TE.random, meta_object$lower.random, meta_object$upper.random) * pscale
  temp <- formatC(temp, digits = digits, format = "f")
  paste0(temp[1], " per ", pscale, " (95% CI, ", temp[2], " to ", temp[3], ")")
}

risk_diff_meta_rr_kq4 <- function(meta_select = temp_meta, scale = 1000, digits = 1) {
  paste0("Pooled risk difference ", rd_ci_from_meta_rr_kq4(meta_select, pscale = scale, digits = digits))
}



#
# # RESUME: 2024-11-07
# ## risk difference from meta with rr with subset ------ (2024-03-23 10:05) @----
# # separate function needed because meta object includes all data
# rd_ci_from_meta_subset_rr <- function(meta_object, pscale = 100, digits = 2) {
#   meta_object <- update(meta_object, sm = "RD")
#   temp <- c(meta_object$TE.random, meta_object$lower.random, meta_object$upper.random) * pscale
#   temp <- formatC(temp, digits = digits, format = "f")
#   temp_control <- metaprop(meta_object$data$.event.c[meta_object$subset], meta_object$data$.n.c[meta_object$subset])
#   # temp_control <- metaprop(meta_object$event.c[meta_object$subset], meta_object$n.c[meta_object$subset])
#   control_arm <- boot::inv.logit(temp_control$TE.common) * pscale
#   control_arm <- formatC(control_arm, digits = digits, format = "f")
#   paste0(temp[1], " per ", pscale, " (95% CI, ", temp[2], " to ", temp[3], ")", ";", " control arm event rate ", control_arm, " per ", pscale, ".")
# }
#
# ## text for risk difference from rr meta -------------- (2024-01-22 16:53) @----
# # risk_diff_meta_rr <- function(digits = 1) {paste0("Pooled risk difference ", rd_ci_from_meta_rr(temp_meta, pscale = 1000, digits = digits), ".")}
#
# risk_diff_meta_rr <- function(meta_select = temp_meta, scale = 1000, digits = 1) {
#   paste0("Pooled risk difference ", rd_ci_from_meta_rr(meta_select, pscale = scale, digits = digits))
# }
#
# risk_diff_meta_rr_soe <- function(meta_select = temp_meta, scale = 1000, digits = 1) {
#   paste0("Pooled risk difference ", rd_ci_from_meta_rr_soe(meta_select, pscale = scale, digits = digits))
# }
#
# risk_diff_meta_subset_rr <- function(meta_select = temp_meta, scale = 1000, digits = 1) {
#   paste0("Pooled risk difference ", riskdiff_ci_from_meta_subset_rr(meta_select, pscale = scale, digits = digits))
# }
#
# ## clip risk difference from rr meta ------------------ (2024-01-22 16:53) @----
# risk_diff_meta_rr_clip <- function(meta_select = temp_meta, scale = 1000, digits = 1) {
#   (clipr::write_clip(paste0("Pooled risk difference ", rd_ci_from_meta_rr(meta_select, pscale = scale, digits = digits))))
# }
#
# ## text for risk difference from or meta -------------- (2024-01-22 16:53) @----
# risk_diff_meta_or <- function(meta_object = temp_meta, digits = 1, scale = 1000) {
#   paste0("Approximate pooled risk difference ", riskdiff_ci_from_meta_or(meta_object, pscale = scale, digits = digits))
# }
#
# ## clip risk difference from or meta ------------------ (2024-01-22 16:53) @----
# risk_diff_meta_or_clip <- function(meta_object = temp_meta, digits = 1, scale = 1000) {
#   (clipr::write_clip(paste0("Approximate pooled risk difference ", riskdiff_ci_from_meta_or(meta_object, pscale = scale, digits = digits))))
# }
#
#
#
# soe_meta_rd_or <- function(meta_object = temp_meta, digits = 1, scale = 1000) {
#   (clipr::write_clip(paste0("RD ≈ ", riskdiff_ci_from_meta_or(meta_object, pscale = scale, digits = digits))))
# }
#
# soe_meta_rd_rr <- function(meta_object = temp_meta, digits = 1, scale = 1000) {
#   (clipr::write_clip(paste0("RD ", rd_ci_from_meta_rr(meta_object, pscale = scale, digits = digits))))
# }
#
# soe_meta_rd_rr_wcontrol <- function(meta_object = temp_meta, digits = 1, scale = 1000) {
#   (clipr::write_clip(paste0("RD ", rd_ci_from_meta_rr_soe(meta_object, pscale = scale, digits = digits))))
# }
#


#
# rd_ci_from_meta_rr_kq4 <- function(meta_object, pscale = 100, digits = 2) {
#   meta_object <- update(meta_object, sm = "RD")
#   temp <- c(meta_object$TE.random, meta_object$lower.random, meta_object$upper.random) * pscale
#   temp <- formatC(temp, digits = digits, format = "f")
#   paste0(temp[1], " per ", pscale, " (95% CI, ", temp[2], " to ", temp[3], ")")
# }
#
# risk_diff_meta_rr_kq4 <- function(meta_select = temp_meta, scale = 1000, digits = 1) {
#   paste0("Pooled risk difference ", rd_ci_from_meta_rr_kq4(meta_select, pscale = scale, digits = digits))
# }
#
# # risk_diff_meta_rr_clip_kq4 <- function(meta_select = temp_meta, scale = 1000, digits = 1) {
# #   (clipr::write_clip(paste0("Pooled risk difference ", rd_ci_from_meta_rr_kq4(meta_select, pscale = scale, digits = digits))))
# # }
#

#
# risk_diff_meta_or_clip_kq4 <- function(meta_object = temp_meta, digits = 1, scale = 1000) {
#   (clipr::write_clip(paste0("Approximate pooled risk difference ", riskdiff_ci_from_meta_or_kq4(meta_object, pscale = scale, digits = digits))))
# }
#
#
#
# # no control arm event rate 2024-11-04 see below
# # rd_ci_from_meta_rr_kq4 <- function(meta_object, pscale = 100, digits = 2) {
# #   meta_object <- update(meta_object, sm = "RD")
# #   temp <- c(meta_object$TE.random, meta_object$lower.random, meta_object$upper.random) * pscale
# #   temp <- formatC(temp, digits = digits, format = "f")
# #   paste0(temp[1], " per ", pscale, " (95% CI, ", temp[2], " to ", temp[3], ")")
# # }
#
#
