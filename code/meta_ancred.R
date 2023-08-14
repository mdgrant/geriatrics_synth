# AnCred for meta-analysis
meta_ancred <- function(meta_fit){
  cpi_sig_or <- function(lower, upper){
    upper <- log(upper)
    lower <- log(lower)
    sl <- (upper - lower)^2 / (4 * sqrt(upper * lower))
    exp(sl)
  }

  cpi_nsig_or <- function(lower, upper){
    al <- exp((log(upper*lower) * (log(upper/lower))^2) / (2*log(upper) * log(lower)))
    al
  }

  lt_1 <- ifelse(exp(update(meta_fit, sm = "OR")$TE.random) < 1, TRUE, FALSE)

  odds_ratio <- exp(update(meta_fit, sm = "OR")$TE.random)
  odds_ratio_low <- exp(update(meta_fit, sm = "OR")$lower.random)
  odds_ratio_up <- exp(update(meta_fit, sm = "OR")$upper.random)
  cpi <- case_when(
    meta_fit$pval.random < 0.05 ~ cpi_sig_or(odds_ratio_low, odds_ratio_up),
    meta_fit$pval.random < 0.05 ~ cpi_nsig_or(odds_ratio_low, odds_ratio_up),
    .default = NA
  )
  event_rate <- gtools::inv.logit(metaprop(meta_fit$event.c, meta_fit$n.c)$TE.random)
  cpi <- ifelse(lt_1 == TRUE, 1/cpi, cpi)
  cpi_rr <- DescTools::ORToRelRisk(cpi, event_rate)

  result <- case_when(
    lt_1 == TRUE & cpi_rr < 1 ~ paste("Convincing to a skeptic if plausible RR <", round(cpi_rr, 2)),
    lt_1 == TRUE & cpi_rr > 1 ~ paste("Convincing to a skeptic if plausible RR >", round(cpi_rr, 2)),
    lt_1 == FALSE ~ paste("Convincing to an advocate if prior evidence supports a RR between 1 and ", round(cpi_rr, 2)),
  )
  result
  # list(odds_ratio, odds_ratio_low, odds_ratio_up, meta_fit$pval.random, cpi, event_rate, cpi_rr)
}


