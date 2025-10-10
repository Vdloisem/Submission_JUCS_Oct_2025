# ======================= Decision-Gap Audit (All-in-one) =======================
# Packages ---------------------------------------------------------------------
req <- c("tidyverse","pROC","effsize","ggplot2")
to_install <- req[!req %in% installed.packages()[, "Package"]]
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(req, library, character.only = TRUE))

# ----------------------------- User parameters --------------------------------
in_csv   <- "slr_outputs_sq22/SQ2.2_with_ReasonCode.csv"  
out_dir  <- "decision_gap_outputs_SQ2.2"                   
tau      <- 4                                              # decision threshold (round(score) >= tau)
eps      <- c(0.05, 0.10, 0.20)                            # margins "borderline"
bin_width <- 0.25                                          # width of fixed calibration bins

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
set.seed(42)

# ----------------------------- Load & sanity ----------------------------------
df <- readr::read_csv(in_csv, show_col_types = FALSE)

# Utilities
norm_chr <- function(x) tolower(trimws(as.character(x)))
numify   <- function(x) suppressWarnings(as.numeric(gsub(",", ".", x)))
val_col  <- function(nm) nm %in% names(df) && !all(is.na(df[[nm]]))

# score/judgement fields
if (val_col("Screening score")) {
  score <- numify(df[["Screening score"]])
} else if (val_col("score_num")) {
  score <- numify(df[["score_num"]])
} else {
  stop("No “Screening score” or “score_num” column found.")
}
if (!val_col("Screening judgement")) stop("'Screening judgement' column missing.")
judg <- norm_chr(df[["Screening judgement"]])
incl <- as.integer(judg == "include")

n_total   <- length(score)
n_include <- sum(incl == 1, na.rm = TRUE)
n_exclude <- sum(incl == 0, na.rm = TRUE)

# ----------------------------- Decision gap -----------------------------------
boundary <- tau - 0.5
delta    <- score - boundary

border_tbl <- tibble(
  epsilon = eps,
  borderline_rate = sapply(eps, function(e) mean(abs(delta) <= e, na.rm = TRUE))
)

inc_delta <- delta[incl == 1 & is.finite(delta)]
exc_delta <- delta[incl == 0 & is.finite(delta)]
gap_summary <- tibble(
  boundary = boundary,
  inc_min_margin_p5  = if (length(inc_delta)) unname(quantile(inc_delta, 0.05, na.rm = TRUE)) else NA_real_,
  exc_max_margin_p95 = if (length(exc_delta)) unname(quantile(exc_delta, 0.95, na.rm = TRUE)) else NA_real_ # attendu négatif
)

# ----------------------------- Separation -------------------------------------
# AUC (ROC) + CI
roc_obj <- tryCatch(pROC::roc(incl, score, quiet = TRUE), error = function(e) NULL)
auc_val <- if (!is.null(roc_obj)) as.numeric(pROC::auc(roc_obj)) else NA_real_
auc_ci  <- if (!is.null(roc_obj)) suppressWarnings(pROC::ci.auc(roc_obj)) else c(NA, NA, NA)

# Cohen's d
d_val <- tryCatch(as.numeric(effsize::cohen.d(score ~ incl)$estimate), error = function(e) NA_real_)

sep_tbl <- tibble(
  AUC = auc_val, AUC_LCL = as.numeric(auc_ci[1]), AUC_UCL = as.numeric(auc_ci[3]),
  Cohens_d = d_val
)

# ----------------------------- Confusion @ tau --------------------------------
# rule: include_hat = 1 if round(score) >= tau
inc_hat <- as.integer(round(score) >= tau)
tp <- sum(inc_hat == 1 & incl == 1, na.rm = TRUE)
tn <- sum(inc_hat == 0 & incl == 0, na.rm = TRUE)
fp <- sum(inc_hat == 1 & incl == 0, na.rm = TRUE)
fn <- sum(inc_hat == 0 & incl == 1, na.rm = TRUE)
acc <- (tp + tn) / sum(is.finite(inc_hat + incl))
sens <- tp / (tp + fn)
spec <- tn / (tn + fp)

confusion_tbl <- tibble(
  tau = tau, TP = tp, TN = tn, FP = fp, FN = fn,
  Accuracy = acc, Sensitivity = sens, Specificity = spec
)

# ----------------------------- Stability to jitter ----------------------------
replicas <- 1000
stab_eps <- eps
flip_rates <- purrr::map_dfr(stab_eps, function(e) {
  flips <- replicate(replicas, {
    s2 <- score + runif(length(score), -e, e)
    inc_hat0 <- as.integer(round(score) >= tau)
    inc_hat2 <- as.integer(round(s2)    >= tau)
    mean(inc_hat2 != inc_hat0, na.rm = TRUE)
  })
  tibble(epsilon = e, flip_rate = mean(flips, na.rm = TRUE))
})

# ----------------------------- Calibration (quantiles) ------------------------
valid <- is.finite(score) & !is.na(incl)
if (sum(valid) < 2 || length(unique(score[valid])) < 2) {
  calib_q <- tibble(bin = factor(), bin_center = numeric(), empirical_include = numeric(), n = integer())
} else {
  brks <- as.numeric(quantile(score[valid], probs = seq(0, 1, 0.1), na.rm = TRUE))
  brks <- unique(brks)
  if (length(brks) < 2) {
    calib_q <- tibble(bin = factor(), bin_center = numeric(), empirical_include = numeric(), n = integer())
  } else {
    bin_q <- cut(score, breaks = brks, include.lowest = TRUE, right = TRUE)
    # centre of intervals
    centers <- (brks[-1] + brks[-length(brks)]) / 2
    lvl <- levels(bin_q)
    centers <- centers[seq_along(lvl)]
    by_bin <- tibble(bin = bin_q, incl = incl) |>
      filter(!is.na(bin)) |>
      group_by(bin) |>
      summarise(empirical_include = mean(incl == 1, na.rm = TRUE),
                n = n(), .groups = "drop")
    calib_q <- tibble(bin = factor(lvl, levels = lvl), bin_center = centers) |>
      left_join(by_bin, by = "bin")
  }
}

# ----------------------------- Calibration (fixed bins) -----------------------
if (all(!is.finite(score))) {
  calib_f <- tibble(center = numeric(), empirical_include = numeric(), n = integer())
} else {
  brks_f <- seq(floor(min(score, na.rm = TRUE)),
                ceiling(max(score, na.rm = TRUE)), by = bin_width)
  if (length(unique(brks_f)) < 2) {
    calib_f <- tibble(center = numeric(), empirical_include = numeric(), n = integer())
  } else {
    bin_f <- cut(score, breaks = brks_f, include.lowest = TRUE, right = TRUE)
    centers_f <- (brks_f[-1] + brks_f[-length(brks_f)]) / 2
    by_bin_f <- tibble(bin = bin_f, incl = incl) |>
      filter(!is.na(bin)) |>
      group_by(bin) |>
      summarise(empirical_include = mean(incl == 1, na.rm = TRUE),
                n = n(), .groups = "drop")
    # attach digital centre
    map_center <- tibble(bin = levels(bin_f), center = centers_f)
    calib_f <- left_join(map_center, by_bin_f, by = "bin")
  }
}

# ----------------------------- Save tables ------------------------------------
readr::write_csv(tibble(param = c("n_total","n_include","n_exclude","tau","boundary"),
                        value = c(n_total, n_include, n_exclude, tau, boundary)),
                 file.path(out_dir, "00_summary_params.csv"))
readr::write_csv(border_tbl,   file.path(out_dir, "01_borderline_rates.csv"))
readr::write_csv(gap_summary,  file.path(out_dir, "02_gap_summary.csv"))
readr::write_csv(sep_tbl,      file.path(out_dir, "03_separation_auc_cohend.csv"))
readr::write_csv(confusion_tbl,file.path(out_dir, "04_confusion_at_tau.csv"))
readr::write_csv(flip_rates,   file.path(out_dir, "05_flip_rates_jitter.csv"))
readr::write_csv(calib_q,      file.path(out_dir, "06_calibration_quantiles.csv"))
readr::write_csv(calib_f,      file.path(out_dir, "07_calibration_fixedbins.csv"))

# ----------------------------- Plots (PNG) ------------------------------------
# Decision margins
p_margin <- ggplot(tibble(delta = delta, include = factor(incl, labels = c("exclude","include"))),
                   aes(delta, fill = include)) +
  geom_histogram(position = "identity", alpha = .5, bins = 40) +
  geom_vline(xintercept = 0, linetype = 2) +
  labs(x = expression(paste("Decision margin  ", delta, " = score - (", tau, "-0.5)")),
       y = "Count", fill = NULL, title = "Decision-margin distribution") +
  theme_minimal(base_size = 12)
ggsave(file.path(out_dir, "plot_decision_margin_rq2.png"), p_margin, width = 7, height = 4.5, dpi = 150)

# ROC
if (!is.null(roc_obj)) {
  p_roc <- ggplot(data.frame(
    fpr = 1 - roc_obj$specificities,
    tpr = roc_obj$sensitivities
  ), aes(fpr, tpr)) +
    geom_line() + geom_abline(linetype = 2) +
    labs(title = sprintf("ROC (AUC = %.3f)", auc_val),
         x = "1 - Specificity", y = "Sensitivity") +
    theme_minimal(base_size = 12)
  ggsave(file.path(out_dir, "plot_roc_rq2.png"), p_roc, width = 6, height = 5, dpi = 150)
}

# Quantile calibration
if (nrow(calib_q) > 0) {
  p_cal_q <- ggplot(calib_q, aes(bin_center, empirical_include)) +
    geom_point() + geom_line() +
    geom_vline(xintercept = boundary, linetype = 2) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = "Score", y = "Empirical inclusion rate",
         title = "Calibration curve (by score quantiles)") +
    theme_minimal(base_size = 12)
  ggsave(file.path(out_dir, "plot_calibration_quantiles_rq2.png"), p_cal_q, width = 6.5, height = 4.5, dpi = 150)
}

# Calibration of fixed bins
if (nrow(calib_f) > 0) {
  p_cal_f <- ggplot(calib_f, aes(center, empirical_include)) +
    geom_point() + geom_line() +
    geom_text(aes(label = paste0("n=", n)), vjust = -0.8, size = 3) +
    geom_vline(xintercept = boundary, linetype = 2) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = "Score", y = "Empirical inclusion rate",
         title = sprintf("Calibration (fixed-width bins, bw=%.2f)", bin_width)) +
    theme_minimal(base_size = 12)
  ggsave(file.path(out_dir, "plot_calibration_fixedbins_rq2.png"), p_cal_f, width = 7, height = 4.5, dpi = 150)
}

# ----------------------------- Text summary -----------------------------------
summ_lines <- c(
  sprintf("Input: %s", normalizePath(in_csv)),
  sprintf("Output folder: %s", normalizePath(out_dir)),
  sprintf("n = %d  (include=%d, exclude=%d)", n_total, n_include, n_exclude),
  sprintf("tau = %g  -> boundary = tau - 0.5 = %.2f", tau, boundary),
  sprintf("Borderline rates (|delta| <= eps): %s",
          paste(sprintf("eps=%.2f: %.3f", border_tbl$epsilon, border_tbl$borderline_rate), collapse=" ; ")),
  sprintf("Gap summary: inc p5=%.3f ; exc p95=%.3f",
          gap_summary$inc_min_margin_p5, gap_summary$exc_max_margin_p95),
  sprintf("AUC = %.3f (95%% CI: %.3f–%.3f) ; Cohen's d = %.2f",
          sep_tbl$AUC, sep_tbl$AUC_LCL, sep_tbl$AUC_UCL, sep_tbl$Cohens_d),
  sprintf("Confusion@tau: Acc=%.3f ; Sens=%.3f ; Spec=%.3f (TP=%d, TN=%d, FP=%d, FN=%d)",
          acc, sens, spec, tp, tn, fp, fn),
  sprintf("Flip rates under jitter: %s",
          paste(sprintf("eps=%.2f: %.3f", flip_rates$epsilon, flip_rates$flip_rate), collapse=" ; "))
)
writeLines(summ_lines, con = file.path(out_dir, "99_summary.txt"))

# ----------------------------- Session info -----------------------------------
sink(file.path(out_dir, "98_sessionInfo.txt"))
print(sessionInfo())
sink()
# ==============================================================================
