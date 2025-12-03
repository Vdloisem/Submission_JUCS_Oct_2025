# ==============================================================================
# COMPREHENSIVE INTER-JUDGE RELIABILITY AUDIT SCRIPT BY RESEARCH QUESTION (RQ)
# Calculate: Cohen's Kappa, Correlation, Bias, and Gwet's AC1 (manual implementation)
# ==============================================================================

library(irr)
library(dplyr)
library(ggplot2)

# DATA PREPARATION
file_name <- "PhdSLR-Multilingual_Software_Interop_Audit_Results.csv"

df_raw <- read.csv(file_name,
                   header = TRUE,
                   sep = ",",
                   dec = ",",
                   stringsAsFactors = TRUE)

names(df_raw) <- make.names(names(df_raw), unique = TRUE)

df <- df_raw %>%
  select(
    RQ = RQ,
    Score_A = Screening.score.first.auditor,
    Score_B = Screening.score.second.auditor,
    Judge_A = Screening.judgement.first.auditor,
    Judge_B = Screening.judgement.second.auditor
  )

df <- na.omit(df)


# FUNCTION OF THE GWET'S AC1 CALCULATION
# Formula: AC1 = (Pa - Pe(gamma)) / (1 - Pe(gamma))
gwent_ac1_manual <- function(ratings_df) {
  
  N <- nrow(ratings_df)
  if (N == 0) return(NA)
  
  # Convert to characters to identify categories
  rater_A <- as.character(ratings_df[, 1])
  rater_B <- as.character(ratings_df[, 2])
  categories <- sort(unique(c(rater_A, rater_B)))
  
  # 1. Observed agreement (Pa)
  Pa <- sum(rater_A == rater_B) / N
  
  # 2. Expected agreement (Pe(gamma))
  P_e_gamma <- 0
  for (k in categories) {
    # Proportions for each judge and category k
    pi_k_A <- sum(rater_A == k) / N
    pi_k_B <- sum(rater_B == k) / N
    
    # Average marginal proportions for category k
    avg_pi_k <- (pi_k_A + pi_k_B) / 2
    
    # Sum of the squares of the means of the marginal proportions
    P_e_gamma <- P_e_gamma + (avg_pi_k)^2
  }
  
  # AC1 calculation
  if (P_e_gamma == 1) {
    return(ifelse(Pa == 1, 1, NA))
  }
  
  AC1 <- (Pa - P_e_gamma) / (1 - P_e_gamma)
  return(AC1)
}


# FUNCTION FOR CALCULATING AND DISPLAYING STATISTICAL RESULTS

calculate_metrics <- function(data, rq_id) {
  
  # Metrics on binary judgements (nominal reliability)
  ratings_matrix <- as.matrix(data %>% select(Judge_A, Judge_B))
  
  # Brut agreement
  raw_agreement <- sum(ratings_matrix[, 1] == ratings_matrix[, 2]) / nrow(data)
  
  # Cohen's kappa (k)
  kappa_result <- kappa2(ratings_matrix, weight = "unweighted")
  

  # CALCULATION OF GWET'S AC1
  gwent_ac1_value <- gwent_ac1_manual(data %>% select(Judge_A, Judge_B))
  
  gwent_ac1_display <- if (is.na(gwent_ac1_value)) {
    "NOT CALCULATED (Insufficient data)"
  } else {
    paste0(round(gwent_ac1_value, 4), " (Gwet's AC1 Manual)")
  }
  
  # Continuous score metrics (Intensity consistency)
  scores <- data %>% select(Score_A, Score_B)
  scores$Diff <- scores$Score_A - scores$Score_B
  # Addition of Bland-Altman mean
  scores$Mean <- (scores$Score_A + scores$Score_B) / 2
  
  # Pearson's correlation (r)
  correlation_result <- cor(scores$Score_A, scores$Score_B, method = "pearson")
  
  # Bias (Average difference)
  mean_diff <- mean(scores$Diff)

  cat("\n====================================================\n")
  cat(paste("STATISTICAL RESULTS FOR RQ", rq_id, " \n"))
  cat("====================================================\n")
  cat(paste("Number of studies audited (n): ", nrow(data), "\n"))
  
  cat("\n[1] BINARY RELIABILITY (Include/Exclude Judgement)\n")
  cat("----------------------------------------------------\n")
  cat(paste("Gross Agreement: ", round(raw_agreement * 100, 2), "%\n"))
  cat(paste("Cohen's Kappa (k): ", round(kappa_result$value, 4), " (Warning: Kappa may be low/NA)\n"))
  cat(paste("Robust Coefficient: ", gwent_ac1_display, "\n"))
  
  cat("\n[2] CONTINUOUS SCORE CONSISTENCY (Judgement intensity)\n")
  cat("----------------------------------------------------\n")
  cat(paste("Pearson's correlation (r): ", round(correlation_result, 4), "\n"))
  cat(paste("Mean Difference:", round(mean_diff, 4), "\n"))
  cat("====================================================\n")
  
  return(scores)
}


# GRAPHICS GENERATION AND SAVING FUNCTION (PNG)

generate_and_save_plots <- function(scores_data, rq_id) {
  
  # Calculation of Bland-Altman statistics
  mean_diff <- mean(scores_data$Diff)
  sd_diff <- sd(scores_data$Diff)
  # Limits of agreement (LOA): Bias +/- 1.96 * Standard deviation of differences
  loa_upper <- mean_diff + 1.96 * sd_diff
  loa_lower <- mean_diff - 1.96 * sd_diff
  
  # Set the limits of the axes for a fair comparison (Score from 1 to 5)
  score_limit <- c(1, 5)
  
  # Scatter Plot: Consistency of scores (Score A vs. Score B)
  p_scatter <- ggplot(scores_data, aes(x = Score_A, y = Score_B)) +
    geom_point(color = "#0072B2", size = 3) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
    theme_minimal(base_size = 12) +
    labs(title = paste("Consistency of scores (RQ", rq_id, ")"),
         subtitle = paste("Correlation (r):", round(cor(scores_data$Score_A, scores_data$Score_B), 4)),
         x = "Auditor score 1",
         y = "Auditor score 2") +
    coord_fixed(ratio = 1, xlim = score_limit, ylim = score_limit)
  
  scatter_filename <- paste0("audit_rq", rq_id, "_scatter.png")
  ggsave(scatter_filename, plot = p_scatter, width = 6, height = 6)
  cat(paste("Saved:", scatter_filename, "\n"))
  
  # Histogram: Distribution of the difference (Bias)
  p_hist <- ggplot(scores_data, aes(x = Diff)) +
    geom_histogram(binwidth = 0.1, fill = "#009E73", color = "white", alpha = 0.8) +
    # Reference line at 0 (no bias)
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
    theme_minimal(base_size = 12) +
    labs(title = paste("Distribution of score differences (RQ", rq_id, ")"),
         subtitle = paste("Medium bias:", round(mean_diff, 4)),
         x = "Score difference (Auditor 1 - Auditor 2)",
         y = "Frequency")
  
  hist_filename <- paste0("audit_rq", rq_id, "_histogram.png")
  ggsave(hist_filename, plot = p_hist, width = 6, height = 4)
  cat(paste("Saved:", hist_filename, "\n"))
  
  # Bland-Altman plot: Difference vs. Mean 
  p_bland_altman <- ggplot(scores_data, aes(x = Mean, y = Diff)) +
    geom_point(color = "#D55E00", size = 3) +
    # Bias line (Average of differences)
    geom_hline(yintercept = mean_diff, color = "#0072B2", linetype = "solid", size = 1) +
    # Limits of agreement (LOA)
    geom_hline(yintercept = loa_upper, color = "#CC79A7", linetype = "dashed", size = 0.8) +
    geom_hline(yintercept = loa_lower, color = "#CC79A7", linetype = "dashed", size = 0.8) +
    # Perfect agreement Line (difference = 0)
    geom_hline(yintercept = 0, color = "red", linetype = "dotted", size = 0.5) +
    theme_minimal(base_size = 12) +
    labs(title = paste("Bland-Altman plot (RQ", rq_id, ")"),
         subtitle = paste("Bias (Blue line):", round(mean_diff, 3), " | LOA (Dotted lines):", round(loa_lower, 3), " to", round(loa_upper, 3)),
         x = "Average scores ((A + B) / 2)",
         y = "Score difference (A - B)") +
    scale_x_continuous(limits = score_limit)
  
  bland_altman_filename <- paste0("audit_rq", rq_id, "_blandaltman.png")
  ggsave(bland_altman_filename, plot = p_bland_altman, width = 7, height = 5)
  cat(paste("Saved::", bland_altman_filename, "\n"))
}

# EXECUTION OF THE ANALYSIS BY RESEARCH QUESTION (RQ)

cat("\n[START OF AUDIT ANALYSIS]\n")

data_rq1 <- df %>% filter(RQ == 1)
scores_rq1 <- calculate_metrics(data_rq1, 1)
generate_and_save_plots(scores_rq1, 1)

data_rq2 <- df %>% filter(RQ == 2)
scores_rq2 <- calculate_metrics(data_rq2, 2)
generate_and_save_plots(scores_rq2, 2)


cat("\n[ANALYSIS COMPLETED - 6 PNG FILES SAVED]\n")