#' Example Script: Running the RSE Model
#'
#' @description Step-by-step example demonstrating the complete RSE pipeline
#' @author Sports Analytics Research Team

# ==============================================================================
# SETUP
# ==============================================================================

# Clear environment
rm(list = ls())

# Load the RSE model functions
source("rse_model.R")

cat("\n")
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║                                                              ║\n")
cat("║  RSE MODEL - EXAMPLE EXECUTION                              ║\n")
cat("║  Roster Strategy & Efficiency Analysis                      ║\n")
cat("║                                                              ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

# ==============================================================================
# OPTION 1: QUICK START (Automated Pipeline)
# ==============================================================================

cat("OPTION 1: Running automated pipeline...\n")
cat(strrep("-", 60), "\n\n")

# Generate sample data if it doesn't exist
if (!file.exists("nba_player_data.csv")) {
  cat("Sample data not found. Generating...\n")
  source("generate_sample_data.R")
  cat("\n")
}

# Run complete pipeline
results <- run_rse_pipeline(
  data_path = "nba_player_data.csv",
  target_var = "WIN_PCT",
  output_dir = "output"
)

cat("\n✅ Pipeline complete! Check the 'output/' folder for results.\n\n")

# ==============================================================================
# OPTION 2: STEP-BY-STEP EXECUTION (Advanced Users)
# ==============================================================================

cat("OPTION 2: Step-by-step execution (for custom analysis)...\n")
cat(strrep("-", 60), "\n\n")

# Step 1: Load and preprocess
cat("Step 1: Loading and preprocessing data...\n")
processed_data <- load_and_preprocess_data(
  file_path = "nba_player_data.csv",
  min_minutes = 500  # Filter low-minute players
)

# Step 2: PCA
cat("\nStep 2: Performing PCA...\n")
pca_results <- perform_pca(
  data = processed_data,
  variance_threshold = 0.80  # Retain 80% variance
)

# Step 3: LASSO
cat("\nStep 3: Fitting LASSO regression...\n")
lasso_results <- fit_lasso_regression(
  pca_data = pca_results$pca_data,
  target_var = "WIN_PCT",
  train_proportion = 0.75,
  cv_folds = 10
)

# Step 4: Strategic Analysis
cat("\nStep 4: Generating strategic insights...\n")

# 4a. Player Success Index
success_index <- generate_success_index(
  pca_data = pca_results$pca_data,
  lasso_results = lasso_results
)

# 4b. Roster Balance Example
cat("\n--- Roster Balance Comparison ---\n")

superteam_usage <- c(32, 30, 28, 15, 12, 10, 8, 6, 5, 4)
balanced_usage <- c(22, 21, 19, 18, 16, 14, 12, 10, 8, 6)

superteam_balance <- calculate_roster_balance(superteam_usage)
balanced_balance <- calculate_roster_balance(balanced_usage)

cat("Superteam (high concentration) balance:", round(superteam_balance, 3), "\n")
cat("Balanced roster balance:", round(balanced_balance, 3), "\n")
cat("Efficiency gain (balanced):", round((balanced_balance - superteam_balance) * 100, 1), "%\n\n")

# 4c. Monte Carlo Simulation
cat("Running superteam risk simulations...\n")
simulation_results <- simulate_superteam_risk(
  player_data = processed_data,
  success_index = success_index,
  n_simulations = 1000,
  roster_size = 10
)

# ==============================================================================
# CUSTOM ANALYSIS EXAMPLES
# ==============================================================================

cat("\n")
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║  CUSTOM ANALYSIS EXAMPLES                                   ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

# Example 1: Top 5 Undervalued Players
cat("📊 Example 1: Top 5 Undervalued Players for Trading\n")
cat(strrep("-", 60), "\n")

top_5 <- success_index %>%
  arrange(desc(SUCCESS_INDEX)) %>%
  head(5) %>%
  select(RANK, PLAYER_NAME, SUCCESS_INDEX, TIER, PERCENTILE)

print(top_5)

# Example 2: Compare Different Roster Sizes
cat("\n\n📊 Example 2: Optimal Roster Size Analysis\n")
cat(strrep("-", 60), "\n")

roster_sizes <- c(8, 10, 12, 15)
risk_comparison <- map_dfr(roster_sizes, function(size) {
  sim <- simulate_superteam_risk(
    processed_data,
    success_index,
    n_simulations = 500,
    roster_size = size
  )

  data.frame(
    roster_size = size,
    avg_balance = mean(sim$balance_index, na.rm = TRUE),
    avg_risk = mean(sim$risk_score, na.rm = TRUE),
    avg_success = mean(sim$avg_success_index, na.rm = TRUE)
  )
})

print(risk_comparison)

cat("\n💡 Interpretation: Lower risk score = more efficient roster construction\n")

# Example 3: PCA Component Interpretation
cat("\n\n📊 Example 3: Top Contributing Variables to PC1\n")
cat(strrep("-", 60), "\n")

# Extract loadings for PC1
pc1_loadings <- pca_results$pca_obj$rotation[, 1]
top_pc1_vars <- sort(abs(pc1_loadings), decreasing = TRUE)[1:5]

cat("Top 5 variables driving Principal Component 1:\n")
print(top_pc1_vars)

# Example 4: Model Validation
cat("\n\n📊 Example 4: Cross-Validation Error Curve\n")
cat(strrep("-", 60), "\n")

cv_errors <- data.frame(
  lambda = lasso_results$cv_model$lambda,
  mse = lasso_results$cv_model$cvm,
  se = lasso_results$cv_model$cvsd
)

best_lambda_idx <- which.min(cv_errors$mse)
cat("Optimal lambda:", round(cv_errors$lambda[best_lambda_idx], 6), "\n")
cat("CV MSE at optimal lambda:", round(cv_errors$mse[best_lambda_idx], 6), "\n")
cat("Standard error:", round(cv_errors$se[best_lambda_idx], 6), "\n")

# ==============================================================================
# SAVE CUSTOM RESULTS
# ==============================================================================

cat("\n\n💾 Saving custom analysis results...\n")

# Save top players
write_csv(top_5, "output/top_5_undervalued.csv")

# Save roster size comparison
write_csv(risk_comparison, "output/roster_size_analysis.csv")

# Save PCA loadings
loadings_df <- as.data.frame(pca_results$pca_obj$rotation[, 1:pca_results$n_components])
loadings_df$Variable <- rownames(loadings_df)
write_csv(loadings_df, "output/pca_loadings.csv")

cat("✓ Saved: top_5_undervalued.csv\n")
cat("✓ Saved: roster_size_analysis.csv\n")
cat("✓ Saved: pca_loadings.csv\n")

# ==============================================================================
# VISUALIZATIONS
# ==============================================================================

cat("\n📈 Generating additional visualizations...\n")

# Custom plot: Success Index Distribution
p_success <- ggplot(success_index, aes(x = SUCCESS_INDEX, fill = TIER)) +
  geom_histogram(bins = 30, alpha = 0.8, color = "white") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "Player Success Index Distribution by Tier",
    x = "Success Index",
    y = "Count",
    fill = "Player Tier"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("output/success_index_distribution.png", p_success, width = 10, height = 6, dpi = 300)
cat("✓ Saved: success_index_distribution.png\n")

# Custom plot: Risk vs Balance
p_risk <- ggplot(simulation_results, aes(x = balance_index, y = risk_score)) +
  geom_point(alpha = 0.3, color = "#E74C3C") +
  geom_smooth(method = "loess", color = "#3498DB", size = 1.5) +
  labs(
    title = "Roster Balance vs Risk Score (1,000 Simulations)",
    subtitle = "Higher balance index → Lower risk",
    x = "Roster Balance Index",
    y = "Risk Score"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave("output/balance_vs_risk.png", p_risk, width = 10, height = 6, dpi = 300)
cat("✓ Saved: balance_vs_risk.png\n")

# ==============================================================================
# SUMMARY
# ==============================================================================

cat("\n")
cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║                                                              ║\n")
cat("║  ✅ ANALYSIS COMPLETE                                        ║\n")
cat("║                                                              ║\n")
cat("║  All results saved to: output/                              ║\n")
cat("║                                                              ║\n")
cat("║  Key Findings:                                              ║\n")
cat(sprintf("║  • Training RMSE: %.4f                                      ║\n", lasso_results$train_rmse))
cat(sprintf("║  • Test RMSE: %.4f                                          ║\n", lasso_results$test_rmse))
cat(sprintf("║  • RMSE Ratio: %.3f %s                                ║\n",
        lasso_results$test_rmse / lasso_results$train_rmse,
        ifelse(lasso_results$test_rmse / lasso_results$train_rmse < 1.2, "(Good ✓)", "(Review ⚠)")))
cat(sprintf("║  • Non-zero coefficients: %d/%d                              ║\n",
        nrow(lasso_results$non_zero_coef) - 1,
        nrow(lasso_results$coefficients) - 1))
cat("║                                                              ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

cat("📚 Next Steps:\n")
cat("  1. Review visualizations in output/ folder\n")
cat("  2. Analyze top_5_undervalued.csv for draft/trade targets\n")
cat("  3. Compare roster_size_analysis.csv to optimize team construction\n")
cat("  4. Consult README.md for interpretation guidelines\n\n")

cat("🎉 Happy analyzing!\n\n")
