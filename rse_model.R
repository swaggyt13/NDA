#' Roster Strategy & Efficiency (RSE) Model
#'
#' @title NBA Superteam Efficiency Analysis
#' @description Professional-grade statistical model evaluating NBA drafting and
#'   trading strategies using PCA and LASSO regression to assess superteam efficiency
#' @author Senior Research Lead in Sports Analytics
#' @date 2026-02-11

# ==============================================================================
# LIBRARY DEPENDENCIES
# ==============================================================================

suppressPackageStartupMessages({
  library(tidyverse)    # Data manipulation and ggplot2
  library(glmnet)       # LASSO regression
  library(caret)        # Cross-validation utilities
  library(scales)       # Visualization scaling
})

# ==============================================================================
# DATA PREPROCESSING FUNCTIONS
# ==============================================================================

#' Load and preprocess NBA player data
#'
#' @param file_path Character. Path to CSV file containing player statistics
#' @param min_minutes Numeric. Minimum total minutes threshold (default: 500)
#' @return Preprocessed data frame with filtered and normalized metrics
#' @export
load_and_preprocess_data <- function(file_path, min_minutes = 500) {

  cat("Loading data from:", file_path, "\n")

  # Load raw data
  raw_data <- read_csv(file_path, show_col_types = FALSE)

  cat("Initial dataset:", nrow(raw_data), "players\n")

  # Filter low-minute players to reduce noise
  filtered_data <- raw_data %>%
    filter(MIN_TOTAL >= min_minutes)

  cat("After filtering (MIN >=", min_minutes, "):", nrow(filtered_data), "players\n")

  # Identify numeric columns for normalization (exclude ID and target variables)
  numeric_cols <- filtered_data %>%
    select(where(is.numeric), -any_of(c("PLAYER_ID", "WIN_PCT", "NET_RATING"))) %>%
    names()

  # Apply Z-score normalization
  normalized_data <- filtered_data %>%
    mutate(across(all_of(numeric_cols),
                  ~as.numeric(scale(.)),
                  .names = "{.col}_Z"))

  cat("Z-score normalization applied to", length(numeric_cols), "metrics\n\n")

  return(normalized_data)
}

# ==============================================================================
# PRINCIPAL COMPONENT ANALYSIS (PCA)
# ==============================================================================

#' Perform PCA for dimension reduction
#'
#' @param data Data frame with normalized metrics
#' @param variance_threshold Numeric. Minimum cumulative variance to retain (default: 0.80)
#' @return List containing PCA object, transformed data, and summary statistics
#' @export
perform_pca <- function(data, variance_threshold = 0.80) {

  cat("=== PRINCIPAL COMPONENT ANALYSIS ===\n")

  # Select normalized features (excluding original metrics and target variables)
  feature_cols <- data %>%
    select(ends_with("_Z")) %>%
    select(where(~!any(is.na(.)))) %>%
    names()

  feature_matrix <- data %>%
    select(all_of(feature_cols)) %>%
    as.matrix()

  # Perform PCA
  pca_result <- prcomp(feature_matrix, center = FALSE, scale. = FALSE)

  # Calculate variance explained
  variance_explained <- summary(pca_result)$importance[2, ]
  cumulative_variance <- cumsum(variance_explained)

  # Determine number of components for threshold
  n_components <- which(cumulative_variance >= variance_threshold)[1]

  cat("Total components:", length(variance_explained), "\n")
  cat("Components explaining >=", variance_threshold * 100, "% variance:",
      n_components, "\n")
  cat("Cumulative variance explained:",
      round(cumulative_variance[n_components] * 100, 2), "%\n\n")

  # Extract principal component scores
  pc_scores <- as.data.frame(pca_result$x[, 1:n_components])
  colnames(pc_scores) <- paste0("PC", 1:n_components)

  # Combine with target variable
  pca_data <- bind_cols(
    data %>% select(any_of(c("PLAYER_ID", "PLAYER_NAME", "WIN_PCT", "NET_RATING"))),
    pc_scores
  )

  return(list(
    pca_obj = pca_result,
    pca_data = pca_data,
    variance_explained = variance_explained,
    cumulative_variance = cumulative_variance,
    n_components = n_components,
    feature_names = feature_cols
  ))
}

#' Generate PCA Scree Plot
#'
#' @param pca_results List. Output from perform_pca()
#' @return ggplot object
#' @export
plot_pca_scree <- function(pca_results) {

  scree_data <- data.frame(
    PC = 1:length(pca_results$variance_explained),
    Variance = pca_results$variance_explained * 100,
    Cumulative = pca_results$cumulative_variance * 100
  )

  ggplot(scree_data, aes(x = PC)) +
    geom_col(aes(y = Variance), fill = "#2C3E50", alpha = 0.7) +
    geom_line(aes(y = Cumulative), color = "#E74C3C", size = 1.2) +
    geom_point(aes(y = Cumulative), color = "#E74C3C", size = 3) +
    geom_hline(yintercept = 80, linetype = "dashed", color = "#27AE60", size = 1) +
    annotate("text", x = max(scree_data$PC) * 0.8, y = 82,
             label = "80% Threshold", color = "#27AE60", size = 4) +
    labs(
      title = "PCA Scree Plot: Variance Explained by Principal Components",
      subtitle = "Dimension Reduction for NBA Player Impact Metrics",
      x = "Principal Component",
      y = "Variance Explained (%)"
    ) +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      panel.grid.minor = element_blank()
    )
}

# ==============================================================================
# LASSO REGRESSION
# ==============================================================================

#' Fit LASSO regression with cross-validation
#'
#' @param pca_data Data frame. Output from perform_pca()$pca_data
#' @param target_var Character. Name of target variable ("WIN_PCT" or "NET_RATING")
#' @param train_proportion Numeric. Proportion of data for training (default: 0.75)
#' @param cv_folds Integer. Number of cross-validation folds (default: 10)
#' @return List containing LASSO model, predictions, and diagnostics
#' @export
fit_lasso_regression <- function(pca_data, target_var = "WIN_PCT",
                                   train_proportion = 0.75, cv_folds = 10) {

  cat("=== LASSO REGRESSION ===\n")
  cat("Target Variable:", target_var, "\n")

  # Remove rows with missing target
  complete_data <- pca_data %>%
    filter(!is.na(.data[[target_var]]))

  # Train-test split
  set.seed(42)
  train_indices <- sample(1:nrow(complete_data),
                          size = floor(train_proportion * nrow(complete_data)))

  train_data <- complete_data[train_indices, ]
  test_data <- complete_data[-train_indices, ]

  cat("Training set:", nrow(train_data), "observations\n")
  cat("Test set:", nrow(test_data), "observations\n")

  # Prepare matrices
  pc_cols <- grep("^PC[0-9]+$", names(train_data), value = TRUE)

  x_train <- as.matrix(train_data[, pc_cols])
  y_train <- train_data[[target_var]]

  x_test <- as.matrix(test_data[, pc_cols])
  y_test <- test_data[[target_var]]

  # Cross-validated LASSO
  cat("Performing", cv_folds, "-fold cross-validation...\n")

  cv_lasso <- cv.glmnet(
    x = x_train,
    y = y_train,
    alpha = 1,           # LASSO (alpha = 1)
    nfolds = cv_folds,
    type.measure = "mse"
  )

  # Optimal lambda
  lambda_min <- cv_lasso$lambda.min
  lambda_1se <- cv_lasso$lambda.1se

  cat("Optimal lambda (min):", round(lambda_min, 6), "\n")
  cat("Optimal lambda (1se):", round(lambda_1se, 6), "\n")

  # Fit final model with lambda.min
  final_model <- glmnet(x_train, y_train, alpha = 1, lambda = lambda_min)

  # Predictions
  train_pred <- predict(final_model, newx = x_train, s = lambda_min)
  test_pred <- predict(final_model, newx = x_test, s = lambda_min)

  # Calculate RMSE
  train_rmse <- sqrt(mean((y_train - train_pred)^2))
  test_rmse <- sqrt(mean((y_test - test_pred)^2))

  cat("\n--- Model Performance ---\n")
  cat("Training RMSE:", round(train_rmse, 4), "\n")
  cat("Test RMSE:", round(test_rmse, 4), "\n")
  cat("RMSE Ratio (Test/Train):", round(test_rmse / train_rmse, 3), "\n")

  if (test_rmse / train_rmse < 1.2) {
    cat("✓ Model is NOT overfitted (ratio < 1.2)\n\n")
  } else {
    cat("⚠ Potential overfitting detected (ratio >= 1.2)\n\n")
  }

  # Extract coefficients
  coefficients <- as.matrix(coef(final_model, s = lambda_min))
  non_zero_coef <- coefficients[coefficients[, 1] != 0, , drop = FALSE]

  cat("Non-zero coefficients:", nrow(non_zero_coef) - 1, "out of", nrow(coefficients) - 1, "\n\n")

  return(list(
    cv_model = cv_lasso,
    final_model = final_model,
    lambda_min = lambda_min,
    lambda_1se = lambda_1se,
    coefficients = coefficients,
    non_zero_coef = non_zero_coef,
    train_rmse = train_rmse,
    test_rmse = test_rmse,
    train_data = train_data,
    test_data = test_data,
    predictions = list(
      train = data.frame(actual = y_train, predicted = as.vector(train_pred)),
      test = data.frame(actual = y_test, predicted = as.vector(test_pred))
    )
  ))
}

#' Plot LASSO Coefficient Path
#'
#' @param lasso_results List. Output from fit_lasso_regression()
#' @return ggplot object
#' @export
plot_lasso_path <- function(lasso_results) {

  # Extract coefficient path
  coef_matrix <- as.matrix(coef(lasso_results$cv_model$glmnet.fit))
  lambda_seq <- lasso_results$cv_model$glmnet.fit$lambda

  # Convert to long format (exclude intercept)
  coef_df <- as.data.frame(t(coef_matrix[-1, ]))
  coef_df$lambda <- lambda_seq

  coef_long <- coef_df %>%
    pivot_longer(-lambda, names_to = "Variable", values_to = "Coefficient")

  ggplot(coef_long, aes(x = log(lambda), y = Coefficient, color = Variable)) +
    geom_line(size = 1, alpha = 0.8) +
    geom_vline(xintercept = log(lasso_results$lambda_min),
               linetype = "dashed", color = "#E74C3C", size = 1) +
    annotate("text", x = log(lasso_results$lambda_min), y = max(coef_long$Coefficient) * 0.9,
             label = "Optimal λ", color = "#E74C3C", size = 4, hjust = -0.1) +
    labs(
      title = "LASSO Coefficient Path: Feature Selection via Regularization",
      subtitle = "Coefficients shrink to zero as penalty (λ) increases",
      x = "Log(Lambda)",
      y = "Standardized Coefficient"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "right",
      panel.grid.minor = element_blank()
    )
}

# ==============================================================================
# STRATEGIC ANALYSIS FUNCTIONS
# ==============================================================================

#' Calculate Roster Balance Index
#'
#' @description Measures diminishing returns of stacking high-usage players
#' @param usage_rates Numeric vector. Usage rates (%) for roster players
#' @param diminishing_factor Numeric. Penalty factor for overlap (default: 1.5)
#' @return Numeric. Balance index (higher = more balanced roster)
#' @export
calculate_roster_balance <- function(usage_rates, diminishing_factor = 1.5) {

  if (length(usage_rates) == 0) return(NA)

  # Sort usage rates in descending order
  sorted_usage <- sort(usage_rates, decreasing = TRUE)

  # Apply diminishing returns: each additional high-usage player gets penalized
  # Formula: sum(usage_i / (position^diminishing_factor))
  weighted_usage <- sum(sorted_usage / (seq_along(sorted_usage)^diminishing_factor))

  # Normalize by roster size and max possible usage
  max_possible <- sum(sorted_usage)
  balance_index <- weighted_usage / max_possible

  return(balance_index)
}

#' Generate Player Success Index
#'
#' @description Calculates efficiency score based on LASSO coefficients
#' @param pca_data Data frame. Player data with PC scores
#' @param lasso_results List. Output from fit_lasso_regression()
#' @return Data frame with player names and success indices
#' @export
generate_success_index <- function(pca_data, lasso_results) {

  cat("=== PLAYER SUCCESS INDEX ===\n")

  # Extract coefficients (exclude intercept)
  coefs <- lasso_results$non_zero_coef[-1, 1]
  pc_names <- rownames(lasso_results$non_zero_coef)[-1]

  # Calculate success index as weighted sum of PCs
  pc_cols <- grep("^PC[0-9]+$", names(pca_data), value = TRUE)

  success_scores <- as.matrix(pca_data[, pc_cols]) %*% as.matrix(lasso_results$coefficients[-1, ])

  success_df <- data.frame(
    PLAYER_NAME = pca_data$PLAYER_NAME,
    SUCCESS_INDEX = as.vector(success_scores)
  ) %>%
    arrange(desc(SUCCESS_INDEX)) %>%
    mutate(
      RANK = row_number(),
      PERCENTILE = percent_rank(SUCCESS_INDEX) * 100,
      TIER = case_when(
        PERCENTILE >= 90 ~ "Elite",
        PERCENTILE >= 75 ~ "High Value",
        PERCENTILE >= 50 ~ "Above Average",
        PERCENTILE >= 25 ~ "Average",
        TRUE ~ "Below Average"
      )
    )

  cat("Success Index calculated for", nrow(success_df), "players\n")
  cat("\nTop 10 Undervalued Assets:\n")
  print(success_df %>% head(10) %>% select(RANK, PLAYER_NAME, SUCCESS_INDEX, TIER))

  return(success_df)
}

#' Simulate Superteam Risk
#'
#' @description Monte Carlo simulation of roster configurations
#' @param player_data Data frame. Must contain USAGE_RATE_Z
#' @param success_index Data frame. Output from generate_success_index()
#' @param n_simulations Integer. Number of roster simulations (default: 1000)
#' @param roster_size Integer. Players per roster (default: 10)
#' @return Data frame with simulation results
#' @export
simulate_superteam_risk <- function(player_data, success_index,
                                     n_simulations = 1000, roster_size = 10) {

  cat("=== SUPERTEAM RISK SIMULATION ===\n")
  cat("Simulations:", n_simulations, "| Roster size:", roster_size, "\n\n")

  # Merge data
  sim_data <- player_data %>%
    inner_join(success_index, by = "PLAYER_NAME") %>%
    filter(!is.na(USAGE_RATE_Z))

  # Run simulations
  set.seed(123)
  simulation_results <- map_dfr(1:n_simulations, function(sim) {

    # Random roster selection
    roster <- sim_data %>%
      sample_n(min(roster_size, nrow(sim_data)))

    # Calculate metrics
    avg_success <- mean(roster$SUCCESS_INDEX)
    total_usage <- sum(roster$USAGE_RATE_Z)
    balance_index <- calculate_roster_balance(roster$USAGE_RATE_Z)

    data.frame(
      simulation = sim,
      avg_success_index = avg_success,
      total_usage = total_usage,
      balance_index = balance_index,
      risk_score = total_usage / (balance_index + 0.01) # Higher = more risk
    )
  })

  cat("Average Balance Index:", round(mean(simulation_results$balance_index, na.rm = TRUE), 3), "\n")
  cat("Average Risk Score:", round(mean(simulation_results$risk_score, na.rm = TRUE), 3), "\n")

  return(simulation_results)
}

# ==============================================================================
# DIAGNOSTIC VISUALIZATIONS
# ==============================================================================

#' Plot Training vs Test Performance
#'
#' @param lasso_results List. Output from fit_lasso_regression()
#' @return ggplot object
#' @export
plot_diagnostics <- function(lasso_results) {

  train_df <- lasso_results$predictions$train %>% mutate(set = "Training")
  test_df <- lasso_results$predictions$test %>% mutate(set = "Test")

  combined <- bind_rows(train_df, test_df)

  ggplot(combined, aes(x = actual, y = predicted, color = set)) +
    geom_point(alpha = 0.6, size = 2) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black", size = 1) +
    geom_smooth(method = "lm", se = FALSE, size = 1) +
    scale_color_manual(values = c("Training" = "#3498DB", "Test" = "#E74C3C")) +
    labs(
      title = "Model Diagnostics: Predicted vs Actual Performance",
      subtitle = sprintf("Training RMSE: %.4f | Test RMSE: %.4f | Ratio: %.3f",
                         lasso_results$train_rmse,
                         lasso_results$test_rmse,
                         lasso_results$test_rmse / lasso_results$train_rmse),
      x = "Actual Win %",
      y = "Predicted Win %",
      color = "Dataset"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      legend.position = "top"
    )
}

# ==============================================================================
# MAIN EXECUTION PIPELINE
# ==============================================================================

#' Run complete RSE analysis pipeline
#'
#' @param data_path Character. Path to input CSV file
#' @param target_var Character. Target variable name (default: "WIN_PCT")
#' @param output_dir Character. Directory for saving plots (default: "output")
#' @export
run_rse_pipeline <- function(data_path, target_var = "WIN_PCT", output_dir = "output") {

  cat("\n")
  cat("╔══════════════════════════════════════════════════════════════╗\n")
  cat("║  ROSTER STRATEGY & EFFICIENCY (RSE) MODEL                   ║\n")
  cat("║  NBA Superteam Analysis Pipeline                            ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")

  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Step 1: Load and preprocess
  cat("STEP 1: DATA PREPROCESSING\n")
  cat(strrep("=", 60), "\n")
  processed_data <- load_and_preprocess_data(data_path, min_minutes = 500)

  # Step 2: PCA
  cat("STEP 2: DIMENSION REDUCTION\n")
  cat(strrep("=", 60), "\n")
  pca_results <- perform_pca(processed_data, variance_threshold = 0.80)

  # Step 3: LASSO
  cat("STEP 3: REGULARIZED REGRESSION\n")
  cat(strrep("=", 60), "\n")
  lasso_results <- fit_lasso_regression(pca_results$pca_data, target_var = target_var)

  # Step 4: Strategic Analysis
  cat("STEP 4: STRATEGIC ANALYSIS\n")
  cat(strrep("=", 60), "\n")
  success_index <- generate_success_index(pca_results$pca_data, lasso_results)

  simulation_results <- simulate_superteam_risk(
    processed_data,
    success_index,
    n_simulations = 1000,
    roster_size = 10
  )

  # Step 5: Visualizations
  cat("\nSTEP 5: GENERATING VISUALIZATIONS\n")
  cat(strrep("=", 60), "\n")

  p1 <- plot_pca_scree(pca_results)
  ggsave(file.path(output_dir, "pca_scree_plot.png"), p1, width = 10, height = 6, dpi = 300)
  cat("✓ Saved: pca_scree_plot.png\n")

  p2 <- plot_lasso_path(lasso_results)
  ggsave(file.path(output_dir, "lasso_coefficient_path.png"), p2, width = 10, height = 6, dpi = 300)
  cat("✓ Saved: lasso_coefficient_path.png\n")

  p3 <- plot_diagnostics(lasso_results)
  ggsave(file.path(output_dir, "model_diagnostics.png"), p3, width = 10, height = 6, dpi = 300)
  cat("✓ Saved: model_diagnostics.png\n")

  # Save results
  write_csv(success_index, file.path(output_dir, "player_success_index.csv"))
  write_csv(simulation_results, file.path(output_dir, "superteam_simulations.csv"))
  cat("✓ Saved: player_success_index.csv\n")
  cat("✓ Saved: superteam_simulations.csv\n")

  cat("\n")
  cat("╔══════════════════════════════════════════════════════════════╗\n")
  cat("║  PIPELINE COMPLETE                                          ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")

  return(list(
    processed_data = processed_data,
    pca_results = pca_results,
    lasso_results = lasso_results,
    success_index = success_index,
    simulation_results = simulation_results
  ))
}

# ==============================================================================
# EXAMPLE USAGE
# ==============================================================================

# Uncomment to run:
# results <- run_rse_pipeline(
#   data_path = "nba_player_data.csv",
#   target_var = "WIN_PCT",
#   output_dir = "output"
# )
