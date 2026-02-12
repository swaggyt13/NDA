# Roster Strategy & Efficiency (RSE) Model

> **Professional-grade NBA analytics model for evaluating superteam strategies using PCA and LASSO regression**

[![R Version](https://img.shields.io/badge/R-%E2%89%A54.0-blue)](https://www.r-project.org/)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)

## 📋 Overview

The **RSE Model** is a research-grade statistical framework designed to answer a critical question in modern NBA team building:

> **Are "superteam" roster constructions statistically efficient, or do they contain hidden risks regarding winning probability?**

This model combines **Principal Component Analysis (PCA)** for dimension reduction with **LASSO regression** for feature selection to identify optimal roster construction strategies while accounting for diminishing returns from stacking high-usage players.

---

## 🎯 Key Features

- ✅ **Automated Data Preprocessing**: Z-score normalization and noise filtering
- ✅ **Dimension Reduction (PCA)**: Groups correlated metrics into latent "Impact Components"
- ✅ **Regularized Regression (LASSO)**: Feature selection with cross-validation to prevent overfitting
- ✅ **Roster Balance Index**: Quantifies diminishing returns of high-usage player stacking
- ✅ **Player Success Index**: Efficiency scoring system for identifying undervalued assets
- ✅ **Publication-Ready Visualizations**: Scree plots, coefficient paths, and diagnostic charts
- ✅ **Overfitting Detection**: Training vs. test RMSE comparison
- ✅ **Modular, Documented Code**: Full `roxygen2` documentation for all functions

---

## 📦 Installation

### Prerequisites

Ensure you have **R ≥ 4.0** installed. You can download R from [CRAN](https://cran.r-project.org/).

### Required R Packages

Install dependencies by running:

```r
install.packages(c(
  "tidyverse",   # Data manipulation and ggplot2
  "glmnet",      # LASSO regression
  "caret",       # Cross-validation utilities
  "scales"       # Visualization scaling
))
```

---

## 🚀 Quick Start

### Step 1: Generate Sample Data

Run the data generator to create a synthetic NBA dataset (450 players):

```r
source("generate_sample_data.R")
```

This creates `nba_player_data.csv` with realistic player statistics including:
- Box score stats (PTS, REB, AST, etc.)
- Advanced metrics (PER, BPM, Win Shares)
- **Usage Rate** (strategic importance)
- **Defensive Win Shares (DWS)** (defensive impact)
- Team-level outcomes (WIN_PCT, NET_RATING)

### Step 2: Run the RSE Pipeline

Execute the complete analysis pipeline:

```r
source("rse_model.R")

results <- run_rse_pipeline(
  data_path = "nba_player_data.csv",
  target_var = "WIN_PCT",
  output_dir = "output"
)
```

### Step 3: Review Outputs

The pipeline generates:

**📊 Visualizations** (saved in `output/`):
- `pca_scree_plot.png` - Variance explained by principal components
- `lasso_coefficient_path.png` - Feature selection progression
- `model_diagnostics.png` - Training vs. test performance

**📁 Data Files**:
- `player_success_index.csv` - Player efficiency rankings
- `superteam_simulations.csv` - Monte Carlo roster simulations

---

## 🔬 Methodology

### 1. Data Preprocessing

**Objective**: Remove noise and ensure metrics are comparable

- **Noise Filtering**: Exclude players with < 500 total minutes (reduces high-variance outliers)
- **Z-Score Normalization**: Standardizes all metrics to mean = 0, SD = 1

```r
processed_data <- load_and_preprocess_data("nba_player_data.csv", min_minutes = 500)
```

### 2. Principal Component Analysis (PCA)

**Objective**: Reduce multicollinearity by grouping correlated stats

- Identifies latent "Impact Components" (e.g., scoring volume vs. defensive efficiency)
- Retains components explaining ≥ 80% of total variance
- Converts 20+ raw metrics into ~5-8 uncorrelated components

```r
pca_results <- perform_pca(processed_data, variance_threshold = 0.80)
```

**Interpretation**:
- **PC1**: Typically represents overall offensive volume
- **PC2**: Often captures defensive impact
- **PC3+**: Efficiency, shooting touch, playmaking

### 3. LASSO Regression

**Objective**: Predict team success while penalizing irrelevant features

- **Algorithm**: L1-regularized linear regression (penalizes sum of absolute coefficients)
- **Cross-Validation**: 10-fold CV to select optimal penalty (λ)
- **Target Variable**: Team Winning Percentage (WIN_PCT) or Net Rating (NET_RATING)

```r
lasso_results <- fit_lasso_regression(pca_results$pca_data, target_var = "WIN_PCT")
```

**Key Outputs**:
- Non-zero coefficients → "impactful" components
- Zero coefficients → noise variables eliminated
- RMSE ratio (test/train) → overfitting diagnostic

### 4. Strategic Analysis

#### A. Roster Balance Index

**Formula**:
$$
\text{Balance Index} = \frac{\sum_{i=1}^{n} \frac{\text{Usage}_i}{i^{\alpha}}}{\sum_{i=1}^{n} \text{Usage}_i}
$$

Where:
- Usage rates are sorted in descending order
- α (diminishing factor) = 1.5 (default)
- **Higher values** → More balanced roster distribution

**Use Case**: Identify rosters with excessive usage concentration (superteam risk)

```r
balance <- calculate_roster_balance(usage_rates = c(30, 28, 25, 18, 15))
```

#### B. Player Success Index

**Formula**:
$$
\text{Success Index}_i = \sum_{j=1}^{k} \beta_j \times \text{PC}_{ij}
$$

Where:
- β = LASSO coefficients (impact weights)
- PC = Principal component scores

**Use Case**: Rank players by "efficiency per dollar" for drafting/trading

```r
success_index <- generate_success_index(pca_results$pca_data, lasso_results)
```

#### C. Superteam Risk Simulation

Monte Carlo simulation (1,000 iterations) comparing random roster configurations:

```r
simulations <- simulate_superteam_risk(
  player_data = processed_data,
  success_index = success_index,
  n_simulations = 1000,
  roster_size = 10
)
```

**Outputs**:
- Average success index per roster
- Total usage concentration
- Balance index distribution
- Risk score (usage/balance ratio)

---

## 📊 Interpretation Guide

### Scree Plot (PCA)

![PCA Scree Plot](docs/example_scree.png)

- **Bars**: Variance explained by each component
- **Red line**: Cumulative variance
- **Green dashed line**: 80% threshold
- **Takeaway**: How many components capture meaningful variation?

### LASSO Coefficient Path

![LASSO Path](docs/example_lasso_path.png)

- **X-axis**: Log(λ) penalty strength
- **Y-axis**: Standardized coefficients
- **Red line**: Optimal λ (minimizes CV error)
- **Takeaway**: Which components survive regularization?

### Model Diagnostics

![Diagnostics](docs/example_diagnostics.png)

- **Dashed line**: Perfect prediction (y = x)
- **Blue**: Training set predictions
- **Red**: Test set predictions
- **Takeaway**: Are test predictions as accurate as training? (Overfitting check)

**Interpreting RMSE Ratio**:
- **< 1.2**: Model generalizes well ✅
- **1.2 - 1.5**: Moderate overfitting ⚠️
- **> 1.5**: Severe overfitting 🚫

---

## 🧪 Example Use Cases

### Use Case 1: Identify Undervalued Players

Find top-10 "efficient" players for draft/trade targeting:

```r
top_targets <- success_index %>%
  filter(TIER %in% c("Elite", "High Value")) %>%
  arrange(desc(SUCCESS_INDEX)) %>%
  head(10)
```

### Use Case 2: Evaluate Superteam Risk

Compare balance index for different roster configurations:

```r
# High-usage superteam
superteam_usage <- c(32, 30, 28, 15, 12, 10, 8, 6, 5, 4)
superteam_balance <- calculate_roster_balance(superteam_usage)

# Balanced roster
balanced_usage <- c(22, 21, 19, 18, 16, 14, 12, 10, 8, 6)
balanced_balance <- calculate_roster_balance(balanced_usage)

cat("Superteam Balance:", superteam_balance, "\n")
cat("Balanced Roster:", balanced_balance, "\n")
```

**Expected Result**: Balanced roster has higher index → Lower diminishing returns

### Use Case 3: Simulate Roster Strategies

Run 10,000 simulations to find optimal roster size:

```r
results_8 <- simulate_superteam_risk(processed_data, success_index, roster_size = 8)
results_10 <- simulate_superteam_risk(processed_data, success_index, roster_size = 10)
results_12 <- simulate_superteam_risk(processed_data, success_index, roster_size = 12)

mean(results_8$risk_score)   # Compare risk scores
mean(results_10$risk_score)
mean(results_12$risk_score)
```

---

## 🔧 Advanced Configuration

### Custom Target Variables

Use Net Rating instead of Win Percentage:

```r
results <- run_rse_pipeline(
  data_path = "nba_player_data.csv",
  target_var = "NET_RATING",  # Changed from WIN_PCT
  output_dir = "output"
)
```

### Adjust Filtering Thresholds

Include more players (lower minutes threshold):

```r
processed_data <- load_and_preprocess_data(
  "nba_player_data.csv",
  min_minutes = 300  # Default: 500
)
```

### Change PCA Variance Threshold

Retain more/fewer components:

```r
pca_results <- perform_pca(
  processed_data,
  variance_threshold = 0.90  # Default: 0.80
)
```

### Modify Cross-Validation Folds

```r
lasso_results <- fit_lasso_regression(
  pca_results$pca_data,
  target_var = "WIN_PCT",
  cv_folds = 5  # Default: 10
)
```

---

## 📚 Function Reference

### Core Pipeline Functions

| Function | Description |
|----------|-------------|
| `run_rse_pipeline()` | Execute complete analysis pipeline |
| `load_and_preprocess_data()` | Load CSV and apply filtering/normalization |
| `perform_pca()` | Dimension reduction via PCA |
| `fit_lasso_regression()` | Train LASSO with cross-validation |

### Strategic Analysis Functions

| Function | Description |
|----------|-------------|
| `calculate_roster_balance()` | Compute Roster Balance Index |
| `generate_success_index()` | Calculate player efficiency scores |
| `simulate_superteam_risk()` | Monte Carlo roster simulations |

### Visualization Functions

| Function | Description |
|----------|-------------|
| `plot_pca_scree()` | PCA variance scree plot |
| `plot_lasso_path()` | LASSO coefficient regularization path |
| `plot_diagnostics()` | Training vs. test performance |

---

## 📖 Data Requirements

Your input CSV must contain:

### Required Columns:
- `PLAYER_ID` or `PLAYER_NAME` (identifier)
- `MIN_TOTAL` (total minutes played)
- `USAGE_RATE` (usage percentage)
- `DWS` (Defensive Win Shares)
- **Target variable**: `WIN_PCT` or `NET_RATING`

### Recommended Columns:
- Box score stats: `PTS`, `REB`, `AST`, `STL`, `BLK`, `TOV`, `PF`
- Shooting: `FG_PCT`, `FG3_PCT`, `FT_PCT`
- Advanced metrics: `PER`, `BPM`, `OWS`, `TS_PCT`, `WS_48`, `PLUS_MINUS`

### Sample CSV Structure:

```csv
PLAYER_ID,PLAYER_NAME,MIN_TOTAL,PTS,REB,AST,USAGE_RATE,DWS,WIN_PCT
1,Player_001,2847,25.3,7.8,6.9,29.4,4.2,0.612
2,Player_002,2456,19.8,6.2,5.1,24.8,3.7,0.548
3,Player_003,1823,13.5,4.9,2.8,19.3,2.4,0.501
```

---

## 🛠️ Troubleshooting

### Issue: "Error in prcomp: infinite or missing values"

**Cause**: Dataset contains NA values or infinite numbers

**Solution**: Check for missing data before preprocessing
```r
summary(raw_data)  # Identify columns with NAs
raw_data <- na.omit(raw_data)  # Remove incomplete rows
```

### Issue: "All coefficients are zero in LASSO"

**Cause**: Lambda penalty too high (over-regularization)

**Solution**: Use `lambda.1se` instead of `lambda.min` or increase sample size

### Issue: Test RMSE >> Training RMSE

**Cause**: Overfitting (model memorizes training data)

**Solution**:
- Increase cross-validation folds: `cv_folds = 20`
- Reduce PCA components: `variance_threshold = 0.70`
- Collect more data

---

## 📄 Citation

If you use this model in academic research, please cite:

```bibtex
@software{rse_model_2026,
  title = {Roster Strategy & Efficiency (RSE) Model},
  author = {Sports Analytics Research Team},
  year = {2026},
  version = {1.0},
  url = {https://github.com/yourusername/rse-model}
}
```

---

## 📝 License

MIT License - See [LICENSE](LICENSE) file for details

---

## 🤝 Contributing

Contributions are welcome! Please:
1. Fork the repository
2. Create a feature branch (`git checkout -b feature/improvement`)
3. Commit changes (`git commit -m 'Add new metric'`)
4. Push to branch (`git push origin feature/improvement`)
5. Open a Pull Request

---

## 📧 Contact

For questions or collaborations:
- **Email**: research@sportsanalytics.com
- **Issues**: [GitHub Issues](https://github.com/yourusername/rse-model/issues)

---

**Built with ❤️ for advancing NBA analytics research**
