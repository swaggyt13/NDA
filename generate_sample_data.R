#' Generate Sample NBA Player Dataset
#'
#' @description Creates a realistic synthetic dataset for testing the RSE model
#' @author Sports Analytics Research Team
#' @date 2026-02-11

library(tidyverse)

#' Generate synthetic NBA player data
#'
#' @param n_players Integer. Number of players to generate (default: 450)
#' @param seed Integer. Random seed for reproducibility (default: 2024)
#' @return Data frame with player statistics
#' @export
generate_nba_data <- function(n_players = 450, seed = 2024) {

  set.seed(seed)

  cat("Generating synthetic NBA dataset with", n_players, "players...\n")

  # Player archetypes with different statistical profiles
  archetypes <- c("Superstar", "Star", "Starter", "Rotation", "Bench", "Deep Bench")
  archetype_probs <- c(0.05, 0.10, 0.20, 0.30, 0.25, 0.10)

  # Generate base player attributes
  players <- data.frame(
    PLAYER_ID = 1:n_players,
    PLAYER_NAME = paste0("Player_", sprintf("%03d", 1:n_players)),
    ARCHETYPE = sample(archetypes, n_players, replace = TRUE, prob = archetype_probs)
  )

  # Generate statistics based on archetype
  nba_data <- players %>%
    mutate(
      # Minutes played (key filter variable)
      MIN_TOTAL = case_when(
        ARCHETYPE == "Superstar" ~ rnorm(n(), 2800, 200),
        ARCHETYPE == "Star" ~ rnorm(n(), 2400, 300),
        ARCHETYPE == "Starter" ~ rnorm(n(), 1800, 400),
        ARCHETYPE == "Rotation" ~ rnorm(n(), 1200, 300),
        ARCHETYPE == "Bench" ~ rnorm(n(), 600, 200),
        TRUE ~ rnorm(n(), 250, 100)
      ),
      MIN_TOTAL = pmax(MIN_TOTAL, 50), # Minimum floor

      # Points per game
      PTS = case_when(
        ARCHETYPE == "Superstar" ~ rnorm(n(), 26, 4),
        ARCHETYPE == "Star" ~ rnorm(n(), 20, 3),
        ARCHETYPE == "Starter" ~ rnorm(n(), 14, 3),
        ARCHETYPE == "Rotation" ~ rnorm(n(), 9, 2),
        ARCHETYPE == "Bench" ~ rnorm(n(), 5, 2),
        TRUE ~ rnorm(n(), 3, 1)
      ),

      # Rebounds per game
      REB = case_when(
        ARCHETYPE == "Superstar" ~ rnorm(n(), 8, 2),
        ARCHETYPE == "Star" ~ rnorm(n(), 6.5, 1.5),
        ARCHETYPE == "Starter" ~ rnorm(n(), 5, 1.5),
        ARCHETYPE == "Rotation" ~ rnorm(n(), 3.5, 1),
        TRUE ~ rnorm(n(), 2, 0.8)
      ),

      # Assists per game
      AST = case_when(
        ARCHETYPE == "Superstar" ~ rnorm(n(), 7, 2),
        ARCHETYPE == "Star" ~ rnorm(n(), 5, 1.5),
        ARCHETYPE == "Starter" ~ rnorm(n(), 3, 1.2),
        ARCHETYPE == "Rotation" ~ rnorm(n(), 2, 1),
        TRUE ~ rnorm(n(), 1, 0.5)
      ),

      # Steals and Blocks
      STL = pmax(rnorm(n(), 1, 0.5), 0),
      BLK = pmax(rnorm(n(), 0.7, 0.5), 0),

      # Shooting percentages
      FG_PCT = case_when(
        ARCHETYPE %in% c("Superstar", "Star") ~ rnorm(n(), 0.47, 0.04),
        TRUE ~ rnorm(n(), 0.44, 0.05)
      ),
      FG_PCT = pmin(pmax(FG_PCT, 0.30), 0.60),

      FG3_PCT = rnorm(n(), 0.36, 0.05),
      FG3_PCT = pmin(pmax(FG3_PCT, 0.20), 0.50),

      FT_PCT = rnorm(n(), 0.78, 0.08),
      FT_PCT = pmin(pmax(FT_PCT, 0.50), 0.95),

      # Turnovers
      TOV = case_when(
        ARCHETYPE %in% c("Superstar", "Star") ~ rnorm(n(), 2.5, 0.8),
        TRUE ~ rnorm(n(), 1.2, 0.6)
      ),
      TOV = pmax(TOV, 0.2),

      # Personal fouls
      PF = rnorm(n(), 2.2, 0.6),
      PF = pmax(PF, 0.5),

      # Usage Rate (key strategic variable)
      USAGE_RATE = case_when(
        ARCHETYPE == "Superstar" ~ rnorm(n(), 30, 3),
        ARCHETYPE == "Star" ~ rnorm(n(), 25, 2.5),
        ARCHETYPE == "Starter" ~ rnorm(n(), 20, 2),
        ARCHETYPE == "Rotation" ~ rnorm(n(), 16, 2),
        TRUE ~ rnorm(n(), 12, 2)
      ),
      USAGE_RATE = pmin(pmax(USAGE_RATE, 8), 38),

      # Defensive Win Shares (key defensive metric)
      DWS = case_when(
        ARCHETYPE == "Superstar" ~ rnorm(n(), 4.5, 1),
        ARCHETYPE == "Star" ~ rnorm(n(), 3.5, 0.8),
        ARCHETYPE == "Starter" ~ rnorm(n(), 2.5, 0.8),
        ARCHETYPE == "Rotation" ~ rnorm(n(), 1.5, 0.6),
        TRUE ~ rnorm(n(), 0.5, 0.3)
      ),
      DWS = pmax(DWS, 0),

      # Offensive Win Shares
      OWS = case_when(
        ARCHETYPE == "Superstar" ~ rnorm(n(), 6, 1.5),
        ARCHETYPE == "Star" ~ rnorm(n(), 4.5, 1),
        ARCHETYPE == "Starter" ~ rnorm(n(), 2.5, 1),
        ARCHETYPE == "Rotation" ~ rnorm(n(), 1.2, 0.7),
        TRUE ~ rnorm(n(), 0.4, 0.3)
      ),
      OWS = pmax(OWS, 0),

      # Plus/Minus
      PLUS_MINUS = case_when(
        ARCHETYPE == "Superstar" ~ rnorm(n(), 6, 2),
        ARCHETYPE == "Star" ~ rnorm(n(), 3, 2),
        ARCHETYPE == "Starter" ~ rnorm(n(), 1, 2),
        TRUE ~ rnorm(n(), -1, 2)
      ),

      # Player Efficiency Rating
      PER = case_when(
        ARCHETYPE == "Superstar" ~ rnorm(n(), 24, 2),
        ARCHETYPE == "Star" ~ rnorm(n(), 19, 2),
        ARCHETYPE == "Starter" ~ rnorm(n(), 15, 2),
        ARCHETYPE == "Rotation" ~ rnorm(n(), 12, 2),
        TRUE ~ rnorm(n(), 9, 2)
      ),
      PER = pmax(PER, 5),

      # True Shooting Percentage
      TS_PCT = case_when(
        ARCHETYPE %in% c("Superstar", "Star") ~ rnorm(n(), 0.58, 0.04),
        TRUE ~ rnorm(n(), 0.54, 0.05)
      ),
      TS_PCT = pmin(pmax(TS_PCT, 0.40), 0.70),

      # Win Shares Per 48 Minutes
      WS_48 = (OWS + DWS) / (MIN_TOTAL / 48),
      WS_48 = pmin(WS_48, 0.30),

      # Box Plus/Minus
      BPM = case_when(
        ARCHETYPE == "Superstar" ~ rnorm(n(), 7, 1.5),
        ARCHETYPE == "Star" ~ rnorm(n(), 4, 1.5),
        ARCHETYPE == "Starter" ~ rnorm(n(), 1, 1.5),
        TRUE ~ rnorm(n(), -1, 1.5)
      )
    ) %>%
    # Generate team-level target variables
    group_by(ARCHETYPE) %>%
    mutate(
      # Team winning percentage influenced by player quality
      WIN_PCT = case_when(
        ARCHETYPE == "Superstar" ~ 0.60 + rnorm(n(), 0, 0.08),
        ARCHETYPE == "Star" ~ 0.52 + rnorm(n(), 0, 0.08),
        ARCHETYPE == "Starter" ~ 0.48 + rnorm(n(), 0, 0.08),
        TRUE ~ 0.42 + rnorm(n(), 0, 0.08)
      ),
      WIN_PCT = pmin(pmax(WIN_PCT, 0.20), 0.80),

      # Net Rating
      NET_RATING = (WIN_PCT - 0.5) * 20 + rnorm(n(), 0, 2)
    ) %>%
    ungroup() %>%
    # Add some realistic correlation noise
    mutate(
      # Better players on better teams
      WIN_PCT = WIN_PCT + (PER - mean(PER)) * 0.003 + rnorm(n(), 0, 0.02),
      WIN_PCT = pmin(pmax(WIN_PCT, 0.20), 0.80),

      NET_RATING = NET_RATING + (BPM * 0.5) + rnorm(n(), 0, 1.5)
    ) %>%
    select(-ARCHETYPE) # Remove archetype (internal generation variable)

  cat("✓ Generated", nrow(nba_data), "player records\n")
  cat("✓ Variables:", ncol(nba_data), "\n")
  cat("✓ Players with MIN >= 500:", sum(nba_data$MIN_TOTAL >= 500), "\n\n")

  # Data summary
  cat("--- Sample Statistics ---\n")
  cat("Points (mean ± sd):", round(mean(nba_data$PTS), 2), "±", round(sd(nba_data$PTS), 2), "\n")
  cat("Usage Rate (mean ± sd):", round(mean(nba_data$USAGE_RATE), 2), "±", round(sd(nba_data$USAGE_RATE), 2), "\n")
  cat("DWS (mean ± sd):", round(mean(nba_data$DWS), 2), "±", round(sd(nba_data$DWS), 2), "\n")
  cat("Win % (mean ± sd):", round(mean(nba_data$WIN_PCT), 3), "±", round(sd(nba_data$WIN_PCT), 3), "\n\n")

  return(nba_data)
}

# ==============================================================================
# EXECUTION
# ==============================================================================

# Generate data
nba_dataset <- generate_nba_data(n_players = 450, seed = 2024)

# Save to CSV
output_file <- "nba_player_data.csv"
write_csv(nba_dataset, output_file)

cat("✓ Dataset saved to:", output_file, "\n")
cat("\nYou can now run the RSE model with:\n")
cat("  source('rse_model.R')\n")
cat("  results <- run_rse_pipeline('nba_player_data.csv')\n")
