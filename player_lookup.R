#' Interactive Player Lookup Tool
#'
#' @description Search and analyze individual NBA players from RSE model results
#' @author Sports Analytics Research Team

suppressPackageStartupMessages({
  library(tidyverse)
})

# ==============================================================================
# HELPER FUNCTIONS
# ==============================================================================

#' Fuzzy search for player name
#'
#' @param query Character. Player name to search
#' @param player_list Character vector. List of all player names
#' @return Best matching player name
fuzzy_search_player <- function(query, player_list) {

  # Clean query
  query_clean <- tolower(trimws(query))

  # Try exact match first
  exact_match <- player_list[tolower(player_list) == query_clean]
  if (length(exact_match) > 0) {
    return(exact_match[1])
  }

  # Try partial match
  partial_matches <- player_list[grepl(query_clean, tolower(player_list))]
  if (length(partial_matches) > 0) {
    return(partial_matches[1])
  }

  # Try reverse (last name first)
  words <- strsplit(query_clean, " ")[[1]]
  if (length(words) >= 2) {
    # Try "Last First" format
    reverse_query <- paste(words[length(words)], paste(words[-length(words)], collapse = " "))
    partial_matches <- player_list[grepl(reverse_query, tolower(player_list))]
    if (length(partial_matches) > 0) {
      return(partial_matches[1])
    }
  }

  # No match found
  return(NULL)
}

#' Display player analysis
#'
#' @param player_name Character. Name of player to analyze
#' @param success_data Data frame. Success index results
#' @param raw_data Data frame. Raw NBA player data
display_player_analysis <- function(player_name, success_data, raw_data) {

  # Find player in success index
  player_success <- success_data %>% filter(PLAYER_NAME == player_name)

  # Find player in raw data
  player_raw <- raw_data %>% filter(PLAYER_NAME == player_name)

  if (nrow(player_success) == 0) {
    cat("\n❌ Player not found in analyzed dataset (may not meet minimum minutes threshold)\n\n")

    # Check if in raw data
    if (nrow(player_raw) > 0) {
      cat("Found in raw data but excluded from analysis:\n")
      cat("  • Minutes played:", player_raw$MIN_TOTAL, "(threshold: 500)\n")
      cat("  • Team:", player_raw$TEAM, "\n")
      cat("  • Position:", player_raw$POSITION, "\n\n")
    }
    return(NULL)
  }

  # Display header
  cat("\n")
  cat("╔══════════════════════════════════════════════════════════════╗\n")
  cat("║                   PLAYER ANALYSIS REPORT                    ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")

  # Basic Info
  cat("🏀 PLAYER INFORMATION\n")
  cat("─────────────────────────────────────────────────────────────\n")
  cat(sprintf("  Name:           %s\n", player_success$PLAYER_NAME))
  cat(sprintf("  Team:           %s\n", player_raw$TEAM))
  cat(sprintf("  Position:       %s\n", player_raw$POSITION))
  cat(sprintf("  Age:            %s\n", player_raw$AGE))
  cat(sprintf("  Games:          %s (%.0f started)\n", player_raw$GAMES, player_raw$GAMES_STARTED))
  cat(sprintf("  Minutes:        %.0f total\n", player_raw$MIN_TOTAL))
  cat("\n")

  # GP Index Ranking
  cat("📊 GP INDEX (GENERALIZED PERFORMANCE INDEX)\n")
  cat("─────────────────────────────────────────────────────────────\n")
  cat(sprintf("  GP Index:       %.4f\n", player_success$SUCCESS_INDEX))
  cat(sprintf("  Rank:           #%d of %d\n", player_success$RANK, nrow(success_data)))
  cat(sprintf("  Percentile:     %.1fth\n", player_success$PERCENTILE))
  cat(sprintf("  Tier:           %s", player_success$TIER))

  # Add tier emoji
  tier_emoji <- case_when(
    player_success$TIER == "Elite" ~ " 🌟",
    player_success$TIER == "High Value" ~ " ⭐",
    player_success$TIER == "Above Average" ~ " ✅",
    player_success$TIER == "Average" ~ " ➡️",
    TRUE ~ " ⬇️"
  )
  cat(tier_emoji, "\n\n")

  # Performance Stats
  cat("📈 PERFORMANCE STATISTICS\n")
  cat("─────────────────────────────────────────────────────────────\n")
  cat(sprintf("  Points:         %.1f ppg\n", player_raw$PTS))
  cat(sprintf("  Rebounds:       %.1f rpg\n", player_raw$REB))
  cat(sprintf("  Assists:        %.1f apg\n", player_raw$AST))
  cat(sprintf("  Steals:         %.1f spg\n", player_raw$STL))
  cat(sprintf("  Blocks:         %.1f bpg\n", player_raw$BLK))
  cat("\n")

  # Shooting
  cat("🎯 SHOOTING EFFICIENCY\n")
  cat("─────────────────────────────────────────────────────────────\n")
  cat(sprintf("  FG%%:            %.1f%%\n", player_raw$FG_PCT * 100))
  cat(sprintf("  3P%%:            %.1f%%\n", player_raw$FG3_PCT * 100))
  cat(sprintf("  FT%%:            %.1f%%\n", player_raw$FT_PCT * 100))
  cat(sprintf("  True Shooting:  %.1f%%\n", player_raw$TS_PCT * 100))
  cat("\n")

  # Advanced Metrics
  cat("🔬 ADVANCED METRICS\n")
  cat("─────────────────────────────────────────────────────────────\n")
  cat(sprintf("  Usage Rate:     %.1f%%\n", player_raw$USAGE_RATE))
  cat(sprintf("  PER:            %.1f\n", player_raw$PER))
  cat(sprintf("  BPM:            %.1f\n", player_raw$BPM))
  cat(sprintf("  Win Shares:     %.1f (%.2f/48)\n", player_raw$WS, player_raw$WS_48))
  cat(sprintf("  Off. WS:        %.1f\n", player_raw$OWS))
  cat(sprintf("  Def. WS:        %.1f\n", player_raw$DWS))
  cat("\n")

  # Comparison to League Average
  cat("📊 VS LEAGUE AVERAGE (Qualified Players)\n")
  cat("─────────────────────────────────────────────────────────────\n")

  avg_pts <- mean(raw_data$PTS[raw_data$MIN_TOTAL >= 500], na.rm = TRUE)
  avg_usage <- mean(raw_data$USAGE_RATE[raw_data$MIN_TOTAL >= 500], na.rm = TRUE)
  avg_per <- mean(raw_data$PER[raw_data$MIN_TOTAL >= 500], na.rm = TRUE)
  avg_bpm <- mean(raw_data$BPM[raw_data$MIN_TOTAL >= 500], na.rm = TRUE)

  pts_diff <- ((player_raw$PTS / avg_pts) - 1) * 100
  usage_diff <- ((player_raw$USAGE_RATE / avg_usage) - 1) * 100
  per_diff <- ((player_raw$PER / avg_per) - 1) * 100
  bpm_diff <- player_raw$BPM - avg_bpm

  cat(sprintf("  Points:         %+.1f%% %s\n", pts_diff, ifelse(pts_diff > 0, "⬆️", "⬇️")))
  cat(sprintf("  Usage Rate:     %+.1f%% %s\n", usage_diff, ifelse(usage_diff > 0, "⬆️", "⬇️")))
  cat(sprintf("  PER:            %+.1f%% %s\n", per_diff, ifelse(per_diff > 0, "⬆️", "⬇️")))
  cat(sprintf("  BPM:            %+.1f %s\n", bpm_diff, ifelse(bpm_diff > 0, "⬆️", "⬇️")))
  cat("\n")

  # Similar Players
  cat("👥 SIMILAR PLAYERS (by GP Index)\n")
  cat("─────────────────────────────────────────────────────────────\n")

  player_rank <- player_success$RANK
  similar_range <- success_data %>%
    filter(RANK >= (player_rank - 3) & RANK <= (player_rank + 3)) %>%
    filter(PLAYER_NAME != player_name) %>%
    arrange(RANK) %>%
    head(5)

  for (i in 1:nrow(similar_range)) {
    cat(sprintf("  #%d  %s (GP: %.4f)\n",
                similar_range$RANK[i],
                similar_range$PLAYER_NAME[i],
                similar_range$SUCCESS_INDEX[i]))
  }
  cat("\n")

  # Strengths/Weaknesses
  cat("💪 STRENGTHS & WEAKNESSES\n")
  cat("─────────────────────────────────────────────────────────────\n")

  strengths <- c()
  weaknesses <- c()

  if (player_raw$PER > 20) strengths <- c(strengths, "Elite efficiency (PER > 20)")
  if (player_raw$TS_PCT > 0.60) strengths <- c(strengths, "Excellent shooting (TS% > 60%)")
  if (player_raw$BPM > 5) strengths <- c(strengths, "Elite overall impact (BPM > 5)")
  if (player_raw$DWS > 2) strengths <- c(strengths, "Strong defender (DWS > 2)")
  if (player_raw$USAGE_RATE > 25) strengths <- c(strengths, "High usage player")

  if (player_raw$PER < 15) weaknesses <- c(weaknesses, "Below average efficiency (PER < 15)")
  if (player_raw$TS_PCT < 0.50) weaknesses <- c(weaknesses, "Poor shooting efficiency")
  if (player_raw$BPM < 0) weaknesses <- c(weaknesses, "Negative impact on winning")
  if (player_raw$TOV > 3) weaknesses <- c(weaknesses, "Turnover prone (> 3 per game)")

  if (length(strengths) > 0) {
    cat("  Strengths:\n")
    for (s in strengths) cat("    ✓", s, "\n")
  }

  if (length(weaknesses) > 0) {
    cat("\n  Weaknesses:\n")
    for (w in weaknesses) cat("    ✗", w, "\n")
  }

  if (length(strengths) == 0 && length(weaknesses) == 0) {
    cat("  Balanced player with no extreme strengths or weaknesses\n")
  }

  cat("\n")

  # GP Index Explanation
  cat("📖 HOW GP INDEX IS CALCULATED\n")
  cat("─────────────────────────────────────────────────────────────\n")
  cat("  The GP Index uses advanced statistical modeling:\n\n")
  cat("  1. PCA (Principal Component Analysis):\n")
  cat("     • Reduces 25+ NBA metrics into 5 key components\n")
  cat("     • Captures 82% of all variance in player performance\n")
  cat("     • Groups correlated stats (e.g., scoring volume, defense)\n\n")
  cat("  2. LASSO Regression:\n")
  cat("     • Predicts team winning % from player metrics\n")
  cat("     • Eliminates noise variables automatically\n")
  cat("     • Cross-validated to prevent overfitting\n\n")
  cat("  3. GP Index Formula:\n")
  cat("     GP Index = Σ(β × PC_i)\n")
  cat("     • β = LASSO coefficients (impact weights)\n")
  cat("     • PC = Principal component scores\n")
  cat("     • Higher value = Greater winning contribution\n\n")
  cat("  Interpretation:\n")
  cat("     • Positive = Above replacement level\n")
  cat("     • Negative = Below replacement level\n")
  cat("     • Top 10% (Elite) ≈ +0.05 or higher\n")
  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")
}

# ==============================================================================
# MAIN INTERACTIVE LOOP
# ==============================================================================

#' Run interactive player lookup
#'
#' @param success_file Path to success index CSV
#' @param raw_data_file Path to raw NBA data CSV
run_player_lookup <- function(success_file = "output_real/player_success_index.csv",
                               raw_data_file = "nba_player_data_real.csv") {

  # Load data
  cat("Loading NBA data...\n")
  success_data <- read_csv(success_file, show_col_types = FALSE)
  raw_data <- read_csv(raw_data_file, show_col_types = FALSE)

  cat("\n")
  cat("╔══════════════════════════════════════════════════════════════╗\n")
  cat("║                                                              ║\n")
  cat("║         NBA PLAYER LOOKUP - RSE MODEL RESULTS               ║\n")
  cat("║                                                              ║\n")
  cat("╚══════════════════════════════════════════════════════════════╝\n\n")

  cat(sprintf("Loaded %d players from 2025-26 season\n\n", nrow(success_data)))

  # Interactive loop
  repeat {
    cat("Enter a player name (or 'quit' to exit): ")
    user_input <- readLines(con = "stdin", n = 1)

    # Exit condition
    if (tolower(trimws(user_input)) %in% c("quit", "exit", "q", "")) {
      cat("\n👋 Thanks for using the NBA Player Lookup tool!\n\n")
      break
    }

    # Search for player
    matched_player <- fuzzy_search_player(user_input, success_data$PLAYER_NAME)

    if (is.null(matched_player)) {
      cat("\n❌ Player not found. Try again with a different spelling.\n")
      cat("   Examples: 'LeBron James', 'Curry', 'Giannis'\n\n")
      next
    }

    # Confirm match if not exact
    if (tolower(matched_player) != tolower(trimws(user_input))) {
      cat(sprintf("\n🔍 Did you mean: %s? (y/n): ", matched_player))
      confirm <- readLines(con = "stdin", n = 1)

      if (tolower(trimws(confirm)) != "y") {
        cat("\nSearch cancelled. Try again.\n\n")
        next
      }
    }

    # Display analysis
    display_player_analysis(matched_player, success_data, raw_data)
  }
}

# ==============================================================================
# COMMAND-LINE INTERFACE
# ==============================================================================

# Check if running interactively or with command-line argument
args <- commandArgs(trailingOnly = TRUE)

if (length(args) > 0) {
  # Single player lookup mode
  success_data <- read_csv("output_real/player_success_index.csv", show_col_types = FALSE)
  raw_data <- read_csv("nba_player_data_real.csv", show_col_types = FALSE)

  player_query <- paste(args, collapse = " ")
  matched_player <- fuzzy_search_player(player_query, success_data$PLAYER_NAME)

  if (is.null(matched_player)) {
    cat("\n❌ Player '", player_query, "' not found.\n\n", sep = "")
  } else {
    display_player_analysis(matched_player, success_data, raw_data)
  }

} else {
  # Interactive mode
  run_player_lookup()
}
