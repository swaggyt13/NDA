#' Demo script for player lookup
#' Demonstrates multiple player lookups

source("player_lookup.R")

# Load data
success_data <- read_csv("output_real/player_success_index.csv", show_col_types = FALSE)
raw_data <- read_csv("nba_player_data_real.csv", show_col_types = FALSE)

# Demo players
demo_players <- c("Stephen Curry", "Nikola Jokic", "Victor Wembanyama")

cat("╔══════════════════════════════════════════════════════════════╗\n")
cat("║            PLAYER LOOKUP DEMO - RSE MODEL                   ║\n")
cat("╚══════════════════════════════════════════════════════════════╝\n\n")

for (player in demo_players) {
  cat(sprintf("\n🔍 Looking up: %s\n", player))
  Sys.sleep(1)
  display_player_analysis(player, success_data, raw_data)
  cat("\nPress Enter to continue...")
  invisible(readline())
}

cat("\n✅ Demo complete! Use 'Rscript player_lookup.R' for interactive mode.\n\n")
