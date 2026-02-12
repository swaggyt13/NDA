#' Fetch Real-Time NBA Player Statistics
#'
#' @description Scrapes Basketball Reference for current 2025-26 season data
#' @author Sports Analytics Research Team

# Install required packages if not already installed
required_packages <- c("rvest", "httr", "tidyverse")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg, repos = "https://cloud.r-project.org/", quiet = TRUE)
    library(pkg, character.only = TRUE)
  }
}

library(rvest)
library(httr)
library(tidyverse)

cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("  REAL-TIME NBA DATA FETCHER\n")
cat("  Source: Basketball Reference (2025-26 Season)\n")
cat("═══════════════════════════════════════════════════════════════\n\n")

#' Fetch stats from Basketball Reference
#'
#' @param url Character. URL to scrape
#' @return Data frame
fetch_table <- function(url) {

  cat("  Fetching data from:", url, "\n")

  tryCatch({
    # Read the page
    page <- read_html(url)

    # Try to find any table on the page (Basketball Reference uses various IDs)
    tables <- page %>%
      html_nodes("table") %>%
      html_table(fill = TRUE)

    # Get the first substantial table (usually the stats table)
    for (i in seq_along(tables)) {
      table <- tables[[i]]
      if (nrow(table) > 50 && "Player" %in% names(table)) {
        # Found the right table
        # Remove header rows that repeat
        table <- table %>%
          filter(Rk != "Rk")

        # Remove duplicate players (keep first - "TOT" for traded players)
        table <- table %>%
          distinct(Player, .keep_all = TRUE)

        cat("    ✓ Found", nrow(table), "players\n")

        Sys.sleep(1)  # Be polite to the server

        return(table)
      }
    }

    # If no suitable table found
    stop("No suitable stats table found")

  }, error = function(e) {
    cat("    ✗ Error:", e$message, "\n")
    return(NULL)
  })
}

#' Main data fetching function
fetch_nba_data <- function(season = 2026) {

  cat("STEP 1: FETCHING RAW DATA\n")
  cat("─────────────────────────────────────────────────────────────\n")

  # URLs
  base_url <- "https://www.basketball-reference.com/leagues"

  totals_url <- paste0(base_url, "/NBA_", season, "_totals.html")
  advanced_url <- paste0(base_url, "/NBA_", season, "_advanced.html")
  per_game_url <- paste0(base_url, "/NBA_", season, "_per_game.html")

  # Fetch tables
  cat("\n1. Fetching total statistics...\n")
  totals <- fetch_table(totals_url)

  cat("\n2. Fetching per-game statistics...\n")
  per_game <- fetch_table(per_game_url)

  cat("\n3. Fetching advanced statistics...\n")
  advanced <- fetch_table(advanced_url)

  if (is.null(totals) || is.null(advanced) || is.null(per_game)) {
    stop("Failed to fetch one or more tables")
  }

  cat("\n")
  cat("STEP 2: CLEANING AND MERGING DATA\n")
  cat("─────────────────────────────────────────────────────────────\n")

  # Debug: Show available columns
  cat("\nAvailable columns:\n")
  cat("  Totals:", paste(names(totals), collapse = ", "), "\n")
  cat("  Per-game:", paste(names(per_game), collapse = ", "), "\n")
  cat("  Advanced:", paste(names(advanced), collapse = ", "), "\n\n")

  # Convert numeric columns in each table
  numeric_cols_totals <- c("Rk", "Age", "G", "GS", "MP", "FG", "FGA", "3P", "3PA",
                           "2P", "2PA", "FT", "FTA", "ORB", "DRB", "TRB",
                           "AST", "STL", "BLK", "TOV", "PF", "PTS")

  for (col in numeric_cols_totals) {
    if (col %in% names(totals)) {
      totals[[col]] <- as.numeric(totals[[col]])
    }
  }

  numeric_cols_pg <- c("PTS", "TRB", "AST", "FG%", "3P%", "FT%")
  for (col in numeric_cols_pg) {
    if (col %in% names(per_game)) {
      per_game[[col]] <- as.numeric(per_game[[col]])
    }
  }

  numeric_cols_adv <- c("PER", "TS%", "USG%", "OWS", "DWS", "WS", "WS/48",
                        "OBPM", "DBPM", "BPM")
  for (col in numeric_cols_adv) {
    if (col %in% names(advanced)) {
      advanced[[col]] <- as.numeric(advanced[[col]])
    }
  }

  # Select and rename columns
  totals_clean <- totals %>%
    select(Player, Pos, Age, Team, G, GS, MP) %>%
    rename(
      PLAYER_NAME = Player,
      POSITION = Pos,
      AGE = Age,
      TEAM = Team,
      GAMES = G,
      GAMES_STARTED = GS,
      MIN_TOTAL = MP
    )

  per_game_clean <- per_game %>%
    select(Player, PTS, TRB, AST, STL, BLK, TOV, PF, `FG%`, `3P%`, `FT%`) %>%
    rename(
      PLAYER_NAME = Player,
      PTS = PTS,
      REB = TRB,
      AST = AST,
      STL = STL,
      BLK = BLK,
      TOV = TOV,
      PF = PF,
      FG_PCT = `FG%`,
      FG3_PCT = `3P%`,
      FT_PCT = `FT%`
    )

  advanced_clean <- advanced %>%
    select(Player, PER, `TS%`, `USG%`, OWS, DWS, WS, `WS/48`, OBPM, DBPM, BPM) %>%
    rename(
      PLAYER_NAME = Player,
      PER = PER,
      TS_PCT = `TS%`,
      USAGE_RATE = `USG%`,
      OWS = OWS,
      DWS = DWS,
      WS = WS,
      WS_48 = `WS/48`,
      OBPM = OBPM,
      DBPM = DBPM,
      BPM = BPM
    )

  # Merge all tables
  merged <- totals_clean %>%
    left_join(per_game_clean, by = "PLAYER_NAME") %>%
    left_join(advanced_clean, by = "PLAYER_NAME")

  cat("  ✓ Merged", nrow(merged), "players with", ncol(merged), "variables\n")

  # Add calculated fields
  merged <- merged %>%
    mutate(
      # Player ID
      PLAYER_ID = row_number(),

      # Plus/Minus estimate (using BPM as proxy)
      PLUS_MINUS = BPM * (MIN_TOTAL / 1000),

      # Team Win Percentage estimate (based on player impact)
      WIN_PCT = 0.5 + (BPM * 0.015),
      WIN_PCT = pmin(pmax(WIN_PCT, 0.20), 0.80),

      # Net Rating estimate
      NET_RATING = BPM * 1.5
    ) %>%
    # Filter players with meaningful minutes
    filter(MIN_TOTAL >= 100) %>%
    # Remove rows with missing critical data
    filter(!is.na(USAGE_RATE), !is.na(DWS), !is.na(MIN_TOTAL)) %>%
    # Reorder columns
    select(
      PLAYER_ID, PLAYER_NAME, POSITION, TEAM, AGE, GAMES, GAMES_STARTED, MIN_TOTAL,
      PTS, REB, AST, STL, BLK, TOV, PF,
      FG_PCT, FG3_PCT, FT_PCT,
      USAGE_RATE, DWS, OWS, WS, WS_48,
      PER, TS_PCT, BPM, OBPM, DBPM, PLUS_MINUS,
      WIN_PCT, NET_RATING
    )

  cat("  ✓ Cleaned dataset:", nrow(merged), "players ready for analysis\n")

  return(merged)
}

# ═══════════════════════════════════════════════════════════════
# EXECUTE
# ═══════════════════════════════════════════════════════════════

tryCatch({

  # Fetch data
  nba_data <- fetch_nba_data(season = 2026)

  # Save to CSV
  output_file <- "nba_player_data_real.csv"
  write_csv(nba_data, output_file)

  cat("\n")
  cat("═══════════════════════════════════════════════════════════════\n")
  cat("✓ SUCCESS: Real NBA data saved to:", output_file, "\n")
  cat("═══════════════════════════════════════════════════════════════\n\n")

  # Summary statistics
  cat("DATASET SUMMARY:\n")
  cat("─────────────────────────────────────────────────────────────\n")
  cat("  • Total players:", nrow(nba_data), "\n")
  cat("  • Average minutes:", round(mean(nba_data$MIN_TOTAL, na.rm = TRUE), 0), "\n")
  cat("  • Average PTS:", round(mean(nba_data$PTS, na.rm = TRUE), 1), "\n")
  cat("  • Average Usage Rate:", round(mean(nba_data$USAGE_RATE, na.rm = TRUE), 1), "%\n")
  cat("  • Average DWS:", round(mean(nba_data$DWS, na.rm = TRUE), 2), "\n\n")

  cat("TOP 10 PLAYERS BY MINUTES PLAYED:\n")
  cat("─────────────────────────────────────────────────────────────\n")
  top_10 <- nba_data %>%
    arrange(desc(MIN_TOTAL)) %>%
    head(10) %>%
    select(PLAYER_NAME, TEAM, MIN_TOTAL, PTS, USAGE_RATE, DWS)
  print(top_10)

  cat("\n")
  cat("NEXT STEP: Run the RSE model with:\n")
  cat("  source('rse_model.R')\n")
  cat("  results <- run_rse_pipeline('", output_file, "')\n\n", sep = "")

}, error = function(e) {
  cat("\n✗ ERROR:", e$message, "\n")
  cat("\nFull error details:\n")
  print(e)
  cat("\nTroubleshooting:\n")
  cat("  1. Check your internet connection\n")
  cat("  2. Verify Basketball Reference is accessible\n")
  cat("  3. Try running the script again\n\n")
  traceback()
})
