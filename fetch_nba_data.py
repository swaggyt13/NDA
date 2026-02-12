#!/usr/bin/env python3
"""
Fetch Real-Time NBA Player Statistics
Scrapes Basketball Reference for current 2025-26 season data
"""

import pandas as pd
import requests
from io import StringIO
import time

def fetch_nba_stats(season="2026"):
    """
    Fetch NBA player statistics from Basketball Reference

    Args:
        season: Season year (default: 2026 for 2025-26 season)

    Returns:
        DataFrame with combined player statistics
    """

    print(f"Fetching NBA {season} season data from Basketball Reference...")

    # URLs for different stat types
    urls = {
        'totals': f'https://www.basketball-reference.com/leagues/NBA_{season}_totals.html',
        'advanced': f'https://www.basketball-reference.com/leagues/NBA_{season}_advanced.html',
        'per_game': f'https://www.basketball-reference.com/leagues/NBA_{season}_per_game.html'
    }

    headers = {
        'User-Agent': 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36'
    }

    dataframes = {}

    # Fetch each table
    for stat_type, url in urls.items():
        print(f"  Fetching {stat_type} statistics...")

        try:
            response = requests.get(url, headers=headers)
            response.raise_for_status()

            # Parse HTML tables
            tables = pd.read_html(StringIO(response.text))

            # Get the main stats table (usually first table)
            df = tables[0]

            # Remove multi-level column headers if present
            if isinstance(df.columns, pd.MultiIndex):
                df.columns = df.columns.droplevel(0)

            # Remove rows that are header repeats (where Rk == 'Rk')
            df = df[df['Rk'] != 'Rk'].copy()

            # Remove duplicate players (keep first occurrence - Tot for traded players)
            # Players traded show up multiple times with different teams
            df = df.drop_duplicates(subset=['Player'], keep='first')

            dataframes[stat_type] = df
            print(f"    ✓ Found {len(df)} players")

            time.sleep(1)  # Be polite to the server

        except Exception as e:
            print(f"    ✗ Error fetching {stat_type}: {e}")
            return None

    # Merge dataframes
    print("\nMerging datasets...")

    # Start with totals
    merged = dataframes['totals'].copy()

    # Add per_game stats (for averages)
    per_game = dataframes['per_game'][['Player', 'PTS', 'TRB', 'AST', 'FG%', '3P%', 'FT%']].copy()
    per_game.columns = ['Player', 'PTS_PG', 'TRB_PG', 'AST_PG', 'FG_PCT', 'FG3_PCT', 'FT_PCT']
    merged = merged.merge(per_game, on='Player', how='left')

    # Add advanced stats
    advanced = dataframes['advanced'][['Player', 'PER', 'TS%', 'USG%', 'OWS', 'DWS', 'WS', 'WS/48', 'OBPM', 'DBPM', 'BPM']].copy()
    advanced.columns = ['Player', 'PER', 'TS_PCT', 'USAGE_RATE', 'OWS', 'DWS', 'WS', 'WS_48', 'OBPM', 'DBPM', 'BPM']
    merged = merged.merge(advanced, on='Player', how='left')

    print(f"✓ Merged dataset: {len(merged)} players with {len(merged.columns)} columns")

    return merged

def clean_and_format(df):
    """
    Clean and format data for RSE model
    """
    print("\nCleaning and formatting data...")

    # Convert numeric columns
    numeric_cols = ['G', 'GS', 'MP', 'FG', 'FGA', '3P', '3PA', 'FT', 'FTA',
                    'ORB', 'DRB', 'TRB', 'AST', 'STL', 'BLK', 'TOV', 'PF', 'PTS',
                    'PTS_PG', 'TRB_PG', 'AST_PG', 'FG_PCT', 'FG3_PCT', 'FT_PCT',
                    'PER', 'TS_PCT', 'USAGE_RATE', 'OWS', 'DWS', 'WS', 'WS_48',
                    'OBPM', 'DBPM', 'BPM']

    for col in numeric_cols:
        if col in df.columns:
            df[col] = pd.to_numeric(df[col], errors='coerce')

    # Rename columns to match RSE model expectations
    df = df.rename(columns={
        'Player': 'PLAYER_NAME',
        'Pos': 'POSITION',
        'Tm': 'TEAM',
        'MP': 'MIN_TOTAL',
        'PTS_PG': 'PTS',
        'TRB_PG': 'REB',
        'AST_PG': 'AST'
    })

    # Add PLAYER_ID
    df.insert(0, 'PLAYER_ID', range(1, len(df) + 1))

    # Calculate derived metrics
    # Plus/Minus approximation (using BPM * minutes as proxy)
    df['PLUS_MINUS'] = df['BPM'] * (df['MIN_TOTAL'] / 1000)

    # Estimate team winning percentage based on team performance
    # For now, use a simplified formula based on player contribution
    # Real implementation would require team standings data
    df['WIN_PCT'] = 0.5 + (df['BPM'] * 0.015)
    df['WIN_PCT'] = df['WIN_PCT'].clip(0.2, 0.8)  # Reasonable NBA range

    # Net Rating approximation
    df['NET_RATING'] = df['BPM'] * 1.5

    # Keep only players with meaningful minutes
    df = df[df['MIN_TOTAL'] >= 100].copy()

    # Select final columns for model
    final_cols = [
        'PLAYER_ID', 'PLAYER_NAME', 'POSITION', 'TEAM', 'MIN_TOTAL',
        'PTS', 'REB', 'AST', 'STL', 'BLK', 'TOV', 'PF',
        'FG_PCT', 'FG3_PCT', 'FT_PCT',
        'USAGE_RATE', 'DWS', 'OWS', 'WS', 'WS_48',
        'PER', 'TS_PCT', 'BPM', 'OBPM', 'DBPM', 'PLUS_MINUS',
        'WIN_PCT', 'NET_RATING'
    ]

    # Keep only columns that exist
    final_cols = [col for col in final_cols if col in df.columns]
    df = df[final_cols].copy()

    # Remove any remaining NaN rows
    df = df.dropna(subset=['PLAYER_NAME', 'MIN_TOTAL', 'USAGE_RATE', 'DWS'])

    print(f"✓ Cleaned dataset: {len(df)} players ready for analysis")
    print(f"  Columns: {', '.join(df.columns)}")

    return df

def main():
    """Main execution"""

    print("=" * 70)
    print("  REAL-TIME NBA DATA FETCHER")
    print("  Source: Basketball Reference (2025-26 Season)")
    print("=" * 70)
    print()

    # Fetch data
    raw_data = fetch_nba_stats(season="2026")

    if raw_data is None:
        print("\n✗ Failed to fetch data. Please check your internet connection.")
        return

    # Clean and format
    final_data = clean_and_format(raw_data)

    # Save to CSV
    output_file = "nba_player_data_real.csv"
    final_data.to_csv(output_file, index=False)

    print(f"\n{'=' * 70}")
    print(f"✓ SUCCESS: Real NBA data saved to: {output_file}")
    print(f"{'=' * 70}")
    print()
    print("Dataset Summary:")
    print(f"  • Total players: {len(final_data)}")
    print(f"  • Average minutes: {final_data['MIN_TOTAL'].mean():.0f}")
    print(f"  • Average PTS: {final_data['PTS'].mean():.1f}")
    print(f"  • Average Usage Rate: {final_data['USAGE_RATE'].mean():.1f}%")
    print(f"  • Average DWS: {final_data['DWS'].mean():.2f}")
    print()
    print("Top 5 Players by Minutes:")
    print(final_data.nlargest(5, 'MIN_TOTAL')[['PLAYER_NAME', 'TEAM', 'MIN_TOTAL', 'PTS', 'USAGE_RATE']])
    print()
    print("Next step: Run the RSE model with:")
    print(f"  source('rse_model.R')")
    print(f"  results <- run_rse_pipeline('{output_file}')")
    print()

if __name__ == "__main__":
    main()
