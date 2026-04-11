# PURPOSE: Get NHL team stats from Moneypuck
# There is a full dictionary to all of the stats on the website https://moneypuck.com/data.htm

library(tidyverse)

# Load and clean Moneypuck data ------------------------------------------

# These were downloaded from here https://moneypuck.com/data.htm on April 11th
nhl_team_data <- read_csv("data/hockey/nhl-teams-moneypuck.csv")

nhl_games_data <- read_csv("data/hockey/all_nhl_games.csv")

# Get this year's games --------------------------------------------------

nhl_games_2526 <- nhl_games_data |>
    # Just use 5on5, home teams - then join the team stats using home and away columns
    filter(season == 2025, home_or_away == "HOME", situation == "all") |>
    dplyr::select(
        gameId,
        team,
        opposingTeam,
        gameDate,
        goalsFor,
        goalsAgainst
    ) |>
    rename(
        game_id = gameId,
        home_team = team,
        away_team = opposingTeam,
        game_date = gameDate,
        home_goals = goalsFor,
        away_goals = goalsAgainst
    )
write_csv(nhl_games_2526, "data/hockey/nhl_games_2525_411.csv")

# Join the team statistics -----------------------------------------------

# Just grab 5v5 stats for ease
nhl_team_stats <- nhl_team_data |>
    filter(situation == "5on5") |>
    # Drop columns that aren't stats to join:
    dplyr::select(
        -team...1,
        -season,
        -team...4,
        -position,
        -situation,
        -games_played
    )
# Save for ease:
write_csv(nhl_team_stats, "data/hockey/nhl_team_stats_2526_411.csv")

# Make two versions of these with home and away as prefix to column names:
home_team_stats <- nhl_team_stats
colnames(home_team_stats) <- paste0("home_", colnames(nhl_team_stats))
away_team_stats <- nhl_team_stats
colnames(away_team_stats) <- paste0("away_", colnames(nhl_team_stats))

# join for a modeling dataset:
nhl_model_data <- nhl_games_2526 |>
    left_join(home_team_stats, by = c("home_team" = "home_name")) |>
    left_join(away_team_stats, by = c("away_team" = "away_name"))

# Save
write_csv(nhl_model_data, "data/hockey/nhl_game_model_data_2526_411.csv")
