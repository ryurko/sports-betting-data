# PURPOSE: Set-up a dataset that has NBA team statistics so far through March 13th,
#          will then need to join with games to have a modeling dataset ready for the
#          students to use in lab

# UPDATE: Get a new dataset with stats through April 9th

# Ended up needing to use nbastatR - what a throwback, but it still works! seems
# like it is actively being maintained which is nice...

library(tidyverse)

# Get team stats from basketball reference -------------------------------

# Checked that this gives stats up to date through April 9th
nba_team_stats <- nbastatR::bref_teams_stats(seasons = 2026)
View(nba_team_stats$dataTable[[1]])

# Well this is a bit ugly - but it returns several tables include one with
# stats to use:
nba_per_poss_off_stats <- dataBREFPerPossTeams |>
  dplyr::select(
    nameTeam,
    slugTeamBREF,
    astPerPossTeam:minutesPerPossTeam,
    orbPerPossTeam:trbPerPossTeam
  )
# Just offensive stats, so a limited view
nba_per_poss_off_stats <- nba_per_poss_off_stats |>
  rename(team_abbr = slugTeamBREF) |>
  janitor::clean_names()
# Glossary: https://www.basketball-reference.com/leagues/NBA_2026.html
# Scroll down to Per 100 Poss Stats portion

# Fix the abbreviations based on what I saw below:
nba_per_poss_off_stats <- nba_per_poss_off_stats |>
  mutate(
    team_abbr = case_when(
      team_abbr == "CHO" ~ "CHA",
      team_abbr == "PHO" ~ "PHX",
      team_abbr == "BRK" ~ "BKN",
      .default = team_abbr
    )
  )

# Save these stats to use:
# write_csv(nba_per_poss_off_stats, "data/basketball/nba_off_stats_2026.csv")
#write_csv(nba_per_poss_off_stats, "data/basketball/nba_off_stats_2026_321.csv")
#write_csv(nba_per_poss_off_stats, "data/basketball/nba_off_stats_2026_331.csv")
write_csv(nba_per_poss_off_stats, "data/basketball/nba_off_stats_2026_409.csv")

# Get the vector of team abbreviations:
#bref_team_abbr <- unique(nba_per_poss_off_stats$team_abbr)
# Fixed this!

# Need to get a game log to join this information with -------------------

season_schedule <- nbastatR::game_logs(
  seasons = 2026,
  result_types = "team",
  assign_to_environment = FALSE
)

#nba_team_abbr <- unique(season_schedule$slugTeam)

# What needs to be fixed?
#setdiff(nba_team_abbr, bref_team_abbr)
# [1] "BKN" "PHX" "CHA"
#setdiff(bref_team_abbr, nba_team_abbr)
# [1] "BRK" "CHO" "PHO"
# FIXED THIS

# Clean game log to pull out neutral site games --------------------------

game_location_summary <- season_schedule |>
  filter(dateGame < "2026-04-10") |>
  group_by(idGame) |>
  summarize(
    n_home = length(which(locationGame == "H")),
    n_away = length(which(locationGame == "A")),
    .groups = "drop"
  )

# Get the neutral game IDs:
neutral_games <- game_location_summary |>
  filter(n_away == 2) |>
  pull(idGame)

# For ease, we're just going to ignore these

# Create the game data for modeling --------------------------------------

# Okay I can work with this - first get the games for each home team
home_team_schedule_stats <- season_schedule |>
  # Don't include today:
  filter(
    dateGame < "2026-04-09",
    !(idGame %in% neutral_games),
    locationGame == "H"
  ) |>
  # select the necessary columns:
  dplyr::select(
    idGame,
    dateGame,
    slugTeam,
    # Add in points scored
    ptsTeam,
    plusminusTeam,
    outcomeGame
  ) |>
  rename(
    game_id = idGame,
    game_date = dateGame,
    team_abbr = slugTeam,
    points = ptsTeam,
    score_diff = plusminusTeam,
    outcome = outcomeGame
  ) |>
  # Now join the team's stats so far:
  left_join(
    nba_per_poss_off_stats,
    by = c("team_abbr")
  )
# Replace all of the "per_poss_team" underscores with "home"
colnames(home_team_schedule_stats) <-
  str_replace(colnames(home_team_schedule_stats), "per_poss_team", "home")
# The only thing thats wrong here is the missing _ with pct_fg2 and pct_fg3
home_team_schedule_stats <- home_team_schedule_stats |>
  rename(
    pct_fg2_home = pct_fg2home,
    pct_fg3_home = pct_fg3home,
    home_points = points
  )

# Repeat for away:
away_team_schedule_stats <- season_schedule |>
  # Don't include today:
  filter(
    dateGame < "2026-04-09",
    !(idGame %in% neutral_games),
    locationGame == "A"
  ) |>
  # select the necessary columns - don't need the outcomes here
  dplyr::select(
    idGame,
    dateGame,
    slugTeam,
    # Add in points scored
    ptsTeam
  ) |>
  rename(
    game_id = idGame,
    game_date = dateGame,
    team_abbr = slugTeam,
    points = ptsTeam
  ) |>
  # Now join the team's stats so far:
  left_join(
    nba_per_poss_off_stats,
    by = c("team_abbr")
  )
# Replace all of the "per_poss_team" underscores with "home"
colnames(away_team_schedule_stats) <-
  str_replace(colnames(away_team_schedule_stats), "per_poss_team", "away")
# The only thing thats wrong here is the missing _ with pct_fg2 and pct_fg3
away_team_schedule_stats <- away_team_schedule_stats |>
  rename(
    pct_fg2_away = pct_fg2away,
    pct_fg3_away = pct_fg3away,
    away_points = points
  )

# Now need to make a diff (home - away) dataset of the various stats:
game_stats_diff <- home_team_schedule_stats |>
  dplyr::select(game_id, ast_home:trb_home) |>
  pivot_longer(
    cols = ast_home:trb_home,
    names_to = "stat",
    values_to = "home"
  ) |>
  mutate(stat = str_remove(stat, "_home")) |>
  inner_join(
    {
      away_team_schedule_stats |>
        dplyr::select(game_id, ast_away:trb_away) |>
        pivot_longer(
          cols = ast_away:trb_away,
          names_to = "stat",
          values_to = "away"
        ) |>
        mutate(stat = str_remove(stat, "_away"))
    },
    by = c("game_id", "stat")
  ) |>
  mutate(diff = home - away) |>
  # Return wide:
  pivot_wider(
    id_cols = game_id,
    names_from = stat,
    values_from = diff,
    names_glue = "{stat}_diff"
  )

# Create a game model dataset that has all of these columns:
game_model_data <- home_team_schedule_stats |>
  rename(home_team_abbr = team_abbr, home_team_name = name_team) |>
  inner_join(
    away_team_schedule_stats |>
      dplyr::select(-game_date) |>
      rename(away_team_abbr = team_abbr, away_team_name = name_team),
    by = "game_id"
  ) |>
  # the diff stats:
  inner_join(game_stats_diff, by = "game_id") |>
  mutate(total_points = home_points + away_points) |>
  # Reorder the columns slightly:
  dplyr::select(
    game_id,
    game_date,
    home_team_name,
    home_team_abbr,
    home_points,
    away_team_name,
    away_team_abbr,
    away_points,
    total_points,
    score_diff,
    outcome,
    everything()
  )


# Save the dataset -------------------------------------------------------

#write_csv(game_model_data, "data/basketball/nba_game_model_data_2026.csv")
#write_csv(game_model_data, "data/basketball/nba_game_model_data_2026_321.csv")
#write_csv(game_model_data, "data/basketball/nba_game_model_data_2026_331.csv")
write_csv(game_model_data, "data/basketball/nba_game_model_data_2026_409.csv")
