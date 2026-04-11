# PURPOSE: Gather and organize MLB data for final projects
# Two packages used here:
# 1: sabRmetrics: https://saberpowers.github.io/sabRmetrics/index.html
# 2: baseballr: https://billpetti.github.io/baseballr/index.html
# (note that I accessed baseballr without loading it)

library(tidyverse)
library(sabRmetrics)

# Get this year's schedule -----------------------------------------------

mlb_schedule <- download_schedule(
  start_date = "2026-01-01",
  end_date = "2026-04-10"
)

mlb_teams <- baseballr::mlb_teams(season = 2026, sport_ids = 1)
# This returns the full name that can then be used to join
mlb_team_table <- mlb_teams |>
  dplyr::select(team_full_name, team_abbreviation)
setdiff(unique(mlb_schedule$team_name_home), mlb_team_table$team_full_name)
# character(0) - good, they are all matching

# Get fangraphs data -----------------------------------------------------

fg_team_batting <- baseballr::fg_team_batter(
  startseason = "2026",
  endseason = "2026"
)
setdiff(fg_team_batting$team_name, mlb_team_table$team_abbreviation)
# [1] "WSN" "TBR" "KCR" "SDP" "ARI" "SFG" "CHW"
# Need to recode these in the mlb_team_table
setdiff(mlb_team_table$team_abbreviation, fg_team_batting$team_name)
# [1] "SD"  "SF"  "TB"  "CWS" "AZ"  "KC"  "WSH"

mlb_team_table <- mlb_team_table |>
  mutate(
    team_abbreviation = case_match(
      team_abbreviation,
      "SD" ~ "SDP",
      "SF" ~ "SFG",
      "TB" ~ "TBR",
      "CWS" ~ "CHW",
      "AZ" ~ "ARI",
      "KC" ~ "KCR",
      "WSH" ~ "WSN",
      .default = team_abbreviation
    )
  )

fg_team_batting <- fg_team_batting |>
  rename(team_abbreviation = team_name) |>
  # drop unnecessary columns
  dplyr::select(-Season, -SeasonMin, -SeasonMax) |>
  # Clean column names:
  janitor::clean_names()

# Repeat for pitching
fg_team_pitching <- baseballr::fg_team_pitcher(
  startseason = "2026",
  endseason = "2026"
)

fg_team_pitching <- fg_team_pitching |>
  rename(team_abbreviation = team_name) |>
  # drop unnecessary columns
  dplyr::select(-Season, -SeasonMin, -SeasonMax) |>
  # Clean column names:
  janitor::clean_names()

# Save the datasets ------------------------------------------------------

# Save a version of the baseball schedule with the team abbreviations
clean_mlb_schedule <- mlb_schedule |>
  as_tibble() |>
  dplyr::select(-year, -game_type, -venue_id, -team_id_away, -team_id_home) |>
  # Join the abbreviations for away team:
  left_join(
    rename(mlb_team_table, team_abbr_away = team_abbreviation),
    by = c("team_name_away" = "team_full_name")
  ) |>
  # Repeat for home:
  left_join(
    rename(mlb_team_table, team_abbr_home = team_abbreviation),
    by = c("team_name_home" = "team_full_name")
  )

# Save this schedule:
write_csv(clean_mlb_schedule, "data/baseball/mlb_games_041026.csv")

# Save the batting and pitching team stats:
write_csv(fg_team_batting, "data/baseball/mlb_team_batting_041026.csv")
write_csv(fg_team_pitching, "data/baseball/mlb_team_pitching_041026.csv")
