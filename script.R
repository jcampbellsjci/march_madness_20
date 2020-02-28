library(tidyverse)

setwd("C:/Users/jcamp/OneDrive/Documents/Basketball Projects/march_madness_20")

teams <- read_csv("data/MDataFiles_Stage1/MTeams.csv")
seeds <- read_csv("data/MDataFiles_Stage1/MNCAATourneySeeds.csv")
rs_results <- read_csv("data/MDataFiles_Stage1/MRegularSeasonDetailedResults.csv")
tourney_results <- read_csv("data/MDataFiles_Stage1/MNCAATourneyCompactResults.csv")
sample_sub <- read_csv("data/MSampleSubmissionStage1_2020.csv")


# Finding the average box-score results for each team and each season
# Data is separated by winners and losers
bs_avg <- rs_results %>%
  # Rename all winners as 1 and all losers as 2
  rename_at(.vars = vars(starts_with("W")),
            .funs = ~ gsub(pattern = "W", replacement = "team1_", x = .)) %>%
  rename_at(.vars = vars(starts_with("L")),
            .funs = ~ gsub(pattern = "L", replacement = "team2_", x = .)) %>%
  # Binding a tibble where we do the exact opposite
  # This allows a team to be listed as 1 for wins AND losses
  bind_rows(rs_results %>%
              rename_at(.vars = vars(starts_with("W")),
                        .funs = ~ gsub(pattern = "W",
                                       replacement = "team2_", x = .)) %>%
              rename_at(.vars = vars(starts_with("L")),
                        .funs = ~ gsub(pattern = "L",
                                       replacement = "team1_", x = .))) %>%
  # Grouping by team1 and season
  group_by(team1_TeamID, Season) %>%
  # Finding the mean of all counting stats
  summarize_at(.vars = vars(team1_Score, team2_Score, team1_FGM:team2_PF), 
               .funs = ~ mean(., na.rm = T))


# Creating a wins and losses dataframe for every team
outcome <- rs_results %>%
  # Counting number of times a team had a winning ID
  group_by(WTeamID, Season) %>%
  summarize(W = n()) %>%
  # Joining number of times team had a losing ID
  inner_join(rs_results %>%
               group_by(LTeamID, Season) %>%
               summarize(L = n()), by = c("WTeamID" = "LTeamID", "Season"))


# Combining data sets into a season summary data set
season_summary <- teams %>%
  inner_join(seeds, by = "TeamID") %>%
  inner_join(outcome, by = c("TeamID" = "WTeamID", "Season")) %>%
  inner_join(bs_avg, by = c("TeamID" = "team1_TeamID", "Season")) %>%
  rename_at(.vars = vars(starts_with("team1")), 
            .funs = ~ gsub(pattern = "team1_", replacement = "", x = .)) %>%
  rename_at(.vars = vars(starts_with("team2")), 
            .funs = ~ gsub(pattern = "team2",
                           replacement = "opponent", x = .)) %>%
  # Calculating percentages
  mutate(FGP = FGM / FGA, FG3P = FGM3 / FGA3, FTP = FTM / FTA,
         opponent_FGP = opponent_FGM / opponent_FGA,
         opponent_FG3P = opponent_FGM3 / opponent_FGA3,
         opponent_FTP = opponent_FTM / opponent_FTA,
         WP = W / (W + L)) %>%
  # Getting seeds to be numeric
  # Have to remove region character
  mutate(Seed = substring(Seed, 2)) %>%
  mutate(Seed = as.numeric(gsub(pattern = "a|b", replacement = "", x = Seed)))
