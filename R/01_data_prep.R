# Goal of this script is to just clean the data and put it into a
# good modeling format.
# 1. Season averages are found for each team for all box-score stats
# 2. Find record for season
# 3. Find record for past five games
# 4. Find record against tournament teams
# 5. Clean Ken Pom data
# 6. Combine data into a season summary
# 7. Combine season summaries into a head-to-head tibble

#### Data Load ####

library(tidyverse)

setwd("C:/Users/jcamp/OneDrive/Documents/Basketball Projects/march_madness")

# Loading up the files we'll use
teams <- read_csv("data/MDataFiles_Stage1/MTeams.csv")
seeds <- read_csv("data/MDataFiles_Stage1/MNCAATourneySeeds.csv")
rs_results <- read_csv("data/MDataFiles_Stage1/MRegularSeasonDetailedResults.csv")
tourney_results <- read_csv("data/MDataFiles_Stage1/MNCAATourneyCompactResults.csv")
sample_sub <- read_csv("data/MSampleSubmissionStage1_2020.csv")


#### Find Box Score Averages ####

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
               .funs = ~ mean(., na.rm = T)) %>%
  # Calculating percentages
  mutate(team1_FGP = team1_FGM / team1_FGA,
         team1_FG3P = team1_FGM3 / team1_FGA3,
         team1_FTP = team1_FTM / team1_FTA,
         team2_FGP = team2_FGM / team2_FGA,
         team2_FG3P = team2_FGM3 / team2_FGA3,
         team2_FTP = team2_FTM / team2_FTA)
  

#### Find W-L Season Outcome ####

# Creating a wins and losses dataframe for every team
outcome <- rs_results %>%
  # Counting number of times a team had a winning ID
  group_by(WTeamID, Season) %>%
  summarize(W = n()) %>%
  # Joining number of times team had a losing ID
  inner_join(rs_results %>%
               group_by(LTeamID, Season) %>%
               summarize(L = n()), by = c("WTeamID" = "LTeamID", "Season")) %>%
  mutate(WP = W / (W + L))


#### Find W-L Over Past Five Games ####

# Interested in finding wins and losses going into the tournament
outcome_5 <- rs_results %>%
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
  select(DayNum, Season, team1_TeamID, team1_Score, team2_Score) %>%
  mutate(outcome = ifelse(team1_Score - team2_Score > 0, "W", "L")) %>%
  # Grouping by team1 and season
  group_by(team1_TeamID, Season) %>%
  top_n(5, DayNum) %>%
  group_by(team1_TeamID, Season, outcome) %>%
  summarize(result = n()) %>%
  spread(key = outcome, value = result) %>%
  mutate_at(.vars = vars(W, L),
            .funs = ~ ifelse(is.na(.), 0, .)) %>%
  mutate(WP_5 = W / (W + L)) %>%
  select(team1_TeamID, Season, WP_5)


#### Finding W-L Against Tournament Teams ####

outcome_tourney <- rs_results %>%
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
  select(DayNum, Season, team1_TeamID, team2_TeamID, team1_Score,
         team2_Score) %>%
  mutate(outcome = ifelse(team1_Score - team2_Score > 0, "W", "L")) %>%
  inner_join(seeds, by = c("team2_TeamID" = "TeamID", "Season")) %>%
  group_by(team1_TeamID, Season, outcome) %>%
  summarize(result = n()) %>%
  spread(key = outcome, value = result) %>%
  mutate_at(.vars = vars(W, L),
            .funs = ~ ifelse(is.na(.), 0, .)) %>%
  select(team1_TeamID, Season, W_tourney = W, L_tourney = L)


#### Cleaning Ken Pom Data ####

# Load up kpom data and team spellings
kpom <- read_csv(file = "data/our_data/kenpom.csv") %>%
  select(year, name, adjo, adjd, adjt, luck, oppos, oppds, adjemn)
spellings <- read_csv(file = "data/MDataFiles_Stage1/MTeamSpellings.csv")

# Finding everyone in kpom who is not in the team name data
no_names <- kpom %>%
  mutate(name = tolower(name)) %>%
  left_join(spellings, by = c("name" = "TeamNameSpelling")) %>%
  filter(is.na(TeamID))
# Renaming those
spellings <- tibble(old_name = sort(unique(no_names$name)),
                    new_name = c("arkansas-little rock",
                                 "arkansas-pine bluff", "bethune-cookman",
                                 "cal state bakersfield", "ill-chicago",
                                 "liu brooklyn", "louisiana-lafayette",
                                 "louisiana-monroe", "maryland-eastern shore",
                                 "mississippi valley state",
                                 "se missouri state", "sw missouri st.",
                                 "texas st", "st. francis (ny)",
                                 "st. francis (pa)", "tennessee-martin",
                                 "texas a&m cc", "tex.-pan american",
                                 "texas rio grande valley",
                                 "winston-salem-state"
                    )) %>%
  left_join(spellings, by = c("new_name" = "TeamNameSpelling")) %>%
  select(TeamNameSpelling = old_name, TeamID) %>%
  bind_rows(spellings)

kpom <- kpom %>%
  mutate(name = tolower(name)) %>%
  left_join(spellings, by = c("name" = "TeamNameSpelling")) %>%
  select(-name)


#### Combining Data Into a School's Season Summary ####

# Combining data sets into a season summary data set
season_summary <- teams %>%
  left_join(outcome, by = c("TeamID" = "WTeamID")) %>%
  left_join(outcome_5, by = c("TeamID" = "team1_TeamID", "Season")) %>%
  left_join(outcome_tourney, by = c("TeamID" = "team1_TeamID", "Season")) %>%
  left_join(bs_avg, by = c("TeamID" = "team1_TeamID", "Season")) %>%
  left_join(seeds, by = c("TeamID", "Season")) %>%
  left_join(kpom, by = c("TeamID", "Season" = "year")) %>%
  rename_at(.vars = vars(starts_with("team1")), 
            .funs = ~ gsub(pattern = "team1_", replacement = "", x = .)) %>%
  rename_at(.vars = vars(starts_with("team2")), 
            .funs = ~ gsub(pattern = "team2",
                           replacement = "opponent", x = .)) %>%
  # Getting seeds to be numeric
  # Have to remove region character
  mutate(Seed = substring(Seed, 2)) %>%
  mutate(Seed = as.numeric(gsub(pattern = "a|b", replacement = "",
                                x = Seed))) %>%
  filter(Season >= 2000) %>%
  filter(!(is.na(W))) %>%
  select(TeamID:Season, order(colnames(.)))

write_csv(season_summary, path = "data/our_data/season_summary.csv")


#### Combining Data into a H2H Tourney Format ####

winners <- tourney_results %>%
  inner_join(season_summary, by = c("WTeamID" = "TeamID", "Season")) %>%
  inner_join(season_summary, by = c("LTeamID" = "TeamID", "Season")) %>%
  rename_at(.vars = vars(ends_with(".x")),
            .funs = ~ paste0("team1_", .)) %>%
  rename_at(.vars = vars(ends_with(".x")),
            .funs = ~ gsub(pattern = ".x", replacement = "", x = .)) %>%
  rename_at(.vars = vars(ends_with(".y")),
            .funs = ~ paste0("team2_", .)) %>%
  rename_at(.vars = vars(ends_with(".y")),
            .funs = ~ gsub(pattern = ".y", replacement = "", x = .)) %>%
  rename(team1_TeamID = WTeamID, team1_GameScore = WScore,
         team2_TeamID = LTeamID, team2_GameScore = LScore) %>%
  mutate(outcome = "W") %>%
  sample_frac(.5)
losers <- tourney_results %>%
  inner_join(season_summary, by = c("WTeamID" = "TeamID", "Season")) %>%
  inner_join(season_summary, by = c("LTeamID" = "TeamID", "Season")) %>%
  rename_at(.vars = vars(ends_with(".x")),
            .funs = ~ paste0("team2_", .)) %>%
  rename_at(.vars = vars(ends_with(".x")),
            .funs = ~ gsub(pattern = ".x", replacement = "", x = .)) %>%
  rename_at(.vars = vars(ends_with(".y")),
            .funs = ~ paste0("team1_", .)) %>%
  rename_at(.vars = vars(ends_with(".y")),
            .funs = ~ gsub(pattern = ".y", replacement = "", x = .)) %>%
  rename(team2_TeamID = WTeamID, team2_GameScore = WScore,
         team1_TeamID = LTeamID, team1_GameScore = LScore) %>%
  mutate(outcome = "L")

final <- winners %>%
  bind_rows(losers %>%
              filter(!(paste0(team1_TeamID, team2_TeamID) %in%
                         paste0(winners$team2_TeamID,
                                winners$team1_TeamID)))) %>%
  select(Season:NumOT, order(colnames(.)))
  

write_csv(season_summary, path = "data/our_data/tourney_h2h.csv")
