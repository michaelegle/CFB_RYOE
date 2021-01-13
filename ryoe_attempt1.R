library(tidyverse)
library(xgboost)

# try training just on 2019 data for now

set.seed(37)

pbp <- data.frame()
seasons <- 2019
pbp <- purrr::map_df(seasons, function(x) {
  readRDS(
    url(
      glue::glue("https://raw.githubusercontent.com/saiemgilani/cfbscrapR-data/master/data/rds/pbp_players_pos_{x}.rds")
    )
  )
})


# filter out fumbles since those can cause a lot of outliers which we don't care about
runs <- pbp20 %>%
  filter(!is.na(yds_rushed), is.na(fumble_player_name)) %>% 
  select(id_play, game_id, rusher_player_name, yds_rushed, EPA, play_text, down, distance, offense_play,
         defense_play, yards_to_goal)

runs %>%
  group_by(defense_play) %>%
  summarize(def_ypc = mean(yds_rushed),
            count = n()) %>%
  filter(count >= 200) %>%
  select(-count) -> def_ypc

runs <- runs %>%
  left_join(def_ypc)

runs <- runs %>%
  mutate(yds_rushed = case_when(yds_rushed > 15 ~ 15L,
                           yds_rushed < -5 ~ -5L,
                           TRUE ~ as.integer(yds_rushed)),
         label = yds_rushed + 5L)

# ~93% of runs are between -5 and 25 yards, inclusive.
# For the sake of simplicity and the model being intuitive to interpret,
# we'll just set our limits between those values.

model_vars <- runs %>%
  select(label, down, distance, yards_to_goal, def_ypc)

nrounds <- 100
params <-
  list(
    booster = "gbtree",
    objective = "multi:softprob",
    eval_metric = c("mlogloss"),
    num_class = 21,
    eta = .025,
    gamma = 2,
    subsample=0.8,
    colsample_bytree=0.8,
    max_depth = 4,
    min_child_weight = 1
  )

full_train = xgboost::xgb.DMatrix(as.matrix(model_vars %>% select(-label)), label = as.integer(model_vars$label))

ryoe_model <- xgboost::xgboost(params = params, data = full_train, nrounds = nrounds, verbose = 2)





