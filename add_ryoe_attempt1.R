# Try to see if we can add ryoe to the dataset from earlier

runs <- runs %>%
  mutate(index = 1:n())

ryoe_vals <- stats::predict(ryoe_model,
                            as.matrix(runs %>%
                                        select(down, distance, yards_to_goal, def_ypc))) %>%
  tibble::as_tibble() %>%
  dplyr::rename(prob = "value") %>%
  dplyr::bind_cols(purrr::map_dfr(seq_along(runs$index), function(x) {
    tibble::tibble("xyds_rushed" = -5:15,
                   "down" = runs$down[[x]],
                   "distance" = runs$distance[[x]],
                   "yards_to_goal" = runs$yards_to_goal[[x]],
                   "def_ypc" = runs$def_ypc[[x]],
                   "index" = runs$index[[x]])
  })) %>%
  dplyr::group_by(.data$index) %>%
  dplyr::mutate(max_loss = dplyr::if_else(.data$yards_to_goal < 95, -5L, as.integer(.data$yards_to_goal - 99L)),
                max_gain = dplyr::if_else(.data$yards_to_goal > 15, 15L, as.integer(.data$yards_to_goal)),
                cum_prob = cumsum(.data$prob),
                prob = dplyr::case_when(.data$xyds_rushed == .data$max_loss ~ .data$prob,
                                        .data$xyds_rushed == .data$max_gain ~ 1 - dplyr::lag(.data$cum_prob, 1),
                                        TRUE ~ .data$prob),
                yardline_100 = .data$yards_to_goal - .data$xyds_rushed) %>%
  dplyr::filter(.data$xyds_rushed >= .data$max_loss, .data$xyds_rushed <= .data$max_gain) %>%
  dplyr::select(-.data$cum_prob) %>%
  dplyr::summarise(x_rush_yards = sum(.data$prob * .data$xyds_rushed)) %>%
  ungroup()

# Not sure how to feel about the values of ryoe_vals but let's see how the numbers shake out for players

runs <- runs %>%
  inner_join(ryoe_vals)

runs <- runs %>%
  mutate(ryoe = yds_rushed - x_rush_yards)

runs %>%
  group_by(rusher_player_name, offense_play) %>%
  summarize(sum_ryoe = sum(ryoe),
            carries = n()) %>%
  arrange(desc(sum_ryoe)) %>%
  filter(carries > 50) -> rushers_no

# Ideally, I'd like for this to be a zero sum stat or close to it

sum(rushers_no$sum_ryoe)

runs %>%
  group_by(defense_play) %>%
  summarize(sum_ryoe = sum(ryoe),
            count = n()) %>%
  arrange(sum_ryoe) %>%
  filter(count > 200) -> ryoe_teams



