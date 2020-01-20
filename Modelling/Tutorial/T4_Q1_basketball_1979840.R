# Problem Set 4
# Question 1

# Name: Janine Rottmann
# Matrikelnummer: 1979840

#------------------------1a----------------------------

library(ggplot2)
library(tidyverse)
library(hexbin)

setwd('./Tutorial 4')
read_csv2('basketball_complete.csv') -> baskettball

baskettball %>%
  group_by(loc_x, loc_y) %>%
  summarise(goals = sum(shot_made_flag),
            position = n(),
            value = goals / position) %>%
  arrange(value) %>%
  ggplot(aes(x = loc_x, y = loc_y, z = value)) +
  stat_summary_hex()

#------------------------1b----------------------------

library(tidyverse)
library(recipes)
library(rsample)
library(tidymodels)
library(parsnip)

#split
baskettball_split <- initial_split(baskettball, prop = 0.8)
train_set <- training(baskettball_split)
test_set <- testing(baskettball_split)

#predict
recipe(shot_made_flag ~ ., data = train_set) -> rec

#clean_data
sum(is.na(baskettball)) #no NA --> no imputing

rec %>%
  step_num2factor(shot_made_flag) %>%
  step_naomit(season, shot_type, combined_shot_type) %>%
  step_dummy(season, shot_type, combined_shot_type) -> rec


#prep rec
rec %>%
  check_missing(all_predictors()) %>%
  prep() -> prepped_rec

#bake rec
prepped_rec %>%
  juice() -> train_set_baked

prepped_rec %>%
  bake(new_data = test_set) -> test_set_baked

#-----------------------1c------------------------

#linear regression
logistic_reg(mode = 'classification') %>%  
  set_engine('glm') %>%
  fit(shot_made_flag ~ ., data = train_set_baked) -> logisitc_reg

logisitc_reg %>% 
  predict(new_data = test_set_baked) %>%
  bind_cols(test_set_baked %>% dplyr::select(shot_made_flag)) -> predictions_lm


#random forest
rand_forest(mode = "classification", trees = 250) %>%
  set_engine("ranger") %>%
  fit(shot_made_flag ~ ., data = train_set_baked) -> rf_mod

rf_mod%>%
  predict(new_data = test_set_baked) %>%
  bind_cols(test_set_baked %>% dplyr::select(shot_made_flag)) -> predictions_rf

#boosting
boost_tree(mode = "classification", trees = 500, mtry = 5, learn_rate = 0.075, sample_size = 1, tree_depth = 3) %>%
  set_engine("xgboost") %>%
  fit(shot_made_flag ~ ., data = train_set_baked) -> boost_mod

boost_mod %>%
  predict(new_data = test_set_baked) %>%
  bind_cols(test_set_baked %>% dplyr::select(shot_made_flag)) -> predictions_boost

#evaluation of classification models

predictions_lm %>%
  conf_mat(shot_made_flag, .pred_class) %>%
  summary() %>%
  dplyr::select(-.estimator) %>%
  filter(.metric %in%
           c("accuracy", "mcc", "f_meas"))

predictions_rf %>%
  conf_mat(shot_made_flag, .pred_class) %>%
  summary() %>%
  dplyr::select(-.estimator) %>%
  filter(.metric %in%
           c("accuracy", "mcc", "f_meas"))

predictions_boost %>%
  conf_mat(shot_made_flag, .pred_class) %>%
  summary() %>%
  dplyr::select(-.estimator) %>%
  filter(.metric %in%
           c("accuracy", "mcc", "f_meas"))










  
               