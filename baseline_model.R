library(tidymodels)
library(tidyverse)
library(pins)

options(scipen = 999)

board <-
  board_folder(getwd())

train <- 
  board %>% 
  pin_read('train') %>% 
  mutate(day_of_week = as.factor(day_of_week))

valid <-
  board %>% 
  pin_read('valid') %>% 
  mutate(day_of_week = as.factor(day_of_week))



create_baseline_model <- function(x){
  lm(sales ~ day_of_week + open + promo, data = x)
}

train_nest <-
  train %>% 
  group_by(store) %>% 
  nest() %>% 
  mutate(model = map(data, create_baseline_model)) %>% 
  mutate(results = map(model, augment))


train_results <- 
  train_nest %>% 
  select(store, results) %>% 
  unnest() %>% 
  select(store, sales, day_of_week, .fitted, .resid)


train_results %>% 
  ungroup() %>% 
  summarize(avg_residual = mean(abs(.resid)))


valid_results <- 
  valid %>% 
  group_by(store) %>% 
  nest() %>% 
  left_join(
    train_nest %>% 
      select(store, model)
  ) %>% 
  mutate(preds.new = map2(model, data, ~broom::augment(.x, newdata=.y))) %>% 
  select(preds.new) %>% 
  unnest()

valid_results %>% 
  ungroup() %>%
  mutate(rmspe = ((sales - .fitted) / sales)**2,
         rmspe = ifelse(rmspe == Inf | rmspe == -Inf, 0, rmspe)) %>% 
  summarize(avg_residual = mean(abs(sales - .fitted)),
            rmspe = sqrt(mean(rmspe)))
