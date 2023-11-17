library(tidyverse)
library(tidymodels)
library(patchwork)

setwd("C:/Users/rileyw/Item-Demand-Challenge")

train <- read_csv("train.csv")
test <- read_csv("test.csv")

vec = c()
nstores <- max(train$store)
nitems <- max(train$item)
for(s in 1:nstores){
  for(i in 1:nitems){
    storeItemTrain <- train %>%
      filter(store==s, item==i)
    storeItemTest <- test %>%
      filter(store==s, item==i)
    
    
    
    if(s == 1 & i == 1){
      all_preds <- preds
    }
    else{
      all_preds <- bind_rows(all_preds, preds)
    }
  }
}

train %>%
  filter(store == 10, item == 50) %>%
  pull(sales) %>%
  forecast::ggAcf(.) +
  ylim(0, 1) +
  labs(
    title = "Store: 10, Item: 50"
  ) -> plot1

train %>%
  filter(store == 10, item == 40) %>%
  pull(sales) %>%
  forecast::ggAcf(.) +
  ylim(0, 1) +
  labs(
    title = "Store: 10, Item: 40"
  ) -> plot2

train %>%
  filter(store == 10, item == 30) %>%
  pull(sales) %>%
  forecast::ggAcf(.) +
  ylim(0, 1) +
  labs(
    title = "Store: 10, Item: 30"
  ) -> plot3

train %>%
  filter(store == 10, item == 20) %>%
  pull(sales) %>%
  forecast::ggAcf(.) +
  ylim(0, 1) +
  labs(
    title = "Store: 10, Item: 20"
  ) -> plot4
plot_to_save <- ((plot1 | plot2) / (plot3 | plot4))

ggsave("timeseries.png", plot = plot_to_save)


# ---------------- Feature Engineering -------------------

train %>%
  filter(store == 5, item == 18) -> store_item


id_recipe <- recipe(sales ~ date, data = store_item) %>%
  step_date(date, features = c('doy', 'dow', 'decimal')) %>%
  step_range(date_doy, min = 0, max =pi) %>%
  step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy))

rf_mod <- rand_forest(mtry = tune(),
                      min_n = tune(),
                      trees = 400) %>%
  set_engine("ranger") %>%
  set_mode("regression")

rf_wf <- workflow() %>%
  add_model(rf_mod) %>%
  add_recipe(id_recipe)

tuneGrid <- grid_regular(mtry(range(1, 3)),
                         min_n(),
                         levels = 5)
folds <- vfold_cv(store_item, v = 5)

cv_results <- rf_wf %>%
  tune_grid(grid = tuneGrid,
            resamples = folds,
            metrics = metric_set(smape))

collect_metrics(cv_results) %>%
  filter(mtry == 3, min_n == 21) %>%
  pull(mean)

cv_results %>%
  select_best("smape")

# ----------------- Exponential Smoothing --------------------

library(modeltime)
library(tidymodels)

train %>%
  filter(store == 5, item == 18) -> train

cv_split <- time_series_split(train, assess = "3 months", cumulative = T)

cv_split %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, sales, .interactive = F)

es_mod <- exp_smoothing(
  error = "additive",
  trend = "additive",
  season = "additive"
) %>%
  set_engine("ets") %>%
  fit(sales ~ date, data = training(cv_split))

cv_results <- modeltime_calibrate(es_mod, 
                                  new_data = testing(cv_split))

cv_results %>%
  modeltime_forecast(
    new_data = testing(cv_split),
    actual_data = train
  ) %>%
  plot_modeltime_forecast(.interactive = F)

cv_results %>%
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = F)

es_fullfit <- cv_results %>%
  modeltime_refit(data = train)

es_preds <- es_fullfit %>%
  modeltime_forecast(h = "3 months") %>%
  rename(date = .index, sales = .value) %>%
  select(date, sales) %>%
  full_join(., y = test, by = "date") %>%
  select(id, sales)


es_fullfit %>%
  modeltime_forecast(h = "3 months", actual_data = train) %>%
  plot_modeltime_forecast(.interactive = F)

# This is a function to make the two plots that returns them as a list
plot_fun <- function(stores, items){
  train %>%
    filter(store == stores, item == items) -> train
  
  cv_split <- time_series_split(train, assess = "3 months", cumulative = T)
  
  cv_split %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(date, sales, .interactive = F)
  
  es_mod <- exp_smoothing() %>%
    set_engine("ets") %>%
    fit(sales ~ date, data = training(cv_split))
  
  cv_results <- modeltime_calibrate(es_mod, 
                                    new_data = testing(cv_split))
  
  cv_results %>%
    modeltime_forecast(
      new_data = testing(cv_split),
      actual_data = train
    ) %>%
    plot_modeltime_forecast(.interactive = F) -> plot1
  
  cv_results %>%
    modeltime_accuracy() %>%
    table_modeltime_accuracy(.interactive = F)
  
  es_fullfit <- cv_results %>%
    modeltime_refit(data = train)
  
  es_fullfit %>%
    modeltime_forecast(h = "3 months", actual_data = train) %>%
    plot_modeltime_forecast(.interactive = F) -> plot2
  
  return(list(plot1, plot2))
}

plots <- plot_fun(1, 1)

more_plots <- plot_fun(10, 18)

plot1 <- plots[[1]]
plot2 <- more_plots[[1]]
plot3 <- plots[[2]]
plot4 <- more_plots[[2]]

fig <- subplot(plot1, plot2, plot3, plot4, nrows = 2)
fig
