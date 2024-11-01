
# Load packages: ----------------------------------------------------------
library(tidyverse)
library(dplyr)
library(plm)
library(fixest)
library(tidyr)
library(stringr)

# Load data ---------------------------------------------------------------
bike <- read.csv("C:/Users/marce/Documents/PraktikumProjekt_Bauer/final_imputed_bike_v2.csv")
# Transform data:
data = pdata.frame(bike, index = c("station", "date"))
data$month <- as.factor(data$month)
data$weekday <- as.factor(data$weekday)

# Build model -------------------------------------------------------------
mod.lm <- plm(log(count) ~ wind_speed + 
                           precipitation + 
                           snowfall + 
                           snow_depth + 
                           mean_temperature +
                           I(mean_temperature^2) + 
                           as.factor(month) +
                           as.factor(weekday) +
                           IsFreezing +
                           log(adj_lagged_count_by_week),
                         data, model = "within", effect = "individual")
summary(mod.lm)

# Since the models obtained from pglm and fixest have the same coefficent, we
# can fit the model in fixest package and use predict on the fixest model which
# does not work for the pglm model. Thus, we can compare the prediction obtained
# from the FE log-normal model and the FE poisson glm!

fix <- feglm(count ~ wind_speed + 
                       precipitation + 
                       snowfall + 
                       snow_depth + 
                       mean_temperature +
                       I(mean_temperature^2) + 
                       as.factor(weekday) +
                       IsFreezing +
                       adj_lagged_count_by_week
                     | station + month,  
                     bike, family = "poisson",
                     cluster = c("station", "month"))


# Create prediction -------------------------------------------------------
# Example prediction newdata (here, 100th row of dataframe)
# NOTE: prediction object is preferred as pdata.frame --> see details in: ?predict.plm
data[100,]$count

# Log-normal prediction of lambda coef
res <- resid(mod.lm)
# Smearing estimate (according to Wooldridge, section: Predicting y when the 
# dependent variable is log(y)):
alpha_zero = sum(exp(res))/31920
transformed <- predict(mod.lm, newdata = data[100,])
(prediction <- exp(transformed)*alpha_zero)

# Poisson glm prediction of lambda coef
# Compare with log-normal model
predict(fix, newdata = data[100,])


# Get prediction or confidence interval ----------------------------------
# x = 1 row p.dataframe 
# type = either "prediction" (def) or "forecast"
# cleaned = F (def) --> p.dataframe object does not include variables that are 
#                       not in the model
# sigma = Only works if type = "forecast"; sigma = either "default" (def) or 
#         "specific" to calculate MSE of residuals on all stations or your 
#         specific station (only marginal differences between the two)

get_interval <- function(x, plm.model, type = "prediction", cleaned = F, sigma = "default"){
  
  # Define var-cov-mat
  V <- t(as.matrix(plm.model$vcov)) %*% as.matrix(plm.model$vcov)
  station_name <- x$station[1]
  
  # Clean up newdata prediction df of no model covariates; otherwise, continue 
  if(cleaned == F){
    x_j <- x
    x_j <- x_j %>% select(-date, -station, -count, -max_temperature, -min_temperature, -obs_count, -IsWeekend, -IsSummerHoliday, -day) 
  }
  else{
    x_j <- x
  }
  
  # Create dummy columns for weekday and month
  for (i in 11:2){
    x_j <- x_j %>% mutate(!!paste0("as.factor(month)", i) := 0, .after = month)
    
    if(i == 2){
      x_j <- x_j %>% mutate(across(starts_with("as.factor(month)"), 
                                   ~ if_else(month == as.numeric(gsub("as.factor\\(month\\)", "", cur_column())), 1, 0)))
      x_j <- x_j %>% select(-month)
    }
  }
  for (i in 1:6){
    weekday_names <- c("Donnerstag", "Freitag", "Mittwoch", "Montag", "Samstag", "Sonntag")
    x_j <- x_j %>% mutate(!!paste0("as.factor(weekday)", weekday_names[i]) := 0, .after = weekday)
    
    if(i == 6){
      x_j <- x_j %>% mutate(across(starts_with("as.factor(weekday)"), 
                                   ~ if_else(weekday == gsub("as.factor\\(weekday\\)", "", cur_column()), 1, 0)))
      x_j <- x_j %>% select(-weekday)
    }
  }
  
  # Change column names, logarithmise lagged count and add in squared mean temperature
  x_j <- x_j %>% mutate(adj_lagged_count_by_week = log(adj_lagged_count_by_week)) %>%
    mutate(mean_temp.sq = mean_temperature^2, .after = mean_temperature) %>% 
    rename("log(adj_lagged_count_by_week)" = "adj_lagged_count_by_week",
           "I(mean_temperature^2)" = mean_temp.sq)
  
  # Check x_j if correct
  # x_j
  
  # Transform into vector for later matrix multiplication
  x_j <- as.vector(unlist(x_j))
  
  # Calculate standard error of prediction (not forecast)
  if(type == "prediction"){
  se_mean <- sqrt(t(x_j) %*% V %*% x_j) 
  }
  if(type == "forecast"){
    h_j <- t(x_j) %*% V %*% x_j
    if(sigma == "default"){
      sigma <- mean(mod.lm$residuals^2) # take MSE over all stations
    }
    if(sigma == "specific"){
      # take MSE over your specific station
      foo <- mod.lm$residuals %>% as.data.frame() %>% 
        rownames_to_column(var = "station") %>% 
        mutate(station = str_replace(station, "(.*)-[^-]+-[^-]+-[^-]+$", "\\1")) 
      index <- which(foo$station == as.character(station_name))
      foo <- foo[,2][index]
      sigma <- mean(foo^2)
    }
    # if(sigma != "default" & sigma != "specific"){
    #   print("Error: Sigma is wrong; If type = forecast, sigma should be default or specific!")
    # }
    se_mean <- sigma * sqrt(1 + h_j)
  }
  if(type != "prediction" & type != "forecast"){
    print("Error: Type is wrong; Type should be either prediction (def) or forecast!")
  }
  # Obtain prediction
  res <- resid(plm.model)
  # Smearing estimate (according to Wooldridge, section: Predicting y when the 
  # dependent variable is log(y))
  alpha_zero = sum(exp(res))/dim(plm.model$model)[1]
  transformed <- predict(plm.model, newdata = x)
  prediction <- as.vector(exp(transformed)*alpha_zero)
  
  # Obtain t-value DoF for upper and lower
  if(length(station_name) != 0){
    foo <- mod.lm$model %>%
      rownames_to_column(var = "station") %>% 
      mutate(station = str_replace(station, "(.*)-[^-]+-[^-]+-[^-]+$", "\\1")) %>%
      count(station, name = "count")
    ind <- as.vector(which(foo$station == as.character(station_name)))
    N <- as.vector(foo$count[ind])
  }
  else{
    print("Error: Include station column with station name")
  }
  
  alpha = 0.05
  df <- N - 24 - 1 # 24 covariates, 1 station FE
  t_val <- qt(p = 1-alpha/2, df)
  
  upper = prediction + t_val * se_mean
  lower = prediction - t_val * se_mean
  
  # Return Interval
  df <- data.frame(prediction, lower, upper)
  return(df)
}

get_interval(x = data[100,], plm.model = mod.lm, type = "prediction", cleaned = F)
get_interval(x = data[100,], plm.model = mod.lm, type = "forecast", cleaned = F)
get_interval(x = data[100,], plm.model = mod.lm, type = "forecast", sigma = "specific", cleaned = F)



