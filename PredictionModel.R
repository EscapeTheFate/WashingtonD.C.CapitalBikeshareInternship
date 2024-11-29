
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

get_interval <- function(x, plm.model, type = "prediction", cleaned = F,
                 sigma = "default", smearing = "default", alpha = 0.05){
  
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
  
  # Calculate standard error of prediction (in-sample)
  if(type == "prediction"){
    se_mean <- sqrt(t(x_j) %*% V %*% x_j) 
  }
  # Calculate standard error of forecast (out-of-sample)
  if(type == "forecast"){
    if(sigma == "default"){
      sigma <- sqrt(mean(mod.lm$residuals^2)) # take MSE over all stations
    }
    if(sigma == "specific"){
      # take MSE over your specific station
      foo <- mod.lm$residuals %>% as.data.frame() %>% 
        rownames_to_column(var = "station") %>% 
        mutate(station = str_replace(station, "(.*)-[^-]+-[^-]+-[^-]+$", "\\1")) 
      index <- which(foo$station == as.character(station_name))
      foo <- foo[,2][index]
      sigma <- sqrt(mean(foo^2))
    }
    # Calculate h_j
    se_mean <- sqrt(sigma^2 + t(x_j) %*% V %*% x_j) 
  }
  if(type != "prediction" & type != "forecast"){
    return(print("Error: Type is wrong; Type should be either prediction (def) or forecast!"))
  }
  
  # Obtain prediction
  if(smearing == "default"){
    res = resid(plm.model)
    alpha_zero = sum(exp(res))/dim(plm.model$model)[1]
  }
  if(smearing == "specific"){
    foo <- resid(plm.model) %>% as.data.frame() %>% 
      rownames_to_column(var = "station") %>% 
      mutate(station = str_replace(station, "(.*)-[^-]+-[^-]+-[^-]+$", "\\1"))
    index <- which(foo$station == as.character(station_name))
    res <- foo[,2][index]
    alpha_zero = sum(exp(res))/length(res)
  }
  if(smearing != "default" & smearing != "specific"){
    return(print("Error: Smearing should be all (residuals on all stations) or specific (only your station)"))
  }
  
  # Smearing estimate (according to Wooldridge, section: Predicting y when the 
  # dependent variable is log(y))
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
    return(print("Error: Include station column with station name"))
  }
  
  # alpha = 0.05
  df <- N - 24 - 1 # 24 covariates, 1 station FE
  t_val <- qt(p = 1-alpha/2, df)
  
  upper = exp(log(prediction) + t_val * se_mean)
  lower = exp(log(prediction) - t_val * se_mean)
  
  # Return Interval
  df <- data.frame(prediction, lower, upper)
  return(df)
}

get_interval(x = data[100,], plm.model = mod.lm, type = "prediction", cleaned = F, smearing = "default")
get_interval(x = data[100,], plm.model = mod.lm, type = "prediction", cleaned = F, smearing = "default", alpha = 0.1)
get_interval(x = data[100,], plm.model = mod.lm, type = "prediction", cleaned = F, sigma = "specific", smearing = "specific")
get_interval(x = data[100,], plm.model = mod.lm, type = "forecast", cleaned = F , sigma = "specific", smearing = "specific")
get_interval(x = data[100,], plm.model = mod.lm, type = "forecast", cleaned = F, sigma = "default", smearing = "default")

# Regression Results Plots ------------------------------------------------
# We are also including a prediction vs. mean temperature plot:
dep_mean_temp <- seq(10, 90, length.out = 640)
predictions <- data.frame(x = dep_mean_temp,
                          y_hat = c(rep.int(NA, length(dep_mean_temp))),
                          y_hat_lower = c(rep.int(NA, length(dep_mean_temp))),
                          y_hat_upper = c(rep.int(NA, length(dep_mean_temp))),
                          y_hat_lower_forecast = c(rep.int(NA, length(dep_mean_temp))),
                          y_hat_upper_forecast = c(rep.int(NA, length(dep_mean_temp))),
                          IsFreezing = ifelse(dep_mean_temp <= 32, 1, 0))

result.data <- data %>% filter(station == "New Hampshire Ave & T St NW", month == 8)
(vec <- result.data[1,])
# Untouched vars: weekday (monday), month (august), snow_depth (0), snowfall (0)
vec$wind_speed[1] <- mean(result.data$wind_speed)
vec$precipitation[1] <- mean(result.data$precipitation)
vec$adj_lagged_count_by_week[1] <- mean(result.data$adj_lagged_count_by_week)
vec$mean_temperature[1] <- mean(result.data$mean_temperature)
(vec)
for (i in 1:length(dep_mean_temp)){
  vec$mean_temperature[1] <- dep_mean_temp[i]
  if(dep_mean_temp[i] <= 32){
    vec$IsFreezing[1] <- 1
  }
  else{
    vec$IsFreezing[1] <- 0
  }
  pred <- get_interval(x = vec, plm.model = mod.lm, type = "prediction", cleaned = F, sigma = "specific")
  fore <- get_interval(x = vec, plm.model = mod.lm, type = "forecast", cleaned = F, sigma = "specific", smearing = "specific")
  predictions$y_hat[i] <- as.vector(pred[1])
  predictions$y_hat_lower[i] <- as.vector(pred[2])
  predictions$y_hat_upper[i] <- as.vector(pred[3])
  predictions$y_hat_lower_forecast[i] <- as.vector(fore[2])
  predictions$y_hat_upper_forecast[i] <- as.vector(fore[3])
}
# View(predictions)

# Plot ----------------------------------------------------------------
# Add expected val
ind <- which(predictions$IsFreezing == 0)
plot(x = predictions$x[ind], y = predictions$y_hat[ind], type = "l", col = "black",
     xlab = "Mean temperature (°F)", bty = "n", main = "", ylim = c(0, 100), xlim = c(20, 80),
     ylab = "Predicted Bike Count", lwd = 2)
# add fill
polygon(x = c(predictions$x, rev(predictions$x)),
        y = c(predictions$y_hat_upper_forecast, rev(predictions$y_hat_lower_forecast)), col = 'grey80', border = NA)
lines(predictions$x[-ind], predictions$y_hat[-ind], type = "l", col = "black", lwd = 2)
lines(predictions$x[ind], predictions$y_hat[ind], type = "l", col = "black", lwd = 2)

# Add confidence interval lines (these lie practically on the predictions)
#lines(predictions$x, predictions$y_hat_upper, col = "red", lty = 2, lwd = 1.5)  # Upper bound
#lines(predictions$x, predictions$y_hat_lower, col = "red", lty = 2, lwd = 1.5)  # Lower bound
# Add forecast interval lines
lines(predictions$x[ind], predictions$y_hat_upper_forecast[ind], col = "red", lty = 5, lwd = 1.5)  # Upper bound
lines(predictions$x[ind], predictions$y_hat_lower_forecast[ind], col = "red", lty = 5, lwd = 1.5)  # Lower bound
lines(predictions$x[-ind], predictions$y_hat_upper_forecast[-ind], col = "red", lty = 5, lwd = 1.5)  # Upper bound
lines(predictions$x[-ind], predictions$y_hat_lower_forecast[-ind], col = "red", lty = 5, lwd = 1.5)
# Add more info
axis(side = 4, at = seq(0, 100, by = 20), labels = seq(0, 100, by = 20))
abline(h = seq(0, 100, by = 20), col = alpha("black", 0.3), lty = "aa")
