

# Load packages -----------------------------------------------------------
library(tidyverse)
library(dplyr)
library(plm)
library(pglm)
library(fixest)

# Load in data ------------------------------------------------------------
bike <- read.csv("C:/Users/marce/Documents/PraktikumProjekt_Bauer/final_bike.csv")
bike_no_august <- read.csv("C:/Users/marce/Documents/PraktikumProjekt_Bauer/final_bike_no_august.csv")
bike_list <- bike %>% select(-lagged_count, -lagged_count_by_week, -adj_lagged_count) %>% 
  mutate(IsAugust = if_else(month == 8, T, F)) %>% group_by(station) %>% group_split()


# First try: --------------------------------------------------------------
data = pdata.frame(bike_no_august, index = c("station", "date")) # Alt.: bike
summary(plm.final <- plm(log(count) ~ wind_speed + 
                           precipitation + 
                           snowfall + 
                           snow_depth + 
                           mean_temperature +
                           I(mean_temperature^2) + 
                           as.factor(month) +
                           as.factor(weekday) +
                           IsFreezing +
                           log(adj_lagged_count_by_week),
                         data, model = "within", effect = "individual"))

summary(fix <- feglm(count ~ wind_speed + 
                       precipitation + 
                       snowfall + 
                       snow_depth + 
                       mean_temperature +
                       I(mean_temperature^2) + 
                       as.factor(weekday) +
                       IsFreezing +
                       adj_lagged_count_by_week
                       | station + month,  # To add only the variables with varying slopes and not the fixed-effect,
                         bike_no_august, family = "poisson",             # use double square brackets: fixef_var[[var1, var2]].
                         cluster = c("station", "month")))
fixest::fixef(fix)$month

summary(pglm <- pglm(count ~ wind_speed + 
                        precipitation + 
                        snowfall + 
                        snow_depth + 
                        mean_temperature +
                        I(mean_temperature^2) + 
                        month + 
                        weekday +
                        IsFreezing +
                        adj_lagged_count_by_week,
                      (data %>% mutate(month = as.factor(month), weekday = as.factor(weekday))),
                      family = "poisson", model = "within", effect = "individual"))
# --> fixest and pglm show both the same month FE coefficients

# Start off with interpolation of FE month coefs --------------------------
# We want to interpolate the following FE coefs:
coefs <- data.frame(y = fixef(fix)$month, x = setdiff(1:11, 8) + 0.5)
# Run interpolation and extract the coefs for august section (all 31 days)
spl <- spline(y = coefs$y, x = coefs$x, n = 31 * length(coefs$x))
length(which(spl$x > 8 & spl$x < 9)) # the 31 obs we use for august (their index)
index <- which(spl$x > 8 & spl$x < 9) # Intercept x-level 
interpolation_x <- spl$x[index]
interpolation_y <- spl$y[index]

# Prepare for prediction of august section per station:
# To-Do: Fix lag_count!
#        Check for correct monthly coefficient (should be month with baseline coef)
set.seed(1)
for (i in 1:100){ # for-loop over stations
  flag = 0 # flag for potential errors
  
  august_index <- which(bike_list[[i]]$month == 8)
  august_obs_count <- bike_list[[i]]$obs_count[august_index]
  august_count <- august_lambda <- c(rep.int(NA, length(august_index)))
  
  # Change month to 1 for baseline coef (no month FE, so that we can add it ourself)
  bike_list[[i]]$month <- ifelse(bike_list[[i]]$month == 8, 1, bike_list[[i]]$month)
  
  for (j in 1:length(august_lambda)){ # for-loop over august days
    
    # Predict lambda and randomly generate count for that day (also account for possible NA):
    
    if(is.na(bike_list[[i]]$adj_lagged_count_by_week[august_index[j]])){  # if: lagged count is NA (so, count from 1 week ago),
                                                                          # we will also impute the count from 1 week ago for the lagged count
      # Find out which row is from 1 week ago:
      lagged_week_obs_count <- bike_list[[i]][august_index[j],]$obs_count-7
      lagged_row <- which(bike_list[[i]]$obs_count == lagged_week_obs_count)
      
      if(is.na(bike_list[[i]]$adj_lagged_count_by_week[lagged_row]) == F & length(lagged_row) != 0){  # if: lagged count can be imputed (from its data from 1 week ago)
        lagged_lambda <- predict( fix, newdata = bike_list[[i]][lagged_row,], type = "response" ) * exp(fixef(fix)$month[7]) # take july month FE instead here
        lagged_fill_in <- rpois(n = 1, lambda = lagged_lambda) # imputed count from 1 week ago (in july)
        
        # Use imputed count from 1 week ago (in july) as lagged count value for august day prediction
        august_lambda[j] <- predict( fix, newdata = bike_list[[i]][august_index[j],] %>% mutate(adj_lagged_count_by_week = lagged_fill_in), type = "response" ) * exp(interpolation_y[j])
      }
      else{  # we are here if the lagged count for todays prediction is NA due 
             # to skipped row (e.x. day 23 -> 25) one week ago, or NA in lagged
             # count from 2 weeks ago, which leads to NA imputation
             # --> We double impute: count one week ago, and two weeks ago, the 
             #     latter by the row from another station having that row
        
        # Find out which row is from 2 weeks ago:
        lagged_2week_obs_count <- bike_list[[i]][lagged_row,]$obs_count-7
        # Take row from station 2 (has no missings on time dimension)
        lagged_2row <- which(bike_list[[2]]$obs_count == lagged_2week_obs_count)
        station_name <- bike_list[[i]]$station[1]
        # Find out lagged count value for the row from 2 weeks ago (since row is NA, entry does not exist)
        adj_lag_count_3_weeks_ago <- bike_list[[i]]$count[which(bike_list[[i]]$obs_count == lagged_2week_obs_count-7)]
        
        if(length(adj_lag_count_3_weeks_ago) == 0){
          print("Error occured: error 3 weeks ago (NA)")
          flag = 1
          break()
        }
        
        # Impute from 2 weeks ago
        lagged_2lambda <- predict( fix, newdata = bike_list[[2]][lagged_2row,] %>% mutate(station = station_name, adj_lagged_count_by_week = adj_lag_count_3_weeks_ago), type = "response" ) * exp(fixef(fix)$month[7]) # take july month FE instead here
        lagged_2fill_in <- rpois(n = 1, lambda = lagged_2lambda) # imputed count from 2 week ago (in july)
        
        # Impute from 1 weeks ago
        lagged_lambda <- predict( fix, newdata = bike_list[[i]][lagged_row,] %>% mutate(adj_lagged_count_by_week = lagged_2fill_in), type = "response" ) * exp(fixef(fix)$month[7]) # take july month FE instead here
        lagged_fill_in <- rpois(n = 1, lambda = lagged_lambda) # imputed count from 1 week ago (in july)
        
        # Use imputed count from 1 week ago (in july) as lagged count value for todays august day prediction
        august_lambda[j] <- predict( fix, newdata = bike_list[[i]][august_index[j],] %>% mutate(adj_lagged_count_by_week = lagged_fill_in), type = "response" ) * exp(interpolation_y[j])
      }
    } 
    else{     # if lagged count is not NA, continue prediction as usually
      august_lambda[j] <- predict( fix, newdata = bike_list[[i]][august_index[j],], type = "response" ) * exp(interpolation_y[j])
    }
    
    august_count[j] <- rpois(n = 1, lambda = august_lambda[j])
    
    # Overwrite count entry for that day
    bike_list[[i]]$count[august_index[j]] = august_count[j]
    
    # Find out if there exists a lagged count entry
    lagged_entry_obs_count <- bike_list[[i]]$obs_count[august_index[j]] + 7
    lagged_entry_index <- which(bike_list[[i]]$obs_count == lagged_entry_obs_count)
    
    # If it exists, overwrite it; otherwise, do nothing (the row with the lagged count would be NA)
    if(lagged_entry_obs_count %in% bike_list[[i]]$obs_count){
      bike_list[[i]]$adj_lagged_count_by_week[lagged_entry_index] = august_count[j]
    }
  }
  
  if(flag == 1){
    print("Error in loop: Look at i and j for error-prone iteration") 
    break() # get out of station loop
  }
  
  # Fix month variable to now include august again (was overwritten for prediction
  # on january, so that FE month coef is equal to 0)
  bike_list[[i]]$month = ifelse(bike_list[[i]]$IsAugust == T, 8, bike_list[[i]]$month)
  bike_list[[i]] <- bike_list[[i]] %>% select(-IsAugust)
}

# Inspect imputation results: ---------------------------------------------
# Save results:
bike <- bind_rows(bike_list)
bike <- bike %>% select(-IsAugust)

# Lets take a look at the 3 lowest, the 3 highest and somewhere inbetween (by mean)
foo <- bike %>% group_by(station) %>% summarize(mean = mean(count, na.rm = T),
                                                median = median(count, na.rm = T)) %>% arrange(mean) %>% 
  slice(c(1:3, 49:51, 98:100)) %>% select(station) %>% as.vector()
par(mfrow = c(3, 3))
for (i in 1:length(foo$station)){
  foo2 <- bike %>% filter(station == foo$station[i]) %>% select(count, obs_count, month) %>% drop_na()
  station_name <- paste("Station", i)
  plot(y = foo2$count[which(foo2$month != 8)], x = foo2$obs_count[which(foo2$month != 8)], bty = "n", pch = 16, col = alpha("black", 0.6),
       xlab = "Obs. Nr.", ylab = "Counts", main = station_name, xaxt = "n")
  axis(1, at = c(1, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334))
  abline(v = c(31.5, 59.5, 90.5, 120.5, 151.5, 
               181.5, 212.5, 243.5, 273.5, 304.5),
         col=alpha("black", 0.3), lwd=0.5, lty='dashed')
  points(y = foo2$count[which(foo2$month == 8)], x = foo2$obs_count[which(foo2$month == 8)], bty = "n", pch = 16, col = alpha("red", 0.6))
}
par(mfrow = c(1, 1))

# Now, for each station:
foo <- bike %>% group_by(station) %>% summarize(mean = mean(count, na.rm = T),
                                                median = median(count, na.rm = T)) %>% 
             arrange(mean) %>% select(station) %>% as.vector()
par(mfrow = c(1, 1))
for (i in 1:length(foo$station)){
  foo2 <- bike %>% filter(station == foo$station[i]) %>% select(count, obs_count, month) %>% drop_na()
  station_name <- paste("Station", i)
  plot(y = foo2$count[which(foo2$month != 8)], x = foo2$obs_count[which(foo2$month != 8)], bty = "n", pch = 16, col = alpha("black", 0.6),
       xlab = "Obs. Nr.", ylab = "Counts", main = station_name, xaxt = "n")
  axis(1, at = c(1, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334))
  abline(v = c(31.5, 59.5, 90.5, 120.5, 151.5, 
               181.5, 212.5, 243.5, 273.5, 304.5),
         col=alpha("black", 0.3), lwd=0.5, lty='dashed')
  points(y = foo2$count[which(foo2$month == 8)], x = foo2$obs_count[which(foo2$month == 8)], bty = "n", pch = 16, col = alpha("red", 0.6))
}



# Re-fit model: -----------------------------------------------------------
data_with_august = pdata.frame(bike, index = c("station", "date")) # Alt.: bike
summary(plm.final_with_august <- plm(log(count) ~ wind_speed + 
                           precipitation + 
                           snowfall + 
                           snow_depth + 
                           mean_temperature +
                           I(mean_temperature^2) + 
                           as.factor(month) +
                           as.factor(weekday) +
                           IsFreezing +
                           log(adj_lagged_count_by_week),
                           data_with_august, model = "within", effect = "individual"))
# Compare both models:
summary(plm.final)
summary(plm.final_with_august)
df <- as.data.frame(cbind(no_august = fixef(plm.final), with_august = fixef(plm.final_with_august)))
df <- df %>% mutate(diff = no_august - with_august, abs_diff = abs(diff))
View(round(df, 3))

# Check new model's assumptions -------------------------------------------
residuals <- resid(plm.final_with_august)
qqnorm(residuals, main = "QQ Plot of Log-Normal Regression Residuals")
qqline(residuals, col = "blue", lwd = 2)  # Adds a reference line

library(lmtest)
coeftest(plm.final_with_august, vcov = vcovHC(plm.final_with_august, method = "arellano"))
coeftest(plm.final_with_august, vcov = vcovHC(plm.final_with_august, method = "white2", type = "HC3"))

# Check for FE.4 for 9 stations and all variables: ----
foo <- bike_no_august %>% group_by(station) %>% summarize(mean = mean(count, na.rm = T),
                                                          median = median(count, na.rm = T)) %>% arrange(mean) %>% 
  slice(c(1:3, 49:51, 98:100)) %>% select(station) %>% as.vector()

merged <- as.data.frame(cbind(resid(plm.final_with_august), fitted(plm.final_with_august)))
names(merged) <- c("resid.plm.final_with_august.", "fitted")

merged <- inner_join(data %>% rownames_to_column("rowname"),
                     merged %>% rownames_to_column("rowname"), by = "rowname") %>%
  column_to_rownames("rowname")


# First, with the highlighted summer holiday observations
par(mfrow = c(3, 3))
# Wind_speed:
for (i in 1:9){
  
  foo2 <- merged %>% filter(station == foo$station[i]) %>% select(wind_speed, resid.plm.final_with_august.)
  station_name <- paste("Station", i)
  
  x <- foo2$wind_speed[1:length(foo2[,1])]
  y <- foo2$resid.plm.final_with_august.[1:length(foo2$resid.plm.final_with_august.)]
  
  plot(x, y, xlab = names(foo2)[1], ylab = "Residuals",
       main = station_name, bty = "n", pch = 16, col = alpha("black", 0.6))
  abline(a = 0, b = 0, col = "red", lwd = 1.5) # a = Intercept, b = Slope
  kde <- ksmooth(x, y, kernel = "normal", bandwidth = 3)
  lines(kde$x, kde$y, col = "blue", lwd = 2)
}
par(mfrow = c(1, 1))

# precipitation:
par(mfrow = c(3, 3))
for (i in 1:9){
  
  foo2 <- merged %>% filter(station == foo$station[i]) %>% select(precipitation, resid.plm.final_with_august.)
  station_name <- paste("Station", i)
  
  x <- foo2[,1][1:length(foo2[,1])]
  y <- foo2$resid.plm.final_with_august.[1:length(foo2$resid.plm.final_with_august.)]
  
  plot(x, y, xlab = names(foo2)[1], ylab = "Residuals",
       main = station_name, bty = "n", pch = 16, col = alpha("black", 0.6))
  abline(a = 0, b = 0, col = "red", lwd = 1.5) # a = Intercept, b = Slope
  kde <- ksmooth(x, y, kernel = "normal", bandwidth = 3)
  lines(kde$x, kde$y, col = "blue", lwd = 2)
}
par(mfrow = c(1, 1))

# snowfall:
par(mfrow = c(3, 3))
for (i in 1:9){
  
  foo2 <- merged %>% filter(station == foo$station[i]) %>% select(snowfall, resid.plm.final_with_august.)
  station_name <- paste("Station", i)
  
  x <- foo2[,1][1:length(foo2[,1])]
  y <- foo2$resid.plm.final_with_august.[1:length(foo2$resid.plm.final_with_august.)]
  
  plot(x, y, xlab = names(foo2)[1], ylab = "Residuals",
       main = station_name, bty = "n", pch = 16, col = alpha("black", 0.6))
  abline(a = 0, b = 0, col = "red", lwd = 1.5) # a = Intercept, b = Slope
  kde <- ksmooth(x, y, kernel = "normal", bandwidth = 1)
  lines(kde$x, kde$y, col = "blue", lwd = 2)
}
par(mfrow = c(1, 1))

# Snowdepth:
par(mfrow = c(3, 3))
for (i in 1:9){
  
  foo2 <- merged %>% filter(station == foo$station[i]) %>% select(snow_depth, resid.plm.final_with_august.)
  station_name <- paste("Station", i)
  
  x <- foo2[,1][1:length(foo2[,1])]
  y <- foo2$resid.plm.final_with_august.[1:length(foo2$resid.plm.final_with_august.)]
  
  plot(x, y, xlab = names(foo2)[1], ylab = "Residuals",
       main = station_name, bty = "n", pch = 16, col = alpha("black", 0.6))
  abline(a = 0, b = 0, col = "red", lwd = 1.5) # a = Intercept, b = Slope
  kde <- ksmooth(x, y, kernel = "normal", bandwidth = 2)
  lines(kde$x, kde$y, col = "blue", lwd = 2)
}
par(mfrow = c(1, 1))

# Mean_temperature:
par(mfrow = c(3, 3))
for (i in 1:9){
  
  foo2 <- merged %>% filter(station == foo$station[i]) %>% select(mean_temperature, resid.plm.final_with_august.)
  station_name <- paste("Station", i)
  
  x <- foo2[,1][1:length(foo2[,1])]
  y <- foo2$resid.plm.final_with_august.[1:length(foo2$resid.plm.final_with_august.)]
  
  plot(x, y, xlab = names(foo2)[1], ylab = "Residuals",
       main = station_name, bty = "n", pch = 16, col = alpha("black", 0.6))
  abline(a = 0, b = 0, col = "red", lwd = 1.5) # a = Intercept, b = Slope
  kde <- ksmooth(x, y, kernel = "normal", bandwidth = 4)
  lines(kde$x, kde$y, col = "blue", lwd = 2)
}
par(mfrow = c(1, 1))

bike %>% filter(station == foo$station[7]) %>% filter(mean_temperature < 42) %>% pull(date)
# --> Mean_temperature is not modelled well enough in these months/on these days

# months:
par(mfrow = c(3, 3))
for (i in 1:9){
  
  foo2 <- merged %>% filter(station == foo$station[i]) %>% select(month, resid.plm.final_with_august.)
  station_name <- paste("Station", i)
  
  x <- foo2[,1][1:length(foo2[,1])]
  y <- foo2$resid.plm.final_with_august.[1:length(foo2$resid.plm.final_with_august.)]
  
  plot(x, y, xlab = names(foo2)[1], ylab = "Residuals", xlim = c(1, 11), xaxt = "n",
       main = station_name, bty = "n", pch = 16, col = alpha("black", 0.6))
  axis(1, at = seq(1, 11, by = 1))
  abline(a = 0, b = 0, col = "red", lwd = 1.5) # a = Intercept, b = Slope
  monthly_means <- foo2 %>% group_by(month) %>% summarize(mean = mean(resid.plm.final_with_august.))
  lines(monthly_means$month, monthly_means$mean, lwd = 1.5, col = alpha("darkgreen", 1))
  points(monthly_means$month, monthly_means$mean, bty = "n", pch = 16, col = alpha("red", 1))
  
}
par(mfrow = c(1, 1))

# Weekdays:
par(mfrow = c(3, 3))
for (i in 1:9){
  
  foo2 <- merged %>% filter(station == foo$station[i]) %>%
    mutate(weekday = as.factor(weekday)) %>% 
    mutate(weekday = fct_relabel(weekday, ~ substr(.x, 1, 2))) %>%
    mutate(weekday = fct_relevel(weekday, "Mo", "Di", "Mi", "Do", "Fr", "Sa", "So")) %>%
    select(weekday, resid.plm.final_with_august.)
  
  station_name <- paste("Station", i)
  
  x <- foo2[,1][1:length(foo2[,1])]
  y <- foo2$resid.plm.final_with_august.[1:length(foo2$resid.plm.final_with_august.)]
  
  plot(x, y, xlab = names(foo2)[1], ylab = "Residuals",
       main = station_name, bty = "n", pch = 16, col = alpha("black", 0.6))
  abline(a = 0, b = 0, col = "red", lwd = 1.5) # a = Intercept, b = Slope
  weekly_means <- foo2 %>% group_by(weekday) %>% summarize(mean = mean(resid.plm.final_with_august.))
  lines(weekly_means$weekday, weekly_means$mean, lwd = 1.5, col = alpha("blue", 1))
  points(weekly_means$weekday, weekly_means$mean, bty = "n", pch = 16, col = alpha("red", 1))
  
}
par(mfrow = c(1, 1))

# IsFreezing:
par(mfrow = c(3, 3))
for (i in 1:9){
  
  foo2 <- merged %>% filter(station == foo$station[i]) %>%
    select(IsFreezing, resid.plm.final_with_august.)
  
  station_name <- paste("Station", i)
  
  x <- foo2[,1][1:length(foo2[,1])]
  y <- foo2$resid.plm.final_with_august.[1:length(foo2$resid.plm.final_with_august.)]
  
  plot(x, y, xlab = names(foo2)[1], ylab = "Residuals", xlim = c(0, 1), xaxt = "n",
       main = station_name, bty = "n", pch = 16, col = alpha("black", 0.6))
  axis(1, at = seq(0, 1, by = 1))
  abline(a = 0, b = 0, col = "red", lwd = 1.5) # a = Intercept, b = Slope
  freezing_means <- foo2 %>% group_by(IsFreezing) %>% summarize(mean = mean(resid.plm.final_with_august.))
  points(freezing_means$IsFreezing, freezing_means$mean, bty = "n", pch = 16, col = alpha("red", 1))
  
}
par(mfrow = c(1, 1))

# Check for FE.5 (heteroscedasticity) ----
par(mfrow = c(3, 3))
for (i in 1:9){
  
  foo2 <- merged %>% filter(station == foo$station[i]) %>%
    select(fitted, resid.plm.final_with_august.)
  
  x <- foo2[,1][1:length(foo2[,1])] # fitted
  y <- foo2[,2][1:length(foo2[,2])] # resids
  
  station_name <- paste("Station", i)
  
  plot(x, exp(y), xlab = names(foo2)[1], ylab = "Residuals",
       main = station_name, bty = "n", pch = 16, col = alpha("black", 0.6))
  abline(a = 1, b = 0, col = "red", lwd = 1.5) # a = Intercept, b = Slope
  kde <- ksmooth(x, exp(y), kernel = "normal", bandwidth = 1)
  lines(kde$x, kde$y, col = "blue", lwd = 2)
}
par(mfrow = c(1, 1))


# Check for FE.6 (autocorrelation) ----

par(mfrow = c(3, 3))
for (i in 1:9){
  
  foo2 <- merged %>% filter(station == foo$station[i]) %>%
    select(obs_count, resid.plm.final_with_august.)
  station_name <- paste("Station", i)
  
  x <- foo2$obs_count[1:length(foo2$obs_count)]
  y <- foo2$resid.plm.final_with_august.[1:length(foo2$resid.plm.final_with_august.)]
  
  plot(x, y, xlab = names(foo2)[1], ylab = "Residuals", xlim = c(1, 334), xaxt = "n",
       main = station_name, bty = "n", pch = 16, col = alpha("black", 0.6))
  axis(1, at = c(1, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334))
  abline(v = c(31.5, 59.5, 90.5, 120.5, 151.5, 
               181.5, 212.5, 243.5, 273.5, 304.5),
         col=alpha("black", 0.3), lwd=0.5, lty='dashed')
  
  abline(a = 0, b = 0, col = "red", lwd = 1.5) # a = Intercept, b = Slope
  ma1 = stats::filter(foo2$resid.plm.final_with_august.,
                      filter = rep(1/7, 7), method = 'convolution', sides = 2)
  ma2 = stats::filter(foo2$resid.plm.final_with_august.,
                      filter = rep(1/30, 30), method = 'convolution', sides = 2)
  lines(ma2, lty = 2, lwd = 2, col = alpha('blue', 0.9))                  
  lines(lowess(foo2$resid.plm.final_with_august.), col='purple', lwd = 2)
}
par(mfrow = c(1, 1))

# Perform test for autocorrelation of errors - Wooldridge Test for AR(1) Errors
# in FE --> If significant (alt. hypo.), then FE.6 does not hold
pwartest(plm.final_with_august)
u_hat <- resid(plm.final_with_august)
summary(lm <- lm(u_hat ~ lag(u_hat, 1)))
(delta = -1/(334-1))
(delta-coef(lm)[2])/sqrt(diag(vcov(lm)))[2]
qt(0.975, 29390) # critical val 

# Breusch-Godfrey/Wooldridge test of serial correlation for (the idiosyncratic 
# component of) the errors in panel models
pbgtest(plm.final_with_august) # order = NULL (def) uses the min number of obs over the time dim

# Save results:
bike <- bike %>% select(-JanuarySlope, -FebruarySlope, -MarchSlope, -NovemberSlope)
write.csv(bike, "C:/Users/marce/Documents/PraktikumProjekt_Bauer/final_imputed_bike_v2.csv", row.names = FALSE)

# Add in station FE (original and rescaled) to Coordinate DF
coordinate_df <- read.csv("C:/Users/marce/Documents/PraktikumProjekt_Bauer/coords.csv")
coordinate_df <- coordinate_df %>% select(-X)
foo <- fixef(plm.final_with_august) %>% as.data.frame() %>% 
  rownames_to_column(var = "station") %>% rename("FE" = ".")
coordinate_df <- left_join(coordinate_df, foo, by = "station")
scaled_FE <- coordinate_df %>% select(FE) %>% unlist() %>% as.vector()
scaled_FE <- scaled_FE - min(scaled_FE)
scaled_FE <- scaled_FE * 4/max(scaled_FE)
coordinate_df <- coordinate_df %>% mutate(scaled_FE,
                                          scaled_FE_layered = cut(scaled_FE, breaks = c(0, 1, 2, 3, 4.1), 
                                                                      labels = 1:4, 
                                                                      right = FALSE))
coordinate_df <- coordinate_df %>% mutate(round.FE = round(FE, 3),
                                          round.scaled_FE = round(scaled_FE, 3))
# write.csv(coordinate_df, "C:/Users/marce/Documents/PraktikumProjekt_Bauer/coords.csv", row.names = F)

