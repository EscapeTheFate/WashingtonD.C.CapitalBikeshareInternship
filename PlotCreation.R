
# Load packages: ----------------------------------------------------------
library(tidyverse)
library(dplyr)
library(plm)
library(fixest)
library(tidyr)
library(stringr)

# Load data ---------------------------------------------------------------
bike <- read.csv("C:/Users/marce/Documents/PraktikumProjekt_Bauer/final_imputed_bike_v2.csv")
bike_list <- bike %>% group_by(station) %>% group_split()

# Data inspection section -------------------------------------------------
# Count:

# Wind speed:
par(mfrow = c(1, 1))
ind2 <- c(180:260)
ind <- setdiff(1:334, ind2)
plot(y = bike_list[[2]]$wind_speed[ind], x = bike_list[[2]]$obs_count[ind], bty = "n", main = "",
     pch = 16, col = alpha("red", 0.6), xlab = "Days since first obs.", ylab = "Wind speed (mph)", xaxt = "n")
abline(h = mean(bike_list[[2]]$wind_speed), col='red', lwd=3, lty='dashed')      # mean
points(y = bike_list[[2]]$wind_speed[ind2], x = bike_list[[2]]$obs_count[ind2], bty = "n",
       pch = 16, col = alpha("blue", 0.6))
month_breaks <- c(1, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)
month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
abline(v = c(31.5, 59.5, 90.5, 120.5, 151.5, 
             181.5, 212.5, 243.5, 273.5, 304.5),
       col=alpha("black", 0.3), lwd=0.5, lty='dashed')
axis(1, at = month_breaks, labels = month_labels)
lines(lowess(bike_list[[2]] %>% select(wind_speed)), col='purple', lwd = 2) # smoothing trend
legend("topright", legend = c("Trend Smoothing"),  
       col = c("purple"), lty = 1, lwd = 2, cex = 1)

# Correlation of count vs. wind speed
bike_list[[2]] %>% mutate(index = ifelse(obs_count %in% c(180, 260), "inside", "outside")) %>% 
  drop_na() %>% group_by(index) %>% summarize(mean(count), var(count))
bike_list[[2]] %>% mutate(index = case_when(obs_count < 180 ~ "before",
                                            obs_count >= 180 & obs_count <= 260 ~ "inside", TRUE ~ "after")) %>% select(count, index) %>%
  drop_na() %>% group_by(index) %>% summarize(mean(count), var(count))
# --> There appears to be some differences in the bike count attributed to wind_speeds for station 2

# What about all stations?
bike %>% mutate(index = ifelse(obs_count %in% c(180, 260), "inside", "outside")) %>% 
  drop_na() %>% group_by(index) %>% summarize(mean(count), var(count))
bike %>% mutate(index = case_when(obs_count < 180 ~ "before",
                                  obs_count >= 180 & obs_count <= 260 ~ "inside", TRUE ~ "after")) %>% select(count, index) %>%
  drop_na() %>% group_by(index) %>% summarize(mean(count), var(count))

par(mfrow = c(1, 1))
foo <- c(2)
for (i in 1:length(foo)){
  foo2 <- bike_list[[foo[i]]] %>% select(count, wind_speed) %>% drop_na()
  plot(y = foo2$count[ind], x = foo2$wind_speed[ind], bty = "n", pch = 16, col = alpha("red", 0.6),
       xlab = "Wind speed (mph)", ylab = "Bike rental count", xlim = c(2,20))
  points(y = bike_list[[2]]$count[ind2], x = bike_list[[2]]$wind_speed[ind2], bty = "n",
         pch = 16, col = alpha("blue", 0.6))
  
  kde <- ksmooth(x = foo2$wind_speed, y = foo2$count, kernel = "normal", bandwidth = 5)
  lines(kde$x, kde$y, col = "darkorange", lwd = 2)
  legend("topright", legend = c("KDE"),  
         col = c("darkorange"), lty = 1, lwd = 2, cex = 1)
}
par(mfrow = c(1, 1))
rm(foo, foo2, ma1, ma2, i)

# Precipitation:
summary(bike_list[[2]]$precipitation) 
# It does not rain often, but there is an extreme outlier. 
which(bike_list[[2]]$precipitation >= 2) # Obs. 190 is an outlier with a value of 4
# How do the values look like, and how often is there any rain?
bike_list[[2]] %>% mutate(ind = ifelse(precipitation > 0, 1, 0)) %>% count(ind) %>% mutate(probs = (n/334)*100)
# Different rain ranges:
bike_list[[2]] %>%
  mutate(intensity = case_when(
    precipitation == 0 ~ "non",
    precipitation > 0 & precipitation <= 0.10 ~ "light",       # Light rain: <= 0.10 in/hr
    precipitation > 0.10 & precipitation <= 0.30 ~ "moderate", # Moderate rain: 0.11 to 0.30 in/hr
    precipitation > 0.30 ~ "heavy"                             # Heavy rain: > 0.30 in/hr
  )) %>%
  count(intensity) %>%
  mutate(probs = (n/nrow(bike_list[[2]]))*100)
# What about the mean count?
bike_list[[2]] %>%
  mutate(intensity = case_when(
    precipitation == 0 ~ "non",
    precipitation > 0 & precipitation <= 0.10 ~ "light",       # Light rain: <= 0.10 in/hr
    precipitation > 0.10 & precipitation <= 0.30 ~ "moderate", # Moderate rain: 0.11 to 0.30 in/hr
    precipitation > 0.30 ~ "heavy"                             # Heavy rain: > 0.30 in/hr
  )) %>% drop_na() %>%
  group_by(intensity) %>%
  summarize(mean(count))
bike %>%
  mutate(intensity = case_when(
    precipitation == 0 ~ "non",
    precipitation > 0 & precipitation <= 0.10 ~ "light",       # Light rain: <= 0.10 in/hr
    precipitation > 0.10 & precipitation <= 0.30 ~ "moderate", # Moderate rain: 0.11 to 0.30 in/hr
    precipitation > 0.30 ~ "heavy"                             # Heavy rain: > 0.30 in/hr
  )) %>% drop_na() %>%
  group_by(intensity) %>%
  summarize(mean(count))

bike_list[[2]]$date[190] # --> To be more precise, the 9th July 2022 is an outlier

par(mfrow = c(1, 1))
foo <- bike_list[[2]] %>%
  mutate(intensity = case_when(
    precipitation == 0 ~ "non",
    precipitation > 0 & precipitation <= 0.10 ~ "light",       # Light rain: <= 0.10 in/hr
    precipitation > 0.10 & precipitation <= 0.30 ~ "moderate", # Moderate rain: 0.11 to 0.30 in/hr
    precipitation > 0.30 ~ "heavy"                             # Heavy rain: > 0.30 in/hr
  ))

plot(y = bike_list[[2]]$precipitation, x = bike_list[[2]]$obs_count, bty = "n", xaxt = "n",
     pch = 16, col = alpha("red", 0), xlab = "Days since first obs.", ylab = "Precipitation in inches")
month_breaks <- c(1, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)
month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
abline(v = c(31.5, 59.5, 90.5, 120.5, 151.5, 
             181.5, 212.5, 243.5, 273.5, 304.5),
       col=alpha("black", 0.3), lwd=0.5, lty='dashed')
axis(1, at = month_breaks, labels = month_labels)
category = c("non", "light", "moderate", "heavy")
col = c("gray50", "skyblue", "deepskyblue", "darkblue")
for(i in 1:4){
  ind <- which(foo$intensity == category[i])
  y <- foo$precipitation[ind]
  x <- foo$obs_count[ind]
  points(x, y, bty = "n", pch = 16, col = alpha(col[i], 1))
}

par(mfrow = c(1, 1))
foo <- c(2)
for (i in 1:length(foo)){
  foo2 <- bike_list[[foo[i]]] %>% select(count, precipitation) %>% drop_na()
  plot(y = foo2$count, x = foo2$precipitation, bty = "n", pch = 16, col = alpha("darkblue", 0.6),
       xlab = "Precipitation in inches", ylab = "Bike rental count")
  
  kde <- ksmooth(x = foo2$precipitation, y = foo2$count, kernel = "normal", bandwidth = 2)
  lines(kde$x, kde$y, col = "darkorange", lwd = 2)
  legend("topright", legend = c("KDE"),  
         col = c("darkorange"), lty = 1, lwd = 2, cex = 1)
}

par(mfrow = c(1, 1))
foo <- c(2)
for (i in 1:length(foo)){
  foo2 <- bike_list[[foo[i]]] %>% select(count, precipitation) %>% filter(precipitation > 0) %>% drop_na()
  plot(y = foo2$count, x = foo2$precipitation, bty = "n", pch = 16, col = alpha("black", 0.4),
       xlab = "Precipitation in inches", ylab = "Bike rental count")
  
  kde <- ksmooth(x = foo2$precipitation, y = foo2$count, kernel = "normal", bandwidth = 2)
  lines(kde$x, kde$y, col = "darkorange", lwd = 2)
  legend("topright", legend = c("KDE"),  
         col = c("darkorange"), lty = 1, lwd = 2, cex = 1)
}
# Snowfall & Snow depth:
bike_list[[2]] %>% mutate(ind = ifelse(snowfall > 0, 1, 0)) %>% count(ind) %>% mutate(probs = n/334)
# --> It only snowed on 5 days, so we can't really look at plots here
# Calculate mean count for each snowfall day and no snowfall days over all stations:
bike %>% filter(snowfall > 0) %>% group_by(obs_count) %>% select(-adj_lagged_count_by_week) %>% drop_na() %>%
  summarize(mean(count), snow = first(snowfall), snow_depth = first(snow_depth)) %>% 
  arrange(desc(snow))
bike %>% filter(snowfall == 0, snow_depth == 0, month == 1) %>% drop_na() %>% summarize(mean(count))

bike %>% filter(obs_count %in% c(3,4,5,6,7,8,16,17,28,71)) %>% group_by(obs_count) %>% select(-adj_lagged_count_by_week) %>% drop_na() %>%
  summarize(snow = first(snowfall), snow_depth = first(snow_depth), mean(count)) %>% 
  arrange(obs_count)
# Compare this to the mean count when its not snowing/no snow lying around:
bike %>% filter(snowfall == 0, snow_depth == 0, month == 1) %>% drop_na() %>% summarize(mean(count))

# Mean temperature:
par(mfrow = c(1, 1))
ind2 <- c(180:260)
ind <- setdiff(1:334, ind2)
plot(y = bike_list[[2]]$mean_temperature[ind], x = bike_list[[2]]$obs_count[ind], bty = "n", xaxt = "n",
     pch = 16, col = alpha("red", 0.5), xlab = "Days since first obs.", ylab = "Mean temperature (°F)")
abline(h = mean(bike_list[[2]]$mean_temperature), col='red', lwd=3, lty='dashed')      # mean
points(y = bike_list[[2]]$mean_temperature[ind2], x = bike_list[[2]]$obs_count[ind2], bty = "n",
       pch = 16, col = alpha("blue", 0.5))
month_breaks <- c(1, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)
month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
abline(v = c(31.5, 59.5, 90.5, 120.5, 151.5, 
             181.5, 212.5, 243.5, 273.5, 304.5),
       col=alpha("black", 0.3), lwd=0.5, lty='dashed')
axis(1, at = month_breaks, labels = month_labels)
lines(lowess(bike_list[[2]] %>% select(mean_temperature)), col='purple', lwd = 2) # smoothing trend
legend("bottomright", legend = c("Trend Smoothing"),  
       col = c("purple"), lty = 1, lwd = 2, cex = 1)

par(mfrow = c(1, 1))
foo <- c(2)
for (i in 1:9){
  foo2 <- bike_list[[foo[i]]] %>% select(count, mean_temperature) %>% drop_na()
  plot(y = foo2$count[ind], x = foo2$mean_temperature[ind], bty = "n", pch = 16, col = alpha("red", 0.5),
       xlab = "Mean temperature (°F)", ylab = "Bike rental count")
  points(y = foo2$count[ind2], x = foo2$mean_temperature[ind2], bty = "n",
         pch = 16, col = alpha("blue", 0.5))
  kde <- ksmooth(x = foo2$mean_temperature, y = foo2$count, kernel = "normal", bandwidth = 20)
  lines(kde$x, kde$y, col = "darkorange", lwd = 2)
  legend("bottomright", legend = c("KDE"),  
         col = c("darkorange"), lty = 1, lwd = 2, cex = 1)
}
par(mfrow = c(1, 1))
rm(foo, foo2, ma1, ma2, i)

# Imputation results

# Lets take a look at the lowest, the  highest and somewhere inbetween (by mean)
foo <- bike %>% group_by(station) %>% summarize(mean = mean(count, na.rm = T)) %>% arrange(desc(mean)) %>% 
  slice(seq(from = 1, to = 100, length.out = 4)) %>% select(station) %>% as.vector()
rank <- c(seq(from = 1, to = 100, length.out = 4))
#count_y <- c(450, 330, 350, 220, 160, 140, 140, 120, 130)
par(mfrow = c(2, 2))
for (i in 1:length(foo$station)){
  foo2 <- bike %>% filter(station == foo$station[i]) %>% select(count, obs_count, month) %>% drop_na()
  station_name <- paste(foo$station[i],paste0("(Rank:", rank[i], ")"))
  plot(y = foo2$count[which(foo2$month != 8)], x = foo2$obs_count[which(foo2$month != 8)], bty = "n", pch = 16, col = alpha("black", 0.5),
       xlab = "", ylab = "Counts", main = station_name, xaxt = "n") # ylim = c(0,count_y[i])
  month_breaks <- c(1, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)
  month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  abline(v = c(31.5, 59.5, 90.5, 120.5, 151.5, 
               181.5, 212.5, 243.5, 273.5, 304.5),
         col=alpha("black", 0.3), lwd=0.5, lty='dashed')
  axis(1, at = month_breaks, labels = month_labels)
  points(y = foo2$count[which(foo2$month == 8)], x = foo2$obs_count[which(foo2$month == 8)], bty = "n", pch = 16, col = alpha("red", 0.5))
}
par(mfrow = c(1, 1))

# August anomaly
bike_anomaly <- read.csv(file = "C:/Users/marce/Documents/PraktikumProjekt_Bauer/final_bike.csv",
                 header = T)
rank <- c(seq(from = 1, to = 100, length.out = 4))
count_y <- c(450, 280, 175, 130)
par(mfrow = c(2, 2))
for (i in 1:length(foo$station)){
  foo2 <- bike_anomaly %>% filter(station == foo$station[i]) %>% select(count, obs_count, month) %>% drop_na()
  station_name <- paste(foo$station[i],paste0("(Rank:", rank[i], ")"))
  plot(y = foo2$count[which(foo2$month != 8)], x = foo2$obs_count[which(foo2$month != 8)], bty = "n", pch = 16, col = alpha("black", 0.5),
       xlab = "", ylab = "Counts", main = station_name, xaxt = "n", ylim = c(0,count_y[i]))
  month_breaks <- c(1, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334)
  month_labels <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  abline(v = c(31.5, 59.5, 90.5, 120.5, 151.5, 
               181.5, 212.5, 243.5, 273.5, 304.5),
         col=alpha("black", 0.3), lwd=0.5, lty='dashed')
  axis(1, at = month_breaks, labels = month_labels)
  points(y = foo2$count[which(foo2$month == 8)], x = foo2$obs_count[which(foo2$month == 8)], bty = "n", pch = 16, col = alpha("red", 0.5))
}
par(mfrow = c(1, 1))


