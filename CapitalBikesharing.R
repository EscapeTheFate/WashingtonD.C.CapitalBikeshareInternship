
# Project: Capital Bikesharing internship
# Name: Marcel Gumulak
# Due Date: 24.12.2024
# Goals: 1. Bike rental modelling by using weather data in Washington D.C. in 2022
#        2. Hot Zone Visualisation

# 1.1 Read in Data --------------------------------------------------------
bike <- read.csv(file = "C:/Users/marce/Documents/PraktikumProjekt_Bauer/Capital_bikeshare_data_2022_with_NAs.csv",
                    header = T)

# 1.2 Load in necessary Libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(fixest)
library(plm)
library(pglm)

# 2. Explorative Data Analysis -----------------------------------------------

# Preliminary
names(bike) # Variables
length(unique(bike$station)) # 100 different bike stations in sample
head(bike, 5)
summary(bike)
# Some irregularities that can be spotted here:
# --> 300 NAs on 'count'; 301 NAs on 'mean_temperature'
# --> Windspeed has a negative value?


# Is the Panel balanced or unbalanced? ------------------------------------
bike_list <- bike %>% group_by(station) %>% group_split()
View(bike_list) # The Panel is not balanced for 2 reasons. 
# Firstly, while most of the stations are in fact complete (60%), a considerable 
# proportion has some day observations missing. Secondly, the Station labelled as
# Nr. 36 only consists of 191 day observations, which hints at the Station being
# the most recent addition. We have to take deeper look at this
group_rows <- bike %>% group_by(station) %>% count()
table(group_rows$n) # total
table(group_rows$n)/sum(table(group_rows$n)) # percentages
# 60% are complete, 35% lack 1-3 days, 4% lack 4-8 days, and 1% lack 143 days
# ---> Why is that so and what days are specifically missing in comparison to others?
which(group_rows$n < 334) # These are missing some days 
# --> Nr. 1 misses days, Nr. 2 does not, so let's compare them
which(!(bike_list[[2]]$date %in% bike_list[[1]]$date)) # day observation 44 for station 1 is missing 

# Do they skip a day or what is happening?
bike_list[[1]][43:45,] # misses 1 obs --> They actually skip the 13th Feb here
bike_list[[2]][43:45,] # benchmark as full obs

# What about one of the two stations with only 326 obs
which(group_rows$n == 326) # Station Nr. 59 & 92 --> Let's look at 59
which(!(bike_list[[2]]$date %in% bike_list[[59]]$date)) # day observation that are missing
bike_list[[59]][180:183,] # Missing more than a week at a time, and days are consecutive

# What about the station with only 191 observations
which(group_rows$n == 191) # Station Nr. 36
which(!(bike_list[[2]]$date %in% bike_list[[36]]$date)) # day observation that are missing
length(which(!(bike_list[[2]]$date %in% bike_list[[36]]$date))) # 143 consecutive missing obs
bike_list[[36]][1,] # The first observation is on the 24th May up until final day

# Ok, now that were done with this, let's transform the date variable, add in
# an obs. day count, month count and a day count in each month:
bike <- bike %>% mutate(date = ymd(date), month = month(date), day = day(date),
                        weekday = weekdays(date))
foo <- bike %>% select(date) %>% distinct() %>% mutate(obs_count = row_number())
# Perform left join to add obs_count
bike <- left_join(bike, foo, by = "date")
rm(foo)

# What about the weather data? --------------------------------------------
# The weather data has an irregularity of -1 in the wind_speeds. Furthermore,
# the data appears to only consist of day measurements that apply for all 
# stations on that day. We will check this, and it this holds, we can solve 
# the NA problem on the mean temperature, as well as fix the wind data!
check_station_consistency <- function(df, variables) {
  inconsistent_days <- list()
  
  for (col in variables) {
    # Group by date and check if all stations have the same weather value for that date
    # Exception here is mean_temperature, which would just highlight the NAs
    inconsistent <- df %>%
      group_by(date) %>%
      summarise(unique_vals = n_distinct(!!sym(col))) %>% 
      filter(unique_vals > 1) %>%
      pull(date)
    
    inconsistent_days[[col]] <- inconsistent
  }
  return(inconsistent_days)
}

check_station_consistency(bike, names(bike)[c(4:10)])
# --> Only wind_speed seems to be wrong sometimes, and the value should be 
#     exactly equal to the other values, so we overwrite these later.

# Now, for 'mean_temperature' specifically:
bike %>% select(-count) %>% drop_na() %>% group_by(date) %>%
  summarise(unique_vals = n_distinct(!!sym("mean_temperature"))) %>% 
  filter(unique_vals > 1) %>% pull(date)
# --> Each mean temperature value for the respective day is the same for each
#     station. Hence, we can overwrite these NAs here, which is quite nice!
#     Furthermore, we could theoretically add in the missing days for some of
#     these stations, where we leave out the count variable (NA), but there is
#     no real benefit to that (for now) as far as I can see.

# Update dataset in two steps for adjusted wind_speed and mean_temperature
bike <- bike %>% mutate(wind_speed = ifelse(wind_speed == -1, NA, wind_speed)) %>%  # Replace -1 with NA
  group_by(date) %>%
  mutate(wind_speed = ifelse(is.na(wind_speed), first(na.omit(wind_speed)), wind_speed)) %>%  # Fill NAs
  ungroup()
bike <- bike %>% group_by(date) %>% 
  mutate(mean_temperature = ifelse(is.na(mean_temperature), first(na.omit(mean_temperature)), mean_temperature)) %>%
  ungroup()
bike_list <- bike %>% group_by(station) %>% group_split() # Also update each station df
# Check if everything is correct now (i.e. "2022-01-05" had 2 NAs in 
# mean_temperature, and "2022-04-02" had -1 wind_speed)
check_station_consistency(bike, "wind_speed") # first, there are no other divergent wind_speed values other than -1
bike %>% filter(date == "2022-04-02") %>% select(wind_speed) %>% distinct() # Done
bike %>% filter(date == "2022-01-05") %>% select(mean_temperature) %>% distinct() # Done
bike_list <- bike %>% group_by(station) %>% group_split()

# Inspection of dataframe variables ---------------------------------------
names(bike) # Since the weather data is independent of the stations, we can
# simply inspect these variables for any station with full day obs. count (i.e. Station 2)
## For wind_speed ----
summary(bike_list[[2]]$wind_speed)
par(mfrow = c(1, 2))
hist(bike_list[[2]]$wind_speed, breaks = c(seq(from = 2, to = 20, by = 1)),
     main = "Overall Wind speeds (b = 1)",
     xlab = "wind speed in miles per hour",
     col="peachpuff")
abline(v = mean(bike_list[[2]]$wind_speed), col='red', lwd=3, lty='dashed')
# --> Right-skewed distribution
plot(y = bike_list[[2]]$wind_speed, x = bike_list[[2]]$obs_count, bty = "n",
     pch = 16, col = alpha("red", 0.3), xlab = "Days since first obs.", ylab = "Wind speed (mph)", xaxt = "n")
abline(h = mean(bike_list[[2]]$wind_speed), col='red', lwd=3, lty='dashed')      # mean
segments(x0 = 260, y0 = 4, x1 = 260, y1 = 12, col = alpha("blue", 0.4), lwd = 2) # highlight zone
segments(x0 = 180, y0 = 4, x1 = 180, y1 = 12, col = alpha("blue", 0.4), lwd = 2) # highlight zone
bike_list[[2]]$date[c(180,260)]
par(mfrow = c(1, 1))
# Calculate a simple moving average with a window size of 7 and 30
plot(y = bike_list[[2]]$wind_speed, x = bike_list[[2]]$obs_count, bty = "n",
     pch = 16, col = alpha("red", 0.3), xlab = "Days since first obs.", ylab = "Wind speed (mph)")
abline(h = mean(bike_list[[2]]$wind_speed), col='red', lwd=3, lty='dashed')      # mean
segments(x0 = 260, y0 = 4, x1 = 260, y1 = 12, col = alpha("blue", 0.4), lwd = 2) # highlight zone
segments(x0 = 180, y0 = 4, x1 = 180, y1 = 12, col = alpha("blue", 0.4), lwd = 2) # highlight zones
ma1 = stats::filter(bike_list[[2]] %>% select(wind_speed),
                         filter = rep(1/7, 7), method = 'convolution', sides = 2)
ma2 = stats::filter(bike_list[[2]] %>% select(wind_speed),
                         filter = rep(1/30, 30), method = 'convolution', sides = 2)
lines(ma1, lty = 2, lwd = 2, col = alpha('darkgreen', 0.3))                 # moving average (week)
lines(ma2, lty = 2, lwd = 2, col = alpha('blue', 0.6))                      # moving average (month)
lines(lowess(bike_list[[2]] %>% select(wind_speed)), col='purple', lwd = 2) # smoothing trend
legend("topright", legend = c("SMA(7)", "SMA(30)", "Trend smooth"),  
       col = c("darkgreen", "blue", "purple"), lty = 1, cex = 0.6)
# --> So, there is clearly less variance in wind speeds between early July 
# until mid September, and there seems to be general fluctuation within a year

# Let's compare counts for the station within vs. outside the highlighted interval,
# by 2 intervals and by additional comparison of before and after 180-260 interval:
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
# --> Identical results, so Wind_speed seems to influence bike rental count
# For some Stations, this can also be illustrated in the following plots:
par(mfrow = c(3, 3))
foo <- c(round(seq(from = 1, to = 100, length.out = 9)))
for (i in 1:9){
  foo2 <- bike_list[[foo[i]]] %>% select(count, wind_speed) %>% drop_na()
  plot(y = foo2$count, x = foo2$wind_speed, bty = "n", pch = 16, col = alpha("black", 0.6),
       xlab = "Wind speed (mph)", ylab = "Bike rental count")
}
par(mfrow = c(1, 1))
rm(foo, foo2, ma1, ma2, i)

# Correlation for all stations, only stations 2 and over all station
cor(bike$count,bike$wind_speed, use = "pairwise.complete.obs") # Over all
cor(bike_list[[2]]$count,bike_list[[2]]$wind_speed, use = "pairwise.complete.obs") # Station 2
foo <- bike %>% group_by(station) %>% 
  summarize(cor = cor(count, wind_speed, use = "pairwise.complete.obs")) 
hist(foo$cor, breaks = c(seq(from = -0.4, to = -0.1, by = 0.02)),
     main = "Emp. distribution of Cor.", xlab = "Cor. of all stations")

# To summarize, wind speed influences in three ways: High wind speeds correlate 
# with less counts; low wind speeds are associated to increased count variance
# and higher counts; extremely low speeds are again associated to lower counts

## For precipitation ----
summary(bike_list[[2]]$precipitation) 
# It does not rain often, but there is an extreme outlier. 
# How do the values look like, and how often is there any rain?
bike_list[[2]] %>% mutate(ind = ifelse(precipitation > 0, 1, 0)) %>% count(ind) %>% mutate(probs = n/334)
which(bike_list[[2]]$precipitation >= 2) # Obs. 190 is an outlier with a value of 4
bike_list[[2]]$date[190] # --> To be more precise, the 9th July 2022 is an outlier

# Lets see if it has an effect on the count in general, and heavier rain
# First, even in the case of the slightest water particles
bike_list[[2]] %>% mutate(index = ifelse(precipitation > 0, 1, 0)) %>% 
  drop_na() %>% group_by(index) %>% summarize(mean(count), var(count))
bike %>% mutate(index = ifelse(precipitation > 0, 1, 0)) %>% 
  drop_na() %>% group_by(index) %>% summarize(mean(count), var(count))
# Second, if more heavier rain is present
which(bike_list[[2]]$precipitation > 1) # although small sample size
bike_list[[2]] %>% mutate(index = ifelse(precipitation > 1, 1, 0)) %>% 
  drop_na() %>% group_by(index) %>% summarize(mean(count), var(count))
bike %>% mutate(index = ifelse(precipitation > 1, 1, 0)) %>% 
  drop_na() %>% group_by(index) %>% summarize(mean(count), var(count))
# --> Effect seems slight, but becomes larger in the case of more rain

# Let's look at plots to see when it rains heavier and when not
par(mfrow = c(1, 2))
hist(bike_list[[2]]$precipitation,
     main = "Precipitation",
     xlab = "Precipitation in inches",
     col="peachpuff")
plot(y = bike_list[[2]]$precipitation, x = bike_list[[2]]$obs_count, bty = "n",
     pch = 16, col = alpha("red", 0.3), xlab = "Days since first obs.", ylab = "Precipitation in inches")
segments(x0 = 260, y0 = 4, x1 = 260, y1 = 0, col = alpha("blue", 0.4), lwd = 2) # highlight zone
segments(x0 = 180, y0 = 4, x1 = 180, y1 = 0, col = alpha("blue", 0.4), lwd = 2) # highlight zone
ma1 = stats::filter(bike_list[[2]] %>% select(precipitation),
                    filter = rep(1/7, 7), method = 'convolution', sides = 2)
ma2 = stats::filter(bike_list[[2]] %>% select(precipitation),
                    filter = rep(1/60, 60), method = 'convolution', sides = 2)
lines(ma2, lty = 2, lwd = 2, col = alpha('blue', 0.6))                      # moving average (month)
lines(lowess(bike_list[[2]] %>% select(precipitation)), col='purple', lwd = 2) # smoothing trend
legend("topright", legend = c( "SMA(30)", "Trend smooth"),  
       col = c( "blue", "purple"), lty = 1, cex = 0.6)
par(mfrow = c(1, 1))
# --> There does not seem to be much structure or info here otherwise...

par(mfrow = c(3, 3))
foo <- c(round(seq(from = 1, to = 100, length.out = 9)))
for (i in 1:9){
  foo2 <- bike_list[[foo[i]]] %>% select(count, precipitation) %>% filter(precipitation < 2) %>% drop_na()
  plot(y = foo2$count, x = foo2$precipitation, bty = "n", pch = 16, col = alpha("black", 0.6),
       xlab = "Precipitation in inches", ylab = "Bike rental count")
}
par(mfrow = c(1, 1))
rm(foo, foo2, i)
# --> Variance is very large at 0, and counts decrease slightly with increasing
#     precipitation, but the effect does not seem large; If the variable is 
#     actually singificant in the final model, the outlier should be removed

# Correlation for all stations, only station 2 and over all station
cor(bike$count,bike$precipitation, use = "pairwise.complete.obs") # Over all
cor(bike_list[[2]]$count,bike_list[[2]]$precipitation, use = "pairwise.complete.obs") # Station 2
foo <- bike %>% group_by(station) %>% 
  summarize(cor = cor(count, precipitation, use = "pairwise.complete.obs")) 
hist(foo$cor, main = "Emp. distribution of Cor.", xlab = "Cor. of all stations")

## For snowfall ----
summary(bike_list[[2]]$snowfall) 
# Similar to rain, it is not often snowing 
# How do the values look like, and how often is there any snow?
bike_list[[2]] %>% mutate(ind = ifelse(snowfall > 0, 1, 0)) %>% count(ind) %>% mutate(probs = n/334)
# --> It only snowed on 5 days, so we can't really look at plots here
# On which days, and how much?
which(bike_list[[2]]$snowfall > 0) # Obs. 3 7 16 28 71
bike_list[[2]]$snowfall[which(bike_list[[2]]$snowfall > 0)] # 6.9 2.6 2.6 0.2 0.9
# --> The 3rd January obs. is an outlier, although not very uncommon for that time

# Let's calculate the overall avg. count and emp. distribution of counts on all
# 5 days, but also do this in combination with snow_depth variable
bike %>% filter(snowfall > 0) %>% drop_na() %>% group_by(obs_count) %>% 
  summarize(mean(count), snow = first(snowfall), snow_depth = first(snow_depth)) %>% 
  arrange(desc(snow))
# Compare this to the mean count when its not snowing:
bike %>% filter(snowfall == 0) %>% drop_na() %>% summarize(mean(count))
# --> Counts decrease heavily with increasing snowfall, but the sample size is  
#     small and if it is included in the model, it should probably just be a dummy

# Correlation for all stations, only stations 2 and over all station
cor(bike$count,bike$snowfall, use = "pairwise.complete.obs") # Over all
cor(bike_list[[2]]$count,bike_list[[2]]$snowfall, use = "pairwise.complete.obs") # Station 2
# Correlation on all stations for the 5 observations
foo <- bike %>% filter(snowfall > 0) %>% group_by(station) %>% 
  summarize(cor = cor(count, snowfall, use = "pairwise.complete.obs")) 
hist(foo$cor, main = "Emp. distribution of Cor.", xlab = "Cor. of all stations")
# --> Correlation high in the few observations, in which it is snowing


## For snow_depth ----
summary(bike_list[[2]]$snow_depth) 
# Similar to rain and snow, snow lying around is not often the case
# How do the values look like, and how often is there any snow lying around?
bike_list[[2]] %>% mutate(ind = ifelse(snow_depth > 0, 1, 0)) %>% count(ind) %>% mutate(probs = n/334)
# --> Since it only snowed on 5 days, there are only 6 days where there is snow 
#     on the ground

# On which days, and how much?
which(bike_list[[2]]$snow_depth > 0) # Obs. 4 5 6 7 8 17, compared to snowing on 3 7 16 28 71
bike_list[[2]]$snow_depth[which(bike_list[[2]]$snow_depth > 0)] # 7.1 3.9 2.0 3.1 1.2 1.2
# --> The 3rd January obs. is again an outlier, although not very uncommon for that time

# Let's calculate the overall avg. count and emp. distribution of counts on all
# 6 days, but also do this in combination with snow_depth variable
bike %>% filter(snow_depth > 0) %>% drop_na() %>% group_by(obs_count) %>% 
  summarize(mean(count), snow = first(snowfall), snow_depth = first(snow_depth)) %>% 
  arrange(desc(snow_depth))
# Compare this to the mean count when its not snowing:
bike %>% filter(snowfall == 0) %>% drop_na() %>% summarize(mean(count))
# --> Counts decrease heavily with increasing snow on the ground, but the sample
#     size is small; maybe it also should be just a dummy or left out completely

# Correlation for all stations, only station 2 and over all station
cor(bike$count,bike$snow_depth, use = "pairwise.complete.obs") # Over all
cor(bike_list[[2]]$count,bike_list[[2]]$snow_depth, use = "pairwise.complete.obs") # Station 2
# Correlation on all stations for the 6 observations
foo <- bike %>% filter(snow_depth > 0) %>% group_by(station) %>% 
  summarize(cor = cor(count, snow_depth, use = "pairwise.complete.obs")) 
hist(foo$cor, main = "Emp. distribution of Cor.", xlab = "Cor. of all stations")
# --> Correlation is a bit smaller than for snowing


## For mean_temperature ----
summary(bike_list[[2]]$mean_temperature) 
# --> Looks very normally distributed
par(mfrow = c(1, 2))
hist(bike_list[[2]]$mean_temperature,
     main = "Overall mean temperatures (b = 5)",
     xlab = "Mean temperature in Fahrenheit",
     col="peachpuff")
abline(v = mean(bike_list[[2]]$mean_temperature), col='red', lwd=3, lty='dashed')
# --> Left-skewed distribution
plot(y = bike_list[[2]]$mean_temperature, x = bike_list[[2]]$obs_count, bty = "n",
     pch = 16, col = alpha("red", 0.3), xlab = "Days since first obs.", ylab = "Mean temperature (°F)")
abline(h = mean(bike_list[[2]]$mean_temperature), col='red', lwd=3, lty='dashed')      # mean
segments(x0 = 155, y0 = 65, x1 = 110, y1 = 75, col = alpha("blue", 0.4), lwd = 2) # highlight zone
segments(x0 = 260, y0 = 63, x1 = 295, y1 = 73, col = alpha("blue", 0.4), lwd = 2) # highlight zone
bike_list[[2]]$date[c(180,260)]
par(mfrow = c(1, 1))
# Calculate a simple moving average with a window size of 7 and 30
plot(y = bike_list[[2]]$mean_temperature, x = bike_list[[2]]$obs_count, bty = "n",
     pch = 16, col = alpha("red", 0.3), xlab = "Days since first obs.", ylab = "Mean temperature (°F)")
abline(h = mean(bike_list[[2]]$mean_temperature), col='red', lwd=3, lty='dashed')      # mean
segments(x0 = 155, y0 = 65, x1 = 110, y1 = 75, col = alpha("blue", 0.4), lwd = 2) # highlight zone
segments(x0 = 260, y0 = 63, x1 = 295, y1 = 73, col = alpha("blue", 0.4), lwd = 2) # highlight zones
ma1 = stats::filter(bike_list[[2]] %>% select(mean_temperature),
                    filter = rep(1/30, 30), method = 'convolution', sides = 2)
ma2 = stats::filter(bike_list[[2]] %>% select(mean_temperature),
                    filter = rep(1/60, 60), method = 'convolution', sides = 2)
lines(ma1, lty = 2, lwd = 2, col = alpha('darkgreen', 0.3))                 # moving average (week)
lines(ma2, lty = 2, lwd = 2, col = alpha('blue', 0.6))                      # moving average (month)
lines(lowess(bike_list[[2]] %>% select(mean_temperature)), col='purple', lwd = 2) # smoothing trend
legend("bottomright", legend = c("SMA(30)", "SMA(60)", "Trend smooth"),  
       col = c("darkgreen", "blue", "purple"), lty = 1, cex = 0.6)
# --> There is less variance again in the interval between early July until
#     mid September, as was the case in wind_speed. We already did a comparison
#     of mean count values when we looked at wind_speed. But let's first look at
#     observations above and below the mean:
bike_list[[2]] %>% mutate(index = ifelse(mean_temperature > mean(mean_temperature), "upper", "lower")) %>% 
  drop_na() %>% group_by(index) %>% summarize(mean(count), var(count))
bike %>% mutate(index = ifelse(mean_temperature > mean(mean_temperature), "upper", "lower")) %>% 
  drop_na() %>% group_by(index) %>% summarize(mean(count), var(count))
# --> Obviously large differences here

# We illustrate this relationship in the following plots for some stations:
par(mfrow = c(3, 3))
foo <- c(round(seq(from = 1, to = 100, length.out = 9)))
for (i in 1:9){
  foo2 <- bike_list[[foo[i]]] %>% select(count, mean_temperature) %>% drop_na()
  plot(y = foo2$count, x = foo2$mean_temperature, bty = "n", pch = 16, col = alpha("black", 0.6),
       xlab = "Mean temperature (°F)", ylab = "Bike rental count")
}
par(mfrow = c(1, 1))
rm(foo, foo2, ma1, ma2, i)
# --> The plots clearly show that counts and variance drastically increase with 
#     increasing mean temperature values. For large mean temperatures, the
#     distribution also shifts more from a normal towards higher values for
#     count. However, the variance also decreases for most stations between 
#     60 - 70 °F, before increasing again as mentioned before.

# Correlation for all stations, only stations 2 and over all station
cor(bike$count, bike$mean_temperature, use = "pairwise.complete.obs") # Over all
cor(bike_list[[2]]$count,bike_list[[2]]$mean_temperature, use = "pairwise.complete.obs") # Station 2
foo <- bike %>% group_by(station) %>% 
  summarize(cor = cor(count, mean_temperature, use = "pairwise.complete.obs")) 
hist(foo$cor, breaks = c(seq(from = 0.3, to = 0.8, by = 0.025)),
     main = "Emp. distribution of Cor.", xlab = "Cor. of all stations")
# --> Correlation is the highest it has ever been here

# To summarize, temperature is clearly the most important aspect for the model,
# and it looks like an exponential increase 


## For count ----
summary(bike$count) # There are no days where people don't rent any bike at any station?
summary(bike_list[[1]]$count) # Some slight differences between station 2 and all stations
which(is.na(bike_list[[90]]$count)) # It seems like every stations has exactly 3 NAs?
# How many times are there 1 counts in general?
which(bike$count == 1)
length(which(bike$count == 1))/length(bike$count) # less 0.3%
# What stations has to lowest and highest mean/median counts?
# Lowest:
bike %>% group_by(station) %>% summarize(mean = mean(count, na.rm = T),
         median = median(count, na.rm = T)) %>% arrange(mean, median)
# Highest: 
bike %>% group_by(station) %>% summarize(mean = mean(count, na.rm = T),
         median = median(count, na.rm = T)) %>% arrange(desc(mean), desc(median))
# Lets take a look at the 3 lowest, the 3 highest and somewhere inbetween (by mean)
foo <- bike %>% group_by(station) %>% summarize(mean = mean(count, na.rm = T),
          median = median(count, na.rm = T)) %>% arrange(mean) %>% 
          slice(c(1:3, 49:51, 98:100)) %>% select(station) %>% as.vector()
par(mfrow = c(3, 3))
for (i in 1:9){
  foo2 <- bike %>% filter(station == foo$station[i]) %>% select(count, obs_count) %>% drop_na()
  station_name <- paste("Station", i)
  plot(y = foo2$count, x = foo2$obs_count, bty = "n", pch = 16, col = alpha("black", 0.6),
       xlab = "Obs. Nr.", ylab = "Counts", main = station_name)
  segments(x0 = 212, y0 = 0, x1 = 212, y1 = 200, col = alpha("blue", 0.4), lwd = 2) # highlight zone
  segments(x0 = 242, y0 = 0, x1 = 242, y1 = 200, col = alpha("blue", 0.4), lwd = 2) # highlight zone
}
par(mfrow = c(1, 1))
rm(foo, foo2, station_name, i)
bike_list[[2]]$date[210:250] # These are the dates where the plot exhibits very counts

# Lets look at histograms:
foo <- bike %>% group_by(station) %>% summarize(mean = mean(count, na.rm = T),
                                                median = median(count, na.rm = T)) %>% arrange(mean) %>% 
  slice(c(1:3, 49:51, 98:100)) %>% select(station) %>% as.vector()
par(mfrow = c(3, 3))
for (i in 1:8){
  foo2 <- bike %>% filter(station == foo$station[i]) %>% select(count, obs_count) %>% drop_na()
  station_name <- paste("Station", i, "(b = 10")
  hist(foo2$count, breaks = c(seq(from = 0, to = 350, by = 15)),
       main = station_name,
       xlab = "Nr. of counts",
       ylim = c(0, 120),
       col="peachpuff")
  abline(v = mean(foo2$count), col='red', lwd=3, lty='dashed')
  
}
par(mfrow = c(1, 1))

# Let's compare mean count values within the interval 210 - 250:
bike %>% mutate(index = ifelse(obs_count %in% c(210, 250), "inside", "outside")) %>% 
  drop_na() %>% group_by(index) %>% summarize(mean(count), var(count))
# --> In general, not that much different here
bike %>% filter(station == foo$station[1]) %>% mutate(index = ifelse(obs_count %in% c(210, 250), "inside", "outside")) %>% 
  drop_na() %>% group_by(index) %>% summarize(mean(count), var(count))
bike %>% filter(station == foo$station[5]) %>% mutate(index = ifelse(obs_count %in% c(210, 250), "inside", "outside")) %>% 
  drop_na() %>% group_by(index) %>% summarize(mean(count), var(count))
bike %>% filter(station == foo$station[9]) %>% mutate(index = ifelse(obs_count %in% c(210, 250), "inside", "outside")) %>% 
  drop_na() %>% group_by(index) %>% summarize(mean(count), var(count))
# --> However, for some stations, there is a large difference
rm(foo, foo2, station_name, i)

# For missingness (check if maybe MAR, although probably insignificant due to 
# cheer number of data points speaking against it + not counting the NAs of 
# skipped days on time series dimensions, so nothing meaningful, just for fun)
df <- bike
df$missing_indicator <- is.na(df$count)
summary(glm(missing_indicator ~ wind_speed + precipitation + snowfall 
            + snow_depth + mean_temperature, data = df, family = binomial))
summary(glm(missing_indicator ~ mean_temperature, data = df, family = binomial))
rm(df, group_rows)
# --> Seems rather unlikely when regressing on all station data





# .------------------------------------------------------------------------
# Past-processing steps (after first meeting) -----------------------------
# First, obtain geocoordinates in lat/long format
# We build a support dataframe for this beforehand
# EDIT: Did not work for whatever reason --> We are dropping the last tag
# Some iterations ended in errors: Mostly due to '&' being written out
# and some minor details being left out in the actual OSM entry :(

# Just load in the results:
coordinate_df <- read.csv("C:/Users/marce/Documents/PraktikumProjekt_Bauer/coords.csv")
# 
# library(httr)
# library(jsonlite)
# library(stringr)
# station_name <- bike %>% select(station) %>% distinct()
# long <- lat <- c(rep.int(NA, length(station_name)))
# coordinate_df <- mutate(station_name, long, lat)
# failed_iterations <- c()
# 
# for (i in 1:100){
#   city <- "Washington D.C."
#   street <- coordinate_df$station[i]
#   # ending <- "Capital bikeshare"
#   address <- str_trim(paste(city, street))
#   response <- GET(url = "https://nominatim.openstreetmap.org/search",
#                   query = list(q = address, format = "json", limit = 1), # limited
#                   user_agent("R geocoding script"))
#   data <- fromJSON(content(response, as = "text"))
#   if(length(data) == 0){
#     failed_iterations <<- c(failed_iterations, i)
#     cat("Error in i-step:", i)
#     next
#   }
#   else{
#     coordinate_df$lat[i] <- data$lat
#     coordinate_df$long[i] <- data$lon
#   }
# }
# # Have to be filled out by hand:
# failed_iterations
# # 20th St & Florida Ave NW
# i = failed_iterations[1]
# coordinate_df$station[i]
# coordinate_df$lat[i] <- 38.9153997
# coordinate_df$long[i] <- -77.0446000
# 22nd & I St NW / Foggy Bottom
# i = failed_iterations[2]
# coordinate_df$station[i]
# coordinate_df$lat[i] <- 38.9008797
# coordinate_df$long[i] <- -77.0489559
# # 25th St & Pennsylvania Ave NW
# i = failed_iterations[3]
# coordinate_df$station[i]
# coordinate_df$lat[i] <- 38.9038267
# coordinate_df$long[i] <- -77.0534850
# # 2nd St & Massachusetts Ave NE
# i = failed_iterations[4]
# coordinate_df$station[i]
# coordinate_df$lat[i] <- 38.8949798
# coordinate_df$long[i] <- -77.0031373
# # Convention Center / 7th & M St NW
# i = failed_iterations[5]
# coordinate_df$station[i]
# coordinate_df$lat[i] <- 38.9057197 
# coordinate_df$long[i] <- -77.0222640
# # Eastern Market / 7th & North Carolina Ave SE
# i = failed_iterations[6]
# coordinate_df$station[i]
# coordinate_df$lat[i] <- 38.8869517
# coordinate_df$long[i] <- -76.9968060
# # Eastern Market Metro / Pennsylvania Ave & 8th St SE
# i = failed_iterations[7]
# coordinate_df$station[i]
# coordinate_df$lat[i] <- 38.8840555 
# coordinate_df$long[i] <- -76.9952624
# # Eckington Pl & Q St NE
# i = failed_iterations[8]
# coordinate_df$station[i]
# coordinate_df$lat[i] <- 38.9106313 
# coordinate_df$long[i] <- -77.0046117
# # Metro Center / 12th & G St NW
# i = failed_iterations[9]
# coordinate_df$station[i]
# coordinate_df$lat[i] <- 38.8983637
# coordinate_df$long[i] <- -77.0278690
# # Ohio Dr & West Basin Dr SW / MLK & FDR Memorials
# i = failed_iterations[10]
# coordinate_df$station[i]
# coordinate_df$lat[i] <- 38.8841284
# coordinate_df$long[i] <- -77.0465583
# # Smithsonian-National Mall / Jefferson Dr & 12th St SW
# i = failed_iterations[11]
# coordinate_df$station[i]
# coordinate_df$lat[i] <-  38.8887736
# coordinate_df$long[i] <- -77.0285632

# write.csv(coordinate_df, "C:/Users/marce/Documents/PraktikumProjekt_Bauer/coords.csv")

# Some testing for later
bike <- bike %>% mutate(JanuarySlope = if_else(month == 1, 1, 0),
                        FebruarySlope = if_else(month == 2, 1, 0),
                        MarchSlope = if_else(month == 3, 1, 0),
                        NovemberSlope = if_else(month == 11, 1, 0))

# Next, include weekend and summer holiday dummy for later regression
start_sholiday <- as.Date("2022-06-25")
end_sholiday <- as.Date("2022-08-28")
bike <- bike %>% mutate(IsWeekend = if_else(weekday %in% c("Samstag", "Sonntag"), 1, 0),
                        IsSummerHoliday = if_else(date >= start_sholiday & date <= end_sholiday, 1, 0),
                        IsFreezing = if_else(mean_temperature <= 32, 1, 0)) # 32°F = 0°C

# August observations -----
# For now, we analyse the august observations again which had a rather dubious presence
# in the count vs. time plot, which may have been triggered by incorrect measurements
# To do so, we create a secondary dataframe without august observations, and also
# look at plots without august observations and plots that highlight summer holidays
# The secondary dataset is created as follows:
bike_no_august <- bike %>% filter(!(date >= as.Date("2022-08-01") & date <= as.Date("2022-08-31")))

# We look again at the 3 lowest, 3 highest and inbetween counts for stations (by mean)
foo <- bike %>% group_by(station) %>% summarize(mean = mean(count, na.rm = T),
                                                median = median(count, na.rm = T)) %>% arrange(mean) %>% 
  slice(c(1:3, 49:51, 98:100)) %>% select(station) %>% as.vector()

# First, with the highlighted summer holiday observations
par(mfrow = c(3, 3))
for (i in 1:9){
  foo2 <- bike %>% filter(station == foo$station[i]) %>% filter(IsSummerHoliday == 0) %>%
               select(count, obs_count) %>% drop_na()
  station_name <- paste("Station", i)
  plot(y = foo2$count, x = foo2$obs_count, bty = "n", pch = 16, col = alpha("black", 0.6),
       xlab = "Obs. Nr.", ylab = "Counts", main = station_name)
  obs_sholiday = bike %>% filter(station == foo$station[i]) %>% filter(IsSummerHoliday == 1) %>%
        drop_na() %>% select(obs_count, -station) %>% unlist()
  count_sholiday = bike %>% filter(station == foo$station[i]) %>% filter(IsSummerHoliday == 1) %>%
    drop_na() %>% select(count, -station) %>% unlist()
  points(x = obs_sholiday, y = count_sholiday, pch = 16, col = alpha("red", 0.5))
}
par(mfrow = c(1, 1))
rm(foo, foo2, station_name, i, count_sholiday, start_sholiday)
# --> We can see that this effect is not necessarily related to summer holiday 
#     time, because only the latter part of the time windows shows a strange 
#     behaviour. If anything, it makes it more plausible that this is more likely
#     caused from the beforementioned measurement error. Thus, we will check for
#     the same 9 plots, and all plots afterwards if it is only limited to the
#     august timeframe.

# Without august timeframe
foo <- bike_no_august %>% group_by(station) %>% summarize(mean = mean(count, na.rm = T),
                                                median = median(count, na.rm = T)) %>% arrange(mean) %>% 
  slice(c(1:3, 49:51, 98:100)) %>% select(station) %>% as.vector()

# First, with the highlighted summer holiday observations
par(mfrow = c(3, 3))
for (i in 1:9){
  foo2 <- bike_no_august %>% filter(station == foo$station[i]) %>% select(count, obs_count) %>% drop_na()
  station_name <- paste("Station", i)
  plot(y = foo2$count, x = foo2$obs_count, bty = "n", pch = 16, col = alpha("black", 0.6),
       xlab = "Obs. Nr.", ylab = "Counts", main = station_name)
}
par(mfrow = c(1, 1))
rm(foo, foo2, station_name, i)
# --> Looks good now! 

# Now, we inspect every station 
par(mfrow = c(1, 1))
foo <- bike_no_august %>% group_by(station) %>% summarize(mean = mean(count, na.rm = T),
                                                          median = median(count, na.rm = T)) %>% 
                   arrange(mean) %>% select(station) %>% as.vector()
# Sys.sleep(5)
for (i in 1:100){
  foo2 <- bike_no_august %>% filter(station == foo$station[i]) %>% select(count, obs_count) %>% drop_na()
  station_name <- paste("Station", i)
  plot(y = foo2$count, x = foo2$obs_count, bty = "n", pch = 16, col = alpha("black", 0.6),
       xlab = "Obs. Nr.", ylab = "Counts", main = station_name)
  # Sys.sleep(5)
}
par(mfrow = c(1, 1))
# Something noteworthy 1:
i = 16
foo3 <- bike_no_august %>% filter(station == foo$station[16]) 
which(foo3$count > 80)
foo3$date[which(foo3$count > 80)] # 4th. July = Independence Day celebration
foo3$station[1]
# Noteworthy 2:
i = 52
rm(foo, foo2, foo3, station_name, i)
# Some stations have still some outliers before and after, but looks good for 
# most parts


# In order to include lagged count values that account for the NAs, because some 
# days are NA and thus also their row value, we need to calculate them manually,
# as lag() will otherwise just take the value from 2 days ago. 

# Create helper variable for identification of count value
bike <- bike %>% mutate(lagged_obs_count = obs_count-1,
                        lagged_obs_count_by_week = obs_count-7,
                        adj_lagged_count = NA_integer_,
                        adj_lagged_count_by_week = NA_integer_)
bike_no_august <- bike_no_august %>% mutate(lagged_obs_count = obs_count-1,
                        lagged_obs_count_by_week = obs_count-7,
                        adj_lagged_count = NA_integer_,
                        adj_lagged_count_by_week = NA_integer_)
# Now, perform lagged count calculation on list structure, and combine them back afterwards
bike_list <- bike %>% group_by(station) %>% group_split()
for (i in 1:100){
  for(j in 1:length(bike_list[[i]]$count)){
    if(isTRUE(bike_list[[i]]$lagged_obs_count[j] %in% bike_list[[i]]$obs_count)){
      
      index1 <- which(bike_list[[i]]$lagged_obs_count[j] == bike_list[[i]]$obs_count)
      bike_list[[i]]$adj_lagged_count[j] = bike_list[[i]]$count[index1]
        
    }
    if(isTRUE(bike_list[[i]]$lagged_obs_count_by_week[j] %in% bike_list[[i]]$obs_count)){
      
      index2 <- which(bike_list[[i]]$lagged_obs_count_by_week[j] == bike_list[[i]]$obs_count)
      bike_list[[i]]$adj_lagged_count_by_week[j] = bike_list[[i]]$count[index2]
      
    }
  }
}
rm(i, j, index1, index2)
bike <- bind_rows(bike_list) %>% select(-lagged_obs_count, -lagged_obs_count_by_week)

# We will do the same for bike_no_august, and overwrite bike_list back to include august afterwards:
bike_list <- bike_no_august %>% group_by(station) %>% group_split()
for (i in 1:100){
  for(j in 1:length(bike_list[[i]]$count)){
    if(isTRUE(bike_list[[i]]$lagged_obs_count[j] %in% bike_list[[i]]$obs_count)){
      
      index1 <- which(bike_list[[i]]$lagged_obs_count[j] == bike_list[[i]]$obs_count)
      bike_list[[i]]$adj_lagged_count[j] = bike_list[[i]]$count[index1]
      
    }
    if(isTRUE(bike_list[[i]]$lagged_obs_count_by_week[j] %in% bike_list[[i]]$obs_count)){
      
      index2 <- which(bike_list[[i]]$lagged_obs_count_by_week[j] == bike_list[[i]]$obs_count)
      bike_list[[i]]$adj_lagged_count_by_week[j] = bike_list[[i]]$count[index2]
      
    }
  }
}
rm(i, j, index1, index2)
bike_no_august <- bind_rows(bike_list) %>% select(-lagged_obs_count, -lagged_obs_count_by_week)
bike_list <- bike %>% group_by(station) %>% group_split()

# However, we will also include the normal lag results where we disregard NAs 
# on time dimension and just take the previous value. May be interesting for comparison
bike <- bike %>% group_by(station) %>% arrange(station, date) %>%  
        mutate(lagged_count = dplyr::lag(count, 1),
               lagged_count_by_week = dplyr::lag(count, 7))
bike_no_august <- bike_no_august %>% group_by(station) %>% arrange(station, date) %>%  
                     mutate(lagged_count = dplyr::lag(count, 1), 
                            lagged_count_by_week = dplyr::lag(count, 7))
# write.csv(bike, "C:/Users/marce/Documents/PraktikumProjekt_Bauer/final_bike.csv", row.names = FALSE)
# write.csv(bike_no_august, "C:/Users/marce/Documents/PraktikumProjekt_Bauer/final_bike_no_august.csv", row.names = FALSE)

# Some first modelling attempt --------------------------------------------
# First (suboptimal LM)
# summary(lm <- lm(formula = count ~ wind_speed + precipitation + snowfall +
#            snow_depth + mean_temperature + I(mean_temperature^2), data = bike %>% drop_na()))
# plot(bike$mean_temperature, y = resid(lm), xlab = "Regressor", ylab = "Residuals",
#      main = "Regressor vs. Residuals", bty = "n", pch = 16, col = alpha("black", 0.1))
# plot(fitted(lm), y = resid(lm), xlab = "Fitted", ylab = "Residuals",
#      main = "Fitted vs. Residuals", bty = "n", pch = 16, col = alpha("black", 0.1))

# Poisson GLM
summary(glm <- glm(formula = count ~ wind_speed + precipitation + snowfall +
           snow_depth + mean_temperature + I(mean_temperature^2), data = bike, family = poisson))

# Do we need FE intercepts or not? -------------------------------

# Calculate first basic GLM models first (only temperature as covariate)
intercept.coefs <- slope.coefs <- c(rep.int(NA, 101)) # 1:100 for each station, 101 for all stations
names(bike)
for (i in 1:101){
  if(i != 101){
    glm <- glm(formula = count ~ mean_temperature, data = bike_list[[i]], family = poisson)
    intercept.coefs[i] <- coef(glm)[1]
    slope.coefs[i] <- coef(glm)[2]
  }
  else{
    glm <- glm(formula = count ~ mean_temperature, data = bike, family = poisson)
    intercept.coefs[i] <- coef(glm)[1]
    slope.coefs[i] <- coef(glm)[2]
  }
}
rm(glm)
# Perform comparison between station-dependent GLM coefficients
# By calculating summary statistics:
round(c(summary(intercept.coefs[1:100]), all_stations = intercept.coefs[101]), 5)
# --> Large variation on the intercepts here
round(c(summary(slope.coefs[1:100]), all_stations = slope.coefs[101]), 5)
# --> Existing, but much smaller variation on the slope coefs here too

# By empirical distribution of intercept and slope coefficients:
par(mfrow = c(1, 2))
hist(intercept.coefs[1:100], breaks = c(seq(from = 1, to = 4, by = 0.1)),
     main = "emp. intercept distribution (breaks = 0.1)",
     xlab = "Intercept for each station",
     col="peachpuff")
abline(v = intercept.coefs[101], col='red', lwd=3, lty='dashed')
hist(slope.coefs[1:100], breaks = c(seq(from = 0.010, to = 0.040, by = 0.00125)),
     main = "emp. slope distribution (breaks = 0.00125)",
     xlab = "Slope for each station",
     col="lightblue")
abline(v = slope.coefs[101], col='blue', lwd=3, lty='dashed')
par(mfrow = c(1, 1))



# .------------------------------------------------------------------------
# Model specification -----------------------------------------------------
bike <- read.csv("C:/Users/marce/Documents/PraktikumProjekt_Bauer/final_bike.csv")
bike_no_august <- read.csv("C:/Users/marce/Documents/PraktikumProjekt_Bauer/final_bike_no_august.csv")
bike_list <- bike %>% group_by(station) %>% group_split()

# Via plm package and log-normal model ------------------------------------
names(bike) 
# Dep.Variable: count
# Indep. Variables (stables): wind_speed, precipitation, snowfall, snow_depth, mean_temperature
# Indep. Variables (maybes): IsWeekend, IsSummerHoliday, IsFreezing, adj_lagged_count, adj_lagged_count_by_week, lagged_count, lagged_count_by_week
# FE: station, month, weekday
data = pdata.frame(bike_no_august, index = c("station", "obs_count")) # Alt.: data = bike
# In EDA, we already concluded that FE will most likely be the solution. However,
# it would be neat if this hypothesis were also supported by statistical test procedures
# So, we start off with a pooled model and perform a test for pooling vs. FE-effects:
summary(plm.pool <- plm(log(count) ~ wind_speed + 
                          precipitation + 
                          snowfall + 
                          snow_depth + 
                          mean_temperature +
                          I(mean_temperature^2),
                        data, model = "pooling"))
summary(plm1 <- plm(log(count) ~ wind_speed + 
                      precipitation + 
                      snowfall + 
                      snow_depth + 
                      mean_temperature +
                      I(mean_temperature^2),
                    data, model = "within", effect = "individual"))

pvcm <- pvcm(log(count) ~ wind_speed + 
                      precipitation + 
                      snowfall + 
                      snow_depth + 
                      mean_temperature +
                      I(mean_temperature^2),
                    data, model = "within", effect = "individual")

# Now, we perform a pooling test (Chow test), where we test the hypothesis that the same coefficients 
# apply to each individual in a standard F-Test. Here, we are comparing a pooled
# model on the full sample vs. a model based on the estimation of an equation for
# each individual 
pooltest(plm.pool, pvcm) # intercept are identical
pooltest(plm1, pvcm) # intercepts are different 
# --> coefficients change over individuals
pFtest(plm1, plm.pool) # null of pFtest: OLS better than FE --> FE better
plmtest(plm.pool, effect = "individual", type=("bp")) # null: No station-FE --> FE better
pwtest(plm.pool) # there are unobserved effects

# We continue on with plm, and check whether time effects are to be included.
# This is done (again) via F-Test or Lagrange Multiplier Test:
summary(plm2 <- plm(log(count) ~ wind_speed + 
                      precipitation + 
                      snowfall + 
                      snow_depth + 
                      mean_temperature +
                      I(mean_temperature^2) + as.factor(month),
                    data, model = "within", effect = "individual"))
pFtest(plm2, plm1) # --> Time effects should be included
# phtest(plm1, plm.random) # Hausman test for comparison --> FE as good as RE

# Thus, we continue with a model with month- and station-FE. The next is to 
# include the remaining variables, and check for significant F-Tests:
# What is better? Include weekday FE dummies or IsWeekend dummy?
summary(plm3 <- plm(log(count) ~ wind_speed + 
                      precipitation + 
                      snowfall + 
                      snow_depth + 
                      mean_temperature +
                      I(mean_temperature^2) + 
                      as.factor(month) +
                      as.factor(weekday),
                    data, model = "within", effect = "individual"))

summary(plm4 <- plm(log(count) ~ wind_speed + 
                      precipitation + 
                      snowfall + 
                      snow_depth + 
                      mean_temperature +
                      I(mean_temperature^2) + 
                      as.factor(month) +
                      IsWeekend,
                    data, model = "within", effect = "individual"))
# --> Weekdays Dummy seems more powerful

# Include IsSummerHoliday and IsFreezing?
summary(plm5 <- plm(log(count) ~ wind_speed + 
                      precipitation + 
                      snowfall + 
                      snow_depth + 
                      mean_temperature +
                      I(mean_temperature^2) + 
                      as.factor(month) +
                      as.factor(weekday) +
                      IsSummerHoliday,
                    data, model = "within", effect = "individual"))
# --> Insignificant
summary(plm5 <- plm(log(count) ~ wind_speed + 
                      precipitation + 
                      snowfall + 
                      snow_depth + 
                      mean_temperature +
                      I(mean_temperature^2) + 
                      as.factor(month) +
                      as.factor(weekday) +
                      IsFreezing,
                    data, model = "within", effect = "individual"))
# --> Wrong or unexpected sign for IsFreezing? Maybe due to large correlation 
#     or due to high likelihood of switching from car to bike in the event of 
#     potential black ice formation?

# Check for all form of lags?
summary(plm6 <- plm(log(count) ~ wind_speed + 
                      precipitation + 
                      snowfall + 
                      snow_depth + 
                      mean_temperature +
                      I(mean_temperature^2) + 
                      as.factor(month) +
                      as.factor(weekday) +
                      IsFreezing +
                      adj_lagged_count_by_week,
                    data, model = "within", effect = "individual"))
# Significant difference observable between lags in the model. Non-adjusted lags
# have much larger impact, while adjusted lags that account for missing obser-
# vation have definitely less of an impact, but are still quite significant (large t).
# There is also significant differences in R-Sq. and Adj. R-Sq. present when
# switching between adjusted lags and normal lags. However, when using (preferred) 
# adjusted week lags, there is only a small increase of explaining power, meaning
# that there isn't much explaining power left in time dimension. Thus, we will
# (possible) conclude with this model in model diagnostics. We also have to check
# whether FE model assumptions hold true for the given model


# Via pglm ----------------------------------------------------------------
# Dep.Variable: count
# Indep. Variables (stables): wind_speed, precipitation, snowfall, snow_depth, mean_temperature
# Indep. Variables (maybes): IsWeekend, IsSummerHoliday, IsFreezing, adj_lagged_count, adj_lagged_count_by_week, lagged_count, lagged_count_by_week
# FE: station, month, weekday
data = pdata.frame(bike_no_august, index = c("station", "date")) # Alt.: data = bike

summary(pglm1 <- pglm(count ~ wind_speed + 
                      precipitation + 
                      snowfall + 
                      snow_depth + 
                      mean_temperature +
                      I(mean_temperature^2),
                    data, family = "poisson",
                    model = "within", effect = "individual"))

summary(pglm2 <- pglm(count ~ wind_speed + 
                        precipitation + 
                        snowfall + 
                        snow_depth + 
                        mean_temperature +
                        I(mean_temperature^2) + 
                        month,
                      (data %>% mutate(month = as.factor(month))),
                      family = "poisson", model = "within", effect = "individual"))

summary(pglm3 <- pglm(count ~ wind_speed + 
                        precipitation + 
                        snowfall + 
                        snow_depth + 
                        mean_temperature +
                        I(mean_temperature^2) + 
                        month + 
                        weekday +
                        IsFreezing,
                      (data %>% mutate(month = as.factor(month), weekday = as.factor(weekday))),
                      family = "poisson", model = "within", effect = "individual"))

summary(pglm4 <- pglm(count ~ wind_speed + 
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


# Via fixest package ------------------------------------------------------
names(bike) 
# Dep.Variable: count
# Indep. Variables (stables): wind_speed, precipitation, snowfall, snow_depth, mean_temperature
# Indep. Variables (maybes): IsWeekend, IsSummerHoliday, IsFreezing, adj_lagged_count, adj_lagged_count_by_week, lagged_count, lagged_count_by_week
# FE: station, month, weekday
data = bike_no_august # Alt.: data = bike
# Baseline model:
summary(fe.poi1 <- feglm(count ~ wind_speed +   # <---- There are Cov Options to choose from
                          precipitation + 
                          snowfall + 
                          snow_depth + 
                          mean_temperature +
                          I(mean_temperature^2) | station,  # To add only the variables with varying slopes and not the fixed-effect,
                      data, family = "poisson",             # use double square brackets: fixef_var[[var1, var2]].
                      cluster = c("station")))

# BIC: 350,405.8    Adj. Pseudo R2: 0.488343    Squared Cor.: 0.62557

summary(fe.poi2 <- feglm(count ~ wind_speed + 
                           precipitation + 
                           snowfall + 
                           snow_depth + 
                           mean_temperature +
                           I(mean_temperature^2) +
                           as.factor(month) + IsSummerHoliday + IsFreezing
                         | station,
                         data, family = "poisson",
                         cluster = c("station")))
# BIC: 337,954.2   Adj. Pseudo R2: 0.506704    Squared Cor.: 0.645696

summary(fe.poi3 <- feglm(count ~ wind_speed + 
                           precipitation + 
                           snowfall + 
                           snow_depth + 
                           mean_temperature +
                           I(mean_temperature^2) +
                           IsSummerHoliday +
                           IsWeekend +
                           IsFreezing
                           | station + month, cluster = c("station", "month"),
                         data, family = "poisson"))
# BIC: 334,367.9   Adj. Pseudo R2: 0.511966    Squared Cor.: 0.659445

summary(fe.poi4 <- feglm(count ~ wind_speed + 
                           precipitation + 
                           snowfall + 
                           snow_depth + 
                           mean_temperature +
                           I(mean_temperature^2) +
                           IsWeekend +
                           IsFreezing +
                           adj_lagged_count_by_week
                         | station + month, cluster = c("station", "month"),
                         data, family = "poisson"))
etable(fe.poi4)

# via pggls ---------------------------------------------------------------
# General FGLS estimator (robust against any type of intragroup heteroskedasticity
# and serial correlation; general FGLS estimation is inefficient under groupwise heteroskedasticity)
# However, does not fulfil requirements for variance parameter estimation (see help)?
data = pdata.frame(bike_no_august, index = c("station", "date")) # Alt.: data = bike
summary(pggls1 <- pggls(log(count) ~ wind_speed + 
                            precipitation + 
                            snowfall + 
                            snow_depth + 
                            mean_temperature +
                            I(mean_temperature^2) + 
                          month + 
                          weekday +
                          IsFreezing,
                        (data %>% mutate(month = as.factor(month), weekday = as.factor(weekday))),
                        model = "within", effect = "individual"))

# Model diagnostics -------------------------------------------------------
# - check for heteroscedastisticity (whether it depends on time --> Violation of MLR.5
#   and t.test/F-Test invalid)
# - If heteroscedasticity present, use Wald-Test with heteroscedasticity-robust Var-Cov-Mat
# - typical methodology in the case of heteroscedasticity are applicable
# - For FE.4, it most hold: E(u_i,t | X_i, a_i) = 0 --> consistent and unbiased
# - For FE.5, Var(u_i,t | X_i, a_i) = sig_u^2, t = 2,..., T, i = 1,..., N
# - For FE.6, no autocorrelation: For all t != s, it holds: Cov(u_i,t, u_i,s | X_i, a_i) = 0
# Conditional on X_i and a_i, the u_i,t are i.i.d. normally distributed N(0, sig_u^2)
# If FE.1-FE.6 hold, within is the BLUE; If FE.7 holds, the FE-estimator is normal
# If FE.7 does not hold, asymptotic tests hold for large N and fixed T
# FE.6 doesn't hold if ∆u_i,t are not autocorrelated
# If Cov(∆u_i,t, ∆u_i,s) != 0 for t != s, then robust std. errrors necessary

# Load in data:
bike <- read.csv("C:/Users/marce/Documents/PraktikumProjekt_Bauer/final_bike.csv")
bike_no_august <- read.csv("C:/Users/marce/Documents/PraktikumProjekt_Bauer/final_bike_no_august.csv")
bike_list <- bike %>% group_by(station) %>% group_split()

# Specify finalized model first:
data = pdata.frame(bike_no_august, index = c("station", "date")) # Alt.: bike
summary(plm.final <- plm(log(count) ~ wind_speed + 
                      precipitation + 
                      snowfall + 
                      snow_depth + 
                      mean_temperature +
                      I(mean_temperature^2) + 
                      as.factor(month) +
                      as.factor(weekday) +
                      IsFreezing,
                    data, model = "within", effect = "individual"))

# (JanuarySlope + NovemberSlope) * mean_temperature +
# (JanuarySlope + NovemberSlope) * I(mean_temperature^2) + 

# I(JanuarySlope * mean_temperature) +
# I(JanuarySlope * mean_temperature^2) +
# I(FebruarySlope * mean_temperature) +
# I(JanuarySlope * mean_temperature^2) +
# I(NovemberSlope * mean_temperature)


# Other -------------------------------------------------------------------
residuals <- resid(plm.final)
qqnorm(residuals, main = "QQ Plot of Log-Normal Regression Residuals")
qqline(residuals, col = "blue", lwd = 2)  # Adds a reference line

library(lmtest)
coeftest(plm.final, vcov = vcovHC(plm.final, method = "arellano"))
coeftest(plm.final, vcov = vcovHC(plm.final, method = "white2", type = "HC3"))

# Check for FE.4 for 9 stations and all variables: ----
foo <- bike_no_august %>% group_by(station) %>% summarize(mean = mean(count, na.rm = T),
                                                          median = median(count, na.rm = T)) %>% arrange(mean) %>% 
  slice(c(1:3, 49:51, 98:100)) %>% select(station) %>% as.vector()

merged <- as.data.frame(cbind(resid(plm.final), fitted(plm.final)))
names(merged) <- c("resid.plm.final.", "fitted")

merged <- inner_join(data %>% rownames_to_column("rowname"),
                     merged %>% rownames_to_column("rowname"), by = "rowname") %>%
          column_to_rownames("rowname")

# merged <- data %>%
#   rownames_to_column("rowname") %>%
#   inner_join(as.data.frame(resid(plm.final)) %>% rownames_to_column("rowname"), by = "rowname") %>%
#   column_to_rownames("rowname")
# merged <- merged %>%
#   rownames_to_column("rowname") %>%
#   inner_join(as.data.frame(fitted(plm.final)) %>% rownames_to_column("rowname"), by = "rowname") %>%
#   column_to_rownames("rowname")
# names(merged)[26] <- "fitted"


# First, with the highlighted summer holiday observations
par(mfrow = c(3, 3))
# Wind_speed:
for (i in 1:9){
  
  foo2 <- merged %>% filter(station == foo$station[i]) %>% select(wind_speed, resid.plm.final.)
  station_name <- paste("Station", i)
  
  x <- foo2$wind_speed[1:length(foo2[,1])]
  y <- foo2$resid.plm.final.[1:length(foo2$resid.plm.final.)]
  
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
  
  foo2 <- merged %>% filter(station == foo$station[i]) %>% select(precipitation, resid.plm.final.)
  station_name <- paste("Station", i)
  
  x <- foo2[,1][1:length(foo2[,1])]
  y <- foo2$resid.plm.final.[1:length(foo2$resid.plm.final.)]
  
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
  
  foo2 <- merged %>% filter(station == foo$station[i]) %>% select(snowfall, resid.plm.final.)
  station_name <- paste("Station", i)
  
  x <- foo2[,1][1:length(foo2[,1])]
  y <- foo2$resid.plm.final.[1:length(foo2$resid.plm.final.)]
  
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
  
  foo2 <- merged %>% filter(station == foo$station[i]) %>% select(snow_depth, resid.plm.final.)
  station_name <- paste("Station", i)
  
  x <- foo2[,1][1:length(foo2[,1])]
  y <- foo2$resid.plm.final.[1:length(foo2$resid.plm.final.)]
  
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
  
  foo2 <- merged %>% filter(station == foo$station[i]) %>% select(mean_temperature, resid.plm.final.)
  station_name <- paste("Station", i)
  
  x <- foo2[,1][1:length(foo2[,1])]
  y <- foo2$resid.plm.final.[1:length(foo2$resid.plm.final.)]
  
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
  
  foo2 <- merged %>% filter(station == foo$station[i]) %>% select(month, resid.plm.final.)
  station_name <- paste("Station", i)
  
  x <- foo2[,1][1:length(foo2[,1])]
  y <- foo2$resid.plm.final.[1:length(foo2$resid.plm.final.)]
  
  plot(x, y, xlab = names(foo2)[1], ylab = "Residuals", xlim = c(1, 11), xaxt = "n",
       main = station_name, bty = "n", pch = 16, col = alpha("black", 0.6))
  axis(1, at = seq(1, 11, by = 1))
  abline(a = 0, b = 0, col = "red", lwd = 1.5) # a = Intercept, b = Slope
  monthly_means <- foo2 %>% group_by(month) %>% summarize(mean = mean(resid.plm.final.))
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
    select(weekday, resid.plm.final.)
  
  station_name <- paste("Station", i)
  
  x <- foo2[,1][1:length(foo2[,1])]
  y <- foo2$resid.plm.final.[1:length(foo2$resid.plm.final.)]
  
  plot(x, y, xlab = names(foo2)[1], ylab = "Residuals",
       main = station_name, bty = "n", pch = 16, col = alpha("black", 0.6))
  abline(a = 0, b = 0, col = "red", lwd = 1.5) # a = Intercept, b = Slope
  weekly_means <- foo2 %>% group_by(weekday) %>% summarize(mean = mean(resid.plm.final.))
  lines(weekly_means$weekday, weekly_means$mean, lwd = 1.5, col = alpha("blue", 1))
  points(weekly_means$weekday, weekly_means$mean, bty = "n", pch = 16, col = alpha("red", 1))
  
}
par(mfrow = c(1, 1))

# IsFreezing:
par(mfrow = c(3, 3))
for (i in 1:9){
  
  foo2 <- merged %>% filter(station == foo$station[i]) %>%
    select(IsFreezing, resid.plm.final.)
  
  station_name <- paste("Station", i)
  
  x <- foo2[,1][1:length(foo2[,1])]
  y <- foo2$resid.plm.final.[1:length(foo2$resid.plm.final.)]
  
  plot(x, y, xlab = names(foo2)[1], ylab = "Residuals", xlim = c(0, 1), xaxt = "n",
       main = station_name, bty = "n", pch = 16, col = alpha("black", 0.6))
  axis(1, at = seq(0, 1, by = 1))
  abline(a = 0, b = 0, col = "red", lwd = 1.5) # a = Intercept, b = Slope
  freezing_means <- foo2 %>% group_by(IsFreezing) %>% summarize(mean = mean(resid.plm.final.))
  points(freezing_means$IsFreezing, freezing_means$mean, bty = "n", pch = 16, col = alpha("red", 1))
  
}
par(mfrow = c(1, 1))

# Check for FE.5 (heteroscedasticity) ----
par(mfrow = c(3, 3))
for (i in 1:9){
  
  foo2 <- merged %>% filter(station == foo$station[i]) %>%
    select(fitted, resid.plm.final.)
  
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
    select(obs_count, resid.plm.final.)
  station_name <- paste("Station", i)
  
  x <- foo2$obs_count[1:length(foo2$obs_count)]
  y <- foo2$resid.plm.final.[1:length(foo2$resid.plm.final.)]
  
  plot(x, y, xlab = names(foo2)[1], ylab = "Residuals", xlim = c(1, 334), xaxt = "n",
       main = station_name, bty = "n", pch = 16, col = alpha("black", 0.6))
  axis(1, at = c(1, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334))
  abline(v = c(31.5, 59.5, 90.5, 120.5, 151.5, 
               181.5, 212.5, 243.5, 273.5, 304.5),
         col=alpha("black", 0.3), lwd=0.5, lty='dashed')
  
  abline(a = 0, b = 0, col = "red", lwd = 1.5) # a = Intercept, b = Slope
  ma1 = stats::filter(foo2$resid.plm.final.,
                      filter = rep(1/7, 7), method = 'convolution', sides = 2)
  ma2 = stats::filter(foo2$resid.plm.final.,
                      filter = rep(1/30, 30), method = 'convolution', sides = 2)
  lines(ma2, lty = 2, lwd = 2, col = alpha('blue', 0.9))                  
  lines(lowess(foo2$resid.plm.final.), col='purple', lwd = 2)
}
par(mfrow = c(1, 1))

# Perform test for autocorrelation of errors - Wooldridge Test for AR(1) Errors
# in FE --> If significant (alt. hypo.), then FE.6 does not hold
pwartest(plm.final)
u_hat <- resid(plm.final)
summary(lm <- lm(u_hat ~ lag(u_hat, 1)))
(delta = -1/(334-1))
(delta-coef(lm)[2])/sqrt(diag(vcov(lm)))[2]
qt(0.975, 29390) # critical val 

# Other 
# Normally: Rejection of the joint test, though, gives no information on the 
# direction of the departure from the null hypothesis, i.e.: is rejection due 
# to the presence of serial correlation, of random effects or of both
# --> Since RE model does not perform significantly better, probably autocorrelation?
pbsytest(plm.pool) # (at least) AR(1) error / presence of serial correlation
# Breusch-Godfrey/Wooldridge test of serial correlation for (the idiosyncratic 
# component of) the errors in panel models
pbgtest(plm.final) # order = NULL (def) uses the min number of obs over the time dim


# Create map --------------------------------------------------------------
library(osmdata)
library(ggplot2)
library(tidyverse)
library(sf)

roads <- getbb("Liverpool UK") %>% opq(timeout = 3500) %>% 
  add_osm_feature(key = "highway", value = c("motorway", "primary", "secondary",
                                             "tertiary", "residential", "living_street",
                                             "unclassified"))
library(osmdata)
library(ggplot2)
library(sf)
dc_bbox <- osmdata::getbb("Washington D.C.")
main_roads <- opq(bbox = dc_bbox) %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "secondary", "tertiary", "residential")) %>%
  osmdata_sf()


library(osmdata)
library(ggplot2)
setwd("C:/Users/marce/Downloads/")
install.packages("httr2_0.5.1.tar.gz", repos = NULL, type = "source")
install.packages("osmdata_0.1.8.tar.gz", repos = NULL, type = "source")
washington_bb <- getbb("Washington")

# A 2x2 matrix
tucson_bb <- matrix(data = c(-111.0, -110.7, 31.0, 32.3),
                    nrow = 2,
                    byrow = TRUE)
# Update column and row names
colnames(tucson_bb) <- c("min", "max")
rownames(tucson_bb) <- c("x", "y")
# Print the matrix to the console
tucson_major <- tucson_bb %>% opq() %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "secondary")) %>%
  osmdata_sf()

hampi_sf <- opq ("hampi india") %>%
  add_osm_feature (key = "historic", value = "ruins") %>%
  osmdata_sf ()

