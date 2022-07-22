library(ggplot2)
library(readr)
library(tidyverse)
library(tidyr)
library(dplyr)
library(lubridate)
library(AMR)
library(chron)

####----Load data set----####
setwd("C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model")
uurgeg_208_2011_2020 <- read_csv("uurgeg_208_2011-2020.csv")
wind_data <- uurgeg_208_2011_2020[,c(2:5,7,8,10,13)]
colnames(wind_data) <- c("YYYYMMDD", "Time","Wind_direction","Mean_wind_speed",
                         "Max_wind_gust","Temperature","Air_pressure",
                         "Relative_humidity")

####----Time formating----####
wind_data$YYYYMMDD <- format(as.Date(as.character(wind_data$YYYYMMDD),
                                     format="%Y%m%d"))
wind_data$YYYYMMDD <- format(as.POSIXct(wind_data$YYYYMMDD,format="%Y-%m-%d"),
                             "%Y-%m-%d")
wind_data$YYYYMMDD <- as.Date(wind_data$YYYYMMDD)
wind_data$Month <- month(as.POSIXlt(wind_data$YYYYMMDD, format="%Y/%m/%d"))
wind_data$Year <- format(as.Date(wind_data$YYYYMMDD), format = "%Y")
wind_data$Year <- format(as.POSIXct(wind_data$Year,format="%Y"),"%Y")
wind_data$Year <- year(as.POSIXct(wind_data$Year,format="%Y"))
wind_data$Weekdays <- weekdays(wind_data$YYYYMMDD)
wind_data$Day <- as.numeric(format(as.Date(wind_data$YYYYMMDD,
                                           format="%Y-%m-%d"), format = "%d"))
wind_data$Weekdays <- factor(wind_data$Weekdays, levels =c("Monday", "Tuesday",
                                                           "Wednesday",
                                                           "Thursday",
                                                           "Friday",
                                                           "Saturday",
                                                           "Sunday"),
                             ordered =TRUE)
str(wind_data)
wind_data$Time <- wind_data$Time - 1
wind_data$Time <- strptime(x = wind_data$Time, format = "%H")
wind_data$Time <- format(as.POSIXct(wind_data$Time), format = "%H:%M:%S")
wind_data$datetime <- ymd_hms(paste(wind_data$YYYYMMDD, wind_data$Time))
wind_data <- wind_data %>% distinct(datetime,.keep_all = TRUE) #Remove duplication

####----Data cleaning----####

#Check for implausible value 
sum(wind_data$Wind_direction < 0, na.rm = T)
sum(wind_data$Mean_wind_speed < 0, na.rm = T)
sum(wind_data$Max_wind_gust < 0, na.rm = T)
sum(wind_data$Air_pressure < 0, na.rm = T)
sum(wind_data$Relative_humidity <0, na.rm = T)

#Check for missing value
sum(is.na(wind_data$Wind_direction))
sum(is.na(wind_data$Mean_wind_speed))
sum(is.na(wind_data$Max_wind_gust))
sum(is.na(wind_data$Temperature))
sum(is.na(wind_data$Air_pressure))
sum(is.na(wind_data$Relative_humidity))
sum(is.na(wind_data$datetime))

####----Change unit of variable----####
wind_data$Mean_wind_speed <- wind_data$Mean_wind_speed*0.1
wind_data$Max_wind_gust <- wind_data$Max_wind_gust*0.1
wind_data$Temperature <- wind_data$Temperature*0.1
wind_data$Air_pressure <- wind_data$Air_pressure*0.1*0.1 #transform from 0.1hPa to kPa
wind_data$Temperature_0 <- wind_data$Temperature+0.0065*1.5
wind_data$Temperature_10 <- wind_data$Temperature-0.0065*8.5 
wind_data$Temperature_89.5 <- wind_data$Temperature-0.0065*88
wind_data$Air_pressure_10 <- wind_data$Air_pressure*((1+(-0.0065*10/(wind_data$Temperature_0+273.15)))^5.256)
wind_data$Air_pressure_89.5 <- wind_data$Air_pressure*((1-0.0065*89.5/(wind_data$Temperature_0+273.15))^5.256)
wind_data$Air_pressure_10 <- wind_data$Air_pressure_10*0.1 #turn to kPa
wind_data$Air_pressure_89.5 <- wind_data$Air_pressure_89.5*0.1
wind_data$Relative_humidity <- wind_data$Relative_humidity*0.01
wind_data$Mean_wind_89.5 <- wind_data$Mean_wind_speed*1.19
wind_data$Max_wind_gust_89.5 <- wind_data$Max_wind_gust*1.19
summary(wind_data)

library(xts)
library(zoo)
wind_data_xts <- xts(wind_data, order.by = wind_data$datetime, frequency = 24)
str(wind_data)
summary(wind_data)
summary(wind_data_xts)
str(wind_data_xts)

summary(wind_data)
sd(wind_data$Mean_wind_speed, na.rm = T)
sd(wind_data$Max_wind_gust, na.rm = T)
sd(wind_data$Temperature, na.rm = T)
sd(wind_data$Air_pressure, na.rm = T)
sd(wind_data$Relative_humidity, na.rm = T)

####Check the data with 990 degree wind direction
summary(wind_data$Mean_wind_speed[wind_data$Wind_direction==990])
summary(wind_data$Max_wind_gust[wind_data$Wind_direction==990])
summary(wind_data$Temperature[wind_data$Wind_direction==990])
summary(wind_data$Air_pressure[wind_data$Wind_direction == 990])
summary(wind_data$Relative_humidity[wind_data$Wind_direction == 990])

boxplot(wind_data$Mean_wind_speed[wind_data$Wind_direction==990])
boxplot(wind_data$Max_wind_gust[wind_data$Wind_direction==990])
boxplot(wind_data$Temperature[wind_data$Wind_direction==990])
boxplot(wind_data$Air_pressure[wind_data$Wind_direction == 990])
boxplot(wind_data$Relative_humidity[wind_data$Wind_direction == 990])

#### Turn 990 degree in wind direction to NA
wind_data$Wind_direction[wind_data$Wind_direction == 990] <- NA
wind_data_1119 <- wind_data[wind_data$Year <= 2019,]
wind_data_20 <- wind_data[wind_data$Year == 2020,]
str(wind_data_1119)
str(wind_data_20)

write.csv(wind_data_1119, "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/wind_data_1119.csv", row.names = FALSE)
write.csv(wind_data_20, "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/wind_data_20.csv", row.names = FALSE)

###---Create xts object
wind_data_xts_1119 <- wind_data_xts["20110101/20191231"]
wind_data_xts_20 <- wind_data_xts["2020"]
sum(is.na(wind_data_xts_20$Mean_wind_speed))
summary(wind_data_xts)

#This time series is evenly spaced since it is hourly time series
#Simplify assumption for time series
###- COnsecutive observations are equally spaced
###- Apply a discrete - time observation
###- This may only hold approximately since data may missing

start(wind_data_xts)
end(wind_data_xts)
time(wind_data_xts)
deltat(wind_data_xts)
cycle(wind_data_xts)

####----Missing value imputation
install.packages("ggplot2") #Press yes to restart R session then No
install.packages('imputeTS',dependencies=TRUE) 
library(imputeTS)

statsNA(wind_data$Wind_direction)
statsNA(wind_data$Mean_wind_speed)
statsNA(wind_data$Max_wind_gust)
statsNA(wind_data$Temperature)
statsNA(wind_data$Air_pressure)
statsNA(wind_data$Relative_humidity)
statsNA(as.numeric(wind_data$datetime))

sum(is.na(wind_data_1119$Wind_direction))
sum(is.na(wind_data_1119$Mean_wind_speed))
sum(is.na(wind_data_1119$Max_wind_gust))
sum(is.na(wind_data_1119$Temperature))
sum(is.na(wind_data_1119$Air_pressure))
sum(is.na(wind_data_1119$Relative_humidity))

### Missing value imputation using Random Forest
library(missForest)
library(foreach) # for parallel processing
library(parallel)
library(doParallel) # for parallel processing
parallel::detectCores() #8-core CPUs
cores = detectCores()
library(data.table)

#Impute train set
library(dplyr)
library(randomForest)
# train/test_ direction:

typeof(wind_data_1119)

wind_data_1119 <- as.data.frame(wind_data_1119)
wind_data_20 <- as.data.frame(wind_data_20)

#To save time, you can skip this imputation part and used the imputed data set
#provided from line 219
#Missforest with 2 trees
set.seed(1234)
registerDoParallel(cores = cores[1]-1) # set based on number of CPU cores
doRNG::registerDoRNG(seed = 1234)
imputed_wind_1119_2 <- missForest(wind_data_1119[,-c(1,13)], parallelize = 'forests', 
                                  verbose = T, ntree = 2, maxiter = 2,  variablewise = TRUE)
imp_wind_1119_2 <- imputed_wind_1119_2$ximp
write.csv(imp_wind_1119_2,"C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/imp_wind_1119_2.csv", row.names = FALSE)
imputed_wind_1119_2$OOBerror

#missforest with 10 trees
set.seed(1234)
registerDoParallel(cores = cores[1]-1) # set based on number of CPU cores
doRNG::registerDoRNG(seed = 1234)
imputed_wind_1119_10 <- missForest(wind_data_1119[,-c(1,13)], parallelize = 'forests', 
                                   verbose = T, ntree = 10, maxiter = 2,  variablewise = TRUE)
imp_wind_1119_10 <- imputed_wind_1119_10$ximp
write.csv(imp_wind_1119_10,"C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/imp_wind_1119_10.csv", row.names = FALSE)
imputed_wind_1119_10$OOBerror

#missforest with 25 trees
set.seed(1234)
registerDoParallel(cores = cores[1]-1) # set based on number of CPU cores
doRNG::registerDoRNG(seed = 1234)
imputed_wind_1119_25 <- missForest(wind_data_1119[,-c(1,13)], parallelize = 'forests', 
                                   verbose = T, ntree = 25, maxiter = 5,  variablewise = TRUE)
imp_wind_1119_25 <- imputed_wind_1119_25$ximp
write.csv(imp_wind_1119_25,"C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/imp_wind_1119_25.csv", row.names = FALSE)
imputed_wind_1119_25$OOBerror


#Missforest with 50 trees per forest and mtry = 7
set.seed(1234)
registerDoParallel(cores = cores[1]-1) # set based on number of CPU cores
doRNG::registerDoRNG(seed = 1234)
imputed_wind_1119_50_7 <- missForest(wind_data_1119[,-c(1,13)], parallelize = 'forests',mtry = 7,
                                     verbose = T, ntree = 50, maxiter = 5,  variablewise = TRUE)
imp_wind_1119_50_7 <- imputed_wind_1119_50_7$ximp
write.csv(imp_wind_1119_50_7,"C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/imp_wind_1119_50_7.csv", row.names = FALSE)
imputed_wind_1119_50_7$OOBerror


#Load imp_wind_1119_50_7
library(readr)
library(xts)
library(zoo)
imp_wind_1119_50_7 <- read_csv("imp_wind_1119_50_7.csv")
imp_wind_1119_50_7$datetime <- wind_data_1119$datetime 
imp_wind_1119_50_7$Time <- imp_wind_1119_50_7$Time -1
imp_wind_1119_50_7$Temperature_0 <- imp_wind_1119_50_7$Temperature+0.0065*1.5
imp_wind_1119_50_7$Temperature_10 <- imp_wind_1119_50_7$Temperature-0.0065*8.5 
imp_wind_1119_50_7$Temperature_89.5 <- imp_wind_1119_50_7$Temperature-0.0065*88
imp_wind_1119_50_7$Air_pressure_10 <- imp_wind_1119_50_7$Air_pressure*((1+(-0.0065*10/(imp_wind_1119_50_7$Temperature_0+273.15)))^5.256)
imp_wind_1119_50_7$Air_pressure_89.5 <- imp_wind_1119_50_7$Air_pressure*((1-0.0065*89.5/(imp_wind_1119_50_7$Temperature_0+273.15))^5.256)
imp_wind_1119_50_7$Air_pressure_10 <- imp_wind_1119_50_7$Air_pressure_10*0.1 #turn to kPa
imp_wind_1119_50_7$Air_pressure_89.5 <- imp_wind_1119_50_7$Air_pressure_89.5*0.1
imp_wind_1119_50_7$Relative_humidity <- imp_wind_1119_50_7$Relative_humidity*0.01
imp_wind_1119_50_7$Max_wind_gust_89.5 <- imp_wind_1119_50_7$Max_wind_gust*1.19
imp_wind_1119_50_7$Mean_wind_speed_89.5 <- imp_wind_1119_50_7$Mean_wind_speed*1.19
imp_wind_1119_50_7_xts <- xts(imp_wind_1119_50_7, 
                              order.by = imp_wind_1119_50_7$datetime,
                              frequency = 24)

#Daily average mean wind speed from 2011-2019
imp_1119_daily_xts <- apply.daily(imp_wind_1119_50_7_xts["20110101/20191231"],
                                  FUN = mean)
write.zoo(as.data.frame(imp_1119_daily_xts), file = "imp_1119_daily_xts.csv", 
          sep =",")

#Daily average weather data from 2014-2020
wind_daily_1420_xts <- apply.daily(wind_data_xts["2014/2020"],FUN = mean)
write.zoo(as.data.frame(wind_daily_1420_xts), file = "wind_daily_1420_xts.csv", 
          sep =",", row.names = T)
#Daily average weather data from 2014-2019
imp_1419_daily_xts <- apply.daily(imp_wind_1119_50_7_xts["20140101/20191231"],
                                  FUN = mean)
write.zoo(as.data.frame(imp_1419_daily_xts), file = "wind_daily_1419_xts.csv", 
          sep =",", row.names = T)
#Daily average mweather data 2020
weather_data_2020_xts <- apply.daily(wind_data_xts["2020"], FUN = mean)

######-Data analysis-######
#seasonal plot
library(dplyr)
library(lubridate)
library(tsibble)
library(feasts)
library(ggplot2)
library(xts)

imp_1419_hourly_tibble <- imp_wind_1119_50_7 %>% as_tibble(.name_repair = "minimal")
imp_1419_hourly_tsibble <- imp_1419_hourly_tibble %>% as_tsibble(index = datetime)
imp_1419_hourly_tsibble$Day <- as.Date(as.character(imp_1419_hourly_tsibble$datetime),format="%Y-%m-%d")
str(imp_1419_hourly_tsibble)

hourly_wind_comp_10 <- imp_1419_hourly_tsibble %>% select(datetime, Mean_wind_speed) %>%
  model(stl = STL(Mean_wind_speed)) %>% components()

autoplot(hourly_wind_comp_10)+labs(title = "STL decomposition of hourly mean wind speed at 10 m height")

imp_1419_daily_tibble <- imp_1419_daily_xts %>% fortify.zoo %>%
  as_tibble(.name_repair = "minimal")
imp_1419_daily_tibble$Index <- format(as.Date(as.character(
  imp_1419_daily_tibble$Index),format="%Y-%m-%d"))
imp_1419_daily_tibble$Index  <- format(as.POSIXct(
  imp_1419_daily_tibble$Index,format="%Y-%m-%d"),"%Y-%m-%d")
imp_1419_daily_tibble$Index <- as.Date(imp_1419_daily_tibble$Index)

imp_1419_daily_tibble$Month <- month(as.POSIXlt(
  imp_1419_daily_tibble$Index, format="%Y/%m/%d"))
imp_1419_daily_tibble$Day <- as.numeric(format(as.Date(
  imp_1419_daily_tibble$Index,format="%Y-%m-%d"), format = "%d"))
imp_1419_daily_tibble$Weekdays <- weekdays(imp_1419_daily_tibble$Index)
imp_1419_daily_tibble <- imp_1419_daily_tibble[,-c(2,13)]
wind_daily_1419_tsibble <- imp_1419_daily_tibble %>% as_tsibble(index = Index)
str(wind_daily_1419_tsibble)


wind_daily_1420_xts <- na.locf(wind_daily_1420_xts, fromLast = TRUE)
wind_daily_1420_tibble <- wind_daily_1420_xts[,-c(1,2,13)] %>%  fortify.zoo %>%
  as_tibble(.name_repair = "minimal")
wind_daily_1420_tibble$Index <- format(as.Date(as.character(
  wind_daily_1420_tibble$Index),format="%Y-%m-%d"))
wind_daily_1420_tibble$Index <- format(as.POSIXct(
  wind_daily_1420_tibble$Index,format="%Y-%m-%d"),"%Y-%m-%d")
wind_daily_1420_tibble$Index <- as.Date(wind_daily_1420_tibble$Index)
wind_daily_1420_tibble$Month <- month(as.POSIXlt(
  wind_daily_1420_tibble$Index, format="%Y/%m/%d"))
wind_daily_1420_tibble$Day <- as.numeric(format(as.Date(
  wind_daily_1420_tibble$Index,format="%Y-%m-%d"), format = "%d"))
wind_daily_1420_tibble$Weekdays <- weekdays(wind_daily_1420_tibble$Index)
wind_daily_1420_tsibble <- wind_daily_1420_tibble %>% as_tsibble(index = Index)
str(wind_daily_1420_tibble)
summary(wind_daily_1420_tsibble)

library(xts)
weather_data_2020_xts <- weather_data_2020_xts[,-c(1,2,11,13)]
weather_data_2020_xts <- na.locf(weather_data_2020_xts, fromLast = TRUE)
wind_daily_2020_tibble <- weather_data_2020_xts %>%  fortify.zoo %>%
  as_tibble(.name_repair = "minimal")
wind_daily_2020_tibble$Index <- format(as.Date(as.character(
  wind_daily_2020_tibble$Index),format="%Y-%m-%d"))
wind_daily_2020_tibble$Index <- format(as.POSIXct(
  wind_daily_2020_tibble$Index,format="%Y-%m-%d"),"%Y-%m-%d")
wind_daily_2020_tibble$Index <- as.Date(wind_daily_2020_tibble$Index)

wind_daily_2020_tibble$Month <- month(as.POSIXlt(
  wind_daily_2020_tibble$Index, format="%Y/%m/%d"))
wind_daily_2020_tibble$Day <- as.numeric(format(as.Date(
  wind_daily_2020_tibble$Index,format="%Y-%m-%d"), format = "%d"))
wind_daily_2020_tibble$Weekdays <- weekdays(wind_daily_2020_tibble$Index)
wind_daily_2020_tsibble <- wind_daily_2020_tibble %>% as_tsibble(index = Index)
str(wind_daily_2020_tsibble)
summary(wind_daily_2020_tsibble)

#Seasonal plot weekly level
library(ggplot2)
wind_daily_1419_tsibble %>% select(Index, Mean_wind_speed, Month, Year, Weekdays,
                                   Day) %>% filter(Year == 2019) %>% 
  gg_season(Mean_wind_speed,period = "week") +
  theme() + 
  labs(y="m/s", title="Average daily wind speed per weekday 2019")

wind_daily_1419_tsibble %>% select(Index, Mean_wind_speed, Month, Year, Weekdays,
                                   Day) %>% filter(Year == 2019)%>%
  gg_season(Mean_wind_speed,period = "year") +
  theme() + 
  labs(y="m/s", title="Average daily wind speed per quarter 2019")
summary(wind_daily_1419_tsibble)
sd(wind_daily_1419_tsibble$Mean_wind_speed)
sd(wind_daily_1419_tsibble$Max_wind_gust)
sd(wind_daily_1419_tsibble$Temperature_10)
sd(wind_daily_1419_tsibble$Air_pressure_10)
sd(wind_daily_1419_tsibble$Relative_humidity)

#Check distribution of weather data at 10m height
hist(log(imp_wind_1119_50_7$Mean_wind_speed), main = "Log-transformed hourly mean wind speed",
     xlab = "Log-transformed mean wind speed", ylab = "Relative frequency", breaks = 35,
     col= "orange", freq = F, cex.main = 1.5, cex.lab = 2)
hist(log(wind_daily_1419_tsibble$Mean_wind_speed),  main = "Log-transformed average daily mean wind speed",
     xlab = "Log-transformed wind speed", ylab = "Relative frequency", breaks = 35,
     col= "orange", freq = F, cex.main = 1.5, cex.lab = 2)

old.par <-par()
par(mar =c(5.1, 4, 4.1, 1) + 0.1)# set left & right figure margins to 0
par(mfrow =c(3,2))

hist(wind_daily_1419_tsibble$Wind_direction,main="Wind direction distribution 2014-2019", 
     xlab="Direction (in degree)",ylab="Relative frequency",breaks =35 , # more columns
     col="lightblue", freq = F) # color the bars
lines(density(wind_daily_1419_tsibble$Wind_direction, bw =10, na.rm = T) , # "bw = ..." adjusts the smoothing
      type="l" , col="darkred" , lwd =2) # lwd = line width

hist(wind_daily_1419_tsibble$Mean_wind_speed, main = "Wind speed distribution 2014-2019",
     xlab = "Mean wind speed (m/s)", ylab = "Relative frequency", breaks = 35,
     col = "green", freq = F)
lines(density(wind_daily_1419_tsibble$Mean_wind_speed, bw =10, na.rm = T) , # "bw = ..." adjusts the smoothing
      type="l" , col="darkred" , lwd =2) # lwd = line width

hist(wind_daily_1419_tsibble$Max_wind_gust, main = "Maximum wind gust distribution 2014-2019",
     xlab = "Maximum wind gust (m/s)", ylab = "Relative frequency", breaks = 35,
     col = "blue", freq = F)
lines(density(wind_daily_1419_tsibble$Max_wind_gust, bw =10, na.rm = T) , # "bw = ..." adjusts the smoothing
      type="l" , col="darkred" , lwd =2) # lwd = line width

hist(wind_daily_1419_tsibble$Temperature_10, main = "Temperature distribution 2014-2019",
     xlab = "Temperature (degree Celsius)", ylab = "Relative frequency", breaks = 35,
     col = "yellow", freq = F)
lines(density(wind_daily_1419_tsibble$Temperature_10, bw =10, na.rm = T) , # "bw = ..." adjusts the smoothing
      type="l" , col="darkred" , lwd =2) # lwd = line width

hist(wind_daily_1419_tsibble$Air_pressure_10, main = "Air pressure distribution 2014-2019",
     xlab = "Air pressure (kPa)", ylab = "Relative frequency", breaks = 35,
     col = "purple", freq = F)
lines(density(wind_daily_1419_tsibble$Air_pressure_10, bw =10, na.rm = T) , # "bw = ..." adjusts the smoothing
      type="l" , col="darkred" , lwd =2) # lwd = line width

hist(wind_daily_1419_tsibble$Relative_humidity, main = "Relative humidity",
     xlab = "Relative humidity (%)", ylab = "Relative frequency", breaks = 35,
     col= "orange", freq = F)
lines(density(wind_daily_1419_tsibble$Relative_humidity, bw =10, na.rm = T) , # "bw = ..." adjusts the smoothing
      type="l" , col="darkred" , lwd =2) # lwd = line width

old.par
par(mfrow =c(1,1))
#Check distribution of weather data at 89.5 m height

old.par <-par()
par(mar =c(5.1, 4, 4.1, 1) + 0.1)# set left & right figure margins to 0
par(mfrow =c(3,2))

hist(wind_daily_1419_tsibble$Wind_direction,main="Wind direction distribution 2014-2019", 
     xlab="Direction (in degree)",ylab="Relative frequency",breaks =35 , # more columns
     col="lightblue", freq = F) # color the bars
lines(density(wind_daily_1419_tsibble$Wind_direction, bw =10, na.rm = T) , # "bw = ..." adjusts the smoothing
      type="l" , col="darkred" , lwd =2) # lwd = line width

hist(wind_daily_1419_tsibble$Mean_wind_speed_89.5, main = "Wind speed distribution 2014-2019",
     xlab = "Mean wind speed (m/s)", ylab = "Relative frequency", breaks = 35,
     col = "green", freq = F)
lines(density(wind_daily_1419_tsibble$Mean_wind_speed_89.5, bw =10, na.rm = T) , # "bw = ..." adjusts the smoothing
      type="l" , col="darkred" , lwd =2) # lwd = line width

hist(wind_daily_1419_tsibble$Max_wind_gust_89.5, main = "Maximum wind gust distribution 2014-2019",
     xlab = "Maximum wind gust (m/s)", ylab = "Relative frequency", breaks = 35,
     col = "blue", freq = F)
lines(density(wind_daily_1419_tsibble$Max_wind_gust_89.5, bw =10, na.rm = T) , # "bw = ..." adjusts the smoothing
      type="l" , col="darkred" , lwd =2) # lwd = line width

hist(wind_daily_1419_tsibble$Temperature_89.5, main = "Temperature distribution 2014-2019",
     xlab = "Temperature (degree Celsius)", ylab = "Relative frequency", breaks = 35,
     col = "yellow", freq = F)
lines(density(wind_daily_1419_tsibble$Temperature_89.5, bw =10, na.rm = T) , # "bw = ..." adjusts the smoothing
      type="l" , col="darkred" , lwd =2) # lwd = line width

hist(wind_daily_1419_tsibble$Air_pressure_89.5, main = "Air pressure distribution 2014-2019",
     xlab = "Air pressure (kPa)", ylab = "Relative frequency", breaks = 35,
     col = "purple", freq = F)
lines(density(wind_daily_1419_tsibble$Air_pressure_89.5, bw =10, na.rm = T) , # "bw = ..." adjusts the smoothing
      type="l" , col="darkred" , lwd =2) # lwd = line width

hist(wind_daily_1419_tsibble$Relative_humidity, main = "Relative humidity",
     xlab = "Relative humidity (%)", ylab = "Relative frequency", breaks = 35,
     col= "orange", freq = F)
lines(density(wind_daily_1419_tsibble$Relative_humidity, bw =10, na.rm = T) , # "bw = ..." adjusts the smoothing
      type="l" , col="darkred" , lwd =2) # lwd = line width

old.par
par(mfrow =c(1,1))
#Transformation of daily wind data
library(car)
wind_daily_1419_tsibble$Log_meanwind <- log(
  wind_daily_1419_tsibble$Mean_wind_speed)

coef_wind_direction <- coef(powerTransform(
  wind_daily_1419_tsibble$Wind_direction,family = "bcPower"))

wind_direction_tf <- bcPower(
  wind_daily_1419_tsibble$Wind_direction,lambda = coef_wind_direction)

wind_daily_1419_tsibble$bc_wind_direction <- bcPower(
  wind_daily_1419_tsibble$Wind_direction,lambda = coef_wind_direction)

coef_mean_wind_10 <- coef(powerTransform(wind_daily_1419_tsibble$Mean_wind_speed,
                                      family = "bcPower"))
mean_wind_10_tf <- bcPower(
  wind_daily_1419_tsibble$Mean_wind_speed,lambda = coef_mean_wind_10)

wind_daily_1419_tsibble$bc_mean_wind_10 <- bcPower(
  wind_daily_1419_tsibble$Mean_wind_speed,lambda = coef_mean_wind_10)

coef_max_gust_10 <- coef(powerTransform(wind_daily_1419_tsibble$Max_wind_gust, 
                                     family = "bcPower"))

max_gust_10_tf <- bcPower(
  wind_daily_1419_tsibble$Max_wind_gust,lambda = coef_max_gust_10)
  
wind_daily_1419_tsibble$bc_max_gust_10 <- bcPower(
  wind_daily_1419_tsibble$Max_wind_gust,lambda = coef_max_gust_10)

coef_air_pressure_10 <- coef(powerTransform(wind_daily_1419_tsibble$Air_pressure_10,
                                         family = "bcPower"))

air_pressure_10_tf <-  bcPower(
  wind_daily_1419_tsibble$Air_pressure_10,lambda = coef_air_pressure_10)
  
wind_daily_1419_tsibble$bc_air_pressure_10 <- bcPower(
  wind_daily_1419_tsibble$Air_pressure_10,lambda = coef_air_pressure_10)

coef_relative_humidity <- coef(powerTransform(
  wind_daily_1419_tsibble$Relative_humidity,family = "bcPower"))

relative_humidity_tf <- bcPower(
  wind_daily_1419_tsibble$Relative_humidity,lambda = coef_relative_humidity)

wind_daily_1419_tsibble$bc_relative_humidity <- bcPower(
  wind_daily_1419_tsibble$Relative_humidity,lambda = coef_relative_humidity)

coef_temperature_10 <- coef(powerTransform(wind_daily_1419_tsibble$Temperature_10,
                                        family = "bcnPower"))

temperature_10_tf <- bcnPower(wind_daily_1419_tsibble$Temperature_10,
                              lambda = coef_temperature_10[,1],
                              gamma = coef_temperature_10[,2])

wind_daily_1419_tsibble$bc_temperature_10 <- bcnPower(wind_daily_1419_tsibble$Temperature_10,
                                                   lambda = coef_temperature_10[,1],
                                                   gamma = coef_temperature_10[,2])

coef_mean_wind_89.5 <- coef(powerTransform(
  wind_daily_1419_tsibble$Mean_wind_speed_89.5,family = "bcPower"))

mean_wind_89.5_tf <- bcPower(
  wind_daily_1419_tsibble$Mean_wind_speed_89.5,lambda = coef_mean_wind_89.5)


wind_daily_1419_tsibble$bc_mean_wind_89.5 <- bcPower(
  wind_daily_1419_tsibble$Mean_wind_speed_89.5,lambda = coef_mean_wind_89.5)

coef_air_pressure_89.5 <- coef(powerTransform(
  wind_daily_1419_tsibble$Air_pressure_89.5,family = "bcPower"))

air_pressure_89.5_tf <- bcPower(
  wind_daily_1419_tsibble$Air_pressure_89.5,lambda = coef_air_pressure_89.5)

wind_daily_1419_tsibble$bc_air_pressure_89.5 <- bcPower(
  wind_daily_1419_tsibble$Air_pressure_89.5,lambda = coef_air_pressure_89.5)

coef_temperature_89.5 <- coef(powerTransform(
  wind_daily_1419_tsibble$Temperature_89.5,family = "bcnPower"))
  
temperature_89.5_tf <- bcnPower(wind_daily_1419_tsibble$Temperature_89.5,
                                lambda = coef_temperature_89.5[,1],
                                gamma = coef_temperature_89.5[,2])

wind_daily_1419_tsibble$bc_temperature_89.5 <- bcnPower(
  wind_daily_1419_tsibble$Temperature_89.5,lambda = coef_temperature_89.5[,1],
                                             gamma = coef_temperature_89.5[,2])
  
coef_max_gust_89.5 <-  coef(powerTransform(
  wind_daily_1419_tsibble$Max_wind_gust_89.5,family = "bcPower"))

Max_wind_gust_89.5_tf <- bcPower(
  wind_daily_1419_tsibble$Max_wind_gust_89.5,lambda = coef_max_gust_89.5)

wind_daily_1419_tsibble$bc_max_gust_89.5 <- bcPower(
  wind_daily_1419_tsibble$Max_wind_gust_89.5,lambda = coef_max_gust_89.5)
#Check distribution of box-cox transformed weather data at 10 m height

old.par <-par()
par(mar =c(5.1, 4, 4.1, 1) + 0.1)# set left & right figure margins to 0
par(mfrow =c(3,2))

hist(wind_daily_1419_tsibble$bc_wind_direction,main="Wind direction distribution 2014-2019", 
     xlab="Direction (in degree)",ylab="Relative frequency",breaks =35 , # more columns
     col="lightblue", freq = F) # color the bars

hist(wind_daily_1419_tsibble$bc_mean_wind_10, main = "Wind speed distribution 2014-2019",
     xlab = "Mean wind speed (m/s)", ylab = "Relative frequency", breaks = 35,
     col = "green", freq = F)

hist(wind_daily_1419_tsibble$bc_max_gust_10, main = "Maximum wind gust distribution 2014-2019",
     xlab = "Maximum wind gust (m/s)", ylab = "Relative frequency", breaks = 35,
     col = "blue", freq = F)

hist(wind_daily_1419_tsibble$bc_temperature_10, main = "Temperature distribution 2014-2019",
     xlab = "Temperature (degree Celsius)", ylab = "Relative frequency", breaks = 35,
     col = "yellow", freq = F)

hist(wind_daily_1419_tsibble$bc_air_pressure_10, main = "Air pressure distribution 2014-2019",
     xlab = "Air pressure (kPa)", ylab = "Relative frequency", breaks = 35,
     col = "purple", freq = F)

hist(wind_daily_1419_tsibble$bc_relative_humidity, main = "Relative humidity",
     xlab = "Relative humidity (%)", ylab = "Relative frequency", breaks = 35,
     col= "orange", freq = F)

old.par
par(mfrow =c(1,1))

#Check distribution of weather data at 89.5m height after Box-Cox transformed
old.par <-par()
par(mar =c(5.1, 4, 4.1, 1) + 0.1)# set left & right figure margins to 0
par(mfrow =c(3,2))

hist(wind_daily_1419_tsibble$bc_wind_direction,main="Wind direction distribution 2014-2019", 
     xlab="Direction (in degree)",ylab="Relative frequency",breaks =35 , # more columns
     col="lightblue", freq = F) # color the bars

hist(wind_daily_1419_tsibble$bc_mean_wind_89.5, main = "Wind speed distribution 2014-2019",
     xlab = "Mean wind speed (m/s)", ylab = "Relative frequency", breaks = 35,
     col = "green", freq = F)

hist(wind_daily_1419_tsibble$bc_max_gust_89.5, main = "Maximum wind gust distribution 2014-2019",
     xlab = "Maximum wind gust (m/s)", ylab = "Relative frequency", breaks = 35,
     col = "blue", freq = F)

hist(wind_daily_1419_tsibble$bc_temperature_89.5, main = "Temperature distribution 2014-2019",
     xlab = "Temperature (degree Celsius)", ylab = "Relative frequency", breaks = 35,
     col = "yellow", freq = F)

hist(wind_daily_1419_tsibble$bc_air_pressure_89.5, main = "Air pressure distribution 2014-2019",
     xlab = "Air pressure (kPa)", ylab = "Relative frequency", breaks = 35,
     col = "purple", freq = F)

hist(wind_daily_1419_tsibble$bc_relative_humidity, main = "Relative humidity",
     xlab = "Relative humidity (%)", ylab = "Relative frequency", breaks = 35,
     col= "orange", freq = F)

old.par
par(mfrow =c(1,1))

wind_daily_1420_tsibble$bc_mean_wind_10 <- bcPower(
  wind_daily_1420_tsibble$Mean_wind_speed,lambda = coef_mean_wind_10)

wind_daily_1420_tsibble$bc_max_gust_10 <- bcPower(
  wind_daily_1420_tsibble$Max_wind_gust,lambda = coef_max_gust_10)

wind_daily_1420_tsibble$bc_air_pressure_10 <- bcPower(
  wind_daily_1420_tsibble$Air_pressure_10, lambda = coef_air_pressure_10)

wind_daily_1420_tsibble$bc_wind_direction <- bcPower(
  wind_daily_1420_tsibble$Wind_direction, lambda = coef_wind_direction)

wind_daily_1420_tsibble$bc_temperature_10 <- bcnPower(
  wind_daily_1420_tsibble$Temperature_10,lambda = coef_temperature_10[,1],
  gamma = coef_temperature_10[,2])

wind_daily_1420_tsibble$bc_relative_humidity <- bcPower(
  wind_daily_1420_tsibble$Relative_humidity, lambda = coef_relative_humidity)

wind_daily_1420_tsibble$bc_mean_wind_89.10 <- bcPower(
  wind_daily_1420_tsibble$Mean_wind_89.5, lambda = coef_mean_wind_89.5)

wind_daily_1420_tsibble$bc_max_gust_89.5 <- bcPower(
  wind_daily_1420_tsibble$Max_wind_gust_89.5, lambda = coef_max_gust_89.5)

wind_daily_1420_tsibble$bc_air_pressure_89.5 <- bcPower(
  wind_daily_1420_tsibble$Air_pressure_89.5, lambda = coef_air_pressure_89.5)

wind_daily_1420_tsibble$bc_temperature_89.5 <- bcnPower(
  wind_daily_1420_tsibble$Temperature_89.5,lambda = coef_temperature_89.5[,1],
  gamma = coef_temperature_89.5[,2])

wind_daily_2020_tsibble$bc_mean_wind_10 <- bcPower(
  wind_daily_2020_tsibble$Mean_wind_speed, lambda = coef_mean_wind_10)

wind_daily_2020_tsibble$bc_max_gust_10 <- bcPower(
  wind_daily_2020_tsibble$Max_wind_gust, lambda = coef_max_gust_10)

wind_daily_2020_tsibble$bc_wind_direction <- bcPower(
  wind_daily_2020_tsibble$Wind_direction, lambda = coef_wind_direction)

wind_daily_2020_tsibble$bc_air_pressure_10 <- bcPower(
  wind_daily_2020_tsibble$Air_pressure_10, lambda = coef_air_pressure_10)

wind_daily_2020_tsibble$bc_relative_humidity <- bcPower(
  wind_daily_2020_tsibble$Relative_humidity, lambda = coef_relative_humidity)

wind_daily_2020_tsibble$bc_temperature_10 <- bcnPower(
  wind_daily_2020_tsibble$Temperature_10,lambda = coef_temperature_10[,1],
  gamma = coef_temperature_10[,2])

wind_daily_2020_tsibble$bc_mean_wind_89.5 <- bcPower(
  wind_daily_2020_tsibble$Mean_wind_89.5, lambda = coef_mean_wind_89.5)

wind_daily_2020_tsibble$bc_max_gust_89.5 <- bcPower(
  wind_daily_2020_tsibble$Max_wind_gust_89.5, lambda = coef_max_gust_89.5)

wind_daily_2020_tsibble$bc_air_pressure_89.5 <- bcPower(
  wind_daily_2020_tsibble$Air_pressure_89.5, lambda = coef_air_pressure_89.5)

wind_daily_2020_tsibble$bc_temperature_89.5 <- bcnPower(
  wind_daily_2020_tsibble$Temperature_89.5, lambda = coef_temperature_89.5[,1],
  gamma = coef_temperature_89.5[,2])

write.csv(wind_daily_1419_tsibble , file = "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/bc_wind_data_1419.csv")
write.csv(wind_daily_1420_tsibble, file = "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/bc_wind_data_1420.csv")

#Correllation check
library(corrplot)
library(gplots)
#Corrplot of weather data at 10 m height
wind_1419_10 <- cbind(wind_direction_tf,mean_wind_10_tf,max_gust_10_tf,air_pressure_10_tf,
      relative_humidity_tf,temperature_10_tf)

colnames(wind_1419_10) <- c("Wind_direction_tf","Mean_wind_speed_10_tf",
                                 "Max_gust_10_tf","Air_pressure_10_tf",
                            "Relative_humidity_bc","Temperature_10_tf")

df3 <- cor(wind_1419_10, use = "na.or.complete")
corrplot.mixed(df3,upper="ellipse" , tl.pos="lt", 
               upper.col = colorpanel (50, "red" , "gray60" , "blue4"))

#Corrplot of weather data at 89.5 m height
wind_1419_89.5 <- cbind(wind_direction_tf, mean_wind_89.5_tf, Max_wind_gust_89.5_tf,
                        air_pressure_89.5_tf, relative_humidity_tf, temperature_89.5_tf)

colnames(wind_1419_89.5)<- c("Wind_direction_tf","Mean_wind_speed_89.5_tf",
                             "Max_wind_gust_89.5_tf", "Air_pressure_89.5_tf",
                             "Relative_humidity_tf", "Temperature_89.5_tf")

df4 <- cor(wind_1419_89.5, use = "na.or.complete")
corrplot.mixed(df4, upper="ellipse" , tl.pos="lt", 
               upper.col = colorpanel (50, "red" , "gray60" , "blue4"))

#Correlation coefficient check
cor.test(wind_daily_1419_tsibble$bc_wind_direction,wind_daily_1419_tsibble$bc_mean_wind_10)
cor.test(wind_daily_1419_tsibble$bc_mean_wind_10, wind_daily_1419_tsibble$bc_temperature_10)
cor.test(wind_daily_1419_tsibble$bc_wind_direction, wind_daily_1419_tsibble$bc_mean_wind_89.5)
cor.test(wind_daily_1419_tsibble$Mean_wind_speed_89.5, wind_daily_1419_tsibble$bc_air_pressure_89.5)
cor.test(wind_daily_1419_tsibble$Mean_wind_speed_89.5, wind_daily_1419_tsibble$Temperature_89.5)

###Data decomposition for hourly time series
hourly_wind_comp_10 <- imp_1419_hourly_tsibble %>% select(datetime, Mean_wind_speed) %>%
  filter(year(datetime)>= 2015)%>% model(stl = STL(Mean_wind_speed)) %>% components()
  
autoplot(hourly_wind_comp_10)+labs(title = "STL decomposition of hourly mean wind speed at 10 m height")
write.csv(as.data.frame(hourly_wind_comp_10), file = "hourly_wind_comp_10.csv")

Hourly_trend_strength <- 1 - var(hourly_wind_comp_10$remainder)/var(
  hourly_wind_comp_10$trend+hourly_wind_comp_10$remainder)

Hourly_yearly_seasonal_strength <- 1 - var(hourly_wind_comp_10$remainder)/var(
  hourly_wind_comp_10$remainder+hourly_wind_comp_10$season_year)

Hourly_weekly_seasonal_strength <- 1 - var(hourly_wind_comp_10$remainder)/var(
  hourly_wind_comp_10$remainder+hourly_wind_comp_10$season_week)

Hourly_daily_seasonal_strength <- 1 - var(hourly_wind_comp_10$remainder)/var(
  hourly_wind_comp_10$remainder+hourly_wind_comp_10$season_day)

hourly_seasonal_trough_year <- hourly_wind_comp_10 %>% select(datetime,season_year) %>%
  mutate(Year = year(datetime),Month = month(datetime))%>%
  filter(season_year == min(season_year))

hourly_seasonal_peak_year <- hourly_wind_comp_10 %>% select(datetime,season_year) %>%
  mutate(Year = year(datetime), Month = month(datetime))%>%
  filter(season_year == max(season_year))

hourly_seasonal_trough_week <- hourly_wind_comp_10 %>% select(datetime,season_week) %>% 
  mutate(Year = year(datetime), Month = month(datetime), Day = day(datetime))%>%
  filter(season_week == min(season_week))

hourly_seasonal_peak_week <- hourly_wind_comp_10 %>% select(datetime,season_week) %>% 
  mutate(Year = year(datetime), Month = month(datetime), Day = day(datetime))%>%
  filter(season_week == max(season_week))

hourly_seasonal_trough_day <- hourly_wind_comp_10 %>% select(datetime,season_day) %>% 
  mutate(Year = year(datetime), Month = month(datetime), Day = day(datetime))%>%
  filter(season_day == min(season_day))

hourly_seasonal_peak_day <- hourly_wind_comp_10 %>% select(datetime,season_day) %>% 
  mutate(Year = year(datetime), Month = month(datetime), Day = day(datetime))%>%
  filter(season_day == max(season_day))

###Data decomposition for daily time series
mean_wind_comp_10 <- wind_daily_1419_tsibble %>% select(Index, Mean_wind_speed) %>%
  model(stl = STL(Mean_wind_speed)) %>% components()

write.csv(as.data.frame(mean_wind_comp_10), file = "daily_wind_comp_10.csv")

autoplot(mean_wind_comp_10)+labs(title = "STL decomposition of average daily mean wind speed at 10 m height")

mean_wind_comp_89.5 <- wind_daily_1419_tsibble %>% select(Index, Mean_wind_speed_89.5)%>%
  model(stl = STL(Mean_wind_speed_89.5)) %>% components()

write.csv(as.data.frame(mean_wind_comp_89.5), file = "daily_wind_comp_89.5.csv")

autoplot(mean_wind_comp_89.5)+labs(title = "STL decomposition of average daily mean wind speed at 89.5 m height")

Temperature_10_comp <- wind_daily_1419_tsibble %>% select(Index, Temperature_10)%>%
  model(stl=STL(Temperature_10)) %>% components()
autoplot(Temperature_10_comp)
  
seasonal_trough_year <- mean_wind_comp_10 %>% select(Index,season_year) %>%
  mutate(Year = year(Index),Month = month(Index))%>%
  filter(season_year == min(season_year))

seasonal_peak_year <- mean_wind_comp_10 %>% select(Index,season_year) %>%
  mutate(Year = year(Index), Month = month(Index))%>%
  filter(season_year == max(season_year))

seasonal_peak_week <- mean_wind_comp_10 %>% select(Index,season_week) %>% 
  mutate(Year = year(Index), Month = month(Index), Day = day(Index))%>%
  filter(season_week == max(season_week))

seasonal_trough_week <- mean_wind_comp_10 %>% select(Index,season_week) %>% 
  mutate(Year = year(Index), Month = month(Index), Day = day(Index))%>%
  filter(season_week == min(season_week))

##Weekly and seasonaly strength
Daily_trend_strength <- 1 - var(mean_wind_comp_10$remainder)/var(
  mean_wind_comp_10$trend+mean_wind_comp_10$remainder)

Daily_yearly_seasonal_strength <- 1 - var(mean_wind_comp_10$remainder)/var(
  mean_wind_comp_10$remainder+mean_wind_comp_10$season_year)

Daily_weekly_seasonal_strength <- 1 - var(mean_wind_comp_10$remainder)/var(
  mean_wind_comp_10$remainder+mean_wind_comp_10$season_week)

#Boxplot of imputed weather indices per month

boxplot(wind_daily_1419_tsibble$Wind_direction ~ wind_daily_1419_tsibble$Month,
        horizontal= F,ylab=" Wind direction (in degree)" , xlab="Month" , las=1,
        main="Average daily wind direction by month")

boxplot(wind_daily_1419_tsibble$Mean_wind_speed ~ wind_daily_1419_tsibble$Month, 
        horizontal= F,ylab=" Mean Wind Speed (m/s)" , xlab="Month" , las=1,
        main="Average daily mean wind speed by month")

boxplot(wind_daily_1419_tsibble$Max_wind_gust ~ wind_daily_1419_tsibble$Month, 
        horizontal= F,ylab=" Maximum wind gust (m/s)" , xlab="Month" , las=1,
        main="Average daily maximum wind gust by month")

boxplot(wind_daily_1419_tsibble$Temperature ~ wind_daily_1419_tsibble$Month, 
        horizontal= F,ylab="Temperature (degree Celsius)" , xlab="Month" , las=1,
        main="Average daily temperature by month")

boxplot(wind_daily_1419_tsibble$Air_pressure ~ wind_daily_1419_tsibble$Month, 
        horizontal= F,ylab="Air pressure (kPa)" , xlab="Month" , las=1,
        main="Average daily air pressure by month")

boxplot(wind_daily_1419_tsibble$Relative_humidity ~ wind_daily_1419_tsibble$Month,
        horizontal= F,ylab="Relative humidity (%)" , xlab="Month" , las=1,
        main="Average relative humidity by month")

#Autocorrelation plot
library(forecast)
mean_wind_daily_1419_ts <- msts(wind_daily_1419_tsibble$Mean_wind_speed,
                                seasonal.periods = c(7,365.25),start = c(2014,1))

mean_wind_daily_2020_ts <- msts(wind_daily_2020_tsibble$Mean_wind_speed, start = c(2020,1),
                                seasonal.periods = c(7, 365.25))

mean_wind_daily_1420_ts <-  msts(wind_daily_1420_tsibble$Mean_wind_speed,
                                seasonal.periods = c(7,365.25),start = c(2014,1))

temperature_1419_10_ts <- msts(wind_daily_1419_tsibble$Temperature_10,
                            seasonal.periods = c(7,365.25), start = c(2014,1))

temperature_2020_10_ts <- msts(wind_daily_2020_tsibble$Temperature_10,
                               seasonal.periods = c(7,365.25), start = c(2020,1))

sum(is.na(mean_wind_daily_1420_ts))
sum(is.na(mean_wind_daily_2020_ts))
sum(is.na(mean_wind_daily_1419_ts))

ggAcf(wind_daily_1419_tsibble$Mean_wind_speed, lag.max = 1000)+
  ggtitle("ACF plot of average daily wind speed 2014-2019")
ggAcf(wind_daily_1419_tsibble$bc_mean_wind_10, lag.max = 1000)+
  ggtitle("ACF plot of the average daily mean wind speed after Box-Cox transformation 2014-2019")
ggAcf(wind_daily_1419_tsibble$Log_meanwind, lag.max = 1000)+
  ggtitle("ACF plot of the log-transformed average daily mean wind speed 2014-2019")
ggPacf(wind_daily_1419_tsibble$Mean_wind_speed, lag.max = 1000)+
  ggtitle("PACF plot of the average daily mean wind speed 2014-2019")
ggPacf(wind_daily_1419_tsibble$Log_meanwind, lag.max = 1000)+
  ggtitle("PACF plot of the log-transformed average daily mean wind speed 2014-2019")
ggPacf(wind_daily_1419_tsibble$bc_mean_wind_10, lag.max = 1000)+
  ggtitle("PACF plot of the average daily mean wind speed after Box-Cox transformation 2014-2019")

#-->MA(0,1,3) 
#dd_bc_wind_daily1419_ts_365 <- diff(dbc_wind_daily_1419_ts, lag = 7)
#ggAcf(dd_bc_wind_daily1419_ts_365, lag.max = 500) #cuts off at lag 1
#ggPacf(dd_bc_wind_daily1419_ts_365, lag.max = 500) #tails off
#--> SMA(0,1,1)

#11:15-15:51
#library(astsa)
#bc_1419_fit <- Arima(wind_daily_1419_tsibble$bc_mean_wind, order = c(0,1,3),
#                     seasonal = list(order = c(0,1,1),period = 345), lambda = NULL)
#bc_forecast <- forecast(bc_1419_fit, h = 366)
#plot(bc_forecast)
#checkresiduals(bc_1419_fit)
#bc_1419_fit_mean <- exp(log(1 + coef_mean_wind * bc_forecast$mean)/coef_mean_wind) 
#autoplot(bc_1419_fit_mean)
#bc_1419_fit_upper <- exp(log(1 + coef_mean_wind * bc_forecast$upper)/coef_mean_wind)
#bc_1419_fit_lower <- exp(log(1 + coef_mean_wind * bc_forecast$lower)/coef_mean_wind)
#write.csv(bc_1419_fit_mean, file = "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/bc_1419_sarima_mean.csv")
#write.csv(bc_1419_fit_upper,file = "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/bc_1419_sarima_upper.csv")
#write.csv(bc_1419_fit_lower, file = "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/bc_1419_sarima_lower.csv" )

####Fourier ARMA
#Best fit with manual Box-Cox Transformation
library(foreach) # for parallel processing
library(parallel)
library(doParallel) # for parallel processing
parallel::detectCores() #8-core CPUs
cores = detectCores()
registerDoParallel(cores = cores[1]-1)
bestfit <- list(aicc=Inf)
for(i in 1:3){for(j in 1:182){
  fit <- auto.arima(mean_wind_daily_1419_ts, lambda = coef_mean_wind_10,  
                    xreg=fourier(mean_wind_daily_1419_ts, K=c(i,j)), 
                    seasonal=FALSE, parallel = TRUE, stepwise = FALSE,
                    num.cores = cores - 1)
  if(fit$aicc < bestfit$aicc)
    bestfit <- fit
  else break;}}

#K=c(3,2) on mean wind speed
checkresiduals(bestfit)

fc1 <- forecast(bestfit, xreg=fourier(mean_wind_daily_1419_ts, K=c(3,2), h=366))

funct_1 <- function(x,h){forecast(bestfit, xreg = fourier(x, K = c(3,2),h))}

e1 <- tsCV(mean_wind_daily_1419_ts,h=366,forecastfunction = funct_1)

sqrt(mean(e1^2,na.rm = TRUE))

autoplot(fc1, xlab = "Year", ylab = "m/s")

fc1 %>% accuracy(mean_wind_daily_1420_ts, na.rm = TRUE)

fit2 <- auto.arima(mean_wind_daily_1419_ts, lambda = coef_mean_wind_10,  
                    xreg=fourier(mean_wind_daily_1419_ts, K=c(3,20)), 
                    seasonal=FALSE, parallel = TRUE, stepwise = FALSE,
                    num.cores = cores - 1)

checkresiduals(fit2)
fc2 <- forecast(fit2, xreg=fourier(mean_wind_daily_1419_ts, K=c(3,20), h=366))

funct_2 <-  function(x,h){forecast(fit2, xreg=fourier(x, K=c(3,20), h))}

e2 <- tsCV(mean_wind_daily_1419_ts,h=366, forecastfunction = funct_2)

sqrt(mean(e2^2, na.rm = TRUE))

accuracy(fc2,mean_wind_daily_1420_ts, na.rm=TRUE)

autoplot(fc2)

fc3 <- snaive(mean_wind_daily_1419_ts, h = 366, level = c(80, 95),biasadj = TRUE,
               lambda = coef_mean_wind_89.5) 

checkresiduals(fc3)

autoplot(fc3)
accuracy(fc3, mean_wind_daily_1420_ts, na.rm=TRUE)
funct_3 <- function(x,h){snaive(x,h,level = c(80,95),biasadj = TRUE, 
                                lambda = coef_mean_wind_89.5 )}
e3 <- tsCV(mean_wind_daily_1419_ts, h = 366, forecastfunction = funct_3)

sqrt(mean(e3^2, na.rm = TRUE))

bestfit3 <- list(aicc=Inf)
for(i in 1:3){for(j in 1:182){
  xreg = as.matrix(cbind(Fourier = fourier(mean_wind_daily_1419_ts, K=c(i,j)),
                         Temperature = temperature_1419_10_ts))
  fit <- auto.arima(mean_wind_daily_1419_ts, lambda = coef_mean_wind_10,  
                    xreg=xreg, 
                    seasonal=FALSE, parallel = TRUE, stepwise = FALSE,
                    num.cores = cores - 1)
  if(fit$aicc < bestfit3$aicc)
    bestfit3 <- fit
  else break;}} 

checkresiduals(bestfit3)

Temperature_10_2020_fit <- list(aicc=Inf)
for(i in 1:3){for(j in 1:182){
  fit <- auto.arima(temperature_1419_10_ts,  
                    xreg=fourier(temperature_1419_10_ts, K=c(i,j)), 
                    seasonal=FALSE, parallel = TRUE, stepwise = FALSE,
                    num.cores = cores - 1)
  if(fit$aicc < Temperature_10_2020_fit$aicc)
    Temperature_10_2020_fit <- fit
  else break;}}
checkresiduals(Temperature_10_2020_fit)
fc_temp_2020 <- forecast(Temperature_10_2020_fit,
                         xreg=fourier(temperature_1419_10_ts, K=c(1,1), h=366),
                         bootstrap = TRUE)
accuracy(fc_temp_2020,temperature_2020_10_ts, na.rm=TRUE)

Temperature_10_2020_fc <- msts(fc_temp_2020[["mean"]], start = c(2020,1),
                               seasonal.periods = c(7, 365.25))

fc4 <- forecast(bestfit3, xreg = as.matrix(cbind(Fourier = fourier(
  mean_wind_daily_1419_ts, K=c(3,2),h=366),Temperature = Temperature_10_2020_fc)))

accuracy(fc4, mean_wind_daily_1420_ts, na.rm=TRUE)
autoplot(fc4)

funct_4 <- function(x,h){
  forecast(bestfit3, xreg = as.matrix(cbind(Fourier = fourier(
    x, K=c(3,2),h),Temperature = Temperature_10_2020_fc)))
}

e4 <- tsCV(mean_wind_daily_1419_ts, h = 366, forecastfunction = funct_4)
sqrt(mean(e4^2, na.rm = TRUE))

write.csv(as.data.frame(fc1[["lower"]]),"C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/lower32.csv", row.names = FALSE)
write.csv(as.data.frame(fc1[["upper"]]),"C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/upper32.csv", row.names = FALSE)
write.csv(as.data.frame(fc1[["mean"]]),"C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/mean32.csv", row.names = FALSE)
write.csv(as.data.frame(mean_wind_daily_2020), file = "trueobs20.csv", 
          sep =",", row.names = T)
write.csv(as.data.frame(fc2[["lower"]]),"C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/lower320.csv", row.names = FALSE)
write.csv(as.data.frame(fc2[["upper"]]),"C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/upper320.csv", row.names = FALSE)
write.csv(as.data.frame(fc2[["mean"]]),"C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/mean320.csv", row.names = FALSE)

write.csv(as.data.frame(fc3[["mean"]]),"C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/meansanaive.csv", row.names = FALSE)
write.csv(as.data.frame(fc3[["lower"]]),"C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/lowersnaive.csv", row.names = FALSE)
write.csv(as.data.frame(fc3[["upper"]]),"C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/uppersnaive.csv", row.names = FALSE)

write.csv(as.data.frame(fc4[["upper"]]),"C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/uppertemp32.csv", row.names = FALSE)
write.csv(as.data.frame(fc4[["lower"]]),"C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/lowertemp32.csv", row.names = FALSE)
write.csv(as.data.frame(fc4[["mean"]]),"C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/meantemp32.csv", row.names = FALSE)


#To find Winkler score and CRPS
install.packages("Rcpp")#yes then no
library(fable)
library(fabletools)
library(feasts)
library(fpp3)

fit7 <- wind_daily_1419_tsibble %>% model(ARIMA(
  box_cox(Mean_wind_speed, lambda = coef_mean_wind_10)~PDQ(0,0,0)+pdq(d=0)+
    fourier(period = 7, K = 3)+fourier(period = 365.25, K = 2)))

report(fit7)

fc7 <- fit7 %>% forecast(new_data = wind_daily_2020_tsibble)

accuracy(fc7,data = wind_daily_1420_tsibble, 
         measures = list(distribution_accuracy_measures, point_accuracy_measures,
                         interval_accuracy_measures))

fit8 <- wind_daily_1419_tsibble %>% model(ARIMA(
  box_cox(Mean_wind_speed, lambda = coef_mean_wind_10)~PDQ(0,0,0)+pdq(d=0)+
    fourier(period = 7, K= 3)+fourier(period = 365.25, K = 20)))

report(fit8)

fit8 %>% gg_tsresiduals()
fc8 <- fit8 %>% forecast(new_data = wind_daily_2020_tsibble)
accuracy(fc8,data = wind_daily_1420_tsibble, 
         measures = list(distribution_accuracy_measures, point_accuracy_measures,
                         interval_accuracy_measures))
temp_fit_2 <- wind_daily_1419_tsibble %>% model(ARIMA(Temperature_10~PDQ(0,0,0)+
    fourier(period = 7, K= 1)+fourier(period = 365.25, K = 1)))
fc_temp_2 <- temp_fit_2 %>% forecast(h = 366, bootstrap = TRUE)

fit9 <- wind_daily_1419_tsibble %>% model(ARIMA(
  box_cox(Mean_wind_speed, lambda = coef_mean_wind_10)~PDQ(0,0,0)+
    fourier(period = 7, K= 3)+fourier(period = 365.25, K = 2)+Temperature_10))

report(fit9)

wind_daily_2020_tsibble_temp <- wind_daily_1419_tsibble %>% new_data(n=366)%>%
  left_join(fc_temp_2) %>% select(Index, Temperature_10,.mean) %>% rename(Distribution = Temperature_10, Temperature_10 = .mean)

wind_temperature_2020 <- bind_rows(wind_daily_1419_tsibble,wind_daily_2020_tsibble_temp)

fc9 <- fit9 %>% forecast(new_data = wind_temperature_2020)
accuracy(fc9, data = wind_daily_1420_tsibble, 
         measures = list(distribution_accuracy_measures, point_accuracy_measures,
                         interval_accuracy_measures))
autoplot(fc9)

fit10 <- wind_daily_1419_tsibble %>% model(SNAIVE(box_cox(Mean_wind_speed, lambda = coef_mean_wind_10)))
fc10 <- fit10 %>% forecast(h =366)
accuracy(fc10, data = wind_daily_1420_tsibble, 
         measures = list(distribution_accuracy_measures, point_accuracy_measures,
                         interval_accuracy_measures))

#####Wave height data cleaning and exploration
library(readr)
library(lubridate)
WAV_002_014 <- read_csv("WAV_002_014.csv")
str(WAV_002_014)
#Convert time zone from UTC to UTC+1 Amsterdam time
WAV_002_014$Date <- format(as.Date(as.character(WAV_002_014$time),format="%Y-%m-%d"))
WAV_002_014$Date <- format(as.POSIXct(WAV_002_014$Date,format="%Y-%m-%d"),"%Y-%m-%d")
WAV_002_014$Date <- as.Date(WAV_002_014$Date)
WAV_002_014$Month <- month(as.POSIXlt(WAV_002_014$Date, format="%Y/%m/%d"))
WAV_002_014$Year <- format(as.Date(WAV_002_014$Date), format = "%Y")
WAV_002_014$Year <- format(as.POSIXct(WAV_002_014$Year,format="%Y"),"%Y")
WAV_002_014$Year <- year(as.POSIXct(WAV_002_014$Year,format="%Y"))
WAV_002_014$Weekdays <- weekdays(WAV_002_014$Date)
WAV_002_014$Day <- as.numeric(format(as.Date(WAV_002_014$Date,format="%Y-%m-%d"), format = "%d"))
WAV_002_014$Weekdays <- factor(WAV_002_014$Weekdays, levels =c("Monday", "Tuesday",
                                                               "Wednesday",
                                                               "Thursday",
                                                               "Friday",
                                                               "Saturday",
                                                               "Sunday"),ordered =TRUE)

WAV_002_014$Hour <- format(as.POSIXct(WAV_002_014$time),format="%H")
WAV_002_014$Hour <- hour(as.POSIXct(WAV_002_014$Hour,format="%H"))
WAV_002_014$time <- as_datetime(WAV_002_014$time,tz="Europe/Amsterdam")
str(WAV_002_014)
summary(WAV_002_014)

#Check for missing values
library(imputeTS)
library(fpp3)
library(forecast)
library(xts)
statsNA(WAV_002_014$VHM0) #No missing values
hist(log(WAV_002_014$VHM0),main="Log-transformed wave height distribution", 
     xlab="Wave height distribution",ylab="Relative frequency",breaks =35 , # more columns
     col="lightblue", freq = F, cex.main = 1.5, cex.lab = 2)

#boxplot of wave data per month
boxplot(WAV_002_014$VHM0 ~ WAV_002_014$Month, horizontal= F,
        ylab="Wave height (m)" , xlab="Month" , las=1,
        main="Significant wave height per month")

#Create xts object for hourly wave height
a <- as.POSIXct(WAV_002_014$time,format="%Y-%m-%d %H:%M:%S")
wave_data_xts <- as.xts(WAV_002_014, order.by = a)  
wave_data_hourly_1820_tible <- wave_data_xts %>% fortify.zoo %>% as_tibble(.name_repair = "minimal")
wave_data_hourly_1820_tible$VHM0 <- as.numeric(wave_data_hourly_1820_tible$VHM0)

wave_data_hourly_1820_tible$Date <- as.Date(as.character(wave_data_hourly_1820_tible$Date),
                                            format = "%Y-%m-%d")
wave_data_hourly_1820_tible$Month <- as.numeric(wave_data_hourly_1820_tible$Month)
wave_data_hourly_1820_tible$Year <- as.numeric(wave_data_hourly_1820_tible$Year)
wave_data_hourly_1820_tible$Day <- as.numeric(wave_data_hourly_1820_tible$Day)
wave_data_hourly_1820_tible$Hour <- as.numeric(wave_data_hourly_1820_tible$Hour)
wave_data_hourly_1820_tible$Weekdays <- as.factor(wave_data_hourly_1820_tible$Weekdays)
wave_data_hourly_1820_tible$Key <- as.numeric(rownames(wave_data_hourly_1820_tible))
str(wave_data_hourly_1820_tible)
wave_data_hourly_1820_tsible <- wave_data_hourly_1820_tible %>% as_tsibble(index = Index)
hourly_wave_comp_1819 <- wave_data_hourly_1820_tsible %>% select(Index, VHM0, Year) %>%
  filter(Year < 2020)%>%model(stl = STL(VHM0)) %>% components()

autoplot(hourly_wave_comp_1819)+labs(title = "STL decomposition of hourly significant wave height 2018 - 2019")

#Hourly significant wave height has daily and weekly seasonality
Hourly_trend_wave_strength <- 1 - var(hourly_wave_comp_1819$remainder)/var(
  hourly_wave_comp_1819$trend+hourly_wave_comp_1819$remainder)

Hourly_daily_seasonal_wave_strength <- 1 - var(hourly_wave_comp_1819$remainder)/var(
  hourly_wave_comp_1819$remainder+hourly_wave_comp_1819$season_day)

Hourly_weekly_seasonal_wave_strength <- 1 - var(hourly_wave_comp_1819$remainder)/var(
  hourly_wave_comp_1819$remainder+hourly_wave_comp_1819$season_week)

Hourly_seasonal_trough_week_wave <- hourly_wave_comp_1819 %>% select(Index,season_week) %>%
  mutate(Year = year(Index),Month = month(Index))%>%
  filter(season_week == min(season_week))

Hourly_seasonal_peak_week_wave <- hourly_wave_comp_1819 %>% select(Index,season_week) %>%
  mutate(Year = year(Index), Month = month(Index))%>%
  filter(season_week == max(season_week))

Hourly_seasonal_trough_day_wave <- hourly_wave_comp_1819 %>% select(Index,season_day) %>% 
  mutate(Year = year(Index), Month = month(Index), Day = day(Index))%>%
  filter(season_day == min(season_day))

Hourly_seasonal_peak_day_wave <- hourly_wave_comp_1819 %>% select(Index,season_day) %>% 
  mutate(Year = year(Index), Month = month(Index), Day = day(Index))%>%
  filter(season_day == max(season_day))

#Neural network forecasting
install.packages("Rcpp")#yes then no
library(fable)
library(fabletools)
library(feasts)
library(fpp3)
library(tidyverse)
library(zoo)
#Renewable energy 
Energy1718 <- read_csv("Energy1718.csv")
Energy1718_tsibble <- Energy1718 %>% as_tsibble(index = MTU)
Energy1819 <- read_csv("Energy1819.csv")
Energy1819_tsibble <- Energy1819 %>% as_tsibble(index = MTU)
Energy1920 <- read_csv("Energy1920.csv")
Energy1920_tsibble <- Energy1920 %>% as_tsibble(index = MTU)
Energy2021 <- read_csv("Energy2021.csv")
Energy2021_tsibble <- Energy2021 %>% as_tsibble(index = MTU)

Energy1719 <- rbind(Energy1718,Energy1819)
Energy1720 <- rbind(Energy1719,Energy1920)
Energy1721 <- rbind(Energy1720,Energy2021)
Energy1721_tsibble <- Energy1721 %>% as_tsibble(index = MTU)
Energy1721_tsibble$`Wind Offshore  - Actual Aggregated [MW]` <- Energy1721_tsibble$`Wind Offshore  - Actual Aggregated [MW]`*1000
Energy1721_tsibble$`Wind Onshore  - Actual Aggregated [MW]` <- Energy1721_tsibble$`Wind Onshore  - Actual Aggregated [MW]`*1000
Energy1721_tsibble <- Energy1721_tsibble %>% 
  select(MTU, `Wind Offshore  - Actual Aggregated [MW]`)%>% filter_index("2017-05-08"~.)

#Gemini offshore wind power output per turbine
#Before 13/04/2020 Gemini power output was 62.6% total Dutch output, 
#after that Borselle 1,2 start to contribute their energy output per each turbine installed new
#until 7/8/2020, The Borselle 3,4 start to contribute their energy out per each turbine installed new
#Therefore, the energy output of Gemini wind park is 35% of total energy output from 13/04 to 7/8. From 
#7/8, the Gemini output is 24.6% total energy output 
Energy_1721_March <- Energy1721_tsibble %>% 
  select(MTU,`Wind Offshore  - Actual Aggregated [MW]`)%>% filter_index(~"2020-04-13")%>%
  mutate(Wind_power = `Wind Offshore  - Actual Aggregated [MW]`*0.626/150)
Energy_1721_April <- Energy1721_tsibble %>% 
  select(MTU,`Wind Offshore  - Actual Aggregated [MW]`)%>% filter_index("2020-04-14"~"2020-04-30")%>%
  mutate(Wind_power = `Wind Offshore  - Actual Aggregated [MW]`*0.57/150)
Energy_1721_May <- Energy1721_tsibble %>% 
  select(MTU,`Wind Offshore  - Actual Aggregated [MW]`)%>% filter_index("2020-05-01"~"2020-05-31")%>%
  mutate(Wind_power = `Wind Offshore  - Actual Aggregated [MW]`*0.52/150)
Energy_1721_Jun <- Energy1721_tsibble %>% 
  select(MTU,`Wind Offshore  - Actual Aggregated [MW]`)%>% filter_index("2020-06-01"~"2020-06-30")%>%
  mutate(Wind_power = `Wind Offshore  - Actual Aggregated [MW]`*0.48/150)
Energy_1721_Jul <- Energy1721_tsibble %>% 
  select(MTU,`Wind Offshore  - Actual Aggregated [MW]`)%>% filter_index("2020-07-01"~"2020-07-31")%>%
  mutate(Wind_power = `Wind Offshore  - Actual Aggregated [MW]`*0.44/150)
Energy_1721_Aug <- Energy1721_tsibble %>% 
  select(MTU,`Wind Offshore  - Actual Aggregated [MW]`)%>% filter_index("2020-08-01"~"2020-08-31")%>%
  mutate(Wind_power = `Wind Offshore  - Actual Aggregated [MW]`*0.37/150)
Energy_1721_Sep <- Energy1721_tsibble %>% 
  select(MTU,`Wind Offshore  - Actual Aggregated [MW]`)%>% filter_index("2020-09-01"~"2020-09-30")%>%
  mutate(Wind_power = `Wind Offshore  - Actual Aggregated [MW]`*0.31/150)
Energy_1721_Oct <- Energy1721_tsibble %>% 
  select(MTU,`Wind Offshore  - Actual Aggregated [MW]`)%>% filter_index("2020-10-01"~"2020-10-31")%>%
  mutate(Wind_power = `Wind Offshore  - Actual Aggregated [MW]`*0.26/150)
Energy_1721_end <- Energy1721_tsibble %>% 
  select(MTU,`Wind Offshore  - Actual Aggregated [MW]`)%>% filter_index("2020-11-01"~.)%>%
  mutate(Wind_power = `Wind Offshore  - Actual Aggregated [MW]`*0.246/150)
Gemini_1721 <- bind_rows(Energy_1721_March,Energy_1721_April,Energy_1721_May,
                         Energy_1721_Jun,Energy_1721_Jul,Energy_1721_Aug,
                         Energy_1721_Sep,Energy_1721_Oct,Energy_1721_end)

Gemini_1721$Wind_power <- Gemini_1721$Wind_power*0.25
Gemini_1721$Date <- as.Date(as.character(Gemini_1721$MTU),format = "%Y-%m-%d")
Gemini_1721 <- Gemini_1721 %>% rename(Index = MTU) 
summary(Gemini_1721)

hourly_weather_1720 <- imp_1419_hourly_tsibble %>% 
  select(datetime,Day,Mean_wind_speed,Mean_wind_speed_89.5, Temperature_10, Temperature_89.5)%>%
  filter(Day >= "2017-05-08")%>% rename(Index = datetime,Mean_wind_89.5 = Mean_wind_speed_89.5)
summary(hourly_weather_1720)

library(xts)
wind_data_xts <- xts(wind_data, order.by = wind_data$datetime, frequency = 24)
wind_data_xts_1119 <- wind_data_xts["20110101/20191231"]
wind_data_xts_20 <- wind_data_xts["2020"]
hourly_weather_20 <- na.locf(wind_data_xts_20, fromLast = TRUE)%>% fortify.zoo %>%
  as_tibble(.name_repair = "minimal")%>%
  select(Index, YYYYMMDD, Mean_wind_speed,Mean_wind_89.5, Temperature_10, Temperature_89.5)%>%
  as_tsibble(index = Index) %>% rename(Day =  YYYYMMDD)

hourly_weather_20$Day <- as.Date(as.character(hourly_weather_20$Day),format="%Y-%m-%d")
hourly_weather_20$Mean_wind_89.5 <- as.numeric(hourly_weather_20$Mean_wind_89.5)
hourly_weather_20$Mean_wind_speed <- as.numeric(hourly_weather_20$Mean_wind_speed)
hourly_weather_20$Temperature_10 <- as.numeric(hourly_weather_20$Temperature_10)
hourly_weather_20$Temperature_89.5 <- as.numeric(hourly_weather_20$Temperature_89.5)
summary(hourly_weather_20)

library(dplyr)
hourly_weather_1721 <- bind_rows(hourly_weather_1720,hourly_weather_20)
hourly_weather_energy_1721 <- full_join(hourly_weather_1721,Gemini_1721)
write.csv(as.data.frame(hourly_weather_energy_1721), "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/hourly_weather_energy_1721.csv", row.names = FALSE)
summary(hourly_weather_energy_1721)

#STL decomposition of hourly wind power 
hourly_power_comp_1721 <- hourly_weather_energy_1721 %>% select(Index, Wind_power, Date) %>%
  model(stl = STL(Wind_power)) %>% components()

autoplot(hourly_power_comp_1721)+labs(title = "STL decomposition of hourly wind power 2017-2020")
#Hourly wind power has daily, weekly, and yearly components
Hourly_trend_strength_power <- 1 - var(hourly_power_comp_1721$remainder)/var(
  hourly_power_comp_1721$trend+hourly_power_comp_1721$remainder)

Hourly_yearly_seasonal_strength_power <- 1 - var(hourly_power_comp_1721$remainder)/var(
  hourly_power_comp_1721$remainder+hourly_power_comp_1721$season_year)

Hourly_weekly_seasonal_strength_power <- 1 - var(hourly_power_comp_1721$remainder)/var(
  hourly_power_comp_1721$remainder+hourly_power_comp_1721$season_week)

Hourly_daily_seasonal_strength_power <- 1 - var(hourly_power_comp_1721$remainder)/var(
  hourly_power_comp_1721$remainder+hourly_power_comp_1721$season_day)

hourly_seasonal_trough_year_power <- hourly_power_comp_1721 %>% select(Index,season_year) %>%
  mutate(Year = year(Index),Month = month(Index))%>%
  filter(season_year == min(season_year))

hourly_seasonal_peak_year_power <- hourly_power_comp_1721 %>% select(Index,season_year) %>%
  mutate(Year = year(Index), Month = month(Index))%>%
  filter(season_year == max(season_year))

hourly_seasonal_trough_week_power <- hourly_power_comp_1721 %>% select(Index,season_week) %>% 
  mutate(Year = year(Index), Month = month(Index), Day = day(Index))%>%
  filter(season_week == min(season_week))

hourly_seasonal_peak_week_power <- hourly_power_comp_1721 %>% select(Index,season_week) %>% 
  mutate(Year = year(Index), Month = month(Index), Day = day(Index))%>%
  filter(season_week == max(season_week))

hourly_seasonal_trough_day_power <- hourly_power_comp_1721 %>% select(Index,season_day) %>% 
  mutate(Year = year(Index), Month = month(Index), Day = day(Index))%>%
  filter(season_day == min(season_day))

hourly_seasonal_peak_day_power <- hourly_power_comp_1721 %>% select(Index,season_day) %>% 
  mutate(Year = year(Index), Month = month(Index), Day = day(Index))%>%
  filter(season_day == max(season_day))

#Daily power genergation
daily_weather_energy_1721 <- hourly_weather_energy_1721 %>% index_by(Date) %>%
  summarise(Daily_mean_wind_10 = mean(Mean_wind_speed), 
            Daily_mean_wind_89.5 = mean(Mean_wind_89.5), 
            Daily_temperature = mean(Temperature_10), Daily_temperature_89.5 = mean(Temperature_89.5),
            Daily_power = mean(Wind_power))

daily_power_comp_1721 <- daily_weather_energy_1721 %>% select(Date, Daily_power)%>%
  model(stl = STL(Daily_power)) %>% components()
autoplot(daily_power_comp_1721)+labs(title = "STL decomposition of daily wind power 2017-2020")

summary(daily_weather_energy_1721)

Daily_trend_strength_power <- 1 - var(daily_power_comp_1721$remainder)/var(
  daily_power_comp_1721$trend+daily_power_comp_1721$remainder)

Daily_yearly_seasonal_strength_power <- 1 - var(daily_power_comp_1721$remainder)/var(
  daily_power_comp_1721$remainder+daily_power_comp_1721$season_year)

Daily_weekly_seasonal_strength_power <- 1 - var(daily_power_comp_1721$remainder)/var(
  daily_power_comp_1721$remainder+daily_power_comp_1721$season_week)

Daily_seasonal_trough_year_power <- daily_power_comp_1721 %>% select(Date,season_year) %>%
  mutate(Year = year(Date),Month = month(Date))%>%
  filter(season_year == min(season_year))

Daily_seasonal_peak_year_power <- daily_power_comp_1721 %>% select(Date,season_year) %>%
  mutate(Year = year(Date), Month = month(Date))%>%
  filter(season_year == max(season_year))

Daily_seasonal_trough_week_power <- daily_power_comp_1721 %>% select(Date,season_week) %>% 
  mutate(Year = year(Date), Month = month(Date), Day = day(Date))%>%
  filter(season_week == min(season_week))

Daily_seasonal_peak_week_power <- daily_power_comp_1721 %>% select(Date,season_week) %>% 
  mutate(Year = year(Date), Month = month(Date), Day = day(Date))%>%
  filter(season_week == max(season_week))

write.csv(as.data.frame(daily_weather_energy_1721), "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/daily_weather_energy_1721.csv", row.names = FALSE)

#Distribution of the wind power
hist(hourly_weather_energy_1721$Wind_power, main = "Hourly wind power distribution 2017-2021",
     xlab = "kwh", ylab = "Frequency", breaks = 35,
     col = "green", freq = F)
hist(daily_weather_energy_1721$Daily_power, main = "Average daily wind power distribution 2017-2021",
     xlab = "kwh", ylab = "Frequency", breaks = 35,
     col = "yellow", freq = F)
library(car)
coef_windpower_hourly <- coef(powerTransform(hourly_weather_energy_1721$Wind_power,family = "bcnPower"))
coef_windpower_daily <- coef(powerTransform(daily_weather_energy_1721$Daily_power,family = "bcPower"))

#Prediction of wind speed from in March 2020 hourly
install.packages("Rcpp")#yes then no
library(fabletools)
library(fable)
library(feasts)
library(fpp3)
library(tidyverse)
library(zoo)
library(car)
#Prediction of wind speed and power in March 28th March
hourly_weather_power_2703 <- hourly_weather_energy_1721 %>% filter_index(~"2020-03-27")%>%
  select(Index, Mean_wind_speed, Mean_wind_89.5, Temperature_10, Temperature_89.5, Wind_power)
weather_power_test_2803 <- hourly_weather_energy_1721 %>% filter_index("2020-03-28")%>%
  select(Index, Mean_wind_speed, Mean_wind_89.5, Temperature_10, Temperature_89.5, Wind_power)
set.seed(1234)
hourly_wind_fit_2703 <- hourly_weather_power_2703 %>% model(NNETAR(
  Mean_wind_speed, MaxNWts = 100000, scale_inputs = TRUE, future.seed = TRUE))#15h39-16h59/21h12-21h20
report(hourly_wind_fit_2703)
gg_tsresiduals(hourly_wind_fit_2703)
fc_wind_2803 <- hourly_wind_fit_2703 %>% forecast(h=24, bootstrap = TRUE)
accuracy(fc_wind_2803,weather_power_test_2803,
         measures = list(distribution_accuracy_measures, point_accuracy_measures,
                                                              interval_accuracy_measures))
fc_wind_withtime_2803 <- hourly_weather_power_2703 %>% new_data(24)%>%
  mutate(Mean_wind_89.5 = fc_wind_2803$.mean*1.19, Mean_wind_speed = fc_wind_2803$.mean )

hourly_weather_power_2803 <- bind_rows(hourly_weather_power_2703,fc_wind_withtime_2803)
set.seed(123)
windpower_2703_fit1 <- hourly_weather_power_2703 %>% model(NNETAR(
  Wind_power~Mean_wind_89.5, MaxNWts = 100000, scale_inputs = TRUE))#16h21-16h33
report(windpower_2703_fit1)
gg_tsresiduals(windpower_2703_fit1)
fc_power_2803_1 <- windpower_2703_fit1 %>% forecast(new_data = fc_wind_withtime_2803)
accuracy(fc_power_2803_1,weather_power_test_2803,
         measures = list(distribution_accuracy_measures, point_accuracy_measures,
                         interval_accuracy_measures))
fc_power_withtime_2803_1 <- hourly_weather_power_2703 %>% new_data(n=24)%>% 
  left_join(fc_power_2803_1)%>% select(Index, .mean, Mean_wind_speed, Mean_wind_89.5)%>%
  rename(Wind_power = .mean)

set.seed(1234)
windpower_2703_fit2 <- hourly_weather_power_2703 %>% model(NNETAR(
  Wind_power, MaxNWts = 100000, scale_inputs = TRUE))
report(windpower_2703_fit2)
gg_tsresiduals(windpower_2703_fit2)
fc_power_2803_2 <- windpower_2703_fit2 %>% forecast(h=24)
accuracy(fc_power_2803_2,weather_power_test_2803,
         measures = list(distribution_accuracy_measures, point_accuracy_measures,
                         interval_accuracy_measures))

fc_power_withtime_2803_2 <- hourly_weather_power_2703 %>% new_data(n=24)%>% 
  left_join(fc_power_2803_2)%>%select(Index, .mean)%>%
  rename(Wind_power = .mean)

write.csv(as.data.frame(fc_power_withtime_2803_2), "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/fc_power_withtime_2803_2.csv", row.names = FALSE)
write.csv(as.data.frame(hourly_weather_power_2803),"C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/hourly_weather_power_2803.csv", row.names = FALSE)

#Prediction of wind speed and power on 29th March
hourly_weather_power_2803 <- hourly_weather_energy_1721 %>% filter_index(~"2020-03-28")%>%
  select(Index, Mean_wind_speed, Mean_wind_89.5, Temperature_10, Temperature_89.5, Wind_power)
hourly_weather_power_2903_test <- hourly_weather_energy_1721 %>% filter_index("2020-03-29")%>%
  select(Index, Mean_wind_speed, Mean_wind_89.5, Temperature_10, Temperature_89.5, Wind_power)
set.seed(123)
hourly_wind_fit_2903 <- hourly_weather_power_2803 %>% model(NNETAR(
  Mean_wind_speed, MaxNWts = 100000, scale_inputs = TRUE))#15h39-16h53
report(hourly_wind_fit_2903)
gg_tsresiduals(hourly_wind_fit_2903)
3
fc_wind_2903 <- hourly_wind_fit_2903 %>% forecast(h=24, bootstrap = TRUE)
accuracy(fc_wind_2903,hourly_weather_power_2903_test, 
         measures = list(distribution_accuracy_measures, point_accuracy_measures,
                                                                      interval_accuracy_measures))

fc_wind_withtime_2903 <- hourly_weather_power_2803 %>% new_data(24)%>%
  mutate(Mean_wind_89.5 = fc_wind_2903$.mean*1.19, Mean_wind_speed = fc_wind_2903$.mean)

set.seed(1234)
windpower_2803_fit <- hourly_weather_power_2803 %>% model(NNETAR(
  Wind_power, MaxNWts = 100000, scale_inputs = TRUE))
report(windpower_2803_fit)
gg_tsresiduals(windpower_2803_fit)

fc_power_2903 <- windpower_2803_fit %>% forecast(h=24)
accuracy(fc_power_2903,hourly_weather_power_2903_test)
fc_power_withtime_2903 <- hourly_weather_power_2803 %>% new_data(24)%>%
  left_join(fc_power_2903)%>%select(Index, .mean)%>%
  rename(Wind_power = .mean)

write.csv(as.data.frame(fc_wind_withtime_2903), "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/fc_wind_withtime_2903.csv", row.names = FALSE)
write.csv(as.data.frame(fc_power_withtime_2903), "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/fc_power_withtime_2903.csv", row.names = FALSE)

#Prediction of wind speed and power on 30th March

hourly_weather_power_2903 <- hourly_weather_energy_1721 %>% filter_index(~"2020-03-29")%>%
  select(Index, Mean_wind_speed, Mean_wind_89.5, Temperature_10, Temperature_89.5, Wind_power)
weather_power_test_3003 <- hourly_weather_energy_1721 %>% filter_index("2020-03-30")%>%
  select(Index, Mean_wind_speed, Mean_wind_89.5, Temperature_10, Temperature_89.5, Wind_power)
set.seed(12345)
hourly_wind_fit_3003 <- hourly_weather_power_2903 %>% model(NNETAR(
  Mean_wind_speed, MaxNWts = 100000, scale_inputs = TRUE, n_nodes = 40, n_networks = 50))
report(hourly_wind_fit_3003)
gg_tsresiduals(hourly_wind_fit_3003)

fc_wind_3003 <- hourly_wind_fit_3003 %>% forecast(h=24, bootstrap = TRUE)
accuracy(fc_wind_3003,weather_power_test_3003 )
fc_wind_withtime_3003 <- hourly_weather_power_2903 %>% new_data(24)%>%
  mutate(Mean_wind_89.5 = fc_wind_3003$.mean*1.19, Mean_wind_speed = fc_wind_3003$.mean)

write.csv(as.data.frame(fc_wind_withtime_3003), "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/fc_wind_withtime_3003.csv", row.names = FALSE)

set.seed(12367)
windpower_3003_fit <- hourly_weather_power_2903 %>% model(NNETAR(
  Wind_power, MaxNWts = 100000, scale_inputs = TRUE, n_nodes = 30, n_networks = 50))
report(windpower_3003_fit)
gg_tsresiduals(windpower_3003_fit)
fc_power_3003 <- windpower_3003_fit %>% forecast(h=24, bootstrap = TRUE)
accuracy(fc_power_3003,weather_power_test_3003)
fc_power_withtime_3003 <- hourly_weather_power_2903 %>% new_data(24)%>%
  left_join(fc_power_3003)%>%select(Index, .mean)%>%
  rename(Wind_power = .mean) 
write.csv(as.data.frame(fc_power_withtime_3003), "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/fc_power_withtime_3003.csv", row.names = FALSE)

#Prediction of wind speed and power on 31st March
hourly_weather_power_3003 <- hourly_weather_energy_1721 %>% filter_index(~"2020-03-30")%>%
  select(Index, Mean_wind_speed, Mean_wind_89.5, Temperature_10, Temperature_89.5, Wind_power)
weather_power_test_3103 <- hourly_weather_energy_1721 %>% filter_index("2020-03-31")%>%
  select(Index, Mean_wind_speed, Mean_wind_89.5, Temperature_10, Temperature_89.5, Wind_power)
set.seed(12345)
hourly_wind_fit_3103 <- hourly_weather_power_3003 %>% model(NNETAR(
  Mean_wind_speed, MaxNWts = 100000, scale_inputs = TRUE,n_nodes = 40, n_networks = 50))
report(hourly_wind_fit_3103)
gg_tsresiduals(hourly_wind_fit_3103)
fc_wind_3103 <- hourly_wind_fit_3103 %>% forecast(h=24, bootstrap = TRUE)
accuracy(fc_wind_3103,weather_power_test_3103)
fc_wind_withtime_3103 <- hourly_weather_power_3003 %>% new_data(24)%>%
  mutate(Mean_wind_89.5 = fc_wind_3103$.mean*1.19, Mean_wind_speed = fc_wind_3103$.mean)
write.csv(as.data.frame(fc_wind_withtime_3103), "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/fc_wind_withtime_3103.csv", row.names = FALSE)

set.seed(1234)
windpower_3103_fit <- hourly_weather_power_3003 %>% model(NNETAR(
  Wind_power, MaxNWts = 100000, scale_inputs = TRUE))
report(windpower_3103_fit)
gg_tsresiduals(windpower_3103_fit)
fc_power_3103 <- windpower_3103_fit %>% forecast(h=24,bootstrap = TRUE)
accuracy(fc_power_3103,weather_power_test_3103 )
fc_power_withtime_3103 <- hourly_weather_power_3003 %>% new_data(24)%>%
  left_join(fc_power_3103)%>%select(Index, .mean)%>%
  rename(Wind_power = .mean) 
write.csv(as.data.frame(fc_power_withtime_3103), "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/fc_power_withtime_3103.csv", row.names = FALSE)

##Prediction of wind speed and power on 27th April
hourly_weather_power_2604 <- hourly_weather_energy_1721 %>% filter_index(~"2020-04-26")%>%
  select(Index, Mean_wind_speed, Mean_wind_89.5, Temperature_10, Temperature_89.5, Wind_power)
weather_power_test_2704 <- hourly_weather_energy_1721 %>% filter_index("2020-04-27")%>%
  select(Index, Mean_wind_speed, Mean_wind_89.5, Temperature_10, Temperature_89.5, Wind_power)

set.seed(12345)
hourly_wind_fit_2604 <- hourly_weather_power_2604  %>% model(NNETAR(
  Mean_wind_speed, MaxNWts = 100000, scale_inputs = TRUE))
report(hourly_wind_fit_2604)
gg_tsresiduals(hourly_wind_fit_2604)
fc_wind_2704 <- hourly_wind_fit_2604 %>% forecast(h=24, bootstrap = TRUE)
accuracy(fc_wind_2704,weather_power_test_2704)
fc_wind_withtime_2704 <- hourly_weather_power_2604 %>% new_data(24)%>%
  mutate(Mean_wind_89.5 = fc_wind_2704$.mean*1.19, Mean_wind_speed = fc_wind_2704$.mean)
write.csv(as.data.frame(fc_wind_withtime_2704), "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/fc_wind_withtime_2704.csv", row.names = FALSE)

set.seed(1234)
windpower_2604_fit <- hourly_weather_power_2604 %>% model(NNETAR(
  Wind_power, MaxNWts = 100000, scale_inputs = TRUE))
report(windpower_2604_fit)
gg_tsresiduals(windpower_2604_fit)
fc_power_2704 <- windpower_2604_fit %>% forecast(h=24, bootstrap =TRUE)
accuracy(fc_power_2704, weather_power_test_2704)
fc_power_withtime_2704 <- hourly_weather_power_2604 %>% new_data(24)%>%
  left_join(fc_power_2704)%>%select(Index, .mean)%>%
  rename(Wind_power = .mean) 
write.csv(as.data.frame(fc_power_withtime_2704), "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/fc_power_withtime_2704.csv", row.names = FALSE)

##Prediction of wind speed and power on 28th April
hourly_weather_power_2704 <- hourly_weather_energy_1721 %>% filter_index(~"2020-04-27")%>%
  select(Index, Mean_wind_speed, Mean_wind_89.5, Temperature_10, Temperature_89.5, Wind_power)
weather_power_test_2804 <- hourly_weather_energy_1721 %>% filter_index("2020-04-28")%>%
  select(Index, Mean_wind_speed, Mean_wind_89.5, Temperature_10, Temperature_89.5, Wind_power)
set.seed(1234)
hourly_wind_fit_2704 <- hourly_weather_power_2704 %>% model(NNETAR(
  Mean_wind_speed, MaxNWts = 100000, scale_inputs = TRUE, n_nodes = 40, n_networks = 40))
report(hourly_wind_fit_2704)
gg_tsresiduals(hourly_wind_fit_2704)
fc_wind_2804 <- hourly_wind_fit_2704 %>% forecast(h=24, bootstrap = TRUE)
accuracy(fc_wind_2804,weather_power_test_2804)
fc_wind_withtime_2804 <- hourly_weather_power_2704 %>% new_data(24)%>%
  mutate(Mean_wind_89.5 = fc_wind_2804$.mean*1.19, Mean_wind_speed = fc_wind_2804$.mean)
write.csv(as.data.frame(fc_wind_withtime_2804), "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/fc_wind_withtime_2804.csv", row.names = FALSE)

set.seed(123)
windpower_2704_fit <- hourly_weather_power_2704 %>% model(NNETAR(
  Wind_power, MaxNWts = 100000, scale_inputs = TRUE))
report(windpower_2704_fit)
gg_tsresiduals(windpower_2704_fit)
fc_power_2804 <- windpower_2704_fit %>% forecast(h = 24, bootstrap = TRUE)
accuracy(fc_power_2804,weather_power_test_2804 )

write.csv(as.data.frame(fc_wind_withtime_2804), "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/fc_power_withtime_2804.csv", row.names = FALSE)
fc_power_withtime_2804 <- hourly_weather_power_2704 %>% new_data(24)%>%
  left_join(fc_power_2804)%>%select(Index, .mean)%>%
  rename(Wind_power = .mean)
##Prediction of wind speed and power on 29th April
hourly_weather_power_2804 <- hourly_weather_energy_1721 %>% filter_index(~"2020-04-28")%>%
  select(Index, Mean_wind_speed, Mean_wind_89.5, Temperature_10, Temperature_89.5, Wind_power)
weather_power_test_2904 <- hourly_weather_energy_1721 %>% filter_index("2020-04-29")%>%
  select(Index, Mean_wind_speed, Mean_wind_89.5, Temperature_10, Temperature_89.5, Wind_power)
set.seed(1234)
hourly_wind_fit_2804 <- hourly_weather_power_2804 %>% model(NNETAR(
  Mean_wind_speed, MaxNWts = 100000, scale_inputs = TRUE, n_nodes = 40, n_networks = 40))
report(hourly_wind_fit_2804)
gg_tsresiduals(hourly_wind_fit_2804)
fc_wind_2904 <- hourly_wind_fit_2804 %>% forecast(h=24, bootstrap =TRUE)
accuracy(fc_wind_2904, weather_power_test_2904)
fc_wind_withtime_2904 <- hourly_weather_power_2804 %>% new_data(24)%>%
  mutate(Mean_wind_89.5 = fc_wind_2904$.mean*1.19, Mean_wind_speed = fc_wind_2904$.mean)
write.csv(as.data.frame(fc_wind_withtime_2904), "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/fc_wind_withtime_2904.csv", row.names = FALSE)

set.seed(123)
windpower_2904_fit <- hourly_weather_power_2804 %>% model(NNETAR(
  Wind_power, MaxNWts = 100000, scale_inputs = TRUE))
report(windpower_2904_fit)
gg_tsresiduals(windpower_2904_fit)
fc_power_2904 <- windpower_2904_fit %>% forecast(h=24, bootstrap = TRUE)
accuracy(fc_power_2904,weather_power_test_2904 )
fc_power_withtime_2904 <- hourly_weather_power_2804 %>% new_data(24)%>%
  left_join(fc_power_2904)%>%select(Index, .mean)%>%
  rename(Wind_power = .mean)
write.csv(as.data.frame(fc_power_withtime_2904), "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/fc_power_withtime_2904.csv", row.names = FALSE)

#wave height 29/03
hourly_wave_2803 <- wave_data_hourly_1820_tsible %>% filter_index(~"2020-03-28")
hourly_wave_2903_test <- wave_data_hourly_1820_tsible %>% filter_index("2020-03-29")
set.seed(123)
wave_2803_fit <- hourly_wave_2803 %>% model(NNETAR(
  VHM0, MaxNWts = 100000, scale_inputs = TRUE))
report(wave_2803_fit)
gg_tsresiduals(wave_2803_fit)
fc_wave_2903 <- wave_2803_fit %>% forecast(h=24)
accuracy(fc_wave_2903, hourly_wave_2903_test)
fc_wave_withtime_2903 <- hourly_wave_2803 %>% new_data(24)%>% 
  left_join(fc_wave_2903)%>% select(Index, .mean)%>% rename(VHM0 = .mean)
write.csv(as.data.frame(fc_wave_withtime_2903), "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/fc_wave_withtime_2903.csv", row.names = FALSE)

#wave height 30/03
hourly_wave_2903 <- wave_data_hourly_1820_tsible %>% filter_index(~"2020-03-29")
hourly_wave_3003_test <- wave_data_hourly_1820_tsible %>% filter_index("2020-03-30")
set.seed(1234)
wave_2903_fit <- hourly_wave_2903 %>% model(NNETAR(
  VHM0, MaxNWts = 100000, scale_inputs = TRUE))
report(wave_2903_fit)
gg_tsresiduals(wave_2903_fit)
fc_wave_3003 <- wave_2903_fit %>% forecast(h=24, bootstrap = TRUE)
accuracy(fc_wave_3003,hourly_wave_3003_test)
fc_wave_withtime_3003 <- hourly_wave_2903 %>% new_data(24)%>% 
  left_join(fc_wave_3003)%>% select(Index, .mean)%>% rename(VHM0 = .mean)
write.csv(as.data.frame(fc_wave_withtime_3003), "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/fc_wave_withtime_3003.csv", row.names = FALSE)


#wave height 31/03
hourly_wave_3003 <- wave_data_hourly_1820_tsible %>% filter_index(~"2020-03-30")
hourly_wave_3103_test <- wave_data_hourly_1820_tsible %>% filter_index("2020-03-31")
set.seed(1234)
wave_3003_fit <- hourly_wave_3003 %>% model(NNETAR(
  VHM0, MaxNWts = 100000, scale_inputs = TRUE))
report(wave_3003_fit)
gg_tsresiduals(wave_3003_fit)
fc_wave_3103 <- wave_3003_fit %>% forecast(h=24, bootstrap = TRUE)
accuracy(fc_wave_3103,hourly_wave_3103_test )
fc_wave_withtime_3103 <- hourly_wave_3003 %>% new_data(24)%>% 
  left_join(fc_wave_3103)%>% select(Index, .mean)%>% rename(VHM0 = .mean)
write.csv(as.data.frame(fc_wave_withtime_3103), "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/fc_wave_withtime_3103.csv", row.names = FALSE)


#wave height 27/09
hourly_wave_2604 <- wave_data_hourly_1820_tsible %>% filter_index(~"2020-04-26")
hourly_wave_2704_test <- wave_data_hourly_1820_tsible %>% filter_index("2020-04-27")
set.seed(1234)
wave_2704_fit <- hourly_wave_2604 %>% model(NNETAR(
  VHM0, MaxNWts = 100000, scale_inputs = TRUE))
report(wave_2704_fit)
gg_tsresiduals(wave_2704_fit)
fc_wave_2704 <- wave_2704_fit %>% forecast(h=24, bootstrap = TRUE)
accuracy(fc_wave_2704,hourly_wave_2704_test)
fc_wave_withtime_2704 <- hourly_wave_2604 %>% new_data(24)%>% 
  left_join(fc_wave_2704)%>% select(Index, .mean)%>% rename(VHM0 = .mean)
write.csv(as.data.frame(fc_wave_withtime_2704), "C:/Users/Pham Anh Tuan/OneDrive - Erasmus University Rotterdam/Data Science/Thesis/Model/fc_wave_withtime_2704.csv", row.names = FALSE)
