
#####CLEAN TEMPERATURE DATA AND CALCULATE AVERAGE SUMMER AND SPRING TEMPERATURES####

library(tidyverse)
library(readxl) 
#install.packages("hms")
library(hms)#Makes it easier to store and format time-of-day values based on the difftime class.
#install.packages("strucchange")
library(zoo) 
library(strucchange) #Testing, monitoring and dating structural changes in linear regression models.
#install.packages("gvlma")
library(gvlma) #For assessing linear model assumptions. 
#install.packages("reshape2")
#install.packages("data.table")
library(data.table) #Extension of data.frame. Fast aggregation of large data.
library(cowplot)

#install.Rtools()

#Read file:Air Temperature and Soil Temperature. Provide full path to file
dfair <-
  read.csv(
    "Data/temperature_data/Air_temperature/data/View_ClimateBasis_Zackenberg_Data_Air_temperature.csv",
    sep = "\t",
    stringsAsFactors = FALSE,
    header = TRUE
  )
#dfsoil1 <- read.csv("Data/Climate_data_Zackenberg/Soil_temperature_0cm/View_ClimateBasis_Zackenberg_Data_Soil_temperature_Soil_temperature__0cm.csv",sep="\t",stringsAsFactors = FALSE, header = TRUE)
#dfsoil2 <- read.csv("Data/Climate_data_Zackenberg/Soil_temperature_5cm/View_ClimateBasis_Zackenberg_Data_Soil_temperature_Soil_temperature__5cm.csv",sep="\t",stringsAsFactors = FALSE, header = TRUE)
#dfsoil3 <- read.csv("Data/Climate_data_Zackenberg/Soil_temperature_10cm/View_ClimateBasis_Zackenberg_Data_Soil_temperature_Soil_temperature.csv",sep="\t",stringsAsFactors = FALSE, header = TRUE)

dfsoil1 <-
  read.table(
    "Data/temperature_data/Soil_temperature_0cm/ClimateBasisZackenberg_Climate_mainAWS_ET02cm000_60min.dat",
    sep = "\t",
    stringsAsFactors = FALSE,
    header = FALSE,
    skip = 3
  )
dfsoil2 <-
  read.table(
    "Data/temperature_data/Soil_temperature_5cm/ClimateBasisZackenberg_Climate_mainAWS_ET01cm005_60min.dat",
    sep = "\t",
    stringsAsFactors = FALSE,
    header = FALSE,
    skip = 3
  )
dfsoil3 <-
  read.table(
    "Data/temperature_data/Soil_temperature_10cm/ClimateBasisZackenberg_Climate_mainAWS_ET01cm010_60min.dat",
    sep = "\t",
    stringsAsFactors = FALSE,
    header = FALSE,
    skip = 3
  )


dfair = dfair %>% rename("HourTemp" = "AT...C.") #Fejl vises ved Date.
#dfsoil1 = dfsoil1 %>% rename("Date"="ï..Date","HourTemp" = "Soil.temperature..0cm...60min.average..Â.C.") #Fejl vises ved Date.
#dfsoil2 = dfsoil2 %>% rename("Date"="ï..Date","HourTemp" = "Soil.temperature..5cm...60min.average..Â.C.")
#dfsoil3 = dfsoil3 %>% rename("Date"="ï..Date","HourTemp" = "Soil.temperature..10cm...60min.average..Â.C.")

dfsoil1 = dfsoil1 %>% rename("Date" = "V1",
                             "Time" = "V2",
                             "HourTemp" = "V3")
dfsoil2 = dfsoil2 %>% rename("Date" = "V1",
                             "Time" = "V2",
                             "HourTemp" = "V3")
dfsoil3 = dfsoil3 %>% rename("Date" = "V1",
                             "Time" = "V2",
                             "HourTemp" = "V3")

colnames(dfair) #Når script åbnes påny laver den uforstaaelige bogstaver om til ?

#Add sensor column. Inden datasættene forbindes skal der tilføjes en kolonne som angiver hvilke type data der er tale om.
dfair$Sensor <- "Air"
dfsoil1$Sensor <- "Soil0"
dfsoil2$Sensor <- "Soil5"
dfsoil3$Sensor <- "Soil10"

dfair$HourTemp <- as.numeric(dfair$HourTemp)
dfsoil1$HourTemp <- as.numeric(dfsoil1$HourTemp)
dfsoil2$HourTemp <- as.numeric(dfsoil2$HourTemp)
dfsoil3$HourTemp <- as.numeric(dfsoil3$HourTemp)


#combine datafiles
df <- bind_rows(dfair, dfsoil1, dfsoil2, dfsoil3)

#Add columns: Hour, DOY, MONTH, Year
df %>%
  separate(Time, c("Hour", "Minute", 'Seconds'), ":") -> df

df$DOY <- yday(ymd(df$Date))
df$Month <- month(ymd(df$Date))
df$Year <- year(ymd(df$Date))

#Change columns to correct format for calculations
df$HourTemp[df$HourTemp == -9999] <- NA
df$HourTemp <- as.numeric(df$HourTemp)

#Seasonal variation in soil temperature across all three soil temp series
df %>%
  subset(Sensor != "Air") %>%
  group_by(Sensor, Year, DOY) %>%
  summarise(DOYTemp = mean(HourTemp, na.rm = T)) %>%
  ggplot(aes(x = DOY, y = DOYTemp, colour = Sensor)) +
  geom_line() +
  ylab("Mean daily temperature (degrees C)") + ### of Soil measured at 0,5 and 10 cm
  facet_wrap( ~ Year)

#----------------------------------------------------------- 
# Find mean soil temperature across the 3 soil sensors for 
# each hour-day-month-year
df.soil <- df[df$Sensor!="Air",]
df.soil.agg <- aggregate(df.soil[c("HourTemp")], 
                         by = df.soil[c("Hour", "DOY", "Month", "Year")], FUN=mean, na.rm=T)
#----------------------------------------------------------- 

#Seasonal variation in soil temperature
#Middeljordtemperatur for DOY plottes. Ikke grupperet for sensor
df.soil.agg %>%
  group_by(Year,Month,DOY) %>% 
  summarise(DOYTemp=mean(HourTemp,na.rm=T),
            Min=min(HourTemp,na.rm=T),
            Max=max(HourTemp,na.rm=T),
            Dayofyear=mean(DOY,na.rm=T),
            FTCycleSpring=ifelse(Min<0&Max>0&Dayofyear<200,1,0),
            FTCycleFall=ifelse(Min<0&Max>0&Dayofyear>200,1,0),
            FTCycle=ifelse(Min<0&Max>0,1,0),
            Growingseason=ifelse(DOYTemp>2,DOY,NA))->df1
##Problems with NaN for 337 values. Caused by NA in raw data.
which(df.soil.agg$HourTemp == "NaN")


#Calculating mean soil temperature per day
df.soil.agg %>%
  group_by(Year,Month,DOY) %>% 
  summarise(DOYTemp=mean(HourTemp,na.rm=T),
            Min=min(HourTemp,na.rm=T),
            Max=max(HourTemp,na.rm=T)) -> df_mean_temp

#Problem with NA values in 1996 day 224, 1999 day 223 and 2006 day 220 and 221
#Solve this by calculating a mean temperature from 7 days before and after
Temp <-
  c(
    6.8575,
    7.1022,
    5.1704,
    5.3754,
    5.9958,
    5.0339,
    4.5875,
    8.324,
    6.2072,
    5.7031,
    6.0521,
    6.0428,
    5.6092,
    4.9833
  )
df_mean_1996_224 <- data.frame(Temp)
mean(Temp)
Temp1 <-
  c(
    6.8651,
    7.9425,
    8.4319,
    7.2511,
    9.1071,
    7.5021,
    5.0082,
    5.7095,
    4.7331,
    4.7675,
    5.144,
    4.4225,
    4.2894,
    3.5032
  )
df_mean_1999_223 <- data.frame(Temp1)
mean(Temp1)
Temp2 <-
  c(
    9.645,
    10.6308,
    11.0878,
    9.3067,
    8.9128,
    7.8844,
    6.9271,
    7.2387,
    7.3506,
    7.3561,
    8.6874,
    9.1965,
    9.0997,
    8.2647
  )
df_mean_2006_220 <- data.frame(Temp2)
mean(Temp2)

df_mean_temp$DOYTemp[which(df_mean_temp$Year == "1996" &
                             df_mean_temp$DOY == "224")] <- 5.931743
df_mean_temp$DOYTemp[which(df_mean_temp$Year == "1999" &
                             df_mean_temp$DOY == "223")] <- 6.048371
df_mean_temp$DOYTemp[which(df_mean_temp$Year == "2006" &
                             df_mean_temp$DOY == "220")] <- 8.684879
df_mean_temp$DOYTemp[which(df_mean_temp$Year == "2006" &
                             df_mean_temp$DOY == "221")] <- 8.684879

#Calculating the rolling mean with 30 days. In the final analysis, air temperature
#is used, but we also test with soil temperature 
#The temperatures are matched with mean phenological events in another manuscript 
#See air temperature calculation below
df_mean_temp$doymean <-
  rollmean(df_mean_temp$DOYTemp,
           k = 30,
           fill = NA,
           align = "right")

#Visualise

ggplot(df_mean_temp, aes(x = DOY, y = doymean)) +
  geom_line() +
  ylab("Mean rolling 30 day temperature (degrees C)") +
  facet_wrap( ~ Year)


#write.csv(df_mean_temp, file = "Data/Climate_data_Zackenberg\\df_mean_temp.csv", row.names = FALSE)


#Air temperature calculations
df %>%
  subset(Sensor == "Air") %>%
  group_by(Year, Month, DOY) %>%
  summarise(
    DOYTemp = mean(HourTemp, na.rm = T),
    Min = min(HourTemp, na.rm = T),
    Max = max(HourTemp, na.rm = T)
  ) -> dfair1

ggplot(dfair1, aes(x = DOY, y = Max)) +
  geom_line() +
  ylab("Max temperature (degrees C)") +
  facet_wrap( ~ Year)

#Calculating the rolling mean (air temp) which is the temperature used
#to model the effects of temperature on phenology.
# See script called "Rolling mean temperature"
dfair1$doymean <- rollmean(dfair1$DOYTemp,
                           k = 30,
                           fill = NA,
                           align = "right")

ggplot(dfair1, aes(x = DOY, y = doymean)) +
  geom_line() +
  ylab("Mean rolling 30 day temperature (degrees C)") +
  facet_wrap( ~ Year)

#write.csv(dfair1, file="Data/Climate_data_Zackenberg\\dfair1_mean_temp.csv", row.names = FALSE)

#Seasonal temperature for mean soil temp data
df1 %>%
  ggplot(aes(x = DOY, y = DOYTemp)) +
  geom_line() +
  ylab("Mean daily temperature (degrees C)") + ### of Soil measured at 0,5 and 10 cm
  facet_wrap( ~ Year)

####Add seasons to the df1 dataframe####
## Summer=JJA, Spring=April-May, Fall=Sep-Oct., Winter=Nov to March like in the RSOS paper
Seasons <-
  data.frame(Month = c(seq(1:12)),
             Season = c(
               rep("Winter", 3),
               rep("Spring", 2),
               rep("Summer", 3),
               rep("Autumn", 2),
               rep("Winter", 2)
             ))
df1$Season <- Seasons$Season[match(df1$Month, Seasons$Month)]

df1 %>%
  subset(Season != 'Winter') %>%
  group_by(Year, Season) %>%
  summarise(SeasonTemp = mean(DOYTemp, na.rm = T)) %>%
  spread(key = Season, value = SeasonTemp) -> df7

###Calculate mean Temp for spring and summer months
df1 %>%
  subset(4 == Month |
           5 == Month |
           6 == Month | 7 == Month | 8 == Month | 9 == Month | 10 == Month) %>%
  group_by(Year, Month) %>%
  summarise(MonthTemp = mean(DOYTemp, na.rm = T)) %>%
  spread(key = Month, value = MonthTemp) %>%
  setnames(
    .,
    old = c('4', '5', '6', '7', '8', '9', '10') ,
    new = c('April', 'May', 'June', 'July', 'Aug', 'Sep', 'Oct')
  ) -> dfmonth

dfair1 %>%
  subset(4 == Month |
           5 == Month |
           6 == Month | 7 == Month | 8 == Month | 9 == Month | 10 == Month) %>%
  group_by(Year, Month) %>%
  summarise(MonthTemp = mean(DOYTemp, na.rm = T)) %>%
  spread(key = Month, value = MonthTemp) %>%
  setnames(
    .,
    old = c('4', '5', '6', '7', '8', '9', '10') ,
    new = c('April', 'May', 'June', 'July', 'Aug', 'Sep', 'Oct')
  ) -> dfmonthair

#write.csv(dfmonth, file = "Data/Climate_data_Zackenberg\\Temperature_monthly.csv", row.names=FALSE)

#Checking min and max temp
dfair1 %>%
  subset(4 == Month |
           5 == Month |
           6 == Month | 7 == Month | 8 == Month | 9 == Month | 10 == Month) %>%
  group_by(Year, Month) %>%
  summarise(MaxMonthTemp = max(Max, na.rm = T)) %>%
  spread(key = Month, value = MaxMonthTemp) %>%
  setnames(
    .,
    old = c('4', '5', '6', '7', '8', '9', '10') ,
    new = c('April', 'May', 'June', 'July', 'Aug', 'Sep', 'Oct')
  ) -> dfmaxmonthair

ggplot(dfmaxmonthair, aes(Year, July)) +
  geom_point() +
  geom_smooth(method = "lm")

#Checking min and max temp
dfair1 %>%
  subset(4 == Month |
           5 == Month |
           6 == Month | 7 == Month | 8 == Month | 9 == Month | 10 == Month) %>%
  group_by(Year, Month) %>%
  summarise(MinMonthTemp = min(Min, na.rm = T)) %>%
  spread(key = Month, value = MinMonthTemp) %>%
  setnames(
    .,
    old = c('4', '5', '6', '7', '8', '9', '10') ,
    new = c('April', 'May', 'June', 'July', 'Aug', 'Sep', 'Oct')
  ) -> dfminmonthair

ggplot(dfminmonthair, aes(Year, July)) +
  geom_point() +
  geom_smooth(method = "lm")

#Air temperature calculations
df %>%
  subset(Sensor == "Air") %>%
  group_by(Year, Month, DOY) %>%
  summarise(
    DOYTemp = mean(HourTemp, na.rm = T),
    Min = min(HourTemp, na.rm = T),
    Max = max(HourTemp, na.rm = T),
    Dayofyear = mean(DOY, na.rm = T),
    Growingseason = ifelse(DOYTemp > 2, DOY, NA)
  ) -> dfair1


dfair1$Season <- Seasons$Season[match(dfair1$Month, Seasons$Month)]

dfair1 %>%
  subset(Season != 'Winter') %>%
  group_by(Year, Season) %>%
  summarise(SeasonTemp = mean(DOYTemp, na.rm = T)) %>%
  spread(key = Season, value = SeasonTemp) -> dfair2


#write.csv(dfair2, file = "Data/Climate_data_Zackenberg\\Air_seasonal.csv", row.names=FALSE)

dfair4 <- read.csv("Data\\Air_seasonal.csv", sep = ",", header = TRUE)

dfair4 %>%
  rename(Year = Year...1) -> dfair4


dfair4 <- dfair4[complete.cases(dfair4),]

air_summer <- ggplot(dfair4, aes(Year, Summer)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", size = 2) +
  ylab("June-August average air temp (°C)") +
  xlab("") +
  ylim(2.5, 7.5) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 15, face = "bold"),
    axis.title.y = element_text(margin = margin(
      t = 0,
      r = 10,
      b = 0,
      l = 0
    ))
  ) +
  geom_hline(yintercept = c(3, 4, 5, 6, 7), linetype = "dashed")

lm <- lm(Summer ~ Year, dfair4)
summary(lm)

air_spring <- ggplot(dfair4, aes(Year, Spring)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm",
              size = 2,
              linetype = "dashed") +
  ylab("April-May average air temp (°C)") +
  xlab("") +
  ylim(-13, -5) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 15),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 15, face = "bold"),
    axis.title.y = element_text(margin = margin(
      t = 0,
      r = 10,
      b = 0,
      l = 0
    ))
  ) +
  geom_hline(yintercept = c(-12, -10, -8, -6), linetype = "dashed")

lm <- lm(Spring ~ Year...1, dfair4)
summary(lm)

plot_grid(air_spring, air_summer, labels = "AUTO")


