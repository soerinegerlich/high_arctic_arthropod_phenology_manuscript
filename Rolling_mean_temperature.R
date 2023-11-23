#Calculating rolling mean temperature 50 days back

library(tidyverse)
library(readxl) 
library(lubridate) #Functions to work with date-time data.
library(dplyr)
library(tidyr)
library(ggplot2)
#install.packages("hms")
library(hms)#Makes it easier to store and format time-of-day values based on the difftime class.
#install.packages("strucchange")
library(strucchange) #Testing, monitoring and dating structural changes in linear regression models.
#install.packages("gvlma")
library(gvlma) #For assessing linear model assumptions. 
#install.packages("reshape2")
library(reshape2) #Outdated, better to use Tidyr. Makes it easier to transform data between wide and long formats.
#install.packages("data.table")
library(data.table) #Extension of data.frame. Fast aggregation of large data.
library(zoo) #Calculating a rolling mean
library(cowplot)
library(sjPlot)

Air_temp <- read.csv("Data/Climate_data_Zackenberg/Air_temperature/View_ClimateBasis_Zackenberg_Data_Air_temperature_Air_temperature__200cm_@_60min_sample__DegreesC260520221737533841.csv",sep="\t",stringsAsFactors = FALSE, header = TRUE)


Air_temp = Air_temp %>% rename("HourTemp" = "AT...C.")

Air_temp$HourTemp <- as.numeric(Air_temp$HourTemp)

Air_temp %>%
  separate(Time, c("Hour", "Minute", 'Seconds'), ":") -> df

df$DOY <- yday(ymd(df$Date))
df$Month <- month(ymd(df$Date))
df$Year <- year(ymd(df$Date))

#Change columns to correct format for calculations
df$HourTemp[df$HourTemp == -9999] <-NA
df$HourTemp <-as.numeric(df$HourTemp)

df %>%
  group_by(Year,Month,DOY) %>% 
  summarise(DOYTemp=mean(HourTemp,na.rm=T),
            Min=min(HourTemp,na.rm=T),
            Max=max(HourTemp,na.rm=T)) -> dfair1

ggplot(dfair1, aes(x=DOY, y=Max)) + 
  geom_line() +
  ylab("Max temperature (degrees C)") +
  facet_wrap(~Year)


####ROLLING MEAN IS NOW MATCHED WITH AVERAGE PHENO EVENTS ACROSS YEARS FOR ALL TAXA AND PLOTS####


df_phen_event <-
  read.csv("Data/Dataset_for_GAM_NEW\\duration_ptd.csv", sep=",",
           stringsAsFactors = FALSE, header = TRUE)

#For air temperature
#df_mean_temp_air <-
# read.csv(
#  "Data\\dfair1_mean_temp.csv",
# sep = ",",
#  stringsAsFactors = FALSE,
# header = TRUE
#)

df_phen_event%>%
  group_by(SpeciesID, Plot)%>% 
  summarise(Onset_meanDOY=mean(Onset,na.rm=T),
            Peak_meanDOY=mean(Peak,na.rm=T),
            End_meanDOY=mean(End,na.rm=T),
            Onset_SD=sd(Onset,na.rm=T),
            Peak_SD=sd(Peak,na.rm=T),
            End_SD=sd(End,na.rm=T))-> df_phen_event_mean


df_phen_event_mean%>%
  group_by(SpeciesID, Plot)%>% 
  summarise(Onset_DOY=Onset_meanDOY-Onset_SD,
            Peak_DOY=Peak_meanDOY-Peak_SD,
            End_DOY=End_meanDOY-End_SD)-> df_phen_event_min


df_phen_event_min[,-1:-2] <- round(df_phen_event_min[,-1:-2], 0)

#Match with original dataframe
df_phen_event$Onset_DOY <- df_phen_event_min$Onset_DOY[match(paste0(df_phen_event$SpeciesID,df_phen_event$Plot),
                                                             paste0(df_phen_event_min$SpeciesID,df_phen_event_min$Plot))]

df_phen_event$Peak_DOY <- df_phen_event_min$Peak_DOY[match(paste0(df_phen_event$SpeciesID,df_phen_event$Plot),
                                                           paste0(df_phen_event_min$SpeciesID,df_phen_event_min$Plot))]

df_phen_event$End_DOY <- df_phen_event_min$End_DOY[match(paste0(df_phen_event$SpeciesID,df_phen_event$Plot),
                                                         paste0(df_phen_event_min$SpeciesID,df_phen_event_min$Plot))]


####Calculating the rolling mean (air temp) with 60 days####

dfair1$doymean<-rollmean(dfair1$DOYTemp,k=50,fill=NA,align="right")
#Problem with NA values in 1996 which can be fixed with rollapply
dfair1$doymean_apply_50<-rollapply(dfair1$DOYTemp,width=50,FUN=mean,na.rm = TRUE,fill=NA,align="right")

ggplot(dfair1, aes(x=DOY, y=doymean_apply_60)) + 
  geom_line() +
  ylab("Mean rolling 30 day temperature (degrees C)") +
  facet_wrap(~Year)


#Match rollmean temperature 60 days with doy for phen event

df_phen_event$Onset_Temp50 <- dfair1$doymean_apply_50[match(paste0(df_phen_event$Year,df_phen_event$Onset_DOY),
                                                             paste0(dfair1$Year,dfair1$DOY))]

df_phen_event$Peak_Temp50 <- dfair1$doymean_apply_50[match(paste0(df_phen_event$Year,df_phen_event$Peak_DOY),
                                                            paste0(dfair1$Year,dfair1$DOY))]

df_phen_event$End_Temp50 <- dfair1$doymean_apply_50[match(paste0(df_phen_event$Year,df_phen_event$End_DOY),
                                                           paste0(dfair1$Year,dfair1$DOY))]

#write.csv(df_phen_event, file="Data\\Air_temp_50_days_rolling.csv", row.names = FALSE)



####30 day rolling mean####

#Calculating the rolling mean (air temp)
dfair1$doymean<-rollmean(dfair1$DOYTemp,k=30,fill=NA,align="right")
#Problem with NA values in 1996 which can be fixed with rollapply
dfair1$doymean_apply_30<-rollapply(dfair1$DOYTemp,width=30,FUN=mean,na.rm = TRUE,fill=NA,align="right")

ggplot(dfair1, aes(x=DOY, y=doymean_apply_30)) + 
  geom_line() +
  ylab("Mean rolling 30 day temperature (degrees C)") +
  facet_wrap(~Year)

#write.csv(dfair1, file="Data\\dfair1_mean_temp.csv", row.names = FALSE)

####ROLLING MEAN IS NOW MATCHED WITH AVERAGE PHENO EVENTS ACROSS YEARS FOR ALL TAXA AND PLOTS####



#Match rollmean temperature 30 days with doy for phen event

df_phen_event$Onset_Temp30 <- dfair1$doymean_apply_30[match(paste0(df_phen_event$Year,df_phen_event$Onset_DOY),
                                                       paste0(dfair1$Year,dfair1$DOY))]

df_phen_event$Peak_Temp30 <- dfair1$doymean_apply_30[match(paste0(df_phen_event$Year,df_phen_event$Peak_DOY),
                                                      paste0(dfair1$Year,dfair1$DOY))]

df_phen_event$End_Temp30 <- dfair1$doymean_apply_30[match(paste0(df_phen_event$Year,df_phen_event$End_DOY),
                                                     paste0(dfair1$Year,dfair1$DOY))]

write.csv(df_phen_event, file="Data/Climate_data_Zackenberg\\Air_temp_30_days_rolling.csv", row.names = FALSE)


#Which temperature variable fits better


#Linear regression with 30 day rolling mean temp

df_phen_event <- read.csv("Data/Climate_data_Zackenberg/Air_temp_30_50_days_rolling.csv", sep=",",stringsAsFactors = FALSE, header = TRUE)


####30 day rolling mean soil temperature linear regression
df_summary_all<-data.frame(SpeciesID=character(),Plot=character(),Pheno_event=character(),Slope=numeric(),
                           SE=numeric(),Tvalue=numeric(),Pvalue=numeric(),
                           Count=numeric(),n=numeric(),AIC=numeric(),Rsquared=numeric(), 
                           Residual=numeric(),CI_lwr=numeric(),CI_upr=numeric())

for (i in unique(df_phen_event$SpeciesID)){
  print(i)
  df8b<-subset(df_phen_event,SpeciesID==i)
  for (j in unique(df8b$Plot)){
    print(j)
    df8a<-subset(df8b,Plot==j)
    df8sub<-subset(df8a,!is.na(Onset)&!is.na(Onset_Temp30))
    
    if(length(df8sub$Year)<5){ #sum(is.na) finder alle NA værdier. !is.na fjerner alle NA værdier i en vektor. Men denne kan vel ikke bruges her?
      #print(sum(is.na(df8$Onset)))
      #print(sum(!is.na(df8$Onset)))
      #length(df8$Onset[is.na(df8$Onset)]) #Denne kode viser alle værdier af ikke-NA værdier!!
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1], #sum(!is.na()) er antallet af ikke-Na værdier
                          Plot=df8sub$Plot[1],#[1] betyder at indeksere en vektor. I dete tilfælde får du det første element som output.
                          Pheno_event="Onset",
                          Slope=NA,
                          SE=NA,
                          Tvalue=NA,
                          Pvalue=NA,
                          Count=NA,
                          n=NA,
                          AIC=NA,
                          Rsquared=NA,
                          Residual=NA,
                          CI_lwr=NA,
                          CI_upr=NA)
    }
    else{ 
      mod1 <- lm(Onset ~ Onset_Temp30, data=df8sub)       
      AIC <- AIC(mod1)
      R.mod1 <- rsq(mod1, adj = TRUE)
      Residual1 <- sqrt(deviance(mod1)/df.residual(mod1))
      CI <- confint(mod1, level=0.95)
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1], 
                          Plot=df8sub$Plot[1],
                          Pheno_event="Onset",
                          Slope=summary(mod1)$coefficients[2],
                          SE=summary(mod1)$coefficients[4],
                          Tvalue=summary(mod1)$coefficients[6],
                          Pvalue=summary(mod1)$coefficients[8],
                          Count=sum(df8sub$TotalAbundance),
                          n=sum(!is.na(df8sub$Onset)),
                          AIC=AIC,
                          Rsquared=R.mod1,
                          Residual=Residual1,
                          CI_lwr=CI[2],
                          CI_upr=CI[4])
      df_summary_all<-bind_rows(df_summary_all,df_temp)
    }
    #plot(mod1)
    #}
    #} 
    
    
    if(length(df8sub$Year)<5){
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1],
                          Plot=df8sub$Plot[1],
                          Pheno_event="Peak",
                          Slope=NA,
                          SE=NA,
                          Tvalue=NA,
                          Pvalue=NA,
                          Count=NA,
                          n=NA,
                          AIC=NA,
                          Rsquared=NA,
                          Residual=NA,
                          CI_lwr=NA,
                          CI_upr=NA)
    }
    
    else{ 
      mod2 <- lm(Peak ~ Peak_Temp30, data=df8sub)
      AIC2 <- AIC(mod2)
      R.mod2 <- rsq(mod2, adj = TRUE)
      Residual2 <- sqrt(deviance(mod2)/df.residual(mod2))
      CI2 <- confint(mod2, level=0.95)
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1],
                          Plot=df8sub$Plot[1],
                          Pheno_event="Peak",
                          Slope=summary(mod2)$coefficients[2],
                          SE=summary(mod2)$coefficients[4],
                          Tvalue=summary(mod2)$coefficients[6],
                          Pvalue=summary(mod2)$coefficients[8],
                          Count=sum(df8sub$TotalAbundance),
                          n=sum(!is.na(df8sub$Onset)),
                          AIC=AIC2,
                          Rsquared=R.mod2,
                          Residual=Residual2,
                          CI_lwr=CI[2],
                          CI_upr=CI[4])
      df_summary_all<-bind_rows(df_summary_all,df_temp)
    }
    
    #plot(mod2)
    
    if(length(df8sub$Year)<5){
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1],
                          Plot=df8sub$Plot[1],
                          Pheno_event="End",
                          Slope=NA,
                          SE=NA,
                          Tvalue=NA,
                          Pvalue=NA,
                          Count=NA,
                          n=NA,
                          AIC=NA,
                          Rsquared=NA,
                          Residual=NA,
                          CI_lwr=NA,
                          CI_upr=NA)
    }
    
    else{ 
      
      mod3 <- lm(End ~ End_Temp30, data=df8sub)
      AIC3 <- AIC(mod3)
      R.mod3 <- rsq(mod3, adj = TRUE)
      Residual3 <- sqrt(deviance(mod3)/df.residual(mod3))
      CI3 <- confint(mod3, level=0.95)
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1],
                          Plot=df8sub$Plot[1],
                          Pheno_event="End",
                          Slope=summary(mod3)$coefficients[2],
                          SE=summary(mod3)$coefficients[4],
                          Tvalue=summary(mod3)$coefficients[6],
                          Pvalue=summary(mod3)$coefficients[8],
                          Count=sum(df8sub$TotalAbundance),
                          n=sum(!is.na(df8sub$Onset)),
                          AIC=AIC3,
                          Rsquared=R.mod3,
                          Residual=Residual3,
                          CI_lwr=CI[2],
                          CI_upr=CI[4])
      df_summary_all<-bind_rows(df_summary_all,df_temp)
    }
    #plot(mod3)
  }
}

#Save summary with all AIC values
#write_xlsx(df_summary_all, "Data/Temp_window_comparison\\df_summary_30_day_temp.xlsx", col_names = TRUE)



####50 day rolling mean soil temperature linear regression
df_summary_all_50<-data.frame(SpeciesID=character(),Plot=character(),Pheno_event=character(),Slope=numeric(),
                              SE=numeric(),Tvalue=numeric(),Pvalue=numeric(),
                              Count=numeric(),n=numeric(),AIC=numeric(),Rsquared=numeric(), 
                              Residual=numeric(),CI_lwr=numeric(),CI_upr=numeric())

for (i in unique(df_phen_event$SpeciesID)){
  print(i)
  df8b<-subset(df_phen_event,SpeciesID==i)
  for (j in unique(df8b$Plot)){
    print(j)
    df8a<-subset(df8b,Plot==j)
    df8sub<-subset(df8a,!is.na(Onset)&!is.na(Onset_Temp50))
    
    if(length(df8sub$Year)<5){ #sum(is.na) finder alle NA værdier. !is.na fjerner alle NA værdier i en vektor. Men denne kan vel ikke bruges her?
      #print(sum(is.na(df8$Onset)))
      #print(sum(!is.na(df8$Onset)))
      #length(df8$Onset[is.na(df8$Onset)]) #Denne kode viser alle værdier af ikke-NA værdier!!
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1], #sum(!is.na()) er antallet af ikke-Na værdier
                          Plot=df8sub$Plot[1],#[1] betyder at indeksere en vektor. I dete tilfælde får du det første element som output.
                          Pheno_event="Onset",
                          Slope=NA,
                          SE=NA,
                          Tvalue=NA,
                          Pvalue=NA,
                          Count=NA,
                          n=NA,
                          AIC=NA,
                          Rsquared=NA,
                          Residual=NA,
                          CI_lwr=NA,
                          CI_upr=NA)
    }
    else{ 
      mod1 <- lm(Onset ~ Onset_Temp50, data=df8sub)       
      AIC <- AIC(mod1)
      R.mod1 <- rsq(mod1, adj = TRUE)
      Residual1 <- sqrt(deviance(mod1)/df.residual(mod1))
      CI <- confint(mod1, level=0.95)
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1], 
                          Plot=df8sub$Plot[1],
                          Pheno_event="Onset",
                          Slope=summary(mod1)$coefficients[2],
                          SE=summary(mod1)$coefficients[4],
                          Tvalue=summary(mod1)$coefficients[6],
                          Pvalue=summary(mod1)$coefficients[8],
                          Count=sum(df8sub$TotalAbundance),
                          n=sum(!is.na(df8sub$Onset)),
                          AIC=AIC,
                          Rsquared=R.mod1,
                          Residual=Residual1,
                          CI_lwr=CI[2],
                          CI_upr=CI[4])
      df_summary_all_50<-bind_rows(df_summary_all_50,df_temp)
    }
    #plot(mod1)
    #}
    #} 
    
    
    if(length(df8sub$Year)<5){
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1],
                          Plot=df8sub$Plot[1],
                          Pheno_event="Peak",
                          Slope=NA,
                          SE=NA,
                          Tvalue=NA,
                          Pvalue=NA,
                          Count=NA,
                          n=NA,
                          AIC=NA,
                          Rsquared=NA,
                          Residual=NA,
                          CI_lwr=NA,
                          CI_upr=NA)
    }
    
    else{ 
      mod2 <- lm(Peak ~ Peak_Temp50, data=df8sub)
      AIC2 <- AIC(mod2)
      R.mod2 <- rsq(mod2, adj = TRUE)
      Residual2 <- sqrt(deviance(mod2)/df.residual(mod2))
      CI2 <- confint(mod2, level=0.95)
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1],
                          Plot=df8sub$Plot[1],
                          Pheno_event="Peak",
                          Slope=summary(mod2)$coefficients[2],
                          SE=summary(mod2)$coefficients[4],
                          Tvalue=summary(mod2)$coefficients[6],
                          Pvalue=summary(mod2)$coefficients[8],
                          Count=sum(df8sub$TotalAbundance),
                          n=sum(!is.na(df8sub$Onset)),
                          AIC=AIC2,
                          Rsquared=R.mod2,
                          Residual=Residual2,
                          CI_lwr=CI[2],
                          CI_upr=CI[4])
      df_summary_all_50<-bind_rows(df_summary_all_50,df_temp)
    }
    
    #plot(mod2)
    
    if(length(df8sub$Year)<5){
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1],
                          Plot=df8sub$Plot[1],
                          Pheno_event="End",
                          Slope=NA,
                          SE=NA,
                          Tvalue=NA,
                          Pvalue=NA,
                          Count=NA,
                          n=NA,
                          AIC=NA,
                          Rsquared=NA,
                          Residual=NA,
                          CI_lwr=NA,
                          CI_upr=NA)
    }
    
    else{ 
      
      mod3 <- lm(End ~ End_Temp50, data=df8sub)
      AIC3 <- AIC(mod3)
      R.mod3 <- rsq(mod3, adj = TRUE)
      Residual3 <- sqrt(deviance(mod3)/df.residual(mod3))
      CI3 <- confint(mod3, level=0.95)
      df_temp<-data.frame(SpeciesID=df8sub$SpeciesID[1],
                          Plot=df8sub$Plot[1],
                          Pheno_event="End",
                          Slope=summary(mod3)$coefficients[2],
                          SE=summary(mod3)$coefficients[4],
                          Tvalue=summary(mod3)$coefficients[6],
                          Pvalue=summary(mod3)$coefficients[8],
                          Count=sum(df8sub$TotalAbundance),
                          n=sum(!is.na(df8sub$Onset)),
                          AIC=AIC3,
                          Rsquared=R.mod3,
                          Residual=Residual3,
                          CI_lwr=CI[2],
                          CI_upr=CI[4])
      df_summary_all_50<-bind_rows(df_summary_all_50,df_temp)
    }
    #plot(mod3)
  }
}

df_summary_all$AIC_temp50 <- df_summary_all_50$AIC[match(paste0(df_summary_all$SpeciesID,df_summary_all$Plot,df_summary_all$Pheno_event),
                                                         paste0(df_summary_all_50$SpeciesID,df_summary_all_50$Plot,df_summary_all_50$Pheno_event))]

#Save summary with all AIC values
#write_xlsx(df_summary_all_50, "Data/Temp_window_comparison\\df_summary_50_day_temp.xlsx", col_names = TRUE)


AIC_comparison <- function(AIC, AIC_temp50) {
  AIC_diff <- AIC-AIC_temp50;
  
  if (AIC == AIC_temp50) {
    return("No difference")
  }
  else if (AIC>AIC_temp50) {
    return("Temperature50")
  }
  else if (AIC<AIC_temp50) {
    return("Temperature")
  } else {
    print("ERROR: no comparison")
    browser()
    return(NA)
  }
}
df_model_comparison <-
  data.frame(
    SpeciesID = character(),
    Plot = character(),
    Pheno_event = character(),
    Rsquare = numeric(),
    RSE = numeric(),
    AIC = numeric(),
    AIC_temp50 = numeric(),
    AIC_comparison = character()
  )


for (species in unique(df_summary_all$SpeciesID)) {
  print(species)
  df_summary_sub <- subset(df_summary_all, SpeciesID == species)
  for (plot in unique(df_summary_sub$Plot)) {
    print(plot)
    df_summary_sub2 <- subset(df_summary_sub, Plot == plot)
    for (phenoEvent in unique(df_summary_sub2$Pheno_event)) {
      df_summary_sub3 <-
        subset(df_summary_sub2, Pheno_event == phenoEvent)
      
      AIC_comparison_result <- NA
      
      if (!is.na(df_summary_sub3$AIC_temp50)) {
        AIC_comparison_result <-
          AIC_comparison(df_summary_sub3$AIC, df_summary_sub3$AIC_temp50)
      } else {
        print("Pheno event with no AIC_pwr values")
        AIC_comparison_result <- NA
      }
      
      
      df_temp <-
        data.frame(
          SpeciesID = species,
          Plot = plot,
          Pheno_event = phenoEvent,
          Rsquare = df_summary_sub3$Rsquared,
          RSE = df_summary_sub3$Residual,
          AIC = df_summary_sub3$AIC,
          AIC_temp50 = df_summary_sub3$AIC_temp50,
          AIC_comparison = AIC_comparison_result
        )
      
      df_model_comparison <- bind_rows(df_model_comparison, df_temp)
    }
  }
}

df_percentage_best_model <-
  data.frame(
    Plot = character(),
    Pheno_event = character(),
    Temp50 = numeric(),
    Temp = numeric()
  )


for (phenoEvent in unique(df_model_comparison$Pheno_event)) {
  for (plot in unique(df_model_comparison$Plot)) {
    print(paste("Pheno event: ", phenoEvent, " - Plot: ", plot, sep=""))
    
    df_snowmelt<-subset(df_model_comparison, Plot==plot & Pheno_event==phenoEvent)
    
    df_model_comparison_count <- nrow(df_snowmelt)
    df_model_comparison_temperature_count <- nrow(subset(df_snowmelt, AIC_comparison == "Temperature"))
    df_model_comparison_temperature50_count <- nrow(subset(df_snowmelt, AIC_comparison == "Temperature50"))
    df_model_comparison_no_difference_count <- nrow(subset(df_snowmelt, AIC_comparison == "No difference"))
    
    print(paste("df_snowmelt:", df_model_comparison_count))
    print(paste("df_model_comparison_temperature_count:", df_model_comparison_temperature_count))
    print(paste("df_model_comparison_temperature50_count:", df_model_comparison_temperature50_count))
    print(paste("df_model_comparison_no_difference_count:", df_model_comparison_no_difference_count))
    
    df_model_comparison_temperature_percentage <- (df_model_comparison_temperature_count / df_model_comparison_count) * 100
    print(paste("df_model_comparison_temperature_percentage: ", df_model_comparison_temperature_percentage, "%", sep=""))
    
    df_model_comparison_temperature50_percentage <- (df_model_comparison_temperature50_count / df_model_comparison_count) * 100
    print(paste("df_model_comparison_temperature50_percentage: ", df_model_comparison_temperature50_percentage, "%", sep=""))
    
    df_model_comparison_no_difference_percentage <- (df_model_comparison_no_difference_count / df_model_comparison_count) * 100
    print(paste("df_model_comparison_no_difference_percentage: ", df_model_comparison_no_difference_percentage, "%", sep=""))
    
    
    df_temp <-
      data.frame(
        Plot = plot,
        Pheno_event = phenoEvent,
        Temp = df_model_comparison_temperature_count,
        Temp50 = df_model_comparison_temperature50_count,
        No_difference = df_model_comparison_no_difference_count,
        Temperature_fraction = df_model_comparison_temperature_percentage,
        Temperature50_fraction = df_model_comparison_temperature50_percentage,
        No_difference_fraction = df_model_comparison_no_difference_percentage
      )
    
    df_percentage_best_model <- bind_rows(df_percentage_best_model, df_temp)
  }
}

require(writexl)
#write_xlsx(df_model_comparison, "Data/Temp_window_comparison\\df_model_comparison_temp_temp50.xlsx", col_names = TRUE)

#write_xlsx(df_percentage_best_model, "Data/Temp_window_comparison\\df_best_model_temp_temp50.xlsx", col_names = TRUE)

write_xlsx(df_phen_event, "Data/Climate_data_Zackenberg\\df_phen_event_air_new.xlsx", col_names = TRUE)


####Calculate for soil temperature


dfsoil1 <- read.table("Data/Climate_data_Zackenberg/Soil_temperature_0cm/ClimateBasisZackenberg_Climate_mainAWS_ET02cm000_60min.dat",sep="\t",stringsAsFactors = FALSE, header = FALSE, skip=3)
dfsoil2 <- read.table("Data/Climate_data_Zackenberg/Soil_temperature_5cm/ClimateBasisZackenberg_Climate_mainAWS_ET01cm005_60min.dat",sep="\t",stringsAsFactors = FALSE, header = FALSE, skip=3)
dfsoil3 <- read.table("Data/Climate_data_Zackenberg/Soil_temperature_10cm/ClimateBasisZackenberg_Climate_mainAWS_ET01cm010_60min.dat",sep="\t",stringsAsFactors = FALSE, header = FALSE, skip=3)


head(dfsoil1)

dfsoil1 = dfsoil1 %>% rename("Date"="V1", "Time"="V2", "HourTemp" = "V3") #Fejl vises ved Date. 
dfsoil2 = dfsoil2 %>% rename("Date"="V1", "Time"="V2", "HourTemp" = "V3")
dfsoil3 = dfsoil3 %>% rename("Date"="V1", "Time"="V2", "HourTemp" = "V3")


#Add sensor column. Inden datasættene forbindes skal der tilføjes en kolonne som angiver hvilke type data der er tale om.
dfsoil1$Sensor<-"Soil0"
dfsoil2$Sensor<-"Soil5"
dfsoil3$Sensor<-"Soil10"

dfsoil1$HourTemp <- as.numeric(dfsoil1$HourTemp)
dfsoil2$HourTemp <- as.numeric(dfsoil2$HourTemp)
dfsoil3$HourTemp <- as.numeric(dfsoil3$HourTemp)


#combine datafiles
#Bind_rows samler tabellerne under hinanden
df<-bind_rows(dfsoil1,dfsoil2,dfsoil3)

#Add columns: Hour, DOY, MONTH, Year
df %>%
  separate(Time, c("Hour", "Minute", 'Seconds'), ":")->df


df$DOY <- yday(ymd(df$Date))
df$Month <- month(ymd(df$Date))
df$Year <- year(ymd(df$Date))

#Change columns to correct format for calculations
df$HourTemp[df$HourTemp == -9999] <-NA
df$HourTemp <-as.numeric(df$HourTemp)

#----------------------------------------------------------- 
# Find mean soil temperature across the 3 soil sensors for 
# each hour-day-month-year
df.soil.agg <- aggregate(df[c("HourTemp")], 
                         by = df[c("Hour", "DOY", "Month", "Year")], FUN=mean, na.rm=T)
#Beregner middelværdien af de tre soil temp sensorer baseret på time, DOY, måned og Year. Tager højde for NA værdier
which(df.soil.agg$HourTemp == "NaN")
#----------------------------------------------------------- 


#Calculating mean soil temperature per day
df.soil.agg %>%
  group_by(Year,Month,DOY) %>% 
  summarise(DOYTemp=mean(HourTemp,na.rm=T),
            Min=min(HourTemp,na.rm=T),
            Max=max(HourTemp,na.rm=T)) -> df_mean_temp

####MAtch with sd of average DOY


df_phen_event <-
  read.csv("Data/Dataset_for_GAM_NEW\\duration_ptd.csv", sep=",",
           stringsAsFactors = FALSE, header = TRUE)

#For air temperature
#df_mean_temp_air <-
# read.csv(
#  "Data\\dfair1_mean_temp.csv",
# sep = ",",
#  stringsAsFactors = FALSE,
# header = TRUE
#)

df_phen_event%>%
  group_by(SpeciesID, Plot)%>% 
  summarise(Onset_meanDOY=mean(Onset,na.rm=T),
            Peak_meanDOY=mean(Peak,na.rm=T),
            End_meanDOY=mean(End,na.rm=T),
            Onset_SD=sd(Onset,na.rm=T),
            Peak_SD=sd(Peak,na.rm=T),
            End_SD=sd(End,na.rm=T))-> df_phen_event_mean


df_phen_event_mean%>%
  group_by(SpeciesID, Plot)%>% 
  summarise(Onset_DOY=Onset_meanDOY-Onset_SD,
            Peak_DOY=Peak_meanDOY-Peak_SD,
            End_DOY=End_meanDOY-End_SD)-> df_phen_event_min


df_phen_event_min[,-1:-2] <- round(df_phen_event_min[,-1:-2], 0)

#Match with original dataframe
df_phen_event$Onset_DOY <- df_phen_event_min$Onset_DOY[match(paste0(df_phen_event$SpeciesID,df_phen_event$Plot),
                                                             paste0(df_phen_event_min$SpeciesID,df_phen_event_min$Plot))]

df_phen_event$Peak_DOY <- df_phen_event_min$Peak_DOY[match(paste0(df_phen_event$SpeciesID,df_phen_event$Plot),
                                                           paste0(df_phen_event_min$SpeciesID,df_phen_event_min$Plot))]

df_phen_event$End_DOY <- df_phen_event_min$End_DOY[match(paste0(df_phen_event$SpeciesID,df_phen_event$Plot),
                                                         paste0(df_phen_event_min$SpeciesID,df_phen_event_min$Plot))]


####30 day rolling mean####

#Calculating the rolling mean (air temp)
#df.soil.agg$doymean<-rollmean(df.soil.agg$DOYTemp,k=30,fill=NA,align="right")
#Problem with NA values in 1996 which can be fixed with rollapply
df_mean_temp$doymean_apply_30<-rollapply(df_mean_temp$DOYTemp,width=30,FUN=mean,na.rm = TRUE,fill=NA,align="right")

ggplot(df_mean_temp, aes(x=DOY, y=doymean_apply_30)) + 
  geom_line() +
  ylab("Mean rolling 30 day temperature (degrees C)") +
  facet_wrap(~Year)

#write.csv(dfair1, file="Data\\dfair1_mean_temp.csv", row.names = FALSE)

####ROLLING MEAN IS NOW MATCHED WITH AVERAGE PHENO EVENTS ACROSS YEARS FOR ALL TAXA AND PLOTS####



#Match rollmean temperature 30 days with doy for phen event

df_phen_event$Onset_Temp30 <- df_mean_temp$doymean_apply_30[match(paste0(df_phen_event$Year,df_phen_event$Onset_DOY),
                                                            paste0(df_mean_temp$Year,df_mean_temp$DOY))]

df_phen_event$Peak_Temp30 <- df_mean_temp$doymean_apply_30[match(paste0(df_phen_event$Year,df_phen_event$Peak_DOY),
                                                           paste0(df_mean_temp$Year,df_mean_temp$DOY))]

df_phen_event$End_Temp30 <- df_mean_temp$doymean_apply_30[match(paste0(df_phen_event$Year,df_phen_event$End_DOY),
                                                          paste0(df_mean_temp$Year,df_mean_temp$DOY))]

write.csv(df_phen_event, file="Data/Climate_pheno\\Soil_temp_30_days_rolling.csv", row.names = FALSE)


