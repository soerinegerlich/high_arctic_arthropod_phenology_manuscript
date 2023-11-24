#Figure 1 - Spring and Summer temperature as well as snowmelt timing

required_packages <- c('ggplot2', 'readxl', 'dplyr')

#load all packages at once
lapply(required_packages, library, character.only=TRUE)

dfair4 <- read.csv("Data\\Air_seasonal.csv", sep = ",", header = TRUE)
dfsnowmelt_climatestation <- read_xlsx("Data/Snowmelt_climatestation.xlsx")

dfair4 %>%
  rename(Year = Year...1) -> dfair4

####For luft temp er der beregnet middeltemp for hver sæson. Beregnes for måned hvis aftale med Toke####
dfair4 <- dfair4[complete.cases(dfair4),]

Temp <- ggplot(data=dfair4, aes(Year, Summer))+
  geom_point(size = 3, color = "blue")+
  geom_smooth(method = "lm", size = 2, se = FALSE)+
  geom_line(size = 1, color = "blue")+
  geom_point(data = dfair4, aes(x=Year, y=Spring), size = 3, color = "black")+
  geom_line(data = dfair4, aes(x=Year, y=Spring), size = 1, color = "black")+
  geom_smooth(data = dfair4, aes(x=Year, y=Spring),method = "lm", size = 2, linetype = "dashed", color = "black", se = FALSE)+
  ylab("Air temperature (°C)")+
  xlab("")+
  ylim(-16,11)+
  theme_bw()+
  annotate("text", x = 2004, y = 10, label = "Summer temperature", size = 7)+
  annotate("text", x = 2004, y = -3, label = "Spring temperature", size = 7)+
  scale_y_continuous(breaks = c(-14,-10,-6,-2,2,6,10))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.text=element_text(size=20, color = "black"), axis.text.x = element_text(angle = 45, hjust = 1, color ="black"), 
        axis.title=element_text(size=20,face="bold", color = "black"),
        plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"))
#geom_hline(yintercept = c(-12,-6,0,6), linetype="dashed")


Snow <- ggplot(data=dfsnowmelt_climatestation, aes(x=Year, y=DOY)) +
  ylab("Day of Year")+
  xlab("")+
  geom_point(size = 3)+
  geom_line(size = 1)+ 
  geom_smooth(method="lm", linetype = "dashed", color = "black", size = 1.5, se = FALSE)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), axis.text = element_text(size = 20, color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1,size=20), plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"), 
        axis.title.y = element_text(size=20,vjust = 2, face = "bold"))+ 
  scale_x_continuous(breaks=c(1995, 2000, 2005, 2010, 2015, 2020))

cowplot::plot_grid(Snow, Temp, labels = c("(a)", "(b)"), label_size = 20, align = c("hv"))




