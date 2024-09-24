# Figure 1 for manuscript

df_peak <- subset(df8_Peak, df8_Peak$SpeciesID == "Muscidae" | df8_Peak$SpeciesID == "Chironomidae")

df_duration <- subset(df8_Duration, df8_Duration$SpeciesID == "Muscidae" | df8_Duration$SpeciesID == "Chironomidae")

Peak <- ggplot(df_peak, aes(x = Slope, y = Plot, fill = Pvalue < 0.051 | Pvalue == 0.7))+
  xlab("")+
  scale_fill_manual(name = 'Pvalue < 0.05', values = setNames(c('red', 'blue'), c(T,F)))+
  #scale_shape(guide = "none")+
  coord_cartesian(xlim=c(-2,2))+
  geom_crossbar(aes(xmin=Slope-SE, xmax=Slope+SE, y = Plot), alpha = 0.5, linewidth = 0.02, width = 0.5)+
  geom_point(size = 0.3)+
  facet_grid(SpeciesID ~ ., drop = TRUE, scales = "free_y", space = "free")+
  theme(strip.text.y = element_blank(), panel.background = element_rect(fill = "white"), panel.spacing = unit(.01, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.2), axis.title.y = element_blank(),legend.position="none",
        axis.title.x = element_blank(), plot.margin = unit(c(0.5,0.1,1,1.5), "cm"),
        axis.text = element_text(size = 4))+
  geom_vline(xintercept = 0)

Duration <- ggplot(df_duration, aes(x = Slope, y = Plot, fill = Pvalue < 0.051 | Pvalue == 0.5))+
  xlab("")+
  scale_fill_manual(name = 'Pvalue < 0.05', values = setNames(c('red', 'blue'), c(T,F)))+
  #scale_shape(guide = "none")+
  coord_cartesian(xlim=c(-2,2))+
  geom_crossbar(aes(xmin=Slope-SE, xmax=Slope+SE, y = Plot), alpha = 0.5,  linewidth = 0.02, width = 0.5)+
  geom_point(size = 0.3)+
  facet_grid(SpeciesID ~ ., drop = TRUE, scales = "free_y", space = "free")+
  theme(strip.text.y = element_text(angle = 0, size = 5), panel.background = element_rect(fill = "white"), panel.spacing = unit(.01, "lines"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.2), axis.title.y = element_blank(), 
        axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.position="none",
        axis.title.x = element_blank(), plot.margin = unit(c(0.5,0.1,1,0.1), "cm"),
        axis.text = element_text(size = 4))+
  geom_vline(xintercept = 0)


ggarrange(Peak, Duration, labels = c("A) Peak", "B) Duration"), hjust = -0.1, vjust = 0.1, ncol = 2, nrow = 1)+
  theme(plot.margin = margin(13,7,13,7, "cm"))



df_test <- subset(df_Peak, df_Peak$SpeciesID == "Muscidae" | df_Peak$SpeciesID == "Chironomidae" | df_Peak$SpeciesID == "Lycosidae" | df_Peak$SpeciesID == "Ichneumonidae")
df_test <- subset(df_test, df_test$Habitat == "Mesic heath")

df_test1 <- subset(df_Duration, df_Duration$SpeciesID == "Muscidae" | df_Duration$SpeciesID == "Chironomidae"| df_Duration$SpeciesID == "Lycosidae" | df_Duration$SpeciesID == "Ichneumonidae")
df_test1 <- subset(df_test1, df_test1$Habitat == "Mesic heath")

Peak_snow <- ggplot(df_test, aes(x = Slope1, y = SpeciesID, xmin = CI_lwr1, xmax = CI_upr1, color = cut(Pvalue1, breaks = c(0, 0.05, 0.099, 1))))+
  xlab("Shift in peak activity (days per shifted snowmelt day)")+
  ylab("")+
  #scale_colour_discrete(name = "Taxon")+
  #scale_shape(guide = "none")+
  geom_pointrange(size = 0.2, alpha = 0.5, position = position_dodge(width = 0.1))+
  #scale_size(range=c(0.2,1))+
  geom_vline(xintercept=0, lty=2)+
  scale_color_manual(values = c('red', 'orange', 'blue'),
                     limits = c('(0,0.05]', '(0.05,0.099]', '(0.099,1]'))+
  scale_x_continuous(limits=c(-1.5,1.5), breaks = c(-1,0,1), oob=scales::rescale_none)+
  #geom_boxplot(varwidth = TRUE)+
  facet_grid(~ Habitat, scales = "free_x")+
  #scale_color_viridis_d(guide = "none")+
  geom_vline(xintercept = 0)+
  theme(panel.background = element_rect(fill = "white"), panel.spacing = unit(0.05, "lines"),
        axis.text.x = element_text(face = "bold", size = 7, color = "black"), 
        strip.text.x = element_text(angle = 0, size = 5, color = "black"),
        axis.title.x = element_text(face = "bold", size = 7, color = "black", vjust = 0.5), 
        axis.text.y = element_text(face = "bold", size = 5, color = "black"), panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        strip.background = element_rect(colour = "black", fill = "white"))


Duration_snow <- ggplot(df_test1, aes(x = Slope1, y = SpeciesID, xmin = CI_lwr1, xmax = CI_upr1, color = cut(Pvalue1, breaks = c(0, 0.05, 0.099, 1))))+
  xlab("Change in duration of activity (days per shifted snowmelt day)")+
  ylab("")+
  #scale_colour_discrete(name = "Taxon")+
  #scale_shape(guide = "none")+
  geom_pointrange(size = 0.2, alpha = 0.5, position = position_dodge(width = 0.05))+
  #scale_size(range=c(0.2,1))+
  geom_vline(xintercept=0, lty=2)+
  scale_color_manual(values = c('red', 'orange', 'blue'),
                     limits = c('(0,0.05]', '(0.05,0.099]', '(0.099,1]'))+
  scale_x_continuous(limits=c(-1.5,1.5), breaks = c(-1,0,1), oob=scales::rescale_none)+
  #geom_boxplot(varwidth = TRUE)+
  facet_grid(~ Habitat, scales = "free_x")+
  #scale_color_viridis_d(guide = "none")+
  geom_vline(xintercept = 0)+
  theme(panel.background = element_rect(fill = "white"), 
        strip.text.y = element_text(angle = 0, color = "black"), 
        strip.text.x = element_blank(), panel.spacing = unit(0.05, "lines"),
        axis.title.y = element_text(hjust = -3.0, color = "black"), 
        axis.title.x = element_text(face = "bold", size = 7, color = "black", vjust = -0.5), 
        axis.text.y = element_text(face = "bold", size = 5, color = "black"), 
        axis.text.x = element_text(face = "bold", size = 7, color = "black"), panel.border = element_rect(color = "black", fill = NA, size = 0.5))

snow_acc <- ggarrange(Peak_snow,                             # First row 
          ggarrange(Duration_snow, labels = c("(c) Duration"), hjust = -0.1, vjust = -0.2, align = "v", font.label = list(size = 10), legend = "none"), # Second row with box and dot plots
          ncol = 1, nrow = 2, 
          labels = "(a) Peak", hjust = -0.1, vjust = 1.2, widths = c(1,1), font.label = list(size = 10), legend = "none"                                      # Labels of the scatter plot
)


Peak_temp <- ggplot(df_test, aes(x = Slope2, y = SpeciesID, xmin = CI_lwr2, xmax = CI_upr2, color = cut(Pvalue2, breaks = c(0, 0.05, 0.099, 1))))+
  xlab("Shift in peak activity (days per shifted snowmelt day)")+
  ylab("")+
  #scale_colour_discrete(name = "Taxon")+
  #scale_shape(guide = "none")+
  geom_pointrange(size = 0.2, alpha = 0.5, position = position_dodge(width = 0.05))+
  #scale_size(range=c(0.2,1))+
  geom_vline(xintercept=0, lty=2)+
  scale_color_manual(values = c('red', 'orange', 'blue'),
                     limits = c('(0,0.05]', '(0.05,0.099]', '(0.099,1]'))+
  scale_x_continuous(limits=c(-15,15), oob=scales::rescale_none)+
  #geom_boxplot(varwidth = TRUE)+
  facet_grid(~ Habitat, scales = "free_x")+
  #scale_color_viridis_d(guide = "none")+
  geom_vline(xintercept = 0)+
  theme(panel.background = element_rect(fill = "white"), panel.spacing = unit(0.05, "lines"),
        axis.text.x = element_text(face = "bold", size = 7, color = "black"), 
        strip.text.x = element_text(angle = 0, size = 5, color = "black"),
        axis.title.x = element_text(face = "bold", size = 7, color = "black", vjust = 0.5), 
        axis.text.y = element_text(face = "bold", size = 5, color = "black"), panel.border = element_rect(color = "black", fill = NA, size = 0.5),
        strip.background = element_rect(colour = "black", fill = "white"))


Duration_temp <- ggplot(df_test1, aes(x = Slope2, y = SpeciesID, xmin = CI_lwr2, xmax = CI_upr2, color = cut(Pvalue2, breaks = c(0, 0.05, 0.099, 1))))+
  xlab("Change in duration of activity (days per shifted snowmelt day)")+
  ylab("")+
  #scale_colour_discrete(name = "Taxon")+
  #scale_shape(guide = "none")+
  geom_pointrange(size = 0.2, alpha = 0.5, position = position_dodge(width = 0.05))+
  #scale_size(range=c(0.2,1))+
  geom_vline(xintercept=0, lty=2)+
  scale_color_manual(values = c('red', 'orange', 'blue'),
                     limits = c('(0,0.05]', '(0.05,0.099]', '(0.099,1]'))+
  scale_x_continuous(limits=c(-15,15), oob=scales::rescale_none)+
  #geom_boxplot(varwidth = TRUE)+
  facet_grid(~ Habitat, scales = "free_x")+
  #scale_color_viridis_d(guide = "none")+
  geom_vline(xintercept = 0)+
  theme(panel.background = element_rect(fill = "white"), 
        strip.text.y = element_text(angle = 0, color = "black"), 
        strip.text.x = element_blank(), panel.spacing = unit(0.05, "lines"),
        axis.title.y = element_text(hjust = -3.0, color = "black"), 
        axis.title.x = element_text(face = "bold", size = 7, color = "black", vjust = -0.5), 
        axis.text.y = element_text(face = "bold", size = 5, color = "black"), 
        axis.text.x = element_text(face = "bold", size = 7, color = "black"), panel.border = element_rect(color = "black", fill = NA, size = 0.5))

temp_acc <- ggarrange(Peak_temp,                             # First row 
          ggarrange(Duration_temp, labels = c("(d) Duration"), hjust = -0.1, vjust = -0.2, align = "v", font.label = list(size = 10), legend = "none"), # Second row with box and dot plots
          ncol = 1, nrow = 2, 
          labels = "(b) Peak", hjust = -0.1, vjust = 1.2, widths = c(1,1), font.label = list(size = 10), legend = "none"                                      # Labels of the scatter plot
)

ggarrange(snow_acc, temp_acc) +
  theme(plot.margin = margin(12,7,12,7, "cm"))



############################################################################

pkgs <- c("tidyverse", "ggpubr", "viridis")

vapply(pkgs, library, FUN.VALUE = logical(1L), character.only = TRUE,
       logical.return = TRUE)


df_phen_event <-
  read.csv(
    "Data/phenology_data/Pheno_air_temp.csv",
    sep = ",",
    stringsAsFactors = FALSE,
    header = TRUE
  )

dfsnowmelt_climatestation <-
  readxl::read_xlsx("Data/snowmelt_data/Snowmelt_climatestation.xlsx")

df_phen_event$Snowmelt <-
  dfsnowmelt_climatestation$DOY[match(paste0(df_phen_event$Year),
                                      paste0(dfsnowmelt_climatestation$Year))]


df_phen_event %>%
  mutate(Order = case_when(
    SpeciesID == "Acari" ~ "Decomposer",
    SpeciesID == "ANMU" ~ "Mixed feeder",
    SpeciesID == "Aphidoidea" ~ "Herbivor",
    SpeciesID == "Chalcidoidea" ~ "Parasitoid",
    SpeciesID == "CHCE" ~ "Mixed feeder",
    SpeciesID == "Coccoidea" ~ "Herbivor",
    SpeciesID == "Collembola" ~ "Decomposer",
    SpeciesID == "Culicidae" ~ "Mixed feeder",
    SpeciesID == "Ichneumonidae" ~ "Parasitoid",
    SpeciesID == "Linyphiidae" ~ "Predator",
    SpeciesID == "Lycosidae" ~ "Predator",
    SpeciesID == "MYSC" ~ "Mixed feeder",
    SpeciesID == "Nymphalidae" ~ "Mixed feeder",
    SpeciesID == "Phoridae" ~ "Mixed feeder",
    SpeciesID == "Scathophagidae" ~ "Mixed feeder",
    SpeciesID == "Thomisidae" ~ "Predator")) -> df_phen_event

df_phen_event %>%
  mutate(Emerge = case_when(
    SpeciesID == "Acari" ~ "Early",
    SpeciesID == "ANMU" ~ "Early",
    SpeciesID == "Aphidoidea" ~ "Late",
    SpeciesID == "Chalcidoidea" ~ "Late",
    SpeciesID == "CHCE" ~ "Early",
    SpeciesID == "Coccoidea" ~ "Late",
    SpeciesID == "Collembola" ~ "Early",
    SpeciesID == "Culicidae" ~ "Late",
    SpeciesID == "Ichneumonidae" ~ "Late",
    SpeciesID == "Linyphiidae" ~ "Early",
    SpeciesID == "Lycosidae" ~ "Early",
    SpeciesID == "MYSC" ~ "Early",
    SpeciesID == "Nymphalidae" ~ "Late",
    SpeciesID == "Phoridae" ~ "Late",
    SpeciesID == "Scathophagidae" ~ "Late",
    SpeciesID == "Thomisidae" ~ "Early")) -> df_phen_event

df_phen_event %>%
  mutate(Trait = case_when(
    SpeciesID == "Acari" ~ "Surface",
    SpeciesID == "ANMU" ~ "Flying",
    SpeciesID == "Aphidoidea" ~ "Surface",
    SpeciesID == "Chalcidoidea" ~ "Flying",
    SpeciesID == "CHCE" ~ "Flying",
    SpeciesID == "Coccoidea" ~ "Surface",
    SpeciesID == "Collembola" ~ "Surface",
    SpeciesID == "Culicidae" ~ "Flying",
    SpeciesID == "Ichneumonidae" ~ "Flying",
    SpeciesID == "Linyphiidae" ~ "Surface",
    SpeciesID == "Lycosidae" ~ "Surface",
    SpeciesID == "MYSC" ~ "Flying",
    SpeciesID == "Nymphalidae" ~ "Flying",
    SpeciesID == "Phoridae" ~ "Flying",
    SpeciesID == "Scathophagidae" ~ "Flying",
    SpeciesID == "Thomisidae" ~ "Surface")) -> df_phen_event

df_phen_event <-
  df_phen_event[!(df_phen_event$Plot == "Art1" &
                    (df_phen_event$Order == "Decomposer")), ]
df_phen_event <-
  df_phen_event[!(df_phen_event$Plot == "Art1" &
                    (df_phen_event$Order == "Predator")), ]
df_phen_event <-
  df_phen_event[!(df_phen_event$Plot == "Art3" &
                    (df_phen_event$SpeciesID == "Nymphalidae")), ]
df_phen_event <-
  df_phen_event[!(df_phen_event$SpeciesID == "Lygaeidae"), ]

df_phen_event <- subset(df_phen_event,!Plot == "Art6")

df_phen_event %>%
  mutate(
    Habitat = case_when(
      Plot == "Art1" ~ "Pond",
      Plot == "Art2" ~ "Wet fen",
      Plot == "Art3" ~ "Mesic heath",
      Plot == "Art4" ~ "Mesic heath",
      Plot == "Art5" ~ "Arid heath",
      Plot == "Art7" ~ "Arid heath"
    )
  ) -> df_phen_event



df_phen_event$Order[is.na(df_phen_event$Order)] <- "NA"
df_phen_event <- subset(df_phen_event, Order != "NA")

df_phen_event$speciesplot <-
  as.factor(paste(df_phen_event$SpeciesID, df_phen_event$Plot))

df_phen_event$Peak_Temp_new <-
  df_phen_event$Peak_Temp30 - tapply(df_phen_event$Peak_Temp30, df_phen_event$speciesplot, mean)[df_phen_event$speciesplot]

##Creating figure 5####
#Important: All slope estimates are from the script: "Functional group phenological responses"


Order <- c("Decomposer", "Herbivore", "Parasitoid", "Mixed feeder", "Predator", 
           "Decomposer", "Herbivore", "Parasitoid", "Mixed feeder", "Predator")
Pheno_event <- c("Peak","Peak","Peak","Peak","Peak",
                 "Duration","Duration","Duration","Duration",
                 "Duration")
Slope <- c(0.11, 0.28, 0.40, 0.41, 0.08,
           -0.22, -0.07, -0.08, 0.10, -0.40)
SE <- c(0.06, 0.12, 0.10, 0.04, 0.07,
        0.06, 0.12, 0.11, 0.05, 0.07)
Pvalue <- c(0.25, 0.05, 0.001, 0.001, 0.79, 0.22,0.77, 0.19, 0.32, 0.71)

df_functional_group <- as.data.frame(Order)
df_functional_group$Pheno_event <- Pheno_event
df_functional_group$Slope <- Slope
df_functional_group$SE <- SE
df_functional_group$Pvalue <- Pvalue

df_functional_group$Pheno_event <-
  factor(df_functional_group$Pheno_event,
         # Relevel group factor
         levels = c("Peak", "Duration"))

df_functional_group$Order <-
  factor(df_functional_group$Order,
         # Relevel group factor
         levels = c("Predator", "Parasitoid", "Mixed feeder", "Herbivore", "Decomposer"))


Snowmelt <- ggplot(df_functional_group, aes(Slope, Order))+ 
  #facet_grid(~N) +
  xlab("")+
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray", size = 1)+
  geom_errorbar(aes(xmin=Slope-SE, xmax=Slope+SE, color = Order), size = 0.3, width = 0.05)+
  #xlab("Temperature")+
  #xlim(-0.7,0.7)+
  geom_point(aes(fill = Order), shape = 21, color = "black", size = 1, alpha = 0.9, stroke = 0.2) +
  #position=position_dodge(0.05), size = 1, alpha = 0.7)+
  facet_grid(cols = vars(Pheno_event), drop = TRUE, scales = "free_y", space = "free")+
  scale_x_continuous(breaks = c(-0.4,0,0.4))+
  #scale_colour_manual(values=c("white", "black"))+
  scale_fill_viridis(discrete = TRUE)+
  scale_color_viridis(discrete = TRUE)+
  coord_cartesian(xlim=c(-0.6, 0.6))+
  #scale_shape_manual(values = c(22, 21, 24, 23, 25)) +
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 4, color = "black"),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 4, color = "black"),
        axis.line = element_line(color = "black"),
        legend.position = "none",
        plot.margin = margin(0,0.2,1,0, "cm"),
        strip.background =element_rect(fill="white"),
        strip.text.x = element_text(size = 5, color = "black"),
        panel.spacing=unit(0.1,"lines"))


#Temperature

Order <- c("Decomposer", "Herbivore", "Parasitoid", "Mixed feeder", "Predator", 
           "Decomposer", "Herbivore", "Parasitoid", "Mixed feeder", "Predator")
Pheno_event <- c("Peak","Peak","Peak","Peak","Peak",
                 "Duration","Duration","Duration","Duration",
                 "Duration")
Slope <- c(-1.45, -1.14, -2.02, -1.24, -1.52,
           -0.38, -1.15, 2.28, -0.05, -0.41)
SE <- c(0.68, 1.37, 0.83, 0.43, 0.74,
        0.68, 1.30, 1.17, 0.56, 0.76)
Pvalue <- c(0.001, 0.001, 0.01, 0.001, 0.001, 0.06, 0.40, 0.48, 0.002, 0.09)

df_functional_group_temp <- as.data.frame(Order)
df_functional_group_temp$Pheno_event <- Pheno_event
df_functional_group_temp$Slope <- Slope
df_functional_group_temp$SE <- SE
df_functional_group_temp$Pvalue <- Pvalue

df_functional_group_temp$Pheno_event <-
  factor(df_functional_group_temp$Pheno_event,
         # Relevel group factor
         levels = c("Peak", "Duration"))

df_functional_group_temp$Order <-
  factor(df_functional_group_temp$Order,
         # Relevel group factor
         levels = c("Predator", "Parasitoid", "Mixed feeder", "Herbivore", "Decomposer"))

Temperature <- ggplot(df_functional_group_temp, aes(Slope, Order))+ 
  #facet_grid(~N) +
  xlab("")+
  ylab("")+
  #ylim(-3.5,3.5)+
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray", size = 1)+
  geom_errorbar(aes(xmin=Slope-SE, xmax=Slope+SE, color = Order), size = 0.3, width = 0.05)+
  geom_point(aes(fill = Order), shape = 21, color = "black", size = 1, alpha = 0.9, stroke = 0.2) +
  #position=position_dodge(0.05), size = 1, alpha = 0.7)+
  scale_x_continuous(limits=c(-3.5,3.5), breaks = c(-3,-2,-1,0,1,2,3))+
  facet_grid(cols = vars(Pheno_event), drop = TRUE, scales = "free_y", space = "free")+
  #scale_colour_manual(values=c("white", "black"))+
  scale_fill_viridis(discrete = TRUE)+
  scale_color_viridis(discrete = TRUE)+
  #scale_shape_manual(values = c(22, 21, 24, 23, 25)) +
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 4, color = "black"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_line(color = "black"),
        legend.position = "none",
        #legend.key.size = unit(1, 'cm'),
        #legend.key.width = unit(2, 'cm'),
        #legend.text = element_text(size = 14),
        #legend.title = element_text(size = 16),
        plot.margin = margin(0,1.2,1,0, "cm"),
        strip.background =element_rect(fill="white"),
        strip.text.x = element_text(size = 5, color = "black"),
        panel.spacing=unit(0.1,"lines"))

ggarrange(Snowmelt, Temperature, labels = c("(a) Functional groups", "(b)"), 
                        hjust = c(-0.4,-0.2), vjust = -1, 
                        ncol = 2, nrow = 1,
                        font.label=list(color="black",size=5)) +
  theme(plot.margin = margin(13,7,13,7, "cm"))



