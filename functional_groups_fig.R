####Code to produce figure 5 in high-arctic arthropod phenological responses to climate###

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

####Creating figure 5####
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
  geom_errorbar(aes(xmin=Slope-SE, xmax=Slope+SE, color = Order), size = 0.3, width = 0.10)+
  #xlab("Temperature")+
  #xlim(-0.7,0.7)+
  geom_point(aes(fill = Order), shape = 21, color = "black", size = 2, alpha = 0.9, stroke = 0.5) +
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
        axis.text.x = element_text(size = 10, color = "black"),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.line = element_line(color = "black"),
        legend.position = "none",
        plot.margin = margin(0,0.2,1,0, "cm"),
        strip.background =element_rect(fill="white"),
        strip.text.x = element_text(size = 10, color = "black"),
        panel.spacing=unit(0.5,"lines"))


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
  geom_errorbar(aes(xmin=Slope-SE, xmax=Slope+SE, color = Order), size = 0.3, width = 0.10)+
  geom_point(aes(fill = Order), shape = 21, color = "black", size = 2, alpha = 0.9, stroke = 0.5) +
  #position=position_dodge(0.05), size = 1, alpha = 0.7)+
  scale_x_continuous(limits=c(-3.5,3.5), breaks = c(-3,-2,-1,0,1,2,3))+
  facet_grid(cols = vars(Pheno_event), drop = TRUE, scales = "free_y", space = "free")+
  #scale_colour_manual(values=c("white", "black"))+
  scale_fill_viridis(discrete = TRUE)+
  scale_color_viridis(discrete = TRUE)+
  #scale_shape_manual(values = c(22, 21, 24, 23, 25)) +
  theme_bw()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10, color = "black"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_line(color = "black"),
        legend.position = "none",
        #legend.key.size = unit(1, 'cm'),
        #legend.key.width = unit(2, 'cm'),
        #legend.text = element_text(size = 14),
        #legend.title = element_text(size = 16),
        plot.margin = margin(0,1.5,1,0, "cm"),
        strip.background =element_rect(fill="white"),
        strip.text.x = element_text(size = 10, color = "black"),
        panel.spacing=unit(0.5,"lines"))

functional <- ggarrange(Snowmelt, Temperature, labels = c("(a) Functional groups", "(b)"), 
                        hjust = c(-0.4,-0.2), vjust = -1, 
                        ncol = 2, nrow = 1,
                        font.label=list(color="black",size=12))




Trait <- c("Early", "Late", "Flying", "Surface", "Early", "Late", "Flying", "Surface")
Snow <- c("Snowmelt", "Snowmelt", "Snowmelt", "Snowmelt", "Snowmelt", "Snowmelt", "Snowmelt", "Snowmelt")
Event <- c("Peak", "Peak", "Peak", "Peak", "Duration", "Duration", "Duration", "Duration")
Slope_snow <- c(0.23, 0.31, 0.42, 0.12, 
                -0.10, -0.14, 0.07, -0.30)
SE_snow <- c(0.03, 0.06, 0.05, 0.05, 
             0.03, 0.06, 0.04, 0.05)

df_trait <- as.data.frame(Trait)
df_trait$Event <- Event
df_trait$Snow <- Snow
df_trait$Slope_snow <- Slope_snow
df_trait$SE_snow <- SE_snow

df_trait$Trait <-
  factor(df_trait$Trait,
         # Relevel group factor
         levels = c("Early", "Late", "Flying", "Surface"))

df_trait$Event <-
  factor(df_trait$Event,
         # Relevel group factor
         levels = c("Peak", "Duration"))

trait_snow <- ggplot(df_trait, aes(Slope_snow, Trait))+ 
  #facet_grid(~N) +
  xlab("Phenological response\n(days/shifted snowmelt day)")+
  #xlab("")+
  #xlim(-0.7,0.7)+
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray", size = 1)+
  geom_errorbar(aes(Slope_snow, Trait, xmin=Slope_snow-SE_snow, xmax=Slope_snow+SE_snow, color = Trait), size = 0.3, width = 0.10)+
  geom_point(aes(color = Trait, fill = Trait), shape = 21, color = "black", size = 2, alpha = 0.9, stroke = 0.5) +
  #position=position_dodge(0.05), size = 1, alpha = 0.7)+
  scale_x_continuous(breaks = c(-0.4,0,0.4))+
  facet_grid(cols = vars(Event), drop = TRUE, scales = "free_x", space = "free")+
  #scale_colour_manual(values=c("white", "black"))+
  scale_colour_manual(values=c("#0D0887FF", "#A92395FF", "#F89441FF", "#FDC328FF"))+
  scale_fill_manual(values=c("#0D0887FF", "#A92395FF", "#F89441FF", "#FDC328FF"))+
  coord_cartesian(xlim=c(-0.5, 0.5))+
  #scale_shape_manual(values = c(22,21,23,24)) +
  theme_bw()+
  theme(axis.title.x = element_text(size = 12, color = "black", vjust = -1.5),
        axis.text.x = element_text(size = 10, color = "black"),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 8, color = "black"),
        axis.line = element_line(color = "black"),
        legend.position = "none",
        #legend.key.size = unit(1, 'cm'),
        #legend.key.width = unit(1, 'cm'),
        plot.margin = margin(0,0.2,0,0.6, "cm"),
        strip.background =element_rect(fill="white"),
        strip.text.x = element_text(size = 10, color = "black"),
        panel.spacing=unit(0.5,"lines"))



Trait <- c("Early", "Late", "Flying", "Surface","Early", "Late", "Flying", "Surface")
Temp <- c("Temperature", "Temperature", "Temperature", "Temperature","Temperature", "Temperature", "Temperature", "Temperature")
Event <- c("Peak", "Peak", "Peak", "Peak", "Duration", "Duration", "Duration", "Duration")
Slope_temp <- c(-1.86, -1.03, -1.94, -1.32, 
                -0.11, 0.04, 0.60, -0.76)
SE_temp <- c(0.57, 0.77, 0.56, 0.66, 
             0.34, 0.65, 0.42, 0.42)

df_trait_temp <- as.data.frame(Trait)
df_trait_temp$Event <- Event
df_trait_temp$Temp <- Temp
df_trait_temp$Slope_temp <- Slope_temp
df_trait_temp$SE_temp <- SE_temp

df_trait_temp$Trait <-
  factor(df_trait_temp$Trait,
         # Relevel group factor
         levels = c("Early", "Late", "Flying", "Surface"))

df_trait_temp$Event <-
  factor(df_trait_temp$Event,
         # Relevel group factor
         levels = c("Peak", "Duration"))

trait_temp <- ggplot(df_trait_temp, aes(Slope_temp, Trait))+ 
  #facet_grid(~N) +
  xlab("Phenological response\n(days/°C)")+
  #xlab("")+
  #xlim(-3.5,3.5)+
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray", size = 1)+
  geom_errorbar(aes(Slope_temp, Trait, xmin=Slope_temp-SE_temp, xmax=Slope_temp+SE_temp, color = Trait), size = 0.3, width = 0.10)+
  geom_point(aes(color = Trait, fill = Trait), shape = 21, color = "black", size = 2, alpha = 0.9, stroke = 0.5) +
  #position=position_dodge(0.05), size = 1, alpha = 0.7)+
  #scale_y_continuous(limits=c(-3.5,3.5), breaks = c(-3,-2,-1,0,1,2,3))+
  facet_grid(cols = vars(Event), drop = TRUE, scales = "free_x", space = "free")+
  #scale_colour_manual(values=c("white", "black"))+
  scale_x_continuous(limits=c(-3.5,3.5), breaks = c(-3,-2,-1,0,1,2,3))+
  scale_colour_manual(values=c("#0D0887FF", "#A92395FF", "#F89441FF", "#FDC328FF"))+
  scale_fill_manual(values=c("#0D0887FF", "#A92395FF", "#F89441FF", "#FDC328FF"))+
  #scale_shape_manual(values = c(22, 21,23,24)) +
  theme_bw()+
  theme(axis.title.x = element_text(size = 12, color = "black", vjust = -1.5),
        axis.text.x = element_text(size = 10, color = "black"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_line(color = "black"),
        legend.position = "none",
        #legend.key.size = unit(1, 'cm'),
        #legend.key.width = unit(1, 'cm'),
        plot.margin = margin(0,1.5,0,0, "cm"),
        strip.background =element_rect(fill="white"),
        strip.text.x = element_text(size = 10, color = "black"),
        panel.spacing=unit(0.5,"lines"))




Trait_all <- ggarrange(trait_snow, trait_temp,
          labels = c("(c) Traits", "(d)"),
          hjust = c(-1,-0.2), vjust = -1,
          nrow = 1, ncol = 2, font.label=list(color="black",size=12))


ggarrange(functional, Trait_all, ncol = 1, nrow = 2) +
  theme(plot.margin = margin(8,2,8,2, "cm"))



