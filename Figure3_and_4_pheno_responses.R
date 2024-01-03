####Figure 3 and 4 - phenological responses of taxa-by-plot combinations####

pkgs <- c("tidyverse", "readxl", "corrplot")

vapply(pkgs, library, FUN.VALUE = logical(1L), character.only = TRUE,
       logical.return = TRUE)


df_summary <-
  readxl::read_xlsx("Data/phenology_data/Dataset_summaries/df_summary_all_air_detrend_ptd.xlsx")


df_summary %>%
  mutate(Order = case_when(
    SpeciesID == "Acari" ~ "Decomposer",
    SpeciesID == "Muscidae" ~ "Mixed feeder",
    SpeciesID == "Aphidoidea" ~ "Herbivor",
    SpeciesID == "Chalcidoidea" ~ "Parasitoid",
    SpeciesID == "Chironomidae" ~ "Mixed feeder",
    SpeciesID == "Coccoidea" ~ "Herbivor",
    SpeciesID == "Collembola" ~ "Decomposer",
    SpeciesID == "Culicidae" ~ "Mixed feeder",
    SpeciesID == "Ichneumonidae" ~ "Parasitoid",
    SpeciesID == "Linyphiidae" ~ "Predator",
    SpeciesID == "Lycosidae" ~ "Predator",
    SpeciesID == "Sciaridae" ~ "Mixed feeder",
    SpeciesID == "Nymphalidae" ~ "Mixed feeder",
    SpeciesID == "Phoridae" ~ "Mixed feeder",
    SpeciesID == "Scathophagidae" ~ "Mixed feeder",
    SpeciesID == "Thomisidae" ~ "Predator")) -> df_summary


df_Onset <- subset(df_summary, Pheno_event == "Onset")
df_Peak <- subset(df_summary, Pheno_event == "Peak")
df_End <- subset(df_summary, Pheno_event == "End")
df_Duration <- subset(df_summary, Pheno_event == "Duration")


df_Onset$SpeciesID <- factor(df_Onset$SpeciesID,                 # Relevel group factor
                             levels = c("Chalcidoidea", "Aphidoidea", "Phoridae", "Nymphalidae", "Coccoidea", "Ichneumonidae", "Acari", "Culicidae",
                                        "Muscidae", "Collembola", "Lycosidae", "Thomisidae", "Sciaridae", "Linyphiidae", "Chironomidae"))
df_Peak$SpeciesID <- factor(df_Peak$SpeciesID,                 # Relevel group factor
                            levels = c("Chalcidoidea", "Aphidoidea", "Phoridae", "Nymphalidae", "Coccoidea", "Ichneumonidae", "Acari", "Culicidae",
                                       "Muscidae", "Collembola", "Lycosidae", "Thomisidae", "Sciaridae", "Linyphiidae", "Chironomidae"))
df_End$SpeciesID <- factor(df_Peak$SpeciesID,                 # Relevel group factor
                           levels = c("Chalcidoidea", "Aphidoidea", "Phoridae", "Nymphalidae", "Coccoidea", "Ichneumonidae", "Acari", "Culicidae",
                                      "Muscidae", "Collembola", "Lycosidae", "Thomisidae", "Sciaridae", "Linyphiidae", "Chironomidae"))
df_Duration$SpeciesID <- factor(df_Duration$SpeciesID,                 # Relevel group factor
                                levels = c("Chalcidoidea", "Aphidoidea", "Phoridae", "Nymphalidae", "Coccoidea", "Ichneumonidae", "Acari", "Culicidae",
                                           "Muscidae", "Collembola", "Lycosidae", "Thomisidae", "Sciaridae", "Linyphiidae", "Chironomidae"))

df_Onset$Habitat <- factor(df_Onset$Habitat,                 # Relevel group factor
                           levels = c("Arid heath", "Mesic heath", "Wet fen", "Pond"))

df_Peak$Habitat <- factor(df_Peak$Habitat,                 # Relevel group factor
                          levels = c("Arid heath", "Mesic heath", "Wet fen", "Pond"))

df_Duration$Habitat <- factor(df_Duration$Habitat,                 # Relevel group factor
                              levels = c("Arid heath", "Mesic heath", "Wet fen", "Pond"))

df_End$Habitat <- factor(df_End$Habitat,                 # Relevel group factor
                         levels = c("Arid heath", "Mesic heath", "Wet fen", "Pond"))


Onset <- ggplot(df_Onset, aes(x = Slope1, y = SpeciesID, xmin = CI_lwr1, xmax = CI_upr1, color = cut(Pvalue1, breaks = c(0, 0.05, 0.099, 1))))+
  xlab("Phenological sensitivity (days per shifted snowmelt day)")+
  ylab("")+
  #scale_shape(guide = "none")+
  geom_pointrange(aes(size = n), alpha = 0.5, position = position_dodge(width = 0.5))+
  scale_size(range=c(0.4,1.5))+
  geom_vline(xintercept=0, lty=2)+
  scale_color_manual(values = c('red', 'orange', 'blue'),
                     limits = c('(0,0.05]', '(0.05,0.099]', '(0.099,1]'))+
  scale_x_continuous(limits=c(-1.5,1.5), breaks = c(-1,0,1), oob=scales::rescale_none)+
  #geom_boxplot(varwidth = TRUE)+
  facet_grid(~ Habitat, scales = "free_x")+
  #scale_color_viridis_d(guide = "none")+
  geom_vline(xintercept = 0)+
  theme(panel.background = element_rect(fill = "white"), panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(face = "bold", size = 15, color = "black"), strip.text.x = element_text(angle = 0, size = 15, color = "black"),
        axis.title.x = element_text(face = "bold", size = 15, color = "black", vjust = 0.5), 
        axis.text.y = element_text(face = "bold", size = 15, color = "black"), panel.border = element_rect(color = "black", fill = NA, size = 2),
        strip.background = element_rect(colour = "black", fill = "white"))

Peak <- ggplot(df_Peak, aes(x = Slope1, y = SpeciesID, xmin = CI_lwr1, xmax = CI_upr1, color = cut(Pvalue1, breaks = c(0, 0.05, 0.099, 1))))+
  xlab("Shift in peak activity (days per shifted snowmelt day)")+
  ylab("")+
  #scale_colour_discrete(name = "Taxon")+
  #scale_shape(guide = "none")+
  geom_pointrange(aes(size = n), alpha = 0.5, position = position_dodge(width = 0.5))+
  scale_size(range=c(0.4,1.5))+
  geom_vline(xintercept=0, lty=2)+
  scale_color_manual(values = c('red', 'orange', 'blue'),
                     limits = c('(0,0.05]', '(0.05,0.099]', '(0.099,1]'))+
  scale_x_continuous(limits=c(-1.5,1.5), breaks = c(-1,0,1), oob=scales::rescale_none)+
  #geom_boxplot(varwidth = TRUE)+
  facet_grid(~ Habitat, scales = "free_x")+
  #scale_color_viridis_d(guide = "none")+
  geom_vline(xintercept = 0)+
  theme(panel.background = element_rect(fill = "white"), panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(face = "bold", size = 15, color = "black"), strip.text.x = element_text(angle = 0, size = 15, color = "black"),
        axis.title.x = element_text(face = "bold", size = 15, color = "black", vjust = 0.5), 
        axis.text.y = element_text(face = "bold", size = 15, color = "black"), panel.border = element_rect(color = "black", fill = NA, size = 2),
        strip.background = element_rect(colour = "black", fill = "white"))

End <- ggplot(df_End, aes(x = Slope1, y = SpeciesID, xmin = CI_lwr1, xmax = CI_upr1, color = cut(Pvalue1, breaks = c(0, 0.05, 0.099, 1))))+
  xlab("Phenological sensitivity (days per shifted snowmelt day)")+
  ylab("")+
  #scale_colour_discrete(name = "Taxon")+
  #scale_shape(guide = "none")+
  geom_pointrange(aes(size = n), alpha = 0.5, position = position_dodge(width = 0.5))+
  scale_size(range=c(0.4,1.5))+
  geom_vline(xintercept=0, lty=2)+
  scale_color_manual(values = c('red', 'orange', 'blue'),
                     limits = c('(0,0.05]', '(0.05,0.099]', '(0.099,1]'))+
  scale_x_continuous(limits=c(-1.5,1.5), breaks = c(-1,0,1), oob=scales::rescale_none)+
  #geom_boxplot(varwidth = TRUE)+
  facet_grid(~ Habitat, scales = "free_x")+
  #scale_color_viridis_d(guide = "none")+
  geom_vline(xintercept = 0)+
  theme(panel.background = element_rect(fill = "white"), strip.text.y = element_text(angle = 0, color = "black"), strip.text.x = element_blank(), panel.spacing = unit(1, "lines"),
        axis.title.y = element_text(hjust = -3.0, color = "black"), axis.title.x = element_text(face = "bold", size = 15, color = "black", vjust = -0.5), axis.text.y = element_text(face = "bold", size = 15, color = "black"), 
        axis.text.x = element_text(face = "bold", size = 15, color = "black"), panel.border = element_rect(color = "black", fill = NA, size = 2))


Duration <- ggplot(df_Duration, aes(x = Slope1, y = SpeciesID, xmin = CI_lwr1, xmax = CI_upr1, color = cut(Pvalue1, breaks = c(0, 0.05, 0.099, 1))))+
  xlab("Change in duration of activity (days per shifted snowmelt day)")+
  ylab("")+
  #scale_colour_discrete(name = "Taxon")+
  #scale_shape(guide = "none")+
  geom_pointrange(aes(size = n), alpha = 0.5, position = position_dodge(width = 0.5))+
  scale_size(range=c(0.4,1.5))+
  geom_vline(xintercept=0, lty=2)+
  scale_color_manual(values = c('red', 'orange', 'blue'),
                     limits = c('(0,0.05]', '(0.05,0.099]', '(0.099,1]'))+
  scale_x_continuous(limits=c(-1.5,1.5), breaks = c(-1,0,1), oob=scales::rescale_none)+
  #geom_boxplot(varwidth = TRUE)+
  facet_grid(~ Habitat, scales = "free_x")+
  #scale_color_viridis_d(guide = "none")+
  geom_vline(xintercept = 0)+
  theme(panel.background = element_rect(fill = "white"), strip.text.y = element_text(angle = 0, color = "black"), strip.text.x = element_blank(), panel.spacing = unit(1, "lines"),
        axis.title.y = element_text(hjust = -3.0, color = "black"), axis.title.x = element_text(face = "bold", size = 15, color = "black", vjust = -0.5), axis.text.y = element_text(face = "bold", size = 15, color = "black"), 
        axis.text.x = element_text(face = "bold", size = 15, color = "black"), panel.border = element_rect(color = "black", fill = NA, size = 2))


ggarrange(Peak,                             # First row 
          ggarrange(Duration, labels = c("(b) Duration"), hjust = -0.1, vjust = -0.2, align = "v", font.label = list(size = 20), legend = "none"), # Second row with box and dot plots
          ncol = 1, nrow = 2, 
          labels = "(a) Peak", hjust = -0.1, vjust = 1.2, widths = c(1,1), font.label = list(size = 20), legend = "none"                                      # Labels of the scatter plot
)+
  theme(plot.margin = margin(2,2,2,4, "cm"))

ggarrange(Onset,                             # First row 
          ggarrange(End, labels = c("(b) End"), hjust = -0.1, vjust = -0.2, align = "v", font.label = list(size = 20), legend = "none"), # Second row with box and dot plots
          ncol = 1, nrow = 2, 
          labels = "(a) Onset", hjust = -0.1, vjust = 1.2, widths = c(1,1), font.label = list(size = 20), legend = "none"                         # Labels of the scatter plot
)+
  theme(plot.margin = margin(0.4,0.1,0.1,0.1, "cm"))


####Temperature####

Onset_temp <- ggplot(df_Onset, aes(x = Slope2, y = SpeciesID, xmin = CI_lwr2, xmax = CI_upr2, color = cut(Pvalue2, breaks = c(0, 0.05, 0.099, 1))))+
  xlab("Shift in peak activity (days per °C)")+
  ylab("")+
  #scale_shape(guide = "none")+
  geom_pointrange(aes(size = n), position = position_dodge(width = 0.5), alpha = 0.5)+
  scale_size(range=c(0.4,1.5))+
  geom_vline(xintercept=0, lty=2)+
  scale_color_manual(values = c('red', 'orange', 'blue'),
                     limits = c('(0,0.05]', '(0.05,0.099]', '(0.099,1]'))+
  scale_x_continuous(limits=c(-15,15), oob=scales::rescale_none)+
  #geom_boxplot(varwidth = TRUE)+
  facet_grid(~ Habitat)+
  #scale_color_viridis_d(guide = "none")+
  theme(panel.background = element_rect(fill = "white"), panel.spacing = unit(1, "lines"),
        axis.title.x = element_text(face = "bold", size = 15, color = "black", vjust = 0.5),
        axis.text.x = element_text(face = "bold", size = 15, color = "black"), strip.text.x = element_text(angle = 0, size = 15), 
        axis.text.y = element_text(face = "bold", size = 15, color = "black"), 
        panel.border = element_rect(color = "black", fill = NA, size = 2))


Peak_temp <- ggplot(df_Peak, aes(x = Slope2, y = SpeciesID, xmin = CI_lwr2, xmax = CI_upr2, color = cut(Pvalue2, breaks = c(0, 0.05, 0.099, 1))))+
  xlab("Shift in peak activity (days per °C)")+
  ylab("")+
  #xlim(c(-15,10))+
  #scale_colour_discrete(name = "Taxon")+
  #scale_shape(guide = "none")+
  geom_pointrange(aes(size = n), position = position_dodge(width = 0.8), alpha = 0.5)+
  scale_size(range=c(0.4,1.5))+
  geom_vline(xintercept=0, lty=2)+
  scale_color_manual(values = c('red', 'orange', 'blue'),
                     limits = c('(0,0.05]', '(0.05,0.099]', '(0.099,1]'))+
  scale_x_continuous(limits=c(-15,15), oob=scales::rescale_none)+
  #geom_boxplot(varwidth = TRUE)+
  facet_grid(~ Habitat)+
  #scale_color_viridis_d(guide = "none")+
  theme(panel.background = element_rect(fill = "white"), panel.spacing = unit(1, "lines"),
        axis.text.x = element_text(face = "bold", size = 15, color = "black"), strip.text.x = element_text(angle = 0, size = 15), 
        axis.text.y = element_text(face = "bold", size = 15, color = "black"),
        axis.title.x = element_text(face = "bold", size = 15, color = "black", vjust = 0.5),
        panel.border = element_rect(color = "black", fill = NA, size = 2),
        strip.background = element_rect(colour = "black", fill = "white"))


End_temp <- ggplot(df_End, aes(x = Slope2, y = SpeciesID, xmin = CI_lwr2, xmax = CI_upr2, color = cut(Pvalue2, breaks = c(0, 0.05, 0.099, 1))))+
  xlab("Shift in peak activity (days per °C)")+
  ylab("")+
  #scale_colour_discrete(name = "Taxon")+
  #scale_shape(guide = "none")+
  geom_pointrange(aes(size = n), alpha = 0.5, position = position_dodge(width = 0.5))+
  scale_size(range=c(0.4,1.5))+
  geom_vline(xintercept=0, lty=2)+
  scale_color_manual(values = c('red', 'orange', 'blue'),
                     limits = c('(0,0.05]', '(0.05,0.099]', '(0.099,1]'))+
  scale_x_continuous(limits=c(-15,15), oob=scales::rescale_none)+
  #geom_boxplot(varwidth = TRUE)+
  facet_grid(~ Habitat)+
  #scale_color_viridis_d(guide = "none")+
  theme(panel.background = element_rect(fill = "white"), strip.text.y = element_text(angle = 0), strip.text.x = element_blank(), panel.spacing = unit(1, "lines"),
        axis.title.y = element_text(hjust = -3.0, color = "black"), axis.title.x = element_text(face = "bold", size = 15, color = "black", vjust = 0.5), axis.text.y = element_text(face = "bold", size = 15, color = "black"), 
        axis.text.x = element_text(face = "bold", size = 15, color = "black"), panel.border = element_rect(color = "black", fill = NA, size = 2))


Duration_temp <- ggplot(df_Duration, aes(x = Slope2, y = SpeciesID, xmin = CI_lwr2, xmax = CI_upr2, color = cut(Pvalue2, breaks = c(0, 0.05, 0.099, 1))))+
  xlab("Change in duration of activity (days per °C)")+
  ylab("")+
  #xlim(c(-15,10))+
  #scale_colour_discrete(name = "Taxon")+
  #scale_shape(guide = "none")+
  geom_pointrange(aes(size = n), alpha = 0.5, position = position_dodge(width = 0.8))+
  scale_size(range=c(0.4,1.5))+
  geom_vline(xintercept=0, lty=2)+
  scale_color_manual(values = c('red', 'orange', 'blue'),
                     limits = c('(0,0.05]', '(0.05,0.099]', '(0.099,1]'))+
  scale_x_continuous(limits=c(-15,15), oob=scales::rescale_none)+
  #geom_boxplot(varwidth = TRUE)+
  facet_grid(~ Habitat)+
  #scale_color_viridis_d(guide = "none")+
  theme(panel.background = element_rect(fill = "white"), strip.text.y = element_text(angle = 0), strip.text.x = element_blank(), panel.spacing = unit(1, "lines"),
        axis.title.y = element_text(hjust = -3.0, color = "black"), axis.title.x = element_text(face = "bold", size = 15, color = "black", vjust = 0.5), axis.text.y = element_text(face = "bold", size = 15, color = "black"), 
        axis.text.x = element_text(face = "bold", size = 15, color = "black"), panel.border = element_rect(color = "black", fill = NA, size = 2))


ggarrange(Peak_temp,                             # First row 
          ggarrange(Duration_temp, labels = c("(b) Duration"), hjust = -0.1, vjust = -0.2, align = "v", font.label = list(size = 20), legend = "none"), # Second row with box and dot plots
          ncol = 1, nrow = 2, 
          labels = "(a) Peak", hjust = -0.1, vjust = 1.2, widths = c(1,1), font.label = list(size = 20), legend = "none"                                      # Labels of the scatter plot
)+
  theme(plot.margin = margin(2,2,2,4, "cm"))

ggarrange(Onset_temp,                             # First row 
          ggarrange(End_temp, labels = c("(b) End"), hjust = -0.1, vjust = -0.2, align = "v", font.label = list(size = 20), legend = "none"), # Second row with box and dot plots
          ncol = 1, nrow = 2, 
          labels = "(a) Onset", hjust = -0.1, vjust = 1.2, widths = c(1,1), font.label = list(size = 20), legend = "none"                                      # Labels of the scatter plot
)+
  theme(plot.margin = margin(0.4,0.1,0.1,0.1, "cm"))











