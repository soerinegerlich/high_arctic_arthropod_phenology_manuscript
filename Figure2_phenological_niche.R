####Code for creating Figure in high-arctic arthropod phenology ms####

library(splitstackshape)
library(ggh4x)
library(viridis)

df_phenology <- read.csv(
  "Data/phenology_data/phenology_metrics.csv",
  sep = ",",
  stringsAsFactors = FALSE,
  header = TRUE
)


df_phenology %>%
  mutate(
    Order = case_when(
      SpeciesID == "Acari" ~ "Decomposer",
      SpeciesID == "ANMU" ~ "Mixed feeder",
      SpeciesID == "Aphidoidea" ~ "Herbivore",
      SpeciesID == "Chalcidoidea" ~ "Parasitoid",
      SpeciesID == "CHCE" ~ "Mixed feeder",
      SpeciesID == "Coccoidea" ~ "Herbivore",
      SpeciesID == "Collembola" ~ "Decomposer",
      SpeciesID == "Culicidae" ~ "Mixed feeder",
      SpeciesID == "Ichneumonidae" ~ "Parasitoid",
      SpeciesID == "Linyphiidae" ~ "Predator",
      SpeciesID == "Lycosidae" ~ "Predator",
      SpeciesID == "MYSC" ~ "Mixed feeder",
      SpeciesID == "Nymphalidae" ~ "Mixed feeder",
      SpeciesID == "Phoridae" ~ "Mixed feeder",
      SpeciesID == "Thomisidae" ~ "Predator"
    )
  ) -> df_phenology

df_phenology$Order[is.na(df_phenology$Order)] <- "NA"
df_phenology <- subset(df_phenology, Order != "NA")

df_phenology$SpeciesID[df_phenology$SpeciesID == "CHCE"] <-
  "Chironomidae"
df_phenology$SpeciesID[df_phenology$SpeciesID == "ANMU"] <-
  "Muscidae"
df_phenology$SpeciesID[df_phenology$SpeciesID == "MYSC"] <-
  "Sciaridae"


df_phenology %>%
  group_by(Order, SpeciesID, Plot) %>%
  summarise(
    Average_Onset = mean(Onset, na.rm = TRUE),
    Average_Peak = mean(Peak, na.rm = TRUE),
    Average_End = mean(End, na.rm = TRUE),
    SE_Onset = sd(Onset, na.rm = TRUE) / sqrt(sum(!is.na(Onset))),
    SE_Peak = sd(Peak, na.rm = TRUE) / sqrt(sum(!is.na(Peak))),
    SE_End = sd(End, na.rm = TRUE) / sqrt(sum(!is.na(End)))
  ) -> df_mean

df_mean_order <-
  merged.stack(
    df_mean,
    id.vars = c("Order", "SpeciesID", "Plot"),
    var.stubs = c("Average_", "SE_"),
    sep = "var.stubs"
  )

names(df_mean_order) <-
  c("Order", "SpeciesID", "Plot", "Pheno_event", "DOY", "SE")

df_Peak <- subset(df_mean_order, Pheno_event == "Peak")

df_Peak %>%
  mutate(
    Habitat = case_when(
      Plot == "Art1" ~ "Pond",
      Plot == "Art2" ~ "Wet fen",
      Plot == "Art3" ~ "Mesic heath",
      Plot == "Art4" ~ "Mesic heath",
      Plot == "Art5" ~ "Arid heath",
      Plot == "Art6" ~ "Snow bed",
      Plot == "Art7" ~ "Arid heath"
    )
  ) -> df_Peak

df_Peak$Plot[df_Peak$Plot == "Art1"] <- "Plot 1"
df_Peak$Plot[df_Peak$Plot == "Art2"] <- "Plot 2"
df_Peak$Plot[df_Peak$Plot == "Art3"] <- "Plot 3"
df_Peak$Plot[df_Peak$Plot == "Art4"] <- "Plot 4"
df_Peak$Plot[df_Peak$Plot == "Art5"] <- "Plot 5"
df_Peak$Plot[df_Peak$Plot == "Art6"] <- "Plot 6"
df_Peak$Plot[df_Peak$Plot == "Art7"] <- "Plot 7"



df_Peak <-
  df_Peak[!(df_Peak$Plot == "Plot 1" &
              (df_Peak$Order == "Decomposer")), ]
df_Peak <-
  df_Peak[!(df_Peak$Plot == "Plot 1" &
              (df_Peak$Order == "Predator")), ]
df_Peak <-
  df_Peak[!(df_Peak$Plot == "Plot 2" &
              (df_Peak$SpeciesID == "Aphidoidea")), ]
#df_Peak <- df_Peak[!(df_Peak$Habitat == "Mesic heath" & (df_Peak$SpeciesID == "Aphidoidea")),]
df_Peak <-
  df_Peak[!(df_Peak$Plot == "Plot 7" &
              (df_Peak$SpeciesID == "Chalcidoidea")), ]
df_Peak <- df_Peak[!(df_Peak$Plot == "Plot 6"), ]

df_Peak$Plot <-
  factor(df_Peak$Plot,
         # Relevel group factor
         levels = c("Plot 7", "Plot 6", "Plot 5", "Plot 4", "Plot 3", "Plot 2", "Plot 1"))

df_Peak$SpeciesID <- factor(df_Peak$SpeciesID,                 # Relevel group factor
                           levels = c("Chironomidae", "Linyphiidae", "Sciaridae", "Thomisidae", "Lycosidae", "Collembola", "Muscidae", "Culicidae", 
                                      "Acari", "Ichneumonidae", "Coccoidea", "Nymphalidae", "Phoridae", "Aphidoidea", "Chalcidoidea"))

df_Peak$Habitat <-
  factor(df_Peak$Habitat,
         # Relevel group factor
         levels = c("Pond", "Wet fen", "Mesic heath", "Arid heath"))

ggplot(df_Peak[complete.cases(df_Peak), ], aes(x = DOY, y = Plot, na.rm = TRUE)) +
  geom_vline(
    xintercept = c(180, 190, 200, 210, 220, 230),
    linetype = "dashed",
    color = "gray"
  ) +
  geom_point(aes(
    color = Habitat,
    shape = Habitat,
    fill = Habitat
  ),
  size = 5,
  na.rm = TRUE) +
  xlab("Day of Year")+
  #geom_text(aes(label = round(SE, digits = 2)), hjust=-1, vjust=-0.2)+
  facet_grid(SpeciesID ~ .,
             drop = TRUE,
             scales = "free_y",
             space = "free") +
  scale_x_continuous(breaks = c(180, 190, 200, 210, 220, 230, 240)) +
  scale_shape_manual(values = c(22, 21, 24, 23)) +
  scale_fill_viridis(discrete = TRUE)+
  scale_color_viridis(discrete = TRUE)+
  geom_errorbar(aes(
    xmin = DOY - SE,
    xmax = DOY + SE,
    y = Plot,
    color = Habitat,
  )) +
  scale_y_discrete(expand = expansion(add = 1)) +
  force_panelsizes(rows = unit(unit(c(
    3, 3, 3, 3, 2, 3, 3, 1, 3, 3, 3, 3, 2, 2, 2
  ), "cm")),
  TRUE) +
  theme(
    strip.text.y = element_text(angle = 0, size = 20),
    panel.background = element_rect(fill = "white"),
    panel.spacing = unit(.3, "lines"),
    panel.border = element_rect(color = "grey60", fill = NA),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 22),
    axis.ticks.y = element_blank(),
    axis.title.x = element_text(vjust = -1, size = 28),
    legend.text = element_text(size = 22),
    legend.title = element_text(size = 22),
    strip.background = element_rect(colour = "black", fill = "white"),
    plot.margin = margin(t = 3, r = 3, b = 3, l = 5, unit = "cm")
  )
