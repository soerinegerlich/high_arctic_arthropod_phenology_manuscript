# high_arctic_arthropod_phenology_manuscript

## Content
This repository contains the code and data necessary to replicate data analyses, figures and tables in:

Gerlich, Hannah Sørine, Martin Holmstrup, Niels Martin Schmidt and Toke T. Høye. (Accepted). ***Phenological responses to climate change across taxa and local habitats in a high-Arctic arthropod community***. Ecological Monographs.

## Contact
Hannah Sørine Gerlich

Email: soger [at] ecos.au.dk

## Data usage guidelines

### Data

- Phenological observations for Zackenberg were provided by the Greenland Ecosystem Monitoring Programme. Data available at: https://data.g-e-m.dk/ The raw data downloaded from the database is included in this repository (downloaded 14. September 2022) is included in this repository along with a formatted version including all estimated phenological events.

- Environmental predictors: Temperature and snowmelt observations for Zackenberg were also provided by the Greenland Ecosystem Monitoring Programme. Data available at: https://data.g-e-m.dk/ The raw data downloaded from the database is included in this repository (downloaded 13. January 2022) along with a formatted version including all estimated temperature and timing of snowmelt estimates.

### Code 
All code provided for data preparation and analysis is licensed under a MIT License. In accordance with the license the code is available to be shared and adapted, but we would appreciate attribution to the authors, e.g. through citation of the above manuscript, and indications where changes were made. Although not mandatory, we additionally suggest that code users contact and collaborate with contributors should the code form a substantial proportion of a particular publication or analysis.

# Data preparation and clean up
All data (raw as well as cleaned and prepared) can be found here:

```
/Data/phenology_data
/Data/temperature_data
/Data/snowmelt_data

```

*Important:* Please note that this summarised data is for archival purposes only. If you intend to use the phenological observations in this dataset please refer to the data usage guidance for the raw data sets described above. 

These scripts contain R code for data preparation, cleaning and assembly:

```
/Data_cleanup.R
/Data_cleanup_Acari_lyco_correction.R
/CLIMATE_Zackenberg_temperature_data_cleanup_and_calculation_of_variables.R
/CLIMATE_Climatestation_snowdepth

```

The following path leads directly to the phenological estimates and climate variables used to conduct the analysis:

```
/Data/phenology_data/phenology_metrics.csv
/Data/phenology_data/Pheno_air_temp.csv
/Data/snowmelt_data/Snowmelt_Climatestation.xlsx

```
*Please note:* phenology_metrics.csv includes phenology estimates for analysis of temporal trends. Pheno_air_temp.csv also includes temperature data that can be related to arthropod phenology esimates.  

# Analysis scripts
Several scripts are available that include different types of analyses. However, the following quarto documents contain all relevant code and detailed descriptions of each step of the analysis performed in the study that can be used to reproduce the analysis:


```
/Temporal_trends_updated.qmd
/Pheno_response_climate.qmd
/Functional_group_phenological_responses_updated.qmd

```

The following R scripts contain relevant code to reproduce figures 2 - 6

```
/climate_fig.R
/pheno_niche_fig.R
/pheno_climate_trends_fig.R
/functional_groups_fig.R

```

# Data summaries
The following folder includes data summaries from regression analyses (temporal and climate).

```
/Data/phenology_data/Dataset_summaries

```