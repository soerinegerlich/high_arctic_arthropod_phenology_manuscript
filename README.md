# high_arctic_arthropod_phenology_manuscript

## Content
This repository contains the code and data necessary to replicate data analyses, figures and tables in:

Gerlich, Hannah Sørine, Martin Holmstrup, Niels Martin Schmidt and Toke T. Høye. (2024). Preprint. ***Phenological responses to climate change across taxa and local habitats in a high-Arctic arthropod community***

## Contact
Hannah Sørine Gerlich

Email: soger [at] ecos.au.dk

## Data usage guidelines

### Data

- Phenological observations for Zackenberg were provided by the Greenland Ecosystem Monitoring Programme. Data available at: https://data.g-e-m.dk/ The raw data downloaded from the database is included in this repository (downloaded 14. September 2022) is included in this repository along with a formatted version including all estimated phenological events.

- Environmental predictors: Temperature and snowmelt observations for Zackenberg were also provided by the Greenland Ecosystem Monitoring Programme. Data available at: https://data.g-e-m.dk/ The raw data downloaded from the database is included in this repository (downloaded 13. January 2022) along with a formatted version including all estimated temperature and timing of snowmelt estimates.

### Code 
All code provided for data preparation and analysis is licensed under a [Creative Commons Attribution 4.0 International License](http://creativecommons.org/licenses/by/4.0/). In accordance with the license the code is available to be shared and adapted, but requires attribution to the authors, e.g. through citation of the above manuscript, and indications where changes were made. Although not mandatory, we additionally suggest that code users contact and collaborate with contributors should the code form a substantial proportion of a particular publication or analysis.

# Data preparation and clean up
The data preparation, cleaning and assembly scripts can be found here:

```
/Data/phenology_data
/Data/temperature_data
/Data/snowmelt_data

```

*Important:* Please note that this summarised data is for archival purposes only. If you intend to use the phenological observations in this dataset please refer to the data usage guidance for the raw data sets described above. 

The following path leads directly to the phenological estimates and climate variables used to conduct the analysis:

```
/Data/phenology_data/phenology_metrics.csv
/Data/phenology_data/Pheno_air_temp.csv
/Data/snowmelt_data/Snowmelt_Climatestation.xlsx

```
*Please note:* phenology_metrics.csv includes phenology estimates for analysis of temporal trends. Pheno_air_temp.csv also includes temperature data that can be related to arthropod phenology esimates.  

# Analysis scripts
The following quarto documents with all relevant code and detailed descriptions of each step of the analysis performed can be used to reproduce the analysis:


```
/Temporal_trends.qmd
/Pheno_response_climate.qmd
/Multiple_regression_arthropod_taxa_responses_climate_PEAK.qmd
/Multiple_regression_arthropod_taxa_responses_climate_DURATION.qmd
/Functional_group_phenological_responses.qmd

```

The following R scripts contains relevant code to reproduce figures 1 - 5

```
/Figure1_climate.R
/Figure2_phenological_niche.R
/Figure3_and_4_pheno_responses.R
/Figure5_functional_groups.R

```


