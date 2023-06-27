####
# Author: Aidan Watmuff
# Date: 22/03/2023
# Purpose: Derive a localised version of IMD deciles for Bradford - aka 'Bradford IMD'. This uses the the methodology for calculating the national deciles, and applies it to only Bradford LSOAs.
#           This provides an IMD decile measure with meaningful variation that is not captured using the national deciles (due to the prevalent deprivation across Bradford).
# Output: CSV containing LSOA codes and IMD 2019 deciles calculated for Bradford LSOAs
# WARNING: Is only applicable to Bradford. Cannot be compared to the national (original) version of the IMD, which may reduce the generalisablity of your research.  
####

####
# Process:
# 1)	Select IMD data for only Bradford LSOAs
# 2)	Rank the LSOAs by IMD score (ascending)
# 3)	Split the ranked LSOAs into deciles and assign appropriate value
####

### Set up ----
library(tidyverse)#data wrangling
library(sf)       #for spatial data
library(sfheaders)#conversion from/to sf objects

# set working directory. all downloaded files will be saved here, and new csv created will be written here as well. 
setwd("your/file/path")

# Download IMD data
# link to imd data: https://opendatacommunities.org/resource?uri=http%3A%2F%2Fopendatacommunities.org%2Fdata%2Fsocietal-wellbeing%2Fimd2019%2Findices 
# then read csv
imd2019_dclg_data <- read.csv("imd2019lsoa.csv")

# Download lsoa shapefile
# link to lsoa shapefile: https://geoportal.statistics.gov.uk/datasets/ons::lower-layer-super-output-areas-dec-2011-boundaries-full-extent-bfe-ew-v3/explore?location=53.359775%2C-1.185292%2C7.82
# click 'Download' then 'Download Shapefile'
# then read in shapefile
lsoa_data <- st_read("LSOA_2011_full_extent/Lower_Layer_Super_Output_Areas__December_2011__Boundaries_Full_Extent__BFE__EW_V3.shp")


### Format data ----
# identify table structure
imd2019_dclg_data %>%
  count(Indices.of.Deprivation, Measurement)

# BFD decile variable
# formatting LSOA data
lsoa_df <- lsoa_data %>%
  # convert LSOA spatial data to dataframe
  sf_to_df(., fill = T)  %>%
  # select only one record per LSOA code
  distinct(LSOA11CD, .keep_all = T) %>%
  # keep LSOA code and name columns
  select(LSOA11CD, LSOA11NM)

# format imd2019 data
imd2019_dclg <- imd2019_dclg_data %>%
  # match LSOA code variable name to LSOA table 
  rename(LSOA11CD = FeatureCode)

### Create local Bradford declile ----
# create IMD decile for bradford
imd_2019_bfd <- imd2019_dclg %>%
  # data is in long format, so needs filtering frist to get IMD rank data. 
  # select required measurements
  filter(Indices.of.Deprivation == "a. Index of Multiple Deprivation (IMD)" & Measurement == "Rank") %>%
  # join lsoa table with LSOA  names 
  left_join(lsoa_df, by = "LSOA11CD") %>%
  # select only LSOA in Bradford
  filter(grepl("Bradford", LSOA11NM)) %>%
  # arrange in by LSOA rank (lowest to highest)
  arrange(Value) %>%
  # assign each LSOA in BFD a rank based on their score (lowest score = lowest rank)
  mutate(bfd_IMD_rank = 1:nrow(.),
         # then  split ranks into deciles, and assign value corresponding to decile
         IMD_2019_decile_bfd = ntile(bfd_IMD_rank, 10)) %>%
  # keep only LSOA code and Bradford decile variable
  select(LSOA11CD, IMD_2019_decile_bfd)


# checks to make sure deciles have been allocated correctly 
hist(imd_2019_bfd$IMD_2019_decile_bfd, breaks = seq(0, 10, 1))

imd_2019_bfd %>%
  count(IMD_2019_decile_bfd)

### Output ----
# write csv to working directory. Contains LSOA code and  Bradford decile. 
write.csv(imd_2019_bfd, "imd_2019_bfd_deciles.csv", row.names = F)
