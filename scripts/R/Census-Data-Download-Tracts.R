#
rm(list=ls())
library(tidycensus) #Loads US Census files and data for "tidyverse" and "sf"
library(tidyverse) #Supports "tidy" functions
library(tigris) #Load Census shapefiles into R
library(sf) #Simple features for R
library(ggplot2) #Produces visual representations
library(viridis)
library(ggthemes)
library(curl)

v17 <- load_variables(2022, "acs5", cache = TRUE)
data(fips_codes)
stfips <- unique(fips_codes$state_code)[1:51]

options(tigris_class="sf") #Data from shapefiles will come as dataframes in R
options(tigris_use_cache=TRUE) #Stores files used in the analysis 


racevar <- paste0("B03002_", str_pad(rep(c(1,4,5,12)), 3,pad= "0" ),"E") # Total (001), Black (004), Nat Am (005), Hispanic (0012)
tenurevar <- paste0("B25003_", str_pad(rep(1:3), 3,pad= "0" ),"E") # Total (001), Owner Occupied (002), Renter Occupied (003)
rentvar <- paste0("B25070_", str_pad(rep(1:10), 3,pad= "0" ),"E") # Total (001), Rent LT 10% of Inc (001), ... , Rent GT 50% of Inc (010)
medhhvalvar <- "B25077_001E"
medhhincvar <- "B25119_001E"
medagevar <- "B01002_001E"
povratvar <-paste0("B17001_", str_pad(rep(c(1:2)), 3,pad= "0" ),"E") # Total (001), Under 1.0 (002)
agevar <- paste0("B01001_", str_pad(rep(c(1,3:6, 27:30, 20:25,44:49)), 3,pad= "0" ),"E") # Total (001), Men 65-85+ (20:25), Women 65-85+ (44:49)
veteranvar <- paste0("B21001_", str_pad(rep(c(1:2)), 3,pad= "0" ),"E") # Total (001), Veteran (002)
mobilvar <- paste0("B07001_", str_pad(rep(c(1,17)), 3,pad= "0" ),"E") # Total (001), No Mobility (017)
disabilvar <- paste0("B18135_", str_pad(rep(c(1,3,14,25)), 3,pad= "0" ),"E") # Total (001), Disabled by some (017)
sexvar <- paste0("B01001_", str_pad(rep(c(1,2,26)), 3,pad= "0" ),"E") # Total (001), Men and Women

other_vars <- c("B07001_001E", #mobility - total
                "B07001_017E", #mobility - same house 1 year ago
                "C24050_001E", #occupation - total
                "C24050_002E", #occupation - Agriculture, Forestry, Fishing and Hunting, and Mining
                "C24050_003E", #occupation - construction
                "B28002_013E", #internet - no internet
                "B28002_001E") #internet - total
unemployedvar <- paste0("B17005_", str_pad(rep(c(1, 6,11,17, 22)), 3,pad= "0" ),"E") #total, male below poverty unemployed; female below poverty unemployed; male at or above poverty unemployed; female at or above pov unemployed
educationvar <- paste0("B15002_", str_pad(rep(c(1, 3:10, 20:27)), 3,pad= "0" ),"E") #total, male no schooling through 12th grade no diploma, female no schooling through 12th grade no diploma#Educational attainment

paper_variables <- c(racevar, tenurevar, rentvar, medhhvalvar, medhhincvar,
                     medagevar, povratvar, agevar, veteranvar, mobilvar,
                     disabilvar, sexvar, other_vars, unemployedvar,educationvar)

paper_data<-get_acs(geography="tract",
                    state = stfips,
                    variables=paper_variables,
                    year=2022, 
                    output="wide",
                    geometry=TRUE,
                    resolution="20m") %>%
  shift_geometry()

paper_data <- paper_data %>%
  mutate(pov_percent = B17001_002E / B17001_001E * 100,
         pct_black = B03002_004E / paper_data$B03002_001E * 100,
         pct_hispanic = B03002_012E / B03002_001E * 100,
         pct_under18years = (B01001_003E + B01001_004E +B01001_005E +B01001_006E+
                               B01001_027E + B01001_028E +B01001_029E +B01001_030E) / B01001_001E*100,
         pct_65andolder = (B01001_020E+B01001_021E+B01001_022E+B01001_023E+B01001_024E+B01001_025E+B01001_044E+B01001_045E+B01001_046E+
                             B01001_047E+B01001_048E+B01001_049E)/B01001_001E*100,
         pct_immobile_1yr=(1-(B07001_017E /B07001_001E))*100,
         pct_unemployed=(B17005_006E+B17005_011E+B17005_017E+B17005_022E)/B17005_001E*100, #Percent unemployed
         pct_fem = B01001_026E/B01001_001E*100,
         pct_veteran = B21001_002E/B21001_001E *100,
         pct_disabled = (B18135_003E+B18135_014E+B18135_025E)/B18135_001E *100,
         pct_renterOccupied = B25003_003E/B25003_001E*100,
         pct_rentburdened = (B25070_007E+B25070_008E+B25070_009E+B25070_010E)/B25070_001E*100,
         pct_extractworkers = C24050_002E/C24050_001E*100,
         pct_constworkers = C24050_003E/C24050_001E*100,
         pct_nointernet = B28002_013E/B28002_001E*100,
         pct_lths = (B15002_003E+B15002_004E+B15002_005E+B15002_006E+B15002_007E+B15002_008E+B15002_009E+B15002_010E+
                       B15002_020E + B15002_021E + B15002_022E+ B15002_023E+ B15002_024E+ B15002_025E+ B15002_026E+ B15002_027E)/B15002_001E*100,
         medhhinc = B25119_001E,
         medhhval = B25077_001E,
         medage = B01002_001E
  ) %>%
  dplyr::select(GEOID, NAME, pov_percent:medage)


write_rds(paper_data, "./DATA/CENSUSDATA-TRACT.RDS")

