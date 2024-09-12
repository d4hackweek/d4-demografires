source("./SCRIPTS/R/000-Libraries.R")

## Downloads the National Fire Risk Data from FEMA. At the County-level.
download.file("https://hazards.fema.gov/nri/Content/StaticDocuments/DataDownload//NRI_Table_Counties/NRI_Table_Counties.zip",
              destfile = "./DATA/NRI_Table_Counties.zip")

## Unzipping the NRI data into the `./DATA/` folder.
unzip("./DATA/NRI_Table_CensusTracts.zip",
      exdir = "./DATA/")

## Running a check
NRI <- read_csv("./DATA/NRI_table_CensusTracts.csv") %>%
  dplyr::select(STATE:POPULATION, AREA, WFIR_RISKR, SOVI_SCORE )

NRI <- read_csv("./DATA/NRI_table_Counties.csv") %>%
  dplyr::select(STATE:POPULATION, AREA, WFIR_RISKR, SOVI_SCORE )

source("./SCRIPTS/R/Census-Data-Download.R")

alldat <- left_join(NRI, paper_data, by = c("STCOFIPS" = "GEOID")) %>%
  mutate(WFIRRISK = if_else(WFIR_RISKR %in% c("Very High", "Relatively High"),"WildFires","Not Wildfires"))

write_rds(alldat, "./DATA/ALLDATA-TRACT.RDS")
