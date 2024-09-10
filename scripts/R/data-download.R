

download.file("https://hazards.fema.gov/nri/Content/StaticDocuments/DataDownload//NRI_Table_Counties/NRI_Table_Counties.zip",
              destfile = "./DATA/NRI_Table_Counties.zip")

unzip("./DATA/NRI_Table_Counties.zip",
      exdir = "./DATA/")

NRI <- read_csv("./DATA/NRI_table_counties.csv") %>%
  group_by(WFIR_RISKR) %>%
  dplyr::summarise(POPULATION = sum(POPULATION))