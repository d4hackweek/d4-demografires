#Peticion de Perla para MicroJuris
library(tidycensus) #Loads US Census files and data for "tidyverse" and "sf"
library(tidyverse) #Supports "tidy" functions
library(tigris) #Load Census shapefiles into R
library(sf) #Simple features for R
library(ggplot2) #Produces visual representations
library(viridis)
library(ggthemes)
library(curl)


data(fips_codes)
stfips <- unique(fips_codes$state_code)[1:51]

options(tigris_class="sf") #Data from shapefiles will come as dataframes in R
options(tigris_use_cache=TRUE) #Stores files used in the analysis 

paper_variables<-c("B03003_003","B03002_004E","B03002_012E","B03002_001E","B01001_003E",
                   "B01001_004E","B01001_005E","B01001_006E","B01001_027E",
                   "B01001_028E","B01001_029E","B01001_030E","B01001_001E",
                   "B01001_020E","B01001_021E","B01001_022E","B01001_023E",
                   "B01001_024E","B01001_025E","B01001_044E","B01001_045E",
                   "B01001_046E","B01001_047E","B01001_048E","B01001_049E",
                   "B15002_002E","B15002_003E","B15002_004E","B15002_005E",
                   "B15002_006E","B15002_007E","B15002_008E","B15002_009E",
                   "B15002_010E","B15002_011E","B15002_012E","B15002_013E",
                   "B15002_014E","B15002_015E","B15002_016E","B15002_001E",
                   "B15002_020E", "B15002_021E", "B15002_022E", "B15002_023E",
                   "B15002_024E", "B15002_025E", "B15002_026E", "B15002_027E",
                   "C24050_004E","C24050_002E","C24050_029E","C24050_001E",
                   "C24050_014E","B12006_006E","B12006_011E","B12006_017E",
                   "B12006_022E","B12006_029E","B12006_033E","B12006_039E",
                   "B12006_044E","B12006_050E","B12006_055E","B12006_004E",
                   "B12006_009E","B12006_015E","B12006_020E","B12006_026E",
                   "B12006_031E","B12006_037E","B12006_042E","B12006_048E",
                   "B12006_053E","B12006_010E","B12006_008E","B25011_013E",
                   "B25011_037E","B25011_001E","B07003_004E","B07003_007E",
                   "B07003_001E","B17001_002E","B17001_001E","B17005_006E",
                   "B17005_011E","B17005_017E","B17005_022E","B17005_001E",
                   "B12006_021E","B01002_001E","B12006_032E","B12006_043E","B12006_054E",
                   "B12006_019E","B12006_030E","B12006_041E","B12006_052E",
                   "B05001_006E","B05001_001E","B26001_001E", "B01001_001E", 
                   "B01001_026E", "B03002_005E", "B03002_001E", "B03002_007E",
                   "B19013_001E")

vars10 <- c("P005003", "P005004", "P005006", "P004003")

varsag<-c("B01001_025E","B01001_049E")

#data2<-get_decennial(geography="block",                variables=vars10, 
#  year = 2010, summary_var = "P001001",state="PR")


paper_data<-get_acs(geography="tract",
                    state = stfips,
                    variables=paper_variables,
                    year=2022, 
                    output="wide",
                    geometry=TRUE,
                    resolution="20m") %>%
  shift_geometry()

#paper_data<-get_acs(state=72,geography="state",variables=varsag,year=2019, output="wide") 


attach(paper_data) #Will make the data part of R, and then will send it back to the original database. 
#This does not requires us to write "paper_data" everytime in front of each variable. 
#Note that we are creating the variables inside "paper_data"

paper_data$pov_percent<-B17001_002E/B17001_001E #Calculates the percent of the population below the poverty level

paper_data$pct_black<-B03002_004E/B03002_001E*100 #Calculates the percent non-Hispanic black

paper_data$pct_hispanic<-B03002_012E/B03002_001E*100

paper_data$pct_under18years<-(B01001_003E+B01001_004E+B01001_005E+B01001_006E+B01001_027E+B01001_028E+B01001_029E+B01001_030E)/B01001_001E*100 #Calculates the percent of the population under 18 years (0-17 years old)

paper_data$pct_65andolder<-(B01001_020E+B01001_021E+B01001_022E+B01001_023E+B01001_024E+B01001_025E+B01001_044E+B01001_045E+B01001_046E+
                              B01001_047E+B01001_048E+B01001_049E)/B01001_001E*100
#Calculates the percent of the population that is 65 years and older

paper_data$pct_immobile_1yr<-(B07003_004E+B07003_007E)/B07003_001E*100 #Calculates percent immbole 1 year ago

paper_data$percent_lessthanHS<-(B15002_003E+B15002_004E+B15002_005E+B15002_006E+B15002_007E+B15002_008E+B15002_009E+B15002_010E+
                                  B15002_020E + B15002_021E + B15002_022E+ B15002_023E+ B15002_024E+ B15002_025E+ B15002_026E+ B15002_027E)/B15002_001E*100
#Calculates the percent of the population over 25 years without a high school education 

#The following code classified counties as southern if they are in a state in the Southern Census Region. It assigns a one to southern counties and a zero to non-southern counties.
paper_data$pct_manufacturing<-C24050_004E/C24050_001E*100 #Population working in Manufacturing Sector 

paper_data$pct_extractive<-C24050_002E/C24050_001E*100    #Population working in Agriculture, Forestry, Fishing, and Mining

paper_data$pct_service<-C24050_029E/C24050_001E*100       #Population woking in the Service Sector

paper_data$pct_government<-C24050_014E/C24050_001E*100    #Population working in Public Administration (Government)

paper_data$pct_female_headed_hh<-(B25011_013E+B25011_037E)/B25011_001E*100 #Female Headed Households

paper_data$pct_fem_employed<-(B12006_010E+B12006_021E+B12006_032E+B12006_043E+B12006_054E)/(B12006_008E+B12006_019E+B12006_030E+B12006_041E+B12006_052E)*100 #Percent of females employed

paper_data$pct_unemployed<-(B17005_006E+B17005_011E+B17005_017E+B17005_022E)/B17005_001E*100 #Percent unemployed

paper_data$pov<-(paper_data$pov_percent/(1-paper_data$pov_percent)) #The author's make this transformation in the original paper.

paper_data$pov_log<-log(paper_data$pov,base = exp(1)) #The author's make this transformation in the original paper. 

paper_data$noncit<-(paper_data$B05001_006E/paper_data$B05001_001E)*100

paper_data$female <- (paper_data$B01001_026E/paper_data$B01001_001E)*100 #percent female

paper_data$pct_native <- (paper_data$B03002_007E/paper_data$B03002_001E)*100 #pct native_american


paper_data$cat<-ifelse(paper_data$pct_65andolder<14,"Aging or Aged",
                        ifelse(paper_data$pct_65andolder<20,"Aging or Aged", "Super-Aged"))

write_rds(paper_data, "./DATA/CENSUSDATA.RDS")


# 
# library(tidyverse)
# library(tigris) 
# 
# plot(paper_data$geometry)
# 
# ggplot(paper_data,aes(fill=(cat)))+
#   geom_sf()+  
#   labs(x = NULL, 
#        y = NULL, 
#        title = "Percent of the Population 65 years and older", 
#        subtitle = "ACS, 2015-2019")+
#   theme(legend.title=element_blank(),
#         plot.title=element_text(size=16),
#         plot.subtitle =element_text(size=14),
#         plot.caption=element_text(size=12)) +
#         scale_fill_manual(values=c("white","pink"))
# 
# 
# paper_data$cat2<-ifelse(paper_data$B01002_001E<35,"< 35",
#                        ifelse(paper_data$B01002_001E<40.01,"35-40",
#                               ifelse(paper_data$B01002_001E<45.01,"40-45",
#                                             "Over 45")))
# 
# 
# 
# ggplot(paper_data,aes(fill=(cat2)))+
#   geom_sf()+  theme_map() +
#   labs(x = NULL, 
#        y = NULL, 
#        title = "Percent of the Population 65 years and older", 
#        subtitle = "Puerto Rico Community Survey, 2015-2019")+
#   theme(legend.title=element_blank(),
#         plot.title=element_text(size=16),
#         plot.subtitle =element_text(size=14),
#         plot.caption=element_text(size=12))
# 
# 
# 
# 
# 
# 
# 
# 
# pr_county<-get_acs(geography="zcta",variables=c(renter="B25003_003"), state="PA",summary_var="B25003_001", year=2018, geometry=TRUE)
# 
# 
# library(openxlsx)
# write.xlsx(pr_county2,"C:/Users/ars39/Desktop/tracts_names.xlsx")
# 
# pr_county$pctrent<-pr_county$estimate/pr_county$summary_est
# 
# ggplot(pr_county,aes(fill=estimate))+
#   geom_sf()+  theme_map() +
#   labs(x = NULL, 
#        y = NULL, 
#        title = "Porciento de las personas que pagan renta", 
#        subtitle = "Puerto Rico Community Survey, 2014-2018", 
#        caption = "Preparado por Alexis R. Santos @appdemography")+
#   theme(legend.title=element_blank(),
#         plot.title=element_text(size=16),
#         plot.subtitle =element_text(size=14),
#         plot.caption=element_text(size=12))
# 
# #Empleo
# #Auto-Empleo
# pr_county2<-get_acs(geography="county",variables=c("C24060_031","C24060_013"), state="PR",summary_var="C24060_001", year=2018, geometry=TRUE,output="wide")
# 
# ggplot(pr_county2,aes(fill=((C24060_031E+C24060_013E)/summary_est)*100))+
#   geom_sf()+  theme_map() +
#   labs(x = NULL, 
#        y = NULL, 
#        title = "Porciento de empleados - Auto-Empleo/Contratistas", 
#        subtitle = "Puerto Rico Community Survey, 2014-2018", 
#        caption = "Preparado por Alexis R. Santos @appdemography")+
#   theme(legend.title=element_blank(),
#         plot.title=element_text(size=16),
#         plot.subtitle =element_text(size=14),
#         plot.caption=element_text(size=12))
# 
# ggplot(pr_county2,aes(fill=((C24060_031E)/summary_est)*100))+
#   geom_sf()+  theme_map() +
#   labs(x = NULL, 
#        y = NULL, 
#        title = "Porciento de empleados - Contratistas", 
#        subtitle = "Puerto Rico Community Survey, 2014-2018", 
#        caption = "Preparado por Alexis R. Santos @appdemography")+
#   theme(legend.title=element_blank(),
#         plot.title=element_text(size=16),
#         plot.subtitle =element_text(size=14),
#         plot.caption=element_text(size=12))
# 
# #Servicio Privado
# pr_county3<-get_acs(geography="county",variables=c("C24060_009"), state="PR",summary_var="C24060_001", year=2018, geometry=TRUE,output="wide")
# 
# ggplot(pr_county3,aes(fill=((C24060_009E)/summary_est)*100))+
#   geom_sf()+  theme_map() +
#   labs(x = NULL, 
#        y = NULL, 
#        title = "Porciento de empleados - Sector Servicios", 
#        subtitle = "Puerto Rico Community Survey, 2014-2018", 
#        caption = "Preparado por Alexis R. Santos @appdemography")+
#   theme(legend.title=element_blank(),
#         plot.title=element_text(size=16),
#         plot.subtitle =element_text(size=14),
#         plot.caption=element_text(size=12))
# 
# pr_internet<-get_acs(geography="county",variables=c(internet="B28002_007"), state="PR",summary_var="B28002_001", year=2017, geometry=TRUE)
# 
# pr_internet$pct<-pr_internet$estimate/pr_internet$summary_est*100
# 
# pr_internet$urban<-ifelse(pr_internet$GEOID %in% c("72007","72025","72035","72041",
#                                    "72063","72069","72077","72103",
#                                    "72085","72129","72151"),"Yes","No")
# 
# pr_internet<-subset(pr_internet,urban=="Yes")
# 
# unemp <- pr_internet  %>%
#   group_by(urban=="Yes",NAME) %>%
#   summarize(
#     inter=pct)
# 
# 
# ggplot(pr_internet,aes(fill=as.factor((cat))))+
#   geom_sf()+  theme_map() +
#   labs(x = NULL, 
#        y = NULL, 
#        title = "Porciento de casas con acceso al internet de alta velocidad (no celulares)", 
#        subtitle = "Puerto Rico Community Survey, 2014-2018", 
#        caption = "Preparado por Alexis R. Santos @appdemography")+
#   theme(legend.title=element_blank(),
#         plot.title=element_text(size=16),
#         plot.subtitle =element_text(size=14),
#         plot.caption=element_text(size=12))+
#         scale_fill_discrete(name = "Acceso al Internet", labels = c("< 20%", "20%-29%", "30%-39%","40%-49%", "50% +"))
# 
# pr_internet<-get_acs(geography="tract",variables=c("B28002_001","B28002_006","B28002_013"), state="PR", year=2018, geometry=TRUE,output = "wide")
# 
# pr_internet$pct<-(pr_internet$B28002_006E)/pr_internet$B28002_001E*100
# 
# pr_internet$cat<-ifelse(pr_internet$pct<10.01,1,
#                         ifelse(pr_internet$pct<20.01,2,
#                                ifelse(pr_internet$pct<30.01,3,
#                                       ifelse(pr_internet$pct<40.01,4,
#                                              5))))
# 
# 
# 
# 
# 
# ggplot(pr_internet,aes(fill=as.factor((cat))))+
#   geom_sf()+  theme_map() +
#   labs(x = NULL, 
#        y = NULL, 
#        title = "Porciento de solo con celular para uso de internet", 
#        subtitle = "Puerto Rico Community Survey, 2014-2018", 
#        caption = "Preparado por Alexis R. Santos @appdemography")+
#   theme(legend.title=element_blank(),
#         plot.title=element_text(size=16),
#         plot.subtitle =element_text(size=14),
#         plot.caption=element_text(size=12))+
#   scale_fill_discrete(name = "Acceso al Internet", labels = c("< 10%", "10%-19%", "20%-29%","30%-39%", "40% +"))
# 
# 
# 
# us_internet<-get_acs(geography="county",variables=c(internet="B28002_007"),summary_var="B28002_001", year=2018, geometry=TRUE)
# 
# us_internet$pct<-us_internet$estimate/us_internet$summary_est*100
# 
# us_internet$cat<-ifelse(us_internet$pct<20.01,1,
#                         ifelse(us_internet$pct<30.01,2,
#                                ifelse(us_internet$pct<40.01,3,
#                                       ifelse(us_internet$pct<50.01,4,
#                                              5))))
# 
# ggplot(us_internet,aes(fill=as.factor((cat))))+
#   geom_sf()+  theme_map() +
#   labs(x = NULL, 
#        y = NULL, 
#        title = "Porciento de casas con acceso al internet de alta velocidad (no celulares)", 
#        subtitle = "Puerto Rico Community Survey, 2014-2018", 
#        caption = "Preparado por Alexis R. Santos @appdemography")+
#   theme(legend.title=element_blank(),
#         plot.title=element_text(size=16),
#         plot.subtitle =element_text(size=14),
#         plot.caption=element_text(size=12))+
#   scale_fill_discrete(name = "Acceso al Internet", labels = c("< 20%", "20%-29%", "30%-39%","40%-49%", "50% +"))
# 
# 
# B28001011
# 
# 
# pr_computer<-get_acs(geography="county",variables=c("C24030_005","C24030_032"),summary_var="C24030_001",state="PR", year=2018, geometry=TRUE)
# 
# pr_computer$pct<-pr_computer$estimate/pr_computer$summary_est*100
# 
# pr_computer$cat<-ifelse(pr_computer$pct<20.01,1,
#                         ifelse(pr_computer$pct<30.01,2,
#                                ifelse(pr_computer$pct<40.01,3,
#                                       ifelse(pr_computer$pct<50.01,4,
#                                              5))))
# 
# ggplot(pr_computer,aes(fill=as.factor((cat))))+
#   geom_sf()+  theme_map() +
#   labs(x = NULL, 
#        y = NULL, 
#        title = "Porciento de casas con acceso a computadora", 
#        subtitle = "Puerto Rico Community Survey, 2014-2018", 
#        caption = "Preparado por Alexis R. Santos @appdemography")+
#   theme(legend.title=element_blank(),
#         plot.title=element_text(size=16),
#         plot.subtitle =element_text(size=14),
#         plot.caption=element_text(size=12))+
#   scale_fill_discrete(name = "Acceso al Internet", labels = c("< 20%", "20%-29%", "30%-39%","40%-49%", "50% +"))
# 
# 
# 
# us_county<-get_acs(geography="county",variables=c("B25001_001"), year=2018, geometry=TRUE)
# 
# area_county <- counties(year = 2017, cb = TRUE, class = "sf", progress_bar = FALSE) %>%
#   filter(as.numeric(GEOID)<71999) %>%
#   mutate(area = ALAND / 2589988)
# 
# area<-area_county@data
# 
# 
# 
# pr_minning<-get_acs(geography="county",variables=c("C24030_005","C24030_032","C24030_001"),state="PR", year=2018, geometry=TRUE,output = "wide")
# 
# ggplot(pr_minning,aes(fill=(C24030_005E+C24030_032E)/C24030_001E*100))+
#   geom_sf()+  theme_map() +
#   labs(x = NULL, 
#        y = NULL, 
#        title = "Porciento de empleados del sector de miner?a", 
#        subtitle = "Puerto Rico Community Survey, 2014-2018", 
#        caption = "Preparado por Alexis R. Santos @appdemography")+
#   theme(legend.title=element_blank(),
#         plot.title=element_text(size=16),
#         plot.subtitle =element_text(size=14),
#         plot.caption=element_text(size=12))+
#   scale_fill_discrete(name = "% eM", labels = c("< 20%", "20%-29%", "30%-39%","40%-49%", "50% +"))
# 
# 
# 
# 
# pr_minning<-get_acs(geography="county",variables=c("C24030_005","C24030_032","C24030_001"),state="PR", year=2018, geometry=TRUE,output = "wide")
# 
# ggplot(pr_minning,aes(fill=(C24030_005E+C24030_032E)/C24030_001E*100))+
#   geom_sf()+  theme_map() +
#   labs(x = NULL, 
#        y = NULL, 
#        title = "Porciento de empleados del sector de miner?a", 
#        subtitle = "Puerto Rico Community Survey, 2014-2018", 
#        caption = "Preparado por Alexis R. Santos @appdemography")+
#   theme(legend.title=element_blank(),
#         plot.title=element_text(size=16),
#         plot.subtitle =element_text(size=14),
#         plot.caption=element_text(size=12))+
#   scale_fill_discrete(name = "% eM", labels = c("< 20%", "20%-29%", "30%-39%","40%-49%", "50% +"))
# 
# 
# 
# 
# 
# 
# 
# 
# pr_foreign<-get_acs(geography="county",variables=c("B06001PR_049"),summary_var="B06001PR_001",state="PR", year=2019, geometry=TRUE)
# 
# pr_foreign$pct<-pr_foreign$estimate/pr_foreign$summary_est*100
# 
# pr_foreign$cat<-ifelse(pr_foreign$pct<2,1,
#                         ifelse(pr_foreign$pct<4,2,
#                                ifelse(pr_foreign$pct<6,3,
#                                       ifelse(pr_foreign$pct<8,4,
#                                              ifelse(pr_foreign$pct<10,4,
#                                              6)))))
# 
# ggplot(pr_foreign,aes(fill=as.factor((cat))))+
#   geom_sf()+  theme_map() +
#   labs(x = NULL, 
#        y = NULL, 
#        title = "Percent foreign-born population in Puerto Rico ", 
#        subtitle = "Puerto Rico Community Survey, 2015-2019", 
#        caption = "Preparado por Alexis R. Santos @appdemography")+
#   theme(legend.title=element_blank(),
#         plot.title=element_text(size=16),
#         plot.subtitle =element_text(size=14),
#         plot.caption=element_text(size=12))+
#   scale_fill_discrete(name = "", labels = c("< 2%", "2%-3.99%", "4-5.99%","6%-9.99%", "10% +"))
# 
# 
# 
# paper_variables<-c("B06007PR_012E","B06007PR_009E")
# 
# paper_data<-get_acs(state=72,geography="county",variables=paper_variables,year=2019, output="wide",geometry=TRUE) 
# 
# paper_data$pct<-(paper_data$B06007PR_012E/paper_data$B06007PR_009E)*100
# 
# paper_data$cat<-ifelse(paper_data$pct<5,1,
#                        ifelse(paper_data$pct<10,2,
#                               ifelse(paper_data$pct<15,3,
#                                      ifelse(paper_data$pct<20,4,
#                                             ifelse(paper_data$pct<20,5,
#                                                    6)))))
# 
# paper_data$cat2<-ifelse(paper_data$pct<=20,0,1)
# paper_data$cat3<-ifelse(paper_data$pct<=25,0,1)
# paper_data$cat5<-ifelse(paper_data$pct<=30,0,1)
# 
# 
# paper_data$cat4<-ifelse(paper_data$pct<=15,0,1)
# 
# ggplot(paper_data,aes(fill=((B06007PR_012E)/B06007PR_009E)*100))+
#   geom_sf()+  theme_map() +
#   labs(x = NULL, 
#        y = NULL, 
#        title = "Porciento de la poblacion nacida en PR que habla Ingl?s -Muy Bien-", 
#        subtitle = "Puerto Rico Community Survey, 2015-2019", 
#        caption = "Preparado por Alexis R. Santos @appdemography")+
#   theme(legend.title=element_blank(),
#         plot.title=element_text(size=16),
#         plot.subtitle =element_text(size=14),
#         plot.caption=element_text(size=12))
# 
# 
# 
# ggplot(paper_data,aes(fill=as.factor(cat)))+
#   geom_sf()+  theme_map() +
#   labs(x = NULL, 
#        y = NULL, 
#        title = "Porciento de la poblacion nacida en PR que habla Ingl?s -Muy Bien-", 
#        subtitle = "Puerto Rico Community Survey, 2015-2019", 
#        caption = "Preparado por Alexis R. Santos @appdemography")+
#   theme(legend.title=element_blank(),
#         plot.title=element_text(size=16),
#         plot.subtitle =element_text(size=14),
#         plot.caption=element_text(size=12))+
#   scale_fill_discrete(name = "", labels = c("< 5%", "5%-9.99%", "10-14.99%","15%-19.99%", "20% +"))
# 
# 
# ggplot(paper_data,aes(fill=as.factor(cat2)))+
#   geom_sf()+  theme_map() +
#   labs(x = NULL, 
#        y = NULL, 
#        title = "Porciento de la poblacion nacida en PR que habla Ingl?s -Muy Bien-", 
#        subtitle = "Puerto Rico Community Survey, 2015-2019", 
#        caption = "Preparado por Alexis R. Santos @appdemography")+
#   theme(legend.title=element_blank(),
#         plot.title=element_text(size=16),
#         plot.subtitle =element_text(size=14),
#         plot.caption=element_text(size=12))+
#   scale_fill_discrete(name = "", labels = c("< 20%", "20% +"))
# 
# 
# 
# ggplot(paper_data,aes(fill=as.factor(cat3)))+
#   geom_sf()+  theme_map() +
#   labs(x = NULL, 
#        y = NULL, 
#        title = "Porciento de la poblacion nacida en PR que habla Ingl?s -Muy Bien-", 
#        subtitle = "Puerto Rico Community Survey, 2015-2019", 
#        caption = "Preparado por Alexis R. Santos @appdemography")+
#   theme(legend.title=element_blank(),
#         plot.title=element_text(size=16),
#         plot.subtitle =element_text(size=14),
#         plot.caption=element_text(size=12))+
#   scale_fill_discrete(name = "", labels = c("< 25%", "25% +"))
# 
# 
# 
# ggplot(paper_data,aes(fill=as.factor(cat4)))+
#   geom_sf()+  theme_map() +
#   labs(x = NULL, 
#        y = NULL, 
#        title = "Porciento de la poblacion nacida en PR que habla Ingl?s -Muy Bien-", 
#        subtitle = "Puerto Rico Community Survey, 2015-2019", 
#        caption = "Preparado por Alexis R. Santos @appdemography")+
#   theme(legend.title=element_blank(),
#         plot.title=element_text(size=16),
#         plot.subtitle =element_text(size=14),
#         plot.caption=element_text(size=12))+
#   scale_fill_discrete(name = "", labels = c("< 15%", "15% +"))
# 
# 
# ggplot(paper_data,aes(fill=as.factor(cat5)))+
#   geom_sf()+  theme_map() +
#   labs(x = NULL, 
#        y = NULL, 
#        title = "Porciento de la poblacion nacida en PR que habla Ingl?s -Muy Bien-", 
#        subtitle = "Puerto Rico Community Survey, 2015-2019", 
#        caption = "Preparado por Alexis R. Santos @appdemography")+
#   theme(legend.title=element_blank(),
#         plot.title=element_text(size=16),
#         plot.subtitle =element_text(size=14),
#         plot.caption=element_text(size=12))+
#   scale_fill_discrete(name = "", labels = c("<=30%", "30% +"))
