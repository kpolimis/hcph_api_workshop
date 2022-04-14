## ----install, warning=FALSE, eval=TRUE, include=FALSE-------------------------------------------------------------------
#' Uncomment the below code to install all relevant packages
#' Run only once. 
options(scipen=999)
# source("code/install.R")


## ----json_example1, warning=FALSE, eval=TRUE----------------------------------------------------------------------------
#'from:  https://cran.r-project.org/web/packages/jsonlite/vignettes/json-aaquickstart.html
library(jsonlite)
json <-
'[
  {"Name" : "Mario", "Age" : 32, "Occupation" : "Plumber"}, 
  {"Name" : "Peach", "Age" : 21, "Occupation" : "Princess"},
  {},
  {"Name" : "Bowser", "Occupation" : "Koopa"}
]'

prettify(json, indent = 4)

json_df <- fromJSON(json)

## ----rsocrata_setup, warning=F, message=F, eval=TRUE--------------------------------------------------------------------
library(yaml)
library(plyr)
library(dplyr)
library(readr)
library(RSocrata)
library(here)


## ----socrata, warning=F, message=F, eval=TRUE---------------------------------------------------------------------------
 #' will only run if you have an appropriately formatted file,
 #' `credentials/socrata_app_credentials.yml` with valid CDC API credential
socrata_app_credentials <- yaml.load_file(here("credentials/socrata_app_credentials.yml"))

#' Yearly Counts of Deaths by State and Select Causes, 1999-2017
#' https://data.cdc.gov/NCHS/NCHS-Leading-Causes-of-Death-United-States/bi63-dtpu
yearly_deaths_by_state_1999_2017 <- read.socrata(
  "https://data.cdc.gov/resource/bi63-dtpu.json",
  app_token = socrata_app_credentials$app_token,
  email = socrata_app_credentials$email,
  password  = socrata_app_credentials$password
)


## ----socrata_glimpse, warning=F, message=F, eval=TRUE-------------------------------------------------------------------
glimpse(yearly_deaths_by_state_1999_2017)


## ----source_nchs_mortality, warning=F, message=F, eval=TRUE-------------------------------------------------------------
#' will only run if you have an appropriately formatted file, 
#' `credentials/socrata_app_credentials.yml`, with valid CDC API credentials
source(here("code", "01_get_nchs_mortality.R"))


## ----us_deaths_time_series_prep, warning=F, message=F, eval=TRUE--------------------------------------------------------
library(lubridate)
library(scales)
library(ggplot2)
yearly_deaths_by_state_1999_2022$all_deaths <- as.numeric(yearly_deaths_by_state_1999_2022$all_deaths)
yearly_deaths_by_state_1999_2022$year <- ymd(paste0(yearly_deaths_by_state_1999_2022$year, "01-01"))
end_date <- ymd("2021-01-01")
start_date <- ymd("1999-01-01")
us_deaths_time_series <- ggplot(data = yearly_deaths_by_state_1999_2022 %>% 
               filter(state_name=="United States", year<ymd("2022-01-01"))) +
  geom_point(aes(x = year, y = all_deaths, color="darkred", size=1.5)) + 
  geom_vline(xintercept = ymd("2019-12-01"), linetype="dashed", 
             color = "black", size=1) +  
  scale_y_continuous(labels=comma) +
  scale_x_date("", breaks = date_breaks("2 year"),
               limits = c(start_date, end_date),
               labels = date_format(format = "%Y")) + 
  theme(legend.position="none") + 
  labs(x = "Date", y = "Total Deaths",
       title = "US Total Deaths over Time: 1999-2021") +
  annotate(x=ymd("2019-12-01"),y=+Inf,label="COVID-19",vjust=1,geom="label")


## ----us_deaths_time_series_plot, warning=F, message=F, eval=TRUE--------------------------------------------------------
print(us_deaths_time_series)
ggsave(here("output/us_deaths_time_series.png"),
       us_deaths_time_series, width=10.67, height=6, dpi=120)


## ----purpleair_api_manual, warning=F, message=F, eval=TRUE--------------------------------------------------------------
#' will only run if you have an appropriately formatted file,
#' `credentials/purpleair_api_credentials.yml`, with valid PurpleAir API credentials
require(httr)

purpleair_api_credentials <- yaml.load_file(here("credentials/purpleair_api_credentials.yml"))

headers <- c(
  `X-API-Key` = purpleair_api_credentials$read_key
)

base_url <- "https://api.purpleair.com/v1/sensors/"
# sensor <- "48545" # Museum District, Houston, Texas
sensor <- "25999" # Villages Of Bridgestone, Spring, TX,
sensor_url <- paste0(base_url, sensor)

result <- httr::GET(url = sensor_url, httr::add_headers(.headers=headers))

## ----purpleair_api_result, warning=F, message=F, eval=TRUE--------------------------------------------------------------
result


## ----purpleair_api_manual2, warning=F, message=F, eval=TRUE-------------------------------------------------------------
names(content(result))
length(names(content(result)$sensor))
names(content(result)$sensor)[1:20]


## ----purpleair_api_manual3, warning=F, message=F, eval=TRUE-------------------------------------------------------------
content(result)$sensor$stats


## ----purpleair_setup, warning=F, message=F, eval=TRUE-------------------------------------------------------------------
#' load all the packages
library(PWFSLSmoke)
library(AirSensor)
library(AirMonitorPlots)
library(MazamaSpatialUtils)


## ----get_data, warning=F, message=F, eval=TRUE--------------------------------------------------------------------------
#' assign a name to the new local folder
archiveBaseDir <- here("data", "Australia_on_fire")

#' check if the same-named folder exists
#' if a same-named folder exists, print the warning
#' if no same-named folder, create the folder
if (file.exists(archiveBaseDir)) {
 cat("The folder already exists")
} else {
 dir.create(archiveBaseDir)
}

#' set the package base directory to an archive of pre-generated data files 
setArchiveBaseDir(archiveBaseDir)


## ----load_australia_data, warning=F, message=F, eval=TRUE---------------------------------------------------------------
#' set package data directory
#' install required spatial data
#' initialize the package

filePath_pas <- file.path(archiveBaseDir, "pas_au.rda")
setSpatialDataDir(archiveBaseDir)
installSpatialData('NaturalEarthAdm1')
installSpatialData("CA_AirBasins")
setSpatialDataDir(archiveBaseDir)
initializeMazamaSpatialUtils() 


## ----create_pas_au, warning=F, message=F, eval=TRUE---------------------------------------------------------------------
#' Download, parse and enhance synoptic data from PurpleAir
#' and return the results as a useful tibble with class pa_synoptic
pas_au <- pas_createNew(countryCodes = "AU", includePWFSL = TRUE)

#' saving and loading the downloaded local file
#' save the synoptic data into an .rda file
save(pas_au, file = here("data", "pas_au.rda"))

#' load data from the .rda file
# pas_au <- get(load(here("data", "pas_au.rda"))) 


## ----pas_us, warning=F, message=F, eval=TRUE----------------------------------------------------------------------------
pas_us <- pas_createNew(countryCodes = "US")
pas_tx <- pas_us %>% pas_filter(stateCode=="TX")

#' saving and loading the downloaded local file
#' load data from the .rda file
save(pas_us, file = here("data", "pas_us.rda"))
save(pas_tx, file = here("data", "pas_tx.rda"))
# pas_us <- get(load(here("data", "pas_us.rda")))
# pas_tx <- get(load(here("data", "pas_tx.rda")))


## ----pas_au_leaflet, warning=F, message=F, eval=TRUE--------------------------------------------------------------------
pas_leaflet(pas_au)


## ----pas_tx_leaflet, warning=F, message=F, eval=TRUE--------------------------------------------------------------------
pas_leaflet(pas_tx)


## ----humidity_sensor, warning=F, message=F, eval=TRUE-------------------------------------------------------------------
pas_au %>% 
  pas_filter(stateCode == "NS") %>% 
  pas_leaflet(parameter = "humidity")


## ----texas_pm25_leaflet, warning=F, message=F, eval=TRUE----------------------------------------------------------------
pas_tx %>% 
  pas_leaflet(parameter = "pm25_1hr") 


## ----specific_sensor_example1, warning=F, message=F, eval=TRUE----------------------------------------------------------
gymea_bay_label <- c("Gymea Bay") #' Gymea Bay, Sydney, AU (southern Sydney)
north_sydney_label <- c("Glen Street, Milson’s Point, NSW, Australia") #' North Sydney, AU
brisbane_6th_ave_label <- c("St Lucia - 6th Ave") #' Brisbane, AU sensor

#' view unique labels in `pas_au` object
# unique(pas_au$label)

pat_gymea_bay <- pat_createNew(
    pas = pas_au, 
    label = gymea_bay_label, 
    startdate = 20191229, 
    enddate = 20200110
  )

save(pat_gymea_bay, file = here("data", "pat_gymea_bay.rda"))


## ----specific_sensor_example2, warning=F, message=F, eval=TRUE----------------------------------------------------------
pat_north_sydney <- pat_createNew(
    pas = pas_au,
    label = north_sydney_label,
    startdate = 20191229,
    enddate = 20200110
  )

save(pat_north_sydney, file = here("data", "pat_north_sydney.rda"))


## ----specific_sensor_example3, warning=F, message=F, eval=TRUE----------------------------------------------------------
pat_brisbane_6th_ave <- pat_createNew(
    pas = pas_au, 
    label = brisbane_6th_ave_label, 
    startdate = 20191229, 
    enddate = 20200110
  )

save(pat_brisbane_6th_ave, file = here("data", "pat_brisbane_6th_ave.rda"))


## ----pat_gymea_bay, warning=F, message=F, eval=TRUE---------------------------------------------------------------------
pat_multiplot(pat_gymea_bay)


## ----pat_north_sydney, warning=F, message=F, eval=TRUE------------------------------------------------------------------
pat_multiplot(pat_north_sydney)


## ----pat_brisbane_6th_ave, warning=F, message=F, eval=TRUE--------------------------------------------------------------
pat_multiplot(pat_brisbane_6th_ave)


## ----pat_houston, warning=F, message=F, eval=TRUE-----------------------------------------------------------------------
start_date <- 20200101
end_date <- 20200115

pat_houston <- pat_createNew(label = "Royal Oaks Houston Tx - Outside",
                     pas = pas_tx,
                     startdate = start_date,
                     enddate = end_date
                     )

pat_houston %>%
  pat_multiPlot(plottype = "all")

#' save the synoptic data into an .rda file
save(pat_houston, file = here("data", "pat_houston.rda")) 


## ----explore_pas, warning=F, message=F, eval=TRUE-----------------------------------------------------------------------
lon <- pat_gymea_bay$meta$longitude #' get the longitude of sensor "Gymea Bay"
lat <- pat_gymea_bay$meta$latitude #' get the latitude of sensor "Gymea Bay"

pas_sydney <- 
  pas_au %>%
  #' Filter for PurpleAir sensors 
  #' within a specified distance from specified target coordinates.
  pas_filterNear(
    longitude = lon, 
    latitude = lat, 
    radius = "50 km"
  ) 


## ----explore_pas_sydney, warning=F, message=F, eval=TRUE----------------------------------------------------------------
pas_leaflet(pas_sydney)


## ----explore_pas_texas, warning=F, message=F, eval=FALSE----------------------------------------------------------------
## library(tidygeocoder)
## 
## houston_geocode <- geo("Houston, Texas", method = "osm", full_results = TRUE)
## houston_geocode
## 
## pas_houston <-
##   pas_tx %>%
##   #' Filter for PurpleAir sensors
##   #' within a specified distance from specified target coordinates.
##   pas_filterNear(
##     longitude = ,
##     latitude = ,
##     radius =
##   )


## ----graph_pat, warning=F, message=F, eval=TRUE-------------------------------------------------------------------------
start_date <- 20191210
end_date <- 20200110

pat_chisholm <- pat_createNew(
    label = "Chisholm",
    pas = pas_au,
    startdate = start_date,
    enddate = end_date
  )

pat_moruya <- pat_createNew(
    label = "MORUYA HEADS",
    pas = pas_au,
    startdate = start_date,
    enddate = end_date
  )

pat_windang <- pat_createNew(
    label = "Windang, Ocean Street",
    pas = pas_au,
    startdate = start_date,
    enddate = end_date
  )


## ----graph_pat2, warning=F, message=F, eval=TRUE------------------------------------------------------------------------
colors <- c("Chisholm" = "#1b9e77", 
            "Moruya" = "#d95f02", 
            "Windang" = "#7570b3")

multisensor_pm25_plot <- ggplot(data = pat_chisholm$data) +
  geom_point(aes(x = pat_chisholm$data$datetime, 
                 y = pat_chisholm$data$pm25_A, 
                 color = "Chisholm"), alpha = 0.5) +
  geom_point(data = pat_moruya$data, 
             aes(x = pat_moruya$data$datetime, 
                 y = pat_moruya$data$pm25_A,
                 color = "Moruya"), alpha = 0.5) +
  geom_point(data = pat_windang$data, 
             aes(x = pat_windang$data$datetime, 
                 y = pat_windang$data$pm25_A, 
                 color = "Windang"), alpha = 0.5) +
  labs(title = "PM 2.5 channel A for multiple sensors" ) +
  xlab("date") +
  ylab("ug/m3") +
  scale_colour_manual(name="Sensor",values=colors) +
  theme(legend.position= c(0.9, 0.8))


## ----graph_pat3, warning=F, message=F, eval=TRUE------------------------------------------------------------------------
print(multisensor_pm25_plot)


## ----explore_chisholm_area_sensors, warning=FALSE, message=FALSE, eval=FALSE, include=FALSE-----------------------------
## NA


## ----explore_houston_area_pat, warning=FALSE, message=FALSE, eval=TRUE--------------------------------------------------
pasadena_ids <- c("98633", "99813")
houston_ids <- c("26659", "133994")

pas_tx %>% 
  filter(ID %in% pasadena_ids | ID %in% houston_ids)


## ----dailySoHIndexPlot, warning=FALSE, message=FALSE, eval=TRUE---------------------------------------------------------
pat_dailySoHIndexPlot(pat_chisholm)


## -----------------------------------------------------------------------------------------------------------------------



## ----air_sensor_objects, warning=FALSE, message=FALSE, eval=TRUE--------------------------------------------------------
#' create an airsensor object
airsensor_chisholm <- pat_createAirSensor(
  pat = pat_chisholm,
  parameter = "pm25",
  FUN = PurpleAirQC_hourly_AB_00
)

airsensor_houston <- pat_createAirSensor(
  pat = pat_houston,
  parameter = "pm25",
  FUN = PurpleAirQC_hourly_AB_00
)


## ----air_monitor_plots, warning=FALSE, message=FALSE, eval=TRUE---------------------------------------------------------
AirMonitorPlots::monitor_ggDailyBarplot(airsensor_chisholm)


## ----pollution_rose, warning=FALSE, message=FALSE, eval=TRUE------------------------------------------------------------
sensor_pollutionRose(sensor = airsensor_chisholm)


## ----comp, warning=F, message=F, eval=TRUE------------------------------------------------------------------------------
sensor_pollutionRose(sensor = airsensor_houston)


## ----save_analysis, warning=F, message=F, eval=TRUE, include=FALSE------------------------------------------------------
save.image(here("rdata", "intro_to_api_workshop.RData"), compress = TRUE)
# rm(list=ls("socrata_app_credentials", "purpleair_api_credentials"))
# knitr::purl("markdown/intro_to_api_workshop.Rmd", "code/intro_to_api_workshop.R")

