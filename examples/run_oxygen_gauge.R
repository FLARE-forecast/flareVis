
## read in targets data (borrowed from combined_workflow.R)
library(tidyverse)
library(lubridate)


#setwd('/home/rstudio/SUNP-forecast-code/')
lake_directory <- getwd()
#' Set the lake directory to the repository directory


## Make sure data is available
config_set_name <- "default"
forecast_site <- "sunp"
configure_run_file <- "configure_run.yml"
update_run_config <- TRUE

#' Source the R files in the repository

#' Generate the `config_obs` object and create directories if necessary

#config_obs <- FLAREr::initialize_obs_processing(lake_directory, observation_yml = "observation_processing.yml", config_set_name = config_set_name)
#config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)

#' #' Clone or pull from data repositories
#' FLAREr::get_git_repo(lake_directory,
#'                      directory = config_obs$realtime_insitu_location,
#'                      git_repo = "https://github.com/FLARE-forecast/SUNP-data.git")
#'
#' dir.create(file.path(config_obs$file_path$data_directory, "hist-data"),showWarnings = FALSE)

# high frequency buoy data
FLAREr::get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/499/2/f4d3535cebd96715c872a7d3ca45c196",
                     file = file.path("hist-data", "hist_buoy_do.csv"),
                     lake_directory)#,
#curl_timeout = 120)

FLAREr::get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/499/2/1f903796efc8d79e263a549f8b5aa8a6",
                     file = file.path("hist-data", "hist_buoy_temp.csv"),
                     lake_directory)#,


## Read in oxygen data
field_oxy <- read.csv('data_raw/hist-data/hist_buoy_do.csv')

# extract midnight measurements only and only observations when buoy is deployed
field_oxy$datetime <- as.POSIXct(field_oxy$datetime, format = "%Y-%m-%d %H:%M:%S")
field_noon_oxy <- field_oxy %>%
  dplyr::mutate(day = day(datetime)) %>%
  dplyr::mutate(hour = hour(datetime)) %>%
  dplyr::mutate(minute = minute(datetime))
field_noon_oxy <- field_noon_oxy[field_noon_oxy$hour=='0' & field_noon_oxy$minute=='0',]
field_noon_oxy <- field_noon_oxy[field_noon_oxy$location=='loon',]
field_noon_oxy <- field_noon_oxy %>% dplyr::select(-location, -day, -minute, -hour)

# add depth column and remove from column name
# DO upper is at 1.5m
# DO lower is at 10.5m
# metadata on edi: https://portal.edirepository.org/nis/mapbrowse?packageid=edi.499.2

depths <- c('1.0', '10.0')

oxy <- field_noon_oxy[,c(1, 2, 3, 5)]
oxy$Depth <- 1.5
colnames(oxy) <- c('DateTime', 'DOSat', "DOppm", 'Flag', 'Depth')

oxy_2 <- field_noon_oxy[,c(1, 6, 7, 9)]
oxy_2$Depth <- 10.5
colnames(oxy_2) <- c('DateTime', 'DOSat', "DOppm", 'Flag', 'Depth')

field_format_oxy <- dplyr::full_join(oxy, oxy_2)


# put depth as second column
field_format_oxy <- field_format_oxy %>% dplyr::select( 'DateTime', 'Depth', 'DOppm', 'DOSat','Flag')
field_format_oxy <- field_format_oxy %>%
  dplyr::arrange(DateTime, Depth)

# round depths to match temp depths
field_format_oxy <- field_format_oxy %>%
  #  mutate(depth_cor = ifelse(Depth==1.5, 1.0, Depth)) %>%
  dplyr::mutate(depth_cor = ifelse(Depth==10.5, 10.0, Depth)) %>%
  dplyr::mutate(Depth = depth_cor) %>%
  dplyr::select(-depth_cor)


# remove some data with flags
field_format_oxy <- field_format_oxy[field_format_oxy$Flag != 'm' & field_format_oxy$Flag !='mcep', ]
field_format_oxy <- na.omit(field_format_oxy)
field_format_oxy$Depth <- as.character(field_format_oxy$Depth)
field_format_oxy <- field_format_oxy %>%
  dplyr::select(-Flag) %>%
  dplyr::mutate(DO_mmol_m3 = DOppm*1000/32) #to convert mg/L (or ppm) to molar unit


oxy_value_sfc <- field_format_oxy %>%
  filter(DateTime == '2019-08-01',
         Depth == 10) %>%
  mutate(upper_bound = (DOSat + (DOSat*0.05)),
         lower_bound = (DOSat - (DOSat*0.05)))

oxy_value_bottom <- field_format_oxy |>
  filter(DateTime == '2019-08-01',
         Depth == 10) |>
  mutate(upper_bound = (DOSat + (DOSat*0.05)),
         lower_bound = (DOSat - (DOSat*0.05)))


oxy_gauge_fig_sfc <- oxygen_gauge(median_oxy = oxy_value_sfc$DOSat,
                              q025_oxy =  oxy_value_sfc$lower_bound,
                              q975_oxy =  oxy_value_sfc$upper_bound)
oxy_gauge_fig_sfc


oxy_gauge_fig_bottom <- oxygen_gauge(median_oxy = oxy_value_bottom$DOSat,
                              q025_oxy =  oxy_value_bottom$lower_bound,
                              q975_oxy =  oxy_value_bottom$upper_bound)

oxy_gauge_fig_bottom




## test with bucket access
forecast_s3 <- arrow::s3_bucket('forecasts/parquet', endpoint_override = 's3.flare-forecast.org', anonymous = TRUE)

sunp_df <- arrow::open_dataset(forecast_s3) |>
  dplyr::filter(site_id == 'sunp_oxy',
                reference_datetime ==  '2023-05-09 00:00:00') |>
  collect()
