#' @param scores_data dataframe containing FLARE scores data
#' @param past_future_split date for the cutoff between observational data and forecasted predictions (reference_datetime for forecast)
#' @param plot_depth depth to use for time series

# alex_data <- arrow::s3_bucket('scores/parquet/', endpoint_override = 's3.flare-forecast.org') %>%
#   arrow::open_dataset() %>%
#   filter(reference_datetime >= '2023-01-01 00:00:00',
#          reference_datetime <= '2023-04-30 00:00:00',
#          site_id == 'ALEX',
#          model_id == 'test_runS3',
#          variable == 'temperature',
#          depth == 0.5) %>%
#   collect()


time_series <- function(scores_data, past_future_date_split, plot_depth){

past_obs <- scores_data %>%
  filter(reference_datetime < past_future_date_split) %>%
  filter(reference_datetime == datetime,
         depth == plot_depth) %>%
  drop_na(observation) %>%
  distinct(reference_datetime, observation, .keep_all=TRUE) %>%
  select(reference_datetime,observation)

future_predictions <- scores_data %>%
  filter(reference_datetime == past_future_date_split,
         datetime != (as.Date(past_future_date_split) -1)) %>%
  select(-observation)

full_data <- full_join(future_predictions,past_obs, by = 'reference_datetime')

full_data$reference_datetime <- as.character(full_data$reference_datetime)
full_data$datetime <- as.character(full_data$datetime)

full_data$datetime <- ifelse(is.na(full_data$datetime),full_data$reference_datetime,full_data$datetime)
full_data$datetime <- as.Date(full_data$datetime)

full_data$observation <- ifelse(full_data$reference_datetime == as.character(as.POSIXct(past_future_date_split)) & full_data$datetime != past_future_date_split,
                                NA,
                                full_data$observation)


time_plot <- ggplot(data = full_data, aes(datetime, observation)) +
  geom_point() +
  geom_line() +
  geom_line(aes(datetime,median)) +
  geom_line(aes(datetime,quantile97.5)) +
  geom_line(aes(datetime,quantile02.5)) +
  geom_vline(xintercept = as.Date('2023-01-15'), linetype = 'dashed') +
  ylab('Temperature (degC)') +
  xlab('Date') +
  annotate("text", x=as.Date('2023-01-03'), y=27, label= "Past") +
  annotate("text", x=as.Date('2023-01-23'), y=27, label= "Future")


time_plot

}

## example code for dial plot

## read in targets data (borrowed from combined_workflow.R)
library(tidyverse)
library(lubridate)


setwd('/home/rstudio/SUNP-forecast-code/')
lake_directory <- getwd()
#' Set the lake directory to the repository directory


## Make sure data is available
config_set_name <- "default"
forecast_site <- "sunp"
configure_run_file <- "configure_run.yml"
update_run_config <- TRUE

#' Source the R files in the repository

#' Generate the `config_obs` object and create directories if necessary

config_obs <- FLAREr::initialize_obs_processing(lake_directory, observation_yml = "observation_processing.yml", config_set_name = config_set_name)
config <- FLAREr::set_configuration(configure_run_file,lake_directory, config_set_name = config_set_name)

#' Clone or pull from data repositories
FLAREr::get_git_repo(lake_directory,
                     directory = config_obs$realtime_insitu_location,
                     git_repo = "https://github.com/FLARE-forecast/SUNP-data.git")

dir.create(file.path(config_obs$file_path$data_directory, "hist-data"),showWarnings = FALSE)

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

depths <- c('1.5', '10.5')

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


oxygen_gauge(oxy_data = field_format_oxy,plot_date = '2019-08-01', plot_depth = 1.5)


# plot_func <- function(current_value){
#   fig <- plot_ly(
#     domain = list(x = c(0, 1), y = c(0, 1)),
#     value = current_value,
#     title = list(text = "Rating"),
#     type = "indicator",
#     mode = "number+gauge",
#     gauge = list(
#       axis =list(range = list(100, 85)),
#       bar = list(
#         # color = 'white',
#         # line = list(width = 1),
#         thickness = 0
#       ),
#       steps = list(
#         list(range = c(85,90), color = "#b20000", name = 'E'),
#         list(range = c(90,92.5), color = "#e09999", name = 'D'),
#         list(range = c(92.5, 95), color = "#ffffb2", name = 'C'),
#         list(range = c(95, 97.5), color = '#7fbf7f', name = 'B'),
#         list(range = c(97.5,100), color = "#008000", name = 'A')),
#       threshold = list(
#         line = list(color = "red", width = 4),
#         thickness = 0.75,
#         value = current_value)))
#
#   return(fig)
# }


oxygen_gauge_func <- function(current_value){
  fig <- plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = current_value,
    #title = list(text = "Percent Oxygen Saturation (5ft)"),
    title = list(text = "Percent Oxygen Saturation (33ft)"),
    type = "indicator",
    mode = "number+gauge",
    gauge = list(
      axis =list(range = list(0, 100)),
      bar = list(
        # color = 'white',
        # line = list(width = 1),
        thickness = 0
      ),
      steps = list(
        list(range = c(0,25), color = "#b20000", name = 'TEST_NAME'),
        list(range = c(25,50), color = "#e09999", name = 'D'),
        list(range = c(50, 75), color = "#ffffb2", name = 'C'),
        list(range = c(75, 100), color = '#7fbf7f', name = 'B')),#,
      #list(range = c(97.5,100), color = "#008000", name = 'A')),
      threshold = list(
        line = list(color = "red", width = 4),
        thickness = 0.75,
        value = current_value)))

  return(fig)
}

oxygen_gauge_func(current_value = oxy_value_10m$DOSat)



### Make temp anomoly plot
field_all <- read.csv('data_raw/hist-data/hist_buoy_temp.csv')
field_all$datetime <- as.POSIXct(field_all$datetime, format = "%Y-%m-%d %H:%M:%S")
field_noon <- field_all %>%
  dplyr::mutate(day = day(datetime)) %>%
  dplyr::mutate(hour = hour(datetime)) %>%
  dplyr::mutate(minute = minute(datetime))
field_noon <- field_noon[field_noon$hour=='0' & field_noon$minute=='0',]
field_noon <- field_noon[field_noon$location=='loon',]
field_noon <- field_noon %>% dplyr::select(-location, -day, -minute, -hour)

# add depth column and remove from column name
field_format <- data.frame("DateTime" = as.Date(NA),
                           "Depth" = NA,
                           "Temp" = NA)

depths <- c('0.5', '0.75', '0.85', '1.0', '1.5', '1.75', '1.85', '2.0', '2.5', '2.75',
            '2.85', '3.0', '3.5', '3.75', '3.85', '4.5', '4.75', '4.85', '5.5', '5.75',
            '5.85', '6.5', '6.75', '6.85', '7.5', '7.75', '7.85', '8.5', '8.75', '8.85',
            '9.5', '9.75', '9.85', '10.5', '11.5', '13.5')

for (i in 1:length(depths)) {
  temp <- field_noon[,c(1, i+1)]
  temp$Depth <- depths[i]
  colnames(temp) <- c('DateTime', 'Temp', 'Depth')
  field_format <- full_join(temp, field_format)
}


# put depth as second column
field_format <- field_format %>% dplyr::select( 'DateTime', 'Depth', 'Temp') %>%
  dplyr::arrange(DateTime, Depth)
field_format <- na.omit(field_format)

field_format$Depth <- as.numeric(field_format$Depth)
# round depths to nearest integer for matching with DO
for(i in 1:nrow(field_format)){
  if(field_format$Depth[i] < 1){
    field_format$Depth[i] <- 0.5
  }else if(field_format$Depth[i]==1){
    field_format$Depth[i] <- 1.0
  }else if(field_format$Depth[i]==1.5){
    field_format$Depth[i] <- field_format$Depth[i]
  }else if(field_format$Depth[i] > 1 & field_format$Depth[i] < 2){
    field_format$Depth[i] <- 1.5
  }
  else(field_format$Depth[i] <- ceiling(field_format$Depth[i]))
}

field_format$Depth <- as.character(field_format$Depth)


temp_clim <- field_format %>%
  #filter(Depth == 1.5) %>%
  mutate(doy = yday(DateTime)) %>%
  mutate(temp_f = (Temp * (9/5)) + 32) %>%
  group_by(doy, Depth) %>%
  mutate(average = mean(temp_f, na.rm=TRUE)) %>%
  ungroup() %>%
  mutate(difference = temp_f - average)

temp_anomaly_upper <- temp_clim %>%
  filter(DateTime >= '2020-07-01',
         DateTime <= '2020-08-01',
         Depth == 1.5)

temp_anomaly_lower <- temp_clim %>%
  filter(DateTime >= '2020-07-01',
         DateTime <= '2020-08-01',
         Depth == 10)

temp_anomaly_plot <- ggplot(data=temp_anomaly_upper, aes(x=DateTime,y=difference)) +
  #geom_line() +
  geom_line(color = 'darkred') +
  geom_line(data = temp_anomaly_lower, aes(DateTime,difference), color = 'darkblue') +
  geom_hline(yintercept = 0, linetype = 'dashed') +
  xlab('Day') +
  ylab('Water Temperature Anomaly (deg F)') +
  ggtitle('Water Temperature Anomaly for July 2020') +
  theme(plot.title = element_text(hjust = 0.5))


temp_anomaly_plot

## could do the same with oxygen if interested
# oxy_clim <- field_format_oxy %>%
#   filter(Depth == 1.5) %>%
#   mutate(doy = yday(DateTime)) %>%
#   group_by(doy) %>%
#   mutate(average = mean(DO_mmol_m3, na.rm=TRUE)) %>%
#   ungroup() %>%
#   mutate(difference = DO_mmol_m3 - average)
