lake_directory <- getwd()

## grab data

# high frequency buoy data
FLAREr::get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/499/2/f4d3535cebd96715c872a7d3ca45c196",
                     file = file.path("hist-data", "hist_buoy_do.csv"),
                     lake_directory)#,
#curl_timeout = 120)

FLAREr::get_edi_file(edi_https = "https://pasta.lternet.edu/package/data/eml/edi/499/2/1f903796efc8d79e263a549f8b5aa8a6",
                     file = file.path("hist-data", "hist_buoy_temp.csv"),
                     lake_directory)


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

temp_anom_fig <- temp_anomaly_plot(anomaly_df = temp_anomaly_upper)
