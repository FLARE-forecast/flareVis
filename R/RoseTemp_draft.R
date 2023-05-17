#### Trying to make rose data plot

#### Get FCR data
inUrl2 <- "https://pasta.lternet.edu/package/data/eml/edi/271/7/71e6b946b751aa1b966ab5653b01077f"
infile2 <- paste0(getwd(),"/FCR_Catwalk_2018_2022.csv")
download.file(inUrl2,infile2,method="curl")

#readin data
catwalk <- read.csv("./FCR_Catwalk_2018_2022.csv")

library(tidyverse)
library(lubridate)

#make daily temp data
daily <- catwalk %>%
  mutate(Date = as.Date(DateTime)) %>%
  group_by(Date) %>%
  summarise(temp = mean(ThermistorTemp_C_surface, na.rm = T), .groups = "drop") %>%
  filter(Date >= ymd("2022-01-01"),
         Date <= ymd("2022-12-31")) %>%
  mutate(Julian = yday(Date)) %>%
  mutate(tempbins = ifelse(temp < 10, "cold", NA),
         tempbins = ifelse(temp > 10 & temp < 20, "mild", tempbins),
         tempbins = ifelse(temp > 20, "hot", tempbins)


  )


##### Currently working data rose plot, not a function yet

# basis for this plot: http://rstudio-pubs-static.s3.amazonaws.com/179757_4fd1fbdcaa3f4f28868bfada3b9855d4.html
ggplot(data=daily, aes(x=Julian,y=temp, fill = tempbins))+
  geom_bar(stat="identity",    width=1)+  #color = "black", size =0.1,
  coord_polar()+
  # scale_fill_brewer(palette="Greens")+
  scale_fill_manual(values = c("blue", "red", "seagreen3"))+
  theme_bw()+
  labs(x = element_blank(),
       y = element_blank())



####Fail wind rose Example from ABP
# install.packages("openair")
# library(openair)

# # Visualize wind directions that
# chicago_wind=data2%>%
#   select(datetime,wind_speed,wind_dir)%>%
#   rename(date = datetime, ws = wind_speed, wd = wind_dir)
# pollutionRose(chicago_wind, pollutant="ws")
#
#  chicago_wind<- daily %>%
#   select(Date,Julian,temp)%>%
#   rename(date = Date, ws = temp, wd = Julian)
# pollutionRose(chicago_wind, pollutant="ws")





#### Reading in SUNP forecast we didn't use

#SUNP forecast
# forecast_s3 <- arrow::s3_bucket('forecasts/parquet', endpoint_override = 's3.flare-forecast.org', anonymous = TRUE)
#
# sunp_data <- arrow::open_dataset(forecast_s3) |>
#   dplyr::filter(site_id == 'sunp',
#                 reference_datetime ==  '2023-05-01 00:00:00',
#                 variable == 'temperature',
#                 depth == 1.0) |>
#   dplyr::collect()
#
# head(sunp_data)









