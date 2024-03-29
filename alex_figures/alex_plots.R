library(tidyverse)
library(Metrics)


### PLOT 1: Time series plot
alex_data <- arrow::s3_bucket('scores/parquet/', endpoint_override = 's3.flare-forecast.org') %>%
  arrow::open_dataset() %>%
  filter(reference_datetime >= '2023-01-01 00:00:00',
         reference_datetime <= '2023-04-30 00:00:00',
         site_id == 'ALEX',
         model_id == 'test_runS3',
         variable == 'temperature',
         depth == 0.5) %>%
         collect()

past_obs <- alex_data %>%
  filter(reference_datetime < '2023-01-16') %>%
  filter(reference_datetime == datetime) %>%
  drop_na(observation) %>%
  distinct(reference_datetime, observation, .keep_all=TRUE) %>%
  select(reference_datetime,observation)

future_predictions <- alex_data %>%
  filter(reference_datetime == '2023-01-15 00:00:00',
         datetime != '2023-01-14') %>%
  select(-observation)

full_data <- full_join(future_predictions,past_obs, by = 'reference_datetime')

#full_data$reference_datetime <- as.POSIXct(full_data$reference_datetime)
full_data$reference_datetime <- as.character(full_data$reference_datetime)
full_data$datetime <- as.character(full_data$datetime)

full_data$datetime <- ifelse(is.na(full_data$datetime),full_data$reference_datetime,full_data$datetime)
full_data$datetime <- as.Date(full_data$datetime)

full_data$observation <- ifelse(full_data$reference_datetime == '2023-01-15 00:00:00' & full_data$datetime != '2023-01-15',
                                NA,
                                full_data$observation)

#full_data$datetime <- ifelse(is.na(full_data$datetime),as.Date(full_data$reference_datetime, format = '%Y-%m-%d'),full_data$datetime)


alex_plot <- ggplot(data = full_data, aes(datetime, observation)) +
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


alex_plot


#### PLOT 2: RMSE Plot

alex_clean_horizon <- alex_data %>%
  filter(horizon != -1) %>%
  drop_na(observation)

horizon_store <- unique(alex_clean_horizon$horizon)

alex_rmse_compute <- alex_clean_horizon %>%
  group_by(horizon) %>%
  mutate(rmse_value = sqrt(mean((observation - median)^2))) %>%
  ungroup() %>%
  distinct(depth, horizon, .keep_all = TRUE)


rmse_plot <- ggplot(data=alex_rmse_compute, aes(x=horizon,y=rmse_value)) +
  geom_point() +
  geom_line()

rmse_plot


## PLOT 3: Bias Plot
## data objects borrowed from plot 2

alex_bias_compute <- alex_clean_horizon %>%
  group_by(horizon) %>%
  mutate(bias_value = mean(median-observation)) %>%
  #mutate(bias_value = median-observation) %>%
  ungroup() %>%
  distinct(depth, horizon, .keep_all = TRUE)

bias_plot <- ggplot(data=alex_bias_compute, aes(x=horizon,y=bias_value)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 0, linetype = 'dashed')

bias_plot

