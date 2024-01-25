# This file cleans the raw data from ./01-download_data.R and stores it appropriately for use in this paper
library(arrow)
library(knitr)
library(janitor)
library(lubridate)
library(opendatatoronto)
library(tidyverse)


# reading from the parquet files & making a dataset for each
june_data <- read_parquet("inputs/data/unedited_bikeshare_june_data.parquet")
july_data <- read_parquet("inputs/data/unedited_bikeshare_july_data.parquet")
august_data <- read_parquet("inputs/data/unedited_bikeshare_august_data.parquet")

# cleaning the data and also renaming the columns
# 1. June
june_data_clean <-
  clean_names(june_data) |>
  select(bike_share_ridership_2023_06_csv_trip_id, bike_share_ridership_2023_06_csv_trip_duration, 
         bike_share_ridership_2023_06_csv_start_time, bike_share_ridership_2023_06_csv_start_station_name,
         bike_share_ridership_2023_06_csv_end_time, bike_share_ridership_2023_06_csv_end_station_name,
         bike_share_ridership_2023_06_csv_user_type) |> 
  set_names("id", "duration", 
            "start_time", "start_station_name", 
            "end_time", "end_station_name",
            "user_type") |>
  mutate(duration = duration / 100)

# 2. July
july_data_clean <-
  clean_names(july_data) |>
  select(bike_share_ridership_2023_07_csv_trip_id, bike_share_ridership_2023_07_csv_trip_duration, 
         bike_share_ridership_2023_07_csv_start_time, bike_share_ridership_2023_07_csv_start_station_name,
         bike_share_ridership_2023_07_csv_end_time, bike_share_ridership_2023_07_csv_end_station_name,
         bike_share_ridership_2023_07_csv_user_type) |> 
  set_names("id", "duration", 
            "start_time", "start_station_name", 
            "end_time", "end_station_name",
            "user_type") |>
  mutate(duration = duration / 100)

# 3. August
august_data_clean <-
  clean_names(august_data) |>
  select(bike_share_ridership_2023_08_csv_trip_id, bike_share_ridership_2023_08_csv_trip_duration, 
         bike_share_ridership_2023_08_csv_start_time, bike_share_ridership_2023_08_csv_start_station_name,
         bike_share_ridership_2023_08_csv_end_time, bike_share_ridership_2023_08_csv_end_station_name,
         bike_share_ridership_2023_08_csv_user_type) |> 
  set_names("id", "duration", 
            "start_time", "start_station_name", 
            "end_time", "end_station_name",
            "user_type") |>
  mutate(duration = duration / 100)



# Save cleaned MONTHLY data to a parquet file
write_parquet(june_data_clean, 'outputs/data/june_data_clean.parquet')
write_parquet(july_data_clean, 'outputs/data/july_data_clean.parquet')
write_parquet(august_data_clean, 'outputs/data/august_data_clean.parquet')




