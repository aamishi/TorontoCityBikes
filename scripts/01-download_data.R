install.packages("opendatatoronto")
install.packages("knitr")
install.packages("lubridate")
install.packages("tidyverse")
install.packages("janitor")

library(knitr)
library(janitor)
library(lubridate)
library(opendatatoronto)
library(tidyverse)
# get package
package <- list_package_resources("7e876c24-177c-4605-9cef-e50dd74c617f") |>
  filter(name == 
           "bikeshare-ridership-2023") |>
  get_resource()

# obtain summer months raw data

june_data_raw <- package[6]
july_data_raw <- package[7]
august_data_raw <- package[8]

# convert summer months raw data to csv files
write.csv(
  x = june_data_raw,
  file = "scripts/bikeshare-ridership-2023-06.csv"
)

write.csv(
  x = july_data_raw,
  file = "scripts/bikeshare-ridership-2023-07.csv"
)

write.csv(
  x = august_data_raw,
  file = "scripts/bikeshare-ridership-2023-08.csv"
)

# reading from the csv files
june_data <- read.csv("scripts/bikeshare-ridership-2023-06.csv")
july_data <- read.csv("scripts/bikeshare-ridership-2023-07.csv")
august_data <- read.csv("scripts/bikeshare-ridership-2023-08.csv")


# cleaning the data and also renaming the columns
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

# generating june data for annual and casual members

june_annual_data <- subset(june_data_clean, user_type == "Annual Member") |>
  select("id", "duration", 
            "start_time", "start_station_name", 
            "end_time", "end_station_name") 

june_casual_data <- subset(july_data_clean, user_type == "Casual Member") |>
  select("id", "duration", 
         "start_time", "start_station_name", 
         "end_time", "end_station_name") 

# generating july data for annual and casual members
july_annual_data <- subset(july_data_clean, user_type == "Annual Member") |>
  select("id", "duration", 
         "start_time", "start_station_name", 
         "end_time", "end_station_name") 

july_casual_data <- subset(july_data_clean, user_type == "Casual Member") |>
  select("id", "duration", 
         "start_time", "start_station_name", 
         "end_time", "end_station_name") 

# generating august data for annual and casual members
august_annual_data <- subset(august_data_clean, user_type == "Annual Member") |>
  select("id", "duration", 
         "start_time", "start_station_name", 
         "end_time", "end_station_name") 

august_casual_data <- subset(august_data_clean, user_type == "Casual Member") |>
  select("id", "duration", 
         "start_time", "start_station_name", 
         "end_time", "end_station_name") 


casual <-  subset(data_clean, user_type == "Casual Member")
data_clean_3 <- subset(data_clean_2, 1.00 <= duration)
casual_2 <- subset(casual, 1.00 <= duration)
data_clean_4 <- subset(data_clean_3, duration <= 60)
casual_3 <- subset(casual_2, duration <= 30)
data_clean_3 |>
  ggplot(aes(x = duration)) +
  geom_bar() 


data_clean_4 |>
  ggplot(aes(x = duration)) +
  geom_bar() 

breaks <- seq(0, 60, by = 5)

data_clean_4$periods <- cut(data_clean_4$duration, breaks, labels = FALSE)

# per period
data_clean_4 |>
  ggplot(aes(x = start_station_name)) +
  geom_bar() 

group_start <- 
  subset(data_clean_4, start_station_name != "NULL")

group_start <- 
  group_start |>
  group_by(start_station_name) |>
  summarise(count_s = n())  |>
  arrange(desc(count_s)) |> 
  slice(1:3)

group_start |>
  ggplot(aes(x = start_station_name)) +
  geom_bar() 

barplot(height = group_start$count_s, names.arg = group_start$start_station_name)



