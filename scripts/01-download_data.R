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

june_annual_data <- subset(june_data_clean, user_type == "Annual Member" & 
                             end_station_name != "NULL" & 
                             start_station_name != "NULL") |>
  select("id", "duration", 
            "start_time", "start_station_name", 
            "end_time", "end_station_name") 
  

june_casual_data <- subset(july_data_clean, user_type == "Casual Member" & 
                             end_station_name != "NULL" & 
                             start_station_name != "NULL") |>
  select("id", "duration", 
         "start_time", "start_station_name", 
         "end_time", "end_station_name") 

# generating july data for annual and casual members
july_annual_data <- subset(july_data_clean, user_type == "Annual Member" & 
                             end_station_name != "NULL" & 
                             start_station_name != "NULL") |>
  select("id", "duration", 
         "start_time", "start_station_name", 
         "end_time", "end_station_name") 

july_casual_data <- subset(july_data_clean, user_type == "Casual Member" & 
                             end_station_name != "NULL" & 
                             start_station_name != "NULL") |>
  select("id", "duration", 
         "start_time", "start_station_name", 
         "end_time", "end_station_name") 

# generating august data for annual and casual members
august_annual_data <- subset(august_data_clean, user_type == "Annual Member" &
                               end_station_name != "NULL" & 
                               start_station_name != "NULL") |>
  select("id", "duration", 
         "start_time", "start_station_name", 
         "end_time", "end_station_name") 

august_casual_data <- subset(august_data_clean, user_type == "Casual Member" & 
                               end_station_name != "NULL" & 
                               start_station_name != "NULL") |>
  select("id", "duration", 
         "start_time", "start_station_name", 
         "end_time", "end_station_name") 

# june start stations frequency: how often was this station used as a start
# top 10 most used stations by annual and casual
june_annual_start_frequency <- 
  june_annual_data |>
  group_by(start_station_name) |>
  summarise(frequency = n()) |>
  arrange(desc(frequency)) |> 
  slice(1:10) |>
  mutate(month = "June")

june_casual_start_frequency <- 
  june_casual_data |>
  group_by(start_station_name) |>
  summarise(frequency = n()) |>
  arrange(desc(frequency)) |> 
  slice(1:10) |>
  mutate(month = "June")

  
# graphs
june_annual_start_frequency |> 
  ggplot(aes(y=start_station_name)) + 
geom_bar(aes(x = frequency), stat = "identity", position = "dodge") 

june_casual_start_frequency |> 
  ggplot(aes(y=start_station_name)) + 
  geom_bar(aes(x = frequency), stat = "identity", position = "dodge") 

# july start stations frequency: how often was this station used as a start
# top 10 most used stations by annual and casual
july_annual_start_frequency <- 
  july_annual_data |>
  group_by(start_station_name) |>
  summarise(frequency = n()) |>
  arrange(desc(frequency)) |> 
  slice(1:10) 

july_casual_start_frequency <- 
  july_casual_data |>
  group_by(start_station_name) |>
  summarise(frequency = n()) |>
  arrange(desc(frequency)) |> 
  slice(1:10) 

# graphs
july_annual_start_frequency |> 
  ggplot(aes(y=start_station_name)) + 
  geom_bar(aes(x = frequency), stat = "identity", position = "dodge") 

july_casual_start_frequency |> 
  ggplot(aes(y=start_station_name)) + 
  geom_bar(aes(x = frequency), stat = "identity", position = "dodge") 


# august start stations frequency: how often was this station used as a start
# top 10 most used stations by annual and casual
august_annual_start_frequency <- 
  august_annual_data |>
  group_by(start_station_name) |>
  summarise(frequency = n()) |>
  arrange(desc(frequency)) |> 
  slice(1:10) 

august_casual_start_frequency <- 
  august_casual_data |>
  group_by(start_station_name) |>
  summarise(frequency = n()) |>
  arrange(desc(frequency)) |> 
  slice(1:10) 

# graphs
august_annual_start_frequency |> 
  ggplot(aes(y=start_station_name)) + 
  geom_bar(aes(x = frequency), stat = "identity", position = "dodge") 

august_casual_start_frequency |> 
  ggplot(aes(y=start_station_name)) + 
  geom_bar(aes(x = frequency), stat = "identity", position = "dodge") 



# for each of the three months, which type of user is more likely
june_user_count_stats <- 
  june_data_clean |> 
  group_by(user_type) |>
  summarise(total = n()) |>
  mutate(month = "June")

july_user_count_stats <- 
july_data_clean |> 
  group_by(user_type) |>
  summarise(total = n()) |>
  mutate(month = "July")

august_user_count_stats <- 
august_data_clean |> 
  group_by(user_type) |>
  summarise(total = n()) |>
  mutate(month = "August")

all_user_count_stats <-
  rbind(june_user_count_stats, july_user_count_stats, august_user_count_stats)

ggplot(all_user_count_stats, aes(fill=month, y=total, x=user_type)) + 
  geom_bar(position="dodge", stat="identity")




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



