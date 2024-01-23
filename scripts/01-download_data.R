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
  subset(end_station_name != "NULL") |>
  subset(start_station_name != "NULL") |>
  select("id", "duration", 
            "start_time", "start_station_name", 
            "end_time", "end_station_name") 
  

june_casual_data <- subset(july_data_clean, user_type == "Casual Member") |>
  subset(end_station_name != "NULL") |>
  subset(start_station_name != "NULL") |>
  select("id", "duration", 
         "start_time", "start_station_name", 
         "end_time", "end_station_name") 

# generating july data for annual and casual members
july_annual_data <- subset(july_data_clean, user_type == "Annual Member") |>
  subset(end_station_name != "NULL") |>
  subset(start_station_name != "NULL") |>
  select("id", "duration", 
         "start_time", "start_station_name", 
         "end_time", "end_station_name") 

july_casual_data <- subset(july_data_clean, user_type == "Casual Member") |>
  subset(end_station_name != "NULL") |>
  subset(start_station_name != "NULL") |>
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

august_casual_data <- subset(august_data_clean, user_type == "Casual Member") |>
  subset(end_station_name != "NULL") |>
  subset(start_station_name != "NULL") |>
  select("id", "duration", 
         "start_time", "start_station_name", 
         "end_time", "end_station_name") 

# june start stations frequency: how often was this station used as a start
june_annual_start_frequency <- 
  june_annual_data |>
  group_by(start_station_name) |>
  summarise(frequency = n())

june_casual_start_frequency <- 
  june_casual_data |>
  group_by(start_station_name) |>
  summarise(frequency = n())

# top 10 most used stations by annual and casual and then together
june_most_used_start_station_annual <-
  june_annual_start_frequency |>
  arrange(desc(frequency)) |> 
  slice(1:10) 

june_most_used_start_station_casual <-
  june_casual_start_frequency |>
  arrange(desc(frequency)) |> 
  slice(1:10) 

# union
june_most_used_start_station_all <-
  union(june_most_used_start_station_annual$start_station_name,
        june_most_used_start_station_casual$start_station_name)
june_most_used_start_station_all

june_most_used_start_station_annual_all <-
  june_most_used_start_station_annual |>
  arrange(desc(frequency)) |> 
  slice(1:10) 
  
combined_df <- merge(june_most_used_start_station_annual, 
                     june_most_used_start_station_casual, 
                     by = 'start_station_name', all = TRUE)



plot1 <-june_most_used_start_station_annual |> 
ggplot(aes(y=start_station_name)) +
geom_bar(aes(x = frequency, fill="red"), stat = "identity", position = "dodge") 

plot2 <-june_most_used_start_station_casual |> 
  ggplot(aes(y=start_station_name)) + 
geom_bar(aes(x = frequency, fill="yellow"), stat = "identity", position = "dodge") 

combined_plot <- plot1 + plot2 + plot_layout(ncol = 1)



june_casual_start_frequency |>
  arrange(desc(frequency)) |> 
  slice(1:10) |>
  ggplot(aes(y = start_station_name, x = frequency)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black")



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



