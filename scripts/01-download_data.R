# This file saves the raw data that will be used for the remainder of the paper
install.packages("opendatatoronto")
install.packages("knitr")
install.packages("lubridate")
install.packages("tidyverse")
install.packages("janitor")
install.packages("arrow")

library(arrow)
library(knitr)
library(janitor)
library(lubridate)
library(opendatatoronto)
library(tidyverse)
# get package from Open Data Toronto
package <- list_package_resources("7e876c24-177c-4605-9cef-e50dd74c617f") |>
  filter(name == 
           "bikeshare-ridership-2023") |>
  get_resource()

# obtain summer months raw data
june_data_raw <- package[6]
july_data_raw <- package[7]
august_data_raw <- package[8]


# convert summer months raw data to parquet files
write_parquet(
  x = data.frame(june_data_raw[1]),
  sink = "inputs/data/unedited_bikeshare_june_data.parquet"
)

write_parquet(
  x = data.frame(june_data_raw[1]),
  sink = "inputs/data/unedited_bikeshare_july_data.parquet"
)

write_parquet(
  x = data.frame(june_data_raw[1]),
  sink = "inputs/data/unedited_bikeshare_august_data.parquet"
)