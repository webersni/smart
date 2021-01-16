# install.packages("dplyr")

library(dplyr)
library(lubridate)
library(readr)
library(tibble)
library(stringr)

# Read csv files
data <- read_csv("data/data_RES_SME_profiles_20090714_20101220.csv")
allocations <- read_csv("data/allocations.csv")
residentials <- read_csv("data/Smart meters Residential pre-trial survey data.csv")
SME <- read_csv("data/Smart meters SME pre-trial survey data.csv")

# Convert string to timestamp
data$timestamp <- ymd_hms(data$timestamp)

# Filter time period
data_filter <- dplyr::filter(data, between(timestamp, ymd_hms("2009-08-01 00:00:00"), ymd_hms("2010-07-31 23:59:59")))

# Collapse 30 min intervals to yearly data
data_collapsed <- data_filter %>%
                  dplyr::summarise(across(where(is.numeric), ~ sum(.x))) %>% 
                  dplyr::select(-one_of("col")) %>%
                  t() %>%
                  as.data.frame() %>%
                  tibble::rownames_to_column()

# Give meaningful names
colnames(data_collapsed) <- c("ID", "y_consum")
rownames(data_collapsed) <- NULL

# Remove chars from IDs
data_collapsed$ID <- as.numeric(stringr::str_sub(data_collapsed$ID, 2, 5))

# Join consumption data with survey data and keep only IDs with consumption data available
data_collapsed_merged <- dplyr::right_join(allocations, data_collapsed) %>%
                         dplyr::left_join(residentials) %>%
                         dplyr::left_join(SME)

# Write csv
write_csv(data_collapsed_merged, "~/workspace/DAIS/data/data_RES_SME_profiles_collapsed_merged.csv")
