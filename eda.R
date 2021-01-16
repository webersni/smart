library(readr)
library(dplyr)

data <- read_csv("data/data_RES_SME_profiles_collapsed_merged.csv")
allocations <- read_csv("data/allocations.csv")

# 3.2
# 1.
# Get number of participants
number_allocated <- allocations %>% count(Code) #%>% select(n) %>% sum()
number_with_data <- data %>% count(Code) #%>% select(n) %>% sum()

# 2.
# Get fraction which have electric heating systems
fraction_residential <- sum(data$Code == 1 & (data[47] == 1 | data[48] == 1 | data[56] == 1 | data[57] == 1), na.rm = TRUE) / amount_with_data$n[1]
fraction_sme <- sum(data$Code == 2 & data[173] == 1, na.rm = TRUE) / amount_with_data$n[2]
fraction_other <- sum(data$Code == 3 & (data[47] == 1 | data[48] == 1 | data[56] == 1 | data[57] == 1), na.rm = TRUE) / amount_with_data$n[3]

# Get fraction with NAs
fraction_residential_na <- sum(data$Code == 1 & (is.na(data[47]) & is.na(data[48]) & is.na(data[56]) & is.na(data[57]))) / amount_with_data$n[1]
fraction_sme_na <- sum(data$Code == 2 & is.na(data[173])) / amount_with_data$n[2]
fraction_other_na <- sum(data$Code == 3 & (is.na(data[47]) & is.na(data[48]) & is.na(data[56]) & is.na(data[57]))) / amount_with_data$n[3]
