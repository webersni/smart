library(readr)
library(dplyr)
library(ggplot2)
library(lmtest)
library(fastDummies)
library(car)

data <- read_csv("../data/data_RES_SME_profiles_collapsed_merged.csv")
allocations <- read_csv("../data/allocations.csv")

# 3.2
# 1.
# Get number of participants
number_allocated <- allocations %>% count(Code) #%>% select(n) %>% sum()
number_with_data <- data %>% count(Code) #%>% select(n) %>% sum()

# 2.
# Get fraction which have electric heating systems
fraction_residential <- sum(data$Code == 1 & (data[47] == 1 | data[48] == 1 | data[56] == 1 | data[57] == 1), na.rm = TRUE) / number_with_data$n[1]
fraction_sme <- sum(data$Code == 2 & data[173] == 1, na.rm = TRUE) / number_with_data$n[2]
fraction_other <- sum(data$Code == 3 & (data[47] == 1 | data[48] == 1 | data[56] == 1 | data[57] == 1), na.rm = TRUE) / number_with_data$n[3]

# Get fraction with NAs
fraction_residential_na <- sum(data$Code == 1 & (is.na(data[47]) & is.na(data[48]) & is.na(data[56]) & is.na(data[57]))) / number_with_data$n[1]
fraction_sme_na <- sum(data$Code == 2 & is.na(data[173])) / number_with_data$n[2]
fraction_other_na <- sum(data$Code == 3 & (is.na(data[47]) & is.na(data[48]) & is.na(data[56]) & is.na(data[57]))) / number_with_data$n[3]

# 3.
# Proportion of residential households with children
fraction_residential_children <- sum(data$Code == 1 & (data[15] == 3 | !is.na(data[18])), na.rm = TRUE) / number_with_data$n[1]

# 4.
# Number of people living in households
number_children_residential <- data[data$Code == 1,][18]
number_children_residential[is.na(number_children_residential),] <- 0

people_live_residential <- na.omit(unlist(data[data$Code == 1,][16] + number_children_residential))

people_live_residential_desc <- c(mean(people_live_residential), sd(people_live_residential), median(people_live_residential), var(people_live_residential))

# 5.
# Small and medium size definition following EU Commission: https://ec.europa.eu/regional_policy/sources/conferences/state-aid/sme/smedefinitionguide_en.pdf
# Number of small enterprises (turnover <)
number_small_sme <- count(data[data$Code == 2 & data[154] <= 8 & !is.na(data[154]),]) #/ number_with_data$n[2]

# Number of medium enterprises
number_medium_sme <- count(data[data$Code == 2 & data[154] < 13 & data[154] > 8 & !is.na(data[154]),]) #/ number_with_data$n[2]

# Number of NAs or refused
number_na_refused_sme <- count(data[data$Code == 2 & (data[154] == 13 | is.na(data[154])),]) #/ number_with_data$n[2]

# 6.
# Industry shares
industries <- data[data$Code == 2,][150]
industries_freq <- as.data.frame(table(industries, useNA = "always"))

ggplot(industries_freq, aes(x="", y=Freq, fill=industries)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) +
  theme_void()

# 7.
# Satisfaction
sd(rowMeans(data[data$Code == 2,][228:235]), na.rm = TRUE)


# 3.3

# 1.
# Descriptive statistics of yearly electricity consumption
electricity_consum_residential <- c(mean(data[data$Code == 1,]$y_consum), var(data[data$Code == 1,]$y_consum), sd(data[data$Code == 1,]$y_consum))
electricity_consum_sme <- c(mean(data[data$Code == 2,]$y_consum), var(data[data$Code == 2,]$y_consum), sd(data[data$Code == 2,]$y_consum))
electricity_consum_other <- c(mean(data[data$Code == 3,]$y_consum), var(data[data$Code == 3,]$y_consum), sd(data[data$Code == 3,]$y_consum))

# 2.
# Save household size as column in data

# Question 43111 has NA for zero children, need to handle this issue
number_children <- data[18]
number_children[is.na(number_children)] <- 0

data$household_size <- data[16] + number_children

# Look at data for distribution
min(data[data$Code == 1,]$household_size, na.rm = TRUE)
max(data[data$Code == 1,]$household_size, na.rm = TRUE)

small_households_consum <- data[data$Code == 1 & data$household_size <= 3 & !is.na(data$household_size),]$y_consum

medium_households_consum <- data[data$Code == 1 & data$household_size > 3 & data$household_size < 6 & !is.na(data$household_size),]$y_consum

large_households_consum <- data[data$Code == 1 & data$household_size >= 6 & !is.na(data$household_size),]$y_consum

descriptives_consum <- data.frame(c(mean(small_households_consum), var(small_households_consum), sd(small_households_consum)),
                                  c(mean(medium_households_consum), var(medium_households_consum), sd(medium_households_consum)),
                                  c(mean(large_households_consum), var(large_households_consum), sd(large_households_consum)),
                                  row.names = c("mean", "var", "std"))

colnames(descriptives_consum) <- c("small", "medium", "large")

descriptives_consum <- t(descriptives_consum)
# 3.

# Load large data set
set.seed(1)
data_large <- read_csv("../data/data_RES_SME_profiles_filter.csv")
data_large$timestamp <- ymd_hms(data_large$timestamp)


# Take samples of households and SMEs
sample_ids_households <- data[data$Code == 1,][sample(nrow(data[data$Code == 1,]), 100),]$ID
sample_ids_smes <- data[data$Code == 2,][sample(nrow(data[data$Code == 2,]), 100),]$ID

# Get sample electricity consumption data
sample_data_households <- data_large[,c(as.character(sample_ids_households), "timestamp")]
sample_data_smes <- data_large[,c(as.character(sample_ids_smes), "timestamp")]

# a) Households
average_consum_monthly <- sample_data_households %>%
                group_by(month = format(timestamp, "%m")) %>%
                dplyr::summarize(across(where(is.numeric), ~ sum(.x))) %>%
                rowwise() %>%
                summarize(month = month, mean = mean(c_across(where(is.numeric))))

ggplot(data=average_consum_monthly, aes(x=month, y=mean, group=1)) + geom_line()

# b) SMEs
average_consum_monthly <- sample_data_smes %>%
  group_by(month = format(timestamp, "%m")) %>%
  dplyr::summarize(across(where(is.numeric), ~ sum(.x))) %>%
  rowwise() %>%
  summarize(month = month, mean = mean(c_across(where(is.numeric))))

ggplot(data=average_consum_monthly, aes(x=month, y=mean, group=1)) + geom_line()

# c)
# Households
average_consum_households_daily <- sample_data_households %>%
  group_by(day = format(timestamp, "%Y-%m-%d")) %>%
  dplyr::summarize(across(where(is.numeric), ~ sum(.x))) %>%
  rowwise() %>%
  summarize(day = day, mean = mean(c_across(where(is.numeric)))) %>%
  mutate(weekday = weekdays(as.Date(day)))

average_consum_households_daily$day <- as.Date(average_consum_households_daily$day)

# SMEs
average_consum_smes_daily <- sample_data_smes %>%
  group_by(day = format(timestamp, "%Y-%m-%d")) %>%
  dplyr::summarize(across(where(is.numeric), ~ sum(.x))) %>%
  rowwise() %>%
  summarize(day = day, mean = mean(c_across(where(is.numeric)))) %>%
  mutate(weekday = weekdays(as.Date(day)))

average_consum_smes_daily$day <- as.Date(average_consum_smes_daily$day)

# Summer/Winter definition: https://www.met.ie/climate/what-we-measure/temperature
# Summer week: 2010-07-19 - 2010-07-25
average_consum_households_daily_summer <- dplyr::filter(average_consum_households_daily, between(day, as.Date("2010-07-19"), as.Date("2010-07-25")))
average_consum_smes_daily_summer <- dplyr::filter(average_consum_smes_daily, between(day, as.Date("2010-07-19"), as.Date("2010-07-25")))

ggplot(data=average_consum_households_daily_summer, aes(x= reorder(weekday, day), y=mean, group=1)) + geom_line()
ggplot(data=average_consum_smes_daily_summer, aes(x= reorder(weekday, day), y=mean, group=1)) + geom_line()


# Winter week: 2010-01-11 - 2010-01-17
average_consum_households_daily_winter <- dplyr::filter(average_consum_households_daily, between(day, as.Date("2010-01-11"), as.Date("2010-01-17")))
average_consum_smes_daily_winter <- dplyr::filter(average_consum_smes_daily, between(day, as.Date("2010-01-11"), as.Date("2010-01-17")))
              
ggplot(data=average_consum_households_daily_winter, aes(x= reorder(weekday, day), y=mean, group=1)) + geom_line()
ggplot(data=average_consum_smes_daily_winter, aes(x= reorder(weekday, day), y=mean, group=1)) + geom_line()

# Three days of choice
dplyr::filter(average_consum_households_daily, day == "2010-04-06" | day == "2010-04-08" | day == "2010-04-11")
dplyr::filter(sample_data_smes, timestamp == "2010-04-06" | timestamp == "2010-04-08" | timestamp == "2010-04-11")

# Christmas week
average_consum_households_daily_christmas <- dplyr::filter(average_consum_households_daily, between(day, as.Date("2009-12-21"), as.Date("2009-12-27")))
average_consum_smes_daily_christmas <- dplyr::filter(average_consum_smes_daily, between(day, as.Date("2009-12-21"), as.Date("2009-12-27")))

ggplot(data=average_consum_households_daily_christmas, aes(x= reorder(weekday, day), y=mean, group=1)) + geom_line()
ggplot(data=average_consum_smes_daily_christmas, aes(x= reorder(weekday, day), y=mean, group=1)) + geom_line()

# 3.4

# 1.
data_truncated <- data[data$y_consum < quantile(data$y_consum, 0.99, names=FALSE),]

ggplot(data_truncated, aes(factor(Code), y_consum)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(breaks = round(seq(0, max(data_truncated$y_consum), len = 10), -3), labels=function(x) format(x, big.mark = ",", scientific = FALSE))

t.test(data[data$Code == 1,]$y_consum, data[data$Code == 2,]$y_consum)
t.test(data[data$Code == 1,]$y_consum, data[data$Code == 3,]$y_consum)
t.test(data[data$Code == 2,]$y_consum, data[data$Code == 3,]$y_consum)

# 2.

df_regression <- data.frame(data[data$Code == 1,]$y_consum, data[data$Code == 1,]$household_size, as.numeric(data[data$Code == 1,][15] == 3))
colnames(df_regression) <- c("consumption", "household_size", "children")

summary(lm(consumption ~ household_size, data=df_regression, na.action=na.exclude))

ggplot(df_regression, aes(x = household_size, y = consumption)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red")

model <- lm(consumption ~ household_size + children + (household_size * children), data=df_regression, na.action=na.exclude)
summary(model)

# bptest(model, data=df_regression)
coeftest(model, vcov = vcovHC(model, type = "HC0"))

# 3.
# Cleaning and converting
floor_area_convert <- data.frame(data[44], data[45])
colnames(floor_area_convert) <- c("floor_area", "unit")
data$home_floor_area <- ifelse(floor_area_convert$unit == 1 & !is.na(floor_area_convert$unit),
                               floor_area_convert$floor_area * 10.764,
                               floor_area_convert$floor_area)

data[data$home_floor_area >= 99999999 & !is.na(data$home_floor_area),]$home_floor_area <- NA

df_regression <- data.frame(data[data$Code == 1,]$household_size, data[data$Code == 1,]$home_floor_area)
colnames(df_regression) <- c("household_size", "floor_area")

summary(lm(floor_area ~ household_size, data = df_regression, na.action = na.exclude))

# Questions 402 and 4021 are mutually exclusive, we can merge them
income_convert <- data.frame(data[140], data[141])
colnames(income_convert) <- c("income1", "income2")

data$income_category <- ifelse(is.na(income_convert$income1) & !is.na(income_convert$income2), income_convert$income2, income_convert$income1)

# Remove income that is explicitly not per year
data[!is.na(data$income_category) & data[142] != 3 & !is.na(data[142]),]$income_category <- NA

# Remove income that is explicitly after tax
data[!is.na(data$income_category) & data[143] == 2 & !is.na(data[143]),]$income_category <- NA

# Remove refused income category
data[!is.na(data$income_category) & data$income_category == 6,]$income_category <- NA

income_dummies <- dummy_cols(data[data$Code == 1,]$income_category, remove_first_dummy = TRUE, ignore_na = TRUE, remove_selected_columns = TRUE)
df_regression <- data.frame(data[data$Code == 1,]$household_size, data[data$Code == 1,]$home_floor_area, income_dummies)
colnames(df_regression) <- c("household_size", "floor_area", "income_2", "income_3", "income_4", "income_5")

summary(lm(floor_area ~ household_size + income_2 + income_3 + income_4 + income_5, data = df_regression, na.action = na.exclude))

# SMEs

premise_area_convert <- data.frame(data[161], data[162])
colnames(premise_area_convert) <- c("area", "unit")
data$premise_area <- ifelse(premise_area_convert$unit == 2 & !is.na(premise_area_convert$unit),
                            premise_area_convert$area * 10.764,
                            premise_area_convert$area)

data[data$premise_area >= 99999999 & !is.na(data$premise_area),]$premise_area <- NA

# Clean annual turnover
# Split in small / medium to ensure sufficient number of observations for t-test
turnover_convert <- data.frame(data[data$Code == 2,][154])
colnames(turnover_convert) <- "turnover"
turnover_dummy <- ifelse(turnover_convert$turnover < 13 & turnover_convert$turnover > 4 & !is.na(turnover_convert$turnover),
                          1,
                          ifelse(turnover_convert$turnover < 5 & !is.na(turnover_convert$turnover), 0, NA))

employees_convert <- data.frame(data[data$Code == 2,][151])
colnames(employees_convert) <- "employees"
employees_dummy <- ifelse(employees_convert$employees > 4 & !is.na(employees_convert$employees),
                          1,
                          ifelse(!is.na(employees_convert$employees), 0, NA))

df_regression <- data.frame(log(data[data$Code == 2,]$premise_area), turnover_dummy, employees_dummy)
colnames(df_regression) <- c("premise_area", "turnover_medium", "employees_medium")

summary(lm(premise_area ~ turnover_medium + employees_medium, data = df_regression, na.action = na.exclude))

# 4.
# Clean year built
data[(data[42] < 1500 | data[42] > 2010) & !is.na(data[42]),][42] <- NA

# Generate categories
data$year_built_category <- cut(pull(data[42]), breaks=c(1500, 1800, 1900, 1950, 1980, 2000, 2010), labels=c("<1800", "1800-1900", "1900-1950", "1950-1980", "1980-2000", ">2000"))

# Plot density for each category
ggplot(data[data$Code == 1 & !is.na(data$year_built_category),], aes(x = y_consum, fill = year_built_category)) + geom_density(alpha = 0.5)

# Confounding factors
# Electric heating
data$residential_electric_heating <- ifelse(data$Code == 1 & (data[47] == 1 | data[48] == 1 | data[56] == 1 | data[57] == 1),
       1,
       ifelse(data$Code == 1 & data[47] == 0 & data[48] == 0 & data[56] == 0 & data[57] == 0 &
              !is.na(data[47]) & !is.na(data[48]) & !is.na(data[56]) & !is.na(data[57]),
              0,
              NA))

# Create dummies for year built
year_built_dummies <- dummy_cols(data[data$Code == 1,]$year_built_category, remove_first_dummy = TRUE, ignore_na = TRUE, remove_selected_columns = TRUE)


df_regression <- data.frame(data[data$Code == 1,]$y_consum, data[data$Code == 1,]$household_size, data[data$Code == 1,]$residential_electric_heating, year_built_dummies)
colnames(df_regression) <- c("consumption", "household_size", "electric_heating", "year_built_1800_1900", "year_built_1900_1950", "year_built_1950_1980", "year_built_1980_2000", "year_built_2000")

model <- lm(consumption ~ household_size + electric_heating + year_built_1800_1900 + year_built_1900_1950 + year_built_1950_1980 + year_built_1980_2000 + year_built_2000, data=df_regression, na.action=na.exclude)
bptest(model)
coeftest(model, vcov = vcovHC(model, type = "HC0"))

# 5.

data$home_category <- as.factor(ifelse(data[40] == 1 & !is.na(data[40]), 1, ifelse(
  data[40] == 5 & !is.na(data[40]), 2, ifelse(
    data[40] > 1 & data[40] < 5 & !is.na(data[40]), 3, NA
  )
)))

ggplot(data[data$Code == 1 & !is.na(data$home_category),], aes(x = y_consum, y = home_category)) + geom_violin() + geom_boxplot(width=0.1) + coord_flip()#geom_density(alpha = 0.5)

# TODO: Descriptives

# Category 1 (Apartments) - Category 2 (Bungalows)
mean(data[data[40] == 1 & !is.na(data[40]),]$y_consum) - mean(data[data[40] == 5 & !is.na(data[40]),]$y_consum)
t.test(data[data[40] == 1 & !is.na(data[40]),]$y_consum, data[data[40] == 5 & !is.na(data[40]),]$y_consum)
var.test(data[data[40] == 1 & !is.na(data[40]),]$y_consum, data[data[40] == 5 & !is.na(data[40]),]$y_consum)

# Category 1 (Apartments) - Category 3 (Houses)
mean(data[data[40] == 1 & !is.na(data[40]),]$y_consum) - mean(data[data[40] > 1 & data[40] < 5 & !is.na(data[40]),]$y_consum)
t.test(data[data[40] == 1 & !is.na(data[40]),]$y_consum, data[data[40] > 1 & data[40] < 5 & !is.na(data[40]),]$y_consum)
var.test(data[data[40] == 1 & !is.na(data[40]),]$y_consum, data[data[40] > 1 & data[40] < 5 & !is.na(data[40]),]$y_consum)

# Category 2 (Bungalows) - Category 3 (Houses)
mean(data[data[40] == 5 & !is.na(data[40]),]$y_consum) - mean(data[data[40] > 1 & data[40] < 5 & !is.na(data[40]),]$y_consum)
t.test(data[data[40] == 5 & !is.na(data[40]),]$y_consum, data[data[40] > 1 & data[40] < 5 & !is.na(data[40]),]$y_consum)
var.test(data[data[40] == 5 & !is.na(data[40]),]$y_consum, data[data[40] > 1 & data[40] < 5 & !is.na(data[40]),]$y_consum)

# 6.

# Set refused to NA
data[data[46] == 6 & !is.na(data[46]),][46] <- NA

# Assumption: Households with more than 5 bedrooms have 5 bedrooms to keep variable continuous
df_regression <- data.frame(data[data$Code == 1,]$y_consum, data[data$Code == 1,][46])
colnames(df_regression) <- c("consumption", "bedrooms")

model <- lm(consumption ~ bedrooms, data = df_regression, na.action = na.exclude)
summary(model)

data$tumble_dryer <- as.numeric(data[87] == 2 | data[87] == 3 | data[87] == 4)

df_regression <- data.frame(data[data$Code == 1,]$y_consum, data[data$Code == 1,][46], data[data$Code == 1,]$residential_electric_heating, data[data$Code == 1,]$tumble_dryer)
colnames(df_regression) <- c("consumption", "bedrooms", "electric_heating", "tumble_dryer")

model <- lm(consumption ~ bedrooms + electric_heating + tumble_dryer, data = df_regression, na.action = na.exclude)
summary(model)
bptest(model)
coeftest(model, vcov = vcovHC(model, type = "HC0"))


# 7.

df_regression <- data.frame(data[data$Code == 1,]$y_consum, data[data$Code == 1,]$home_floor_area)
colnames(df_regression) <- c("consumption", "floor_area")

model <- lm(consumption ~ floor_area, data = df_regression, na.action = na.exclude)
summary(model)
bptest(model)
coeftest(model, vcov = vcovHC(model, type = "HC0"))

# 8.

df_regression <- data.frame(data[data$Code == 1,]$y_consum, data[data$Code == 1,][46], data[data$Code == 1,]$home_floor_area)
colnames(df_regression) <- c("consumption", "bedrooms", "floor_area")
model <- lm(consumption ~ bedrooms + floor_area, data = df_regression, na.action = na.exclude)
summary(model)

vif(model)

# No issue. Bedrooms is better, maybe because people remember number of bedrooms better than their floor area and bedrooms is a better proxy for people living in household than floor area
