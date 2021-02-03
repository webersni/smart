# DAIS bonus project winter term 2020/2021
# By Kevin Kohler and Nicolas Webersinke

# Install packages
# install.packages("readr", "dplyr", "ggplot2", "lmtest", "fastDummies", "car", "purr", "dbscan", "class", "nnet", "psych")

library(readr)
library(dplyr)
library(tibble)
library(stringr)
library(ggplot2)
library(lubridate)
library(scales)
library(lmtest)
library(sandwich)
library(fastDummies)
library(car)
library(purrr)
library(dbscan)
library(class)
library(e1071)
library(caret)

# Prepare raw data

# Read csv files
data <- read_csv("data_RES_SME_profiles_20090714_20101220.csv")
allocations <- read_csv("allocations.csv")
residentials <- read_csv("Smart meters Residential pre-trial survey data.csv")
SME <- read_csv("Smart meters SME pre-trial survey data.csv")

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

data_filter <- data_filter %>% dplyr::select(-one_of("col"))

# Give meaningful names
colnames(data_collapsed) <- c("ID", "y_consum")
rownames(data_collapsed) <- NULL

colnames(data_filter) <- c(stringr::str_sub(colnames(data_filter)[1:(length(data_filter) - 1)], 2, 5), "timestamp")

# Remove chars from IDs
data_collapsed$ID <- as.numeric(stringr::str_sub(data_collapsed$ID, 2, 5))

# Join consumption data with survey data and keep only IDs with consumption data available
data_collapsed_merged <- dplyr::right_join(allocations, data_collapsed) %>%
  dplyr::left_join(residentials) %>%
  dplyr::left_join(SME)

# Write csv
write_csv(data_collapsed_merged, "data_RES_SME_profiles_collapsed_merged.csv")
write_csv(data_filter, "data_RES_SME_profiles_filter.csv")


### End of preparation ###


# Initialize analysis data
# Load files
data <- read_csv("data_RES_SME_profiles_collapsed_merged.csv")
allocations <- read_csv("allocations.csv")
data_large <- read_csv("data_RES_SME_profiles_filter.csv")

# Convert timestamp string to real timestamp
data_large$timestamp <- ymd_hms(data_large$timestamp)

# Set seed for reproducible randomness
set.seed(123)

# 3.2
# 3.2.1
# Get number of participants
number_allocated <- allocations %>% count(Code) #%>% select(n) %>% sum()
number_with_data <- data %>% count(Code) #%>% select(n) %>% sum()

# 3.2.2
# Get fraction which have electric heating systems
fraction_electric_heating_residential <- sum(data$Code == 1 & (data[47] == 1 | data[48] == 1 | data[56] == 1 | data[57] == 1), na.rm = TRUE) / number_with_data$n[1]
fraction_electric_heating_sme <- sum(data$Code == 2 & data[173] == 1, na.rm = TRUE) / number_with_data$n[2]
fraction_electric_heating_other <- sum(data$Code == 3 & (data[47] == 1 | data[48] == 1 | data[56] == 1 | data[57] == 1), na.rm = TRUE) / number_with_data$n[3]

# Get fraction with NAs
fraction_electric_heating_residential_na <- sum(data$Code == 1 & (is.na(data[47]) & is.na(data[48]) & is.na(data[56]) & is.na(data[57]))) / number_with_data$n[1]
fraction_electric_heating_sme_na <- sum(data$Code == 2 & is.na(data[173])) / number_with_data$n[2]
fraction_electric_heating_other_na <- sum(data$Code == 3 & (is.na(data[47]) & is.na(data[48]) & is.na(data[56]) & is.na(data[57]))) / number_with_data$n[3]

# 3.2.3
# Proportion of residential households with children
fraction_children_residential <- sum(data$Code == 1 & ((data[15] == 3 & !is.na(data[15])) | !is.na(data[18])), na.rm = TRUE) / number_with_data$n[1]

# 3.2.4
# Number of people living in households

# Calculate household size
# Question 43111 has NA for zero children, need to handle this issue
number_children <- data[18]
number_children[is.na(number_children)] <- 0

# Save household size as column in data
data$household_size <- data[16] + number_children

people_live_residential_desc <- c(mean(pull(data[data$Code == 1,]$household_size), na.rm = TRUE),
                                  sd(pull(data[data$Code == 1,]$household_size), na.rm = TRUE),
                                  median(pull(data[data$Code == 1,]$household_size), na.rm = TRUE),
                                  var(pull(data[data$Code == 1,]$household_size), na.rm = TRUE))

# 3.2.5
# Small and medium size definition following EU Commission: https://ec.europa.eu/regional_policy/sources/conferences/state-aid/sme/smedefinitionguide_en.pdf
# Number of small enterprises (turnover <)
number_small_sme <- count(data[data$Code == 2 & data[154] <= 8 & !is.na(data[154]),]) #/ number_with_data$n[2]

# Number of medium enterprises
number_medium_sme <- count(data[data$Code == 2 & data[154] < 13 & data[154] > 8 & !is.na(data[154]),]) #/ number_with_data$n[2]

# Number of NAs or refused
number_na_refused_sme <- count(data[data$Code == 2 & (data[154] == 13 | is.na(data[154])),]) #/ number_with_data$n[2]

# 3.2.6
# Industry distribution of SMEs
industries <- data[data$Code == 2, 150]
industries_freq <- as.data.frame(table(industries, useNA = "always"))

ggplot(industries_freq, aes(x="", y=Freq, fill=industries)) +
  geom_bar(stat="identity", width=1) +
  geom_text(aes(x = 1.75, label = percent(Freq/sum(Freq))), position = position_stack(vjust = 0.5), size = 4) +
  coord_polar("y", start=0) +
  scale_fill_grey(na.value = "#E4E4E7") +
  theme_void() +
  theme(text = element_text(size=12))

# 3.2.7
# Satisfaction
# Single question descriptives
satisfaction_descriptives <- data.frame(colMeans(data[data$Code == 2, 228:235], na.rm = TRUE),
      apply(data[data$Code == 2, 228:235], 2, median, na.rm = TRUE),
      apply(data[data$Code == 2, 228:235], 2, sd, na.rm = TRUE))

# Total descriptives
pooled_satisfaction <- as.vector(as.matrix(data[data$Code == 2, 228:235]))

satisfaction_descriptives <- rbind(satisfaction_descriptives , total = c(mean(pooled_satisfaction, na.rm = TRUE),
                      median(pooled_satisfaction, na.rm = TRUE),
                      sd(pooled_satisfaction, na.rm = TRUE)))

colnames(satisfaction_descriptives) <- c("mean", "median", "std")

# 3.3

# 3.3.1
# Descriptive statistics of yearly electricity consumption
electricity_consum_descriptives <- rbind(c(mean(data[data$Code == 1,]$y_consum), var(data[data$Code == 1,]$y_consum), sd(data[data$Code == 1,]$y_consum)),
                                         c(mean(data[data$Code == 2,]$y_consum), var(data[data$Code == 2,]$y_consum), sd(data[data$Code == 2,]$y_consum)),
                                         c(mean(data[data$Code == 3,]$y_consum), var(data[data$Code == 3,]$y_consum), sd(data[data$Code == 3,]$y_consum)))

electricity_consum_descriptives <- round(electricity_consum_descriptives, 2)
colnames(electricity_consum_descriptives) <- c("mean", "var", "std")

# 3.3.2


# Look at data for distribution
min(data[data$Code == 1,]$household_size, na.rm = TRUE)
max(data[data$Code == 1,]$household_size, na.rm = TRUE)

# Calculate descriptives for small, medium and large households
small_households_consum <- data[which(data$Code == 1 & data$household_size <= 3 & !is.na(data$household_size)),]$y_consum

medium_households_consum <- data[which(data$Code == 1 & data$household_size > 3 & data$household_size < 6 & !is.na(data$household_size)),]$y_consum

large_households_consum <- data[which(data$Code == 1 & data$household_size >= 6 & !is.na(data$household_size)),]$y_consum

descriptives_consum <- data.frame(c(mean(small_households_consum), var(small_households_consum), sd(small_households_consum)),
                                  c(mean(medium_households_consum), var(medium_households_consum), sd(medium_households_consum)),
                                  c(mean(large_households_consum), var(large_households_consum), sd(large_households_consum)),
                                  row.names = c("mean", "var", "std"))

colnames(descriptives_consum) <- c("small", "medium", "large")

descriptives_consum <- t(descriptives_consum)

# 3.3.3

# Take samples of households and SMEs of large dataset
sample_ids_households <- data[sample(nrow(data[data$Code == 1,]), 100), "ID"]
sample_ids_smes <- data[sample(nrow(data[data$Code == 2,]), 100), "ID"]

# Get sample electricity consumption data
sample_data_households <- data_large[,c(as.character(pull(sample_ids_households)), "timestamp")]
sample_data_smes <- data_large[, c(as.character(pull(sample_ids_smes)), "timestamp")]

# a) Households
average_consum_monthly <- sample_data_households %>%
                group_by(month = format(timestamp, "%m")) %>%
                dplyr::summarize(across(where(is.numeric), ~ sum(.x))) %>%
                rowwise() %>%
                summarize(month = month, mean = mean(c_across(where(is.numeric))))

ggplot(data=average_consum_monthly, aes(x=month, y=mean, group=1)) +
  geom_line() +
  scale_x_discrete(labels= function(x) month.abb[as.numeric(x)]) +
  scale_y_continuous(breaks = seq(900, 1600, 200), limits = c(800, 1600)) +
  ylab("mean kWh") +
  theme_bw() +
  theme(text = element_text(size=12))

ggsave("average_consum_monthly_residential.png",
       last_plot(),
       width=9,
       height=4,
       dpi="print")

# b) SMEs
average_consum_monthly <- sample_data_smes %>%
  group_by(month = format(timestamp, "%m")) %>%
  dplyr::summarize(across(where(is.numeric), ~ sum(.x))) %>%
  rowwise() %>%
  summarize(month = month, mean = mean(c_across(where(is.numeric))))

ggplot(data=average_consum_monthly, aes(x=month, y=mean, group=1)) +
  geom_line() +
  scale_x_discrete(labels= function(x) month.abb[as.numeric(x)]) +
  ylab("mean kWh") +
  scale_y_continuous(breaks = seq(900, 1600, 200), limits = c(800, 1600)) +
  theme_bw() +
  theme(text = element_text(size=12))

ggsave("average_consum_monthly_sme.png",
       last_plot(),
       width=9,
       height=4,
       dpi="print")

# c)
# Summer/Winter definition: https://www.met.ie/climate/what-we-measure/temperature
# Summer week: 2010-07-19 - 2010-07-25
average_consum_profile_summer <- dplyr::filter(sample_data_households, between(timestamp, ymd_hms("2010-07-19 00:00:00"), ymd_hms("2010-07-25 23:59:59"))) %>%
  rowwise() %>%
  summarize(timestamp = timestamp, residential = mean(c_across(where(is.numeric)))) %>%
  as.data.frame()

average_consum_profile_summer$sme <- dplyr::filter(sample_data_smes, between(timestamp, ymd_hms("2010-07-19 00:00:00"), ymd_hms("2010-07-25 23:59:59"))) %>%
  rowwise() %>%
  summarize(timestamp = timestamp, sme = mean(c_across(where(is.numeric)))) %>%
  pull(sme)

# Plot
ggplot(data=average_consum_profile_summer, aes(x=timestamp)) +
  geom_line(aes(y=residential, group=1, colour="Residential")) +
  geom_line(aes(y=sme, group=1, colour="SME")) +
  theme(text = element_text(size=12)) +
  xlab("timestamp") +
  ylab("mean kWh") +
  scale_y_continuous(breaks = seq(0.4, 1.6, 0.4), limits = c(0.25, 1.65)) +
  theme_bw() +
  scale_color_manual(values=c("black", "darkgray"), name = "groups") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=0.9)) +
  scale_x_datetime(date_labels = "%H:%M", breaks = seq(ymd_hms("2010-07-19 00:00:00"),
                                                       ymd_hms("2010-07-25 23:30:00"),
                                                       "6 hours"))

ggsave("average_consum_week_summer.png",
       last_plot(),
       width=9,
       height=4,
       dpi="print")

# Winter week: 2010-01-11 - 2010-01-17
average_consum_profile_winter <- dplyr::filter(sample_data_households, between(timestamp, ymd_hms("2010-01-11 00:00:00"), ymd_hms("2010-01-17 23:59:59"))) %>%
  rowwise() %>%
  summarize(timestamp = timestamp, residential = mean(c_across(where(is.numeric)))) %>%
  as.data.frame()

average_consum_profile_winter$sme <- dplyr::filter(sample_data_smes, between(timestamp, ymd_hms("2010-01-11 00:00:00"), ymd_hms("2010-01-17 23:59:59"))) %>%
  rowwise() %>%
  summarize(timestamp = timestamp, sme = mean(c_across(where(is.numeric)))) %>%
  pull(sme)

# Plot
ggplot(data=average_consum_profile_winter, aes(x=timestamp)) +
  geom_line(aes(y=residential, group=1, colour="Residential")) +
  geom_line(aes(y=sme, group=1, colour="SME")) +
  theme(text = element_text(size=12)) +
  xlab("timestamp") +
  ylab("mean kWh") +
  scale_y_continuous(breaks = seq(0.4, 1.6, 0.4), limits = c(0.25, 1.65)) +
  theme_bw() +
  scale_color_manual(values=c("black", "darkgray"), name = "groups") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=0.9)) +
  scale_x_datetime(date_labels = "%H:%M", breaks = seq(ymd_hms("2010-01-11 00:00:00"),
                                                       ymd_hms("2010-01-17 23:30:00"),
                                                       "6 hours"))

ggsave("average_consum_week_winter.png",
       last_plot(),
       width=9,
       height=4,
       dpi="print")

# Three days of choice
average_consum_profile_weekday1 <- dplyr::filter(sample_data_households, between(timestamp, ymd_hms("2010-04-06 00:00:00"), ymd_hms("2010-04-06 23:59:59"))) %>%
  rowwise() %>%
  summarize(timestamp = timestamp, residential = mean(c_across(where(is.numeric)))) %>%
  as.data.frame()

average_consum_profile_weekday1$sme <- dplyr::filter(sample_data_smes, between(timestamp, ymd_hms("2010-04-06 00:00:00"), ymd_hms("2010-04-06 23:59:59"))) %>%
  rowwise() %>%
  summarize(timestamp = timestamp, sme = mean(c_across(where(is.numeric)))) %>%
  pull(sme)

ggplot(data=average_consum_profile_weekday1, aes(x=timestamp)) +
  geom_line(aes(y=residential, group=1, colour="Residential")) +
  geom_line(aes(y=sme, group=1, colour="SME")) +
  theme(text = element_text(size=12)) +
  xlab("timestamp") +
  ylab("mean kWh") +
  scale_y_continuous(breaks = seq(0.25, 1.25, 0.25), limits = c(0.25, 1.25)) +
  theme_bw() +
  scale_color_manual(values=c("black", "darkgray"), name = "groups") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=0.9)) +
  scale_x_datetime(date_labels = "%H:%M", breaks = seq(ymd_hms("2010-04-06 00:00:00"),
                                                       ymd_hms("2010-04-07 00:00:00"),
                                                       "1 hours"))

ggsave("average_consum_profile_weekday1.png",
       last_plot(),
       width=9,
       height=4,
       dpi="print")

average_consum_profile_weekday2 <- dplyr::filter(sample_data_households, between(timestamp, ymd_hms("2010-04-08 00:00:00"), ymd_hms("2010-04-08 23:59:59"))) %>%
  rowwise() %>%
  summarize(timestamp = timestamp, residential = mean(c_across(where(is.numeric)))) %>%
  as.data.frame()

average_consum_profile_weekday2$sme <- dplyr::filter(sample_data_smes, between(timestamp, ymd_hms("2010-04-08 00:00:00"), ymd_hms("2010-04-08 23:59:59"))) %>%
  rowwise() %>%
  summarize(timestamp = timestamp, sme = mean(c_across(where(is.numeric)))) %>%
  pull(sme)

ggplot(data=average_consum_profile_weekday2, aes(x=timestamp)) +
  geom_line(aes(y=residential, group=1, colour="Residential")) +
  geom_line(aes(y=sme, group=1, colour="SME")) +
  theme(text = element_text(size=12)) +
  xlab("timestamp") +
  ylab("mean kWh") +
  scale_y_continuous(breaks = seq(0.25, 1.25, 0.25), limits = c(0.25, 1.25)) +
  theme_bw() +
  scale_color_manual(values=c("black", "darkgray"), name = "groups") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=0.9)) +
  scale_x_datetime(date_labels = "%H:%M", breaks = seq(ymd_hms("2010-04-08 00:00:00"),
                                                       ymd_hms("2010-04-09 00:00:00"),
                                                       "1 hours"))

ggsave("average_consum_profile_weekday2.png",
       last_plot(),
       width=9,
       height=4,
       dpi="print")

average_consum_profile_weekend <- dplyr::filter(sample_data_households, between(timestamp, ymd_hms("2010-04-11 00:00:00"), ymd_hms("2010-04-11 23:59:59"))) %>%
  rowwise() %>%
  summarize(timestamp = timestamp, residential = mean(c_across(where(is.numeric)))) %>%
  as.data.frame()

average_consum_profile_weekend$sme <- dplyr::filter(sample_data_smes, between(timestamp, ymd_hms("2010-04-11 00:00:00"), ymd_hms("2010-04-11 23:59:59"))) %>%
  rowwise() %>%
  summarize(timestamp = timestamp, sme = mean(c_across(where(is.numeric)))) %>%
  pull(sme)

ggplot(data=average_consum_profile_weekend, aes(x=timestamp)) +
  geom_line(aes(y=residential, group=1, colour="Residential")) +
  geom_line(aes(y=sme, group=1, colour="SME")) +
  theme(text = element_text(size=12)) +
  xlab("timestamp") +
  ylab("mean kWh") +
  scale_y_continuous(breaks = seq(0.25, 1.25, 0.25), limits = c(0.25, 1.25)) +
  theme_bw() +
  scale_color_manual(values=c("black", "darkgray"), name = "groups") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=0.9)) +
  scale_x_datetime(date_labels = "%H:%M", breaks = seq(ymd_hms("2010-04-11 00:00:00"),
                                                       ymd_hms("2010-04-12 00:00:00"),
                                                       "1 hours"))

ggsave("average_consum_profile_weekend.png",
       last_plot(),
       width=9,
       height=4,
       dpi="print")

# d) 
# Christmas week
average_consum_profile_christmas <- dplyr::filter(sample_data_households, between(timestamp, ymd_hms("2009-12-21 00:00:00"), ymd_hms("2009-12-27 23:59:59"))) %>%
  rowwise() %>%
  summarize(timestamp = timestamp, residential = mean(c_across(where(is.numeric)))) %>%
  as.data.frame()

average_consum_profile_christmas$sme <- dplyr::filter(sample_data_smes, between(timestamp, ymd_hms("2009-12-21 00:00:00"), ymd_hms("2009-12-27 23:59:59"))) %>%
  rowwise() %>%
  summarize(timestamp = timestamp, sme = mean(c_across(where(is.numeric)))) %>%
  pull(sme)

# Plot
ggplot(data=average_consum_profile_christmas, aes(x=timestamp)) +
  geom_line(aes(y=residential, group=1, colour="Residential")) +
  geom_line(aes(y=sme, group=1, colour="SME")) +
  theme(text = element_text(size=12)) +
  xlab("timestamp") +
  ylab("mean kWh") +
  scale_y_continuous(breaks = seq(0.4, 1.8, 0.2), limits = c(0.28, 1.9)) +
  theme_bw() +
  scale_color_manual(values=c("black", "darkgray"), name = "groups") +
  annotate("text", label = "Christmas Eve", x = ymd_hms("2009-12-24 12:00:00"), y = 1.9, size = 3, colour = "black") +
  annotate("text", label = "Christmas Day", x = ymd_hms("2009-12-25 12:00:00"), y = 1.9, size = 3, colour = "black") +
  annotate("text", label = "Stephen's Day", x = ymd_hms("2009-12-26 12:00:00"), y = 1.9, size = 3, colour = "black") +
  geom_vline(xintercept=ymd_hms("2009-12-24 00:00:00"), linetype="dotted", size=0.4) +
  geom_vline(xintercept=ymd_hms("2009-12-25 00:00:00"), linetype="dotted", size=0.4) +
  geom_vline(xintercept=ymd_hms("2009-12-26 00:00:00"), linetype="dotted", size=0.4) +
  geom_vline(xintercept=ymd_hms("2009-12-27 00:00:00"), linetype="dotted", size=0.4) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.8, hjust=0.9)) +
  scale_x_datetime(date_labels = "%H:%M", breaks = seq(ymd_hms("2009-12-21 00:00:00"),
                                                       ymd_hms("2009-12-27 23:30:00"),
                                                       "6 hours"))

ggsave("average_consum_week_christmas.png",
       last_plot(),
       width=9,
       height=4,
       dpi="print")

# 3.4

# 3.4.1
# Get mean differences
mean(data[data$Code == 1,]$y_consum) - mean(data[data$Code == 2,]$y_consum)
mean(data[data$Code == 1,]$y_consum) - mean(data[data$Code == 3,]$y_consum)
mean(data[data$Code == 2,]$y_consum) - mean(data[data$Code == 3,]$y_consum)

length(data[data$Code == 1,]$y_consum)
length(data[data$Code == 2,]$y_consum)
length(data[data$Code == 3,]$y_consum)

t.test(data[data$Code == 1,]$y_consum, data[data$Code == 2,]$y_consum)
t.test(data[data$Code == 1,]$y_consum, data[data$Code == 3,]$y_consum)
t.test(data[data$Code == 2,]$y_consum, data[data$Code == 3,]$y_consum)

# Truncate data for illustration purposes
data_truncated <- data[data$y_consum < quantile(data$y_consum, 0.99, names=FALSE),]

ggplot(data_truncated, aes(factor(Code), y_consum)) +
  geom_boxplot() +
  coord_flip() +
  scale_y_continuous(breaks = round(seq(0, max(data_truncated$y_consum), len = 10), -3)) +
  theme_bw() +
  xlab("group") +
  ylab("annual electricity consumption in kWh") +
  scale_x_discrete(labels=c("Residential", "SME", "Other"))
  
ggsave("boxplot_consum.png",
       last_plot(),
       width=9,
       height=4,
       dpi="print")

# 3.4.2
df_regression <- data.frame(data[data$Code == 1,]$y_consum, data[data$Code == 1,]$household_size, as.numeric(data[data$Code == 1, 15] == 3))
colnames(df_regression) <- c("consumption", "household_size", "children")

summary(lm(consumption ~ household_size, data=df_regression, na.action=na.exclude))

ggplot(df_regression, aes(x = household_size, y = consumption)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "red") +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  xlab("household size") +
  ylab("annual electricity consumption in kWh") +
  theme_bw() +
  theme(text = element_text(size=12))

ggsave("regression_plot_3_4_2.png",
       last_plot(),
       width=9,
       height=4,
       dpi="print")

model <- lm(consumption ~ household_size + children + (household_size * children), data=df_regression, na.action=na.exclude)
summary(model)

# Breusch-Pagan test implies heteroskedasticity
bptest(model, data=df_regression)

# Correct standard errors following White (1980)
coeftest(model, vcov = vcovHC(model, type = "HC0"))

# 3.4.3
# Cleaning and converting
floor_area_convert <- data.frame(data[44], data[45])
colnames(floor_area_convert) <- c("floor_area", "unit")

# Convert square meters to square foot
data$home_floor_area <- ifelse(floor_area_convert$unit == 1 & !is.na(floor_area_convert$unit),
                               floor_area_convert$floor_area * 10.764,
                               floor_area_convert$floor_area)

data[data$home_floor_area >= 99999999 & !is.na(data$home_floor_area),]$home_floor_area <- NA

df_regression <- data.frame(data[data$Code == 1,]$household_size, log(data[data$Code == 1,]$home_floor_area))
colnames(df_regression) <- c("household_size", "floor_area")

model <- lm(floor_area ~ household_size, data = df_regression, na.action = na.exclude)
summary(model)

coeftest(model, vcov = vcovHC(model, type = "HC0"))

# Questions 402 and 4021 are mutually exclusive, we can merge them
income_convert <- data.frame(data[140], data[141])
colnames(income_convert) <- c("income1", "income2")

data$income_category <- ifelse(is.na(income_convert$income1) & !is.na(income_convert$income2), income_convert$income2, income_convert$income1)

# Remove income that is explicitly not per year and income that is explicitly after tax
data[which(!is.na(data$income_category) & (data[142] != 3 | data[143] == 2) & !is.na(data[142])), "income_category"] <- NA

# Remove refused income category
data[which(!is.na(data$income_category) & data$income_category == 6), "income_category"] <- NA

income_dummies <- dummy_cols(data[data$Code == 1,]$income_category, remove_first_dummy = TRUE, ignore_na = TRUE, remove_selected_columns = TRUE)

df_regression <- data.frame(data[data$Code == 1,]$household_size, log(data[data$Code == 1,]$home_floor_area), income_dummies)
colnames(df_regression) <- c("household_size", "floor_area", "income_2", "income_3", "income_4", "income_5")

model <- lm(floor_area ~ household_size + income_2 + income_3 + income_4 + income_5, data = df_regression, na.action = na.exclude)
summary(model)

coeftest(model, vcov = vcovHC(model, type = "HC0"))

# SMEs

premise_area_convert <- data.frame(data[161], data[162])
colnames(premise_area_convert) <- c("area", "unit")

# Convert square meters to square foot
data$premise_area <- ifelse(premise_area_convert$unit == 2 & !is.na(premise_area_convert$unit),
                            premise_area_convert$area * 10.764,
                            premise_area_convert$area)

data[which(data$premise_area >= 99999999 & !is.na(data$premise_area)), "premise_area"] <- NA

# Clean annual turnover
# Split in small / medium to ensure sufficient number of observations for t-test
turnover_convert <- data.frame(data[data$Code == 2, 154])
colnames(turnover_convert) <- "turnover"
turnover_dummy <- ifelse(turnover_convert$turnover < 13 & turnover_convert$turnover > 4 & !is.na(turnover_convert$turnover),
                          1,
                          ifelse(turnover_convert$turnover < 5 & !is.na(turnover_convert$turnover), 0, NA))

employees_convert <- data.frame(data[data$Code == 2, 151])
colnames(employees_convert) <- "employees"
employees_dummy <- ifelse(employees_convert$employees > 4 & !is.na(employees_convert$employees),
                          1,
                          ifelse(!is.na(employees_convert$employees), 0, NA))

df_regression <- data.frame(log(data[data$Code == 2,]$premise_area), turnover_dummy, employees_dummy)
colnames(df_regression) <- c("premise_area", "turnover_medium", "employees_medium")

model <- lm(premise_area ~ employees_medium, data = df_regression, na.action = na.exclude)
summary(model)

coeftest(model, vcov = vcovHC(model, type = "HC0"))

model <- lm(premise_area ~ turnover_medium + employees_medium, data = df_regression, na.action = na.exclude)
summary(model)

coeftest(model, vcov = vcovHC(model, type = "HC0"))

# 3.4.4
# Clean year built
data[which((data[42] < 1500 | data[42] > 2010) & !is.na(data[42])), 42] <- NA

# Generate categories
data$year_built_category <- cut(pull(data[42]), breaks=c(1500, 1800, 1900, 1950, 1980, 2000, 2010), labels=c("<1800", "1800-1900", "1900-1950", "1950-1980", "1980-2000", ">2000"))

# Plot density for each category
ggplot(data[which(data$Code == 1 & !is.na(data$year_built_category)),], aes(x = y_consum, fill = year_built_category)) +
  geom_density(alpha = 0.5) +
  xlab("annual electricity consumption in kWh") +
  theme_bw()

ggsave("construction_year_density.png",
       last_plot(),
       width=9,
       height=4,
       dpi="print")

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

# 3.4.5
# Compare different home categories

# Get home categories
data$home_category <- as.factor(ifelse(data[40] == 1 & !is.na(data[40]), 1, ifelse(
  data[40] == 5 & !is.na(data[40]), 2, ifelse(
    data[40] > 1 & data[40] < 5 & !is.na(data[40]), 3, NA
  )
)))

ggplot(data[data$Code == 1 & !is.na(data$home_category),], aes(x = y_consum, y = home_category)) +
  geom_violin() +
  geom_boxplot(width=0.1) +
  coord_flip() +
  theme_bw() +
  scale_y_discrete(labels=c("apartment", "bungalow", "house")) +
  xlab("annual electricity consumption in kWh") +
  ylab("home category")

ggsave("home_category.png",
       last_plot(),
       width=9,
       height=4,
       dpi="print")

consum_apartments <- pull(data[which(data$home_category == 1 & !is.na(data$home_category)), "y_consum"])
consum_bungalows <- pull(data[which(data$home_category == 2 & !is.na(data$home_category)), "y_consum"])
consum_houses <- pull(data[which(data$home_category == 3 & !is.na(data$home_category)), "y_consum"])

# Category 1 (Apartments) - Category 2 (Bungalows)
mean(consum_apartments) - mean(consum_bungalows)
t.test(consum_apartments, consum_bungalows)

# Category 1 (Apartments) - Category 3 (Houses)
mean(consum_apartments) - mean(consum_houses)
t.test(consum_apartments, consum_houses)

# Category 2 (Bungalows) - Category 3 (Houses)
mean(consum_bungalows) - mean(consum_houses)
t.test(consum_bungalows, consum_houses)

# 3.4.6

# Set refused to NA
data[which(data[46] == 6 & !is.na(data[46])), 46] <- NA

# Assumption: Households with more than 5 bedrooms have 5 bedrooms to keep variable continuous
df_regression <- data.frame(data[data$Code == 1,]$y_consum, data[data$Code == 1, 46])
colnames(df_regression) <- c("consumption", "bedrooms")

model <- lm(consumption ~ bedrooms, data = df_regression, na.action = na.exclude)
summary(model)

bptest(model)
coeftest(model, vcov = vcovHC(model, type = "HC0"))

# Get tumble_dryer information
data$tumble_dryer <- as.numeric(data[87] == 2 | data[87] == 3 | data[87] == 4)

df_regression <- data.frame(data[data$Code == 1,]$y_consum, data[data$Code == 1, 46], data[data$Code == 1,]$residential_electric_heating, data[data$Code == 1,]$tumble_dryer)
colnames(df_regression) <- c("consumption", "bedrooms", "electric_heating", "tumble_dryer")

model <- lm(consumption ~ bedrooms + electric_heating + tumble_dryer, data = df_regression, na.action = na.exclude)
summary(model)
bptest(model)
coeftest(model, vcov = vcovHC(model, type = "HC0"))

# 3.4.7

df_regression <- data.frame(log(data[data$Code == 1,]$y_consum), log(data[data$Code == 1,]$home_floor_area))
colnames(df_regression) <- c("consumption", "floor_area")

model <- lm(consumption ~ floor_area, data = df_regression, na.action = na.exclude)
summary(model)

bptest(model)
coeftest(model, vcov = vcovHC(model, type = "HC0"))

# 3.4.8

# Try out regression model to check VIFs
df_regression <- data.frame(data[data$Code == 1,]$y_consum, data[data$Code == 1, 46], data[data$Code == 1,]$home_floor_area)
colnames(df_regression) <- c("consumption", "bedrooms", "floor_area")
model <- lm(consumption ~ bedrooms + floor_area, data = df_regression, na.action = na.exclude)
summary(model)

vif(model)

# 3.5

# 3.5.1

# Clustering with k-means is sensitive to outliers
# Remove outliers with too large floor area or electricity consumption
# These outliers should be analyzed separately 
quantile_area <- quantile(data[data$Code == 1,]$home_floor_area, 0.95, names=FALSE, na.rm=TRUE)
quantile_consumption <- quantile(data[data$Code == 1,]$y_consum, 0.95, names=FALSE, na.rm=TRUE)

selected_clustering <- which(data$Code == 1 & data$home_floor_area < quantile_area & data$y_consum < quantile_consumption)

df_clustering <- na.omit(data.frame(scale(data[selected_clustering,]$y_consum), scale(data[selected_clustering,]$home_floor_area)))
colnames(df_clustering) <- c("consumption", "floor_area")

cluster <- kmeans(df_clustering, centers = 3)
df_clustering$cluster <- as.factor(cluster$cluster)

ggplot(data = df_clustering, aes(x = consumption, y = floor_area, colour = cluster)) +
  geom_point() +
  theme_bw() +
  xlab("annual electricity consumption") +
  ylab("floor area") +
  theme(legend.position = "None")

ggsave("kmeans_cluster.png",
       last_plot(),
       width=9,
       height=4,
       dpi="print")

# Find optimal k via elbow method
wss <- function(k) {
  kmeans(df_clustering, k, nstart = 10)$tot.withinss
}
k_values <- 1:10
wss_values <- map_dbl(k_values, wss)
df_wss_values <- data.frame(wss_values, no_cluster = seq(length(wss_values)))
ggplot(data = df_wss_values, aes(x = no_cluster, y = wss_values)) + geom_line() +
  scale_x_continuous(breaks = round(seq(1, max(df_wss_values$no_cluster), len = 10), 0)) +
  theme_bw() +
  geom_vline(xintercept=3, linetype="dotted", size=0.8)

ggsave("elbow.png",
       last_plot(),
       width=9,
       height=4,
       dpi="print")

# DBSCAN
# No need to remove outliers
df_clustering <- na.omit(data.frame(scale(data[data$Code == 1,]$y_consum), scale(data[data$Code == 1,]$home_floor_area)))
colnames(df_clustering) <- c("consumption", "floor_area")

# Get optimal eps
dbscan::kNNdistplot(df_clustering, k =  3)
cluster_dbscan <- dbscan(df_clustering, eps = 0.5, minPts = 10)
df_clustering$cluster_dbscan <- as.factor(cluster_dbscan$cluster)
ggplot(data = df_clustering, aes(x = consumption, y = floor_area, colour = cluster_dbscan)) +
  geom_point() +
  theme_bw() +
  xlab("annual electricity consumption") +
  ylab("floor area") +
  theme(legend.position = "None")

ggsave("dbscan_cluster.png",
       last_plot(),
       width=9,
       height=4,
       dpi="print")

# 3.5.2

# Pull washing machine data for encoding
washing_machine_loads <- pull(data[data$Code == 1, 106])
# Set more than 3 loads to category 3
washing_machine_loads[which(washing_machine_loads > 3 & !is.na(washing_machine_loads))] <- 3

# One-hot encode categorial variables
washing_machine_dummies <- dummy_cols(washing_machine_loads, remove_first_dummy = FALSE, ignore_na = TRUE, remove_selected_columns = TRUE)

df_classification <- na.omit(data.frame(data[data$Code == 1,]$household_size, data[data$Code == 1,]$y_consum, data[data$Code == 1, 46], washing_machine_dummies))
colnames(df_classification) <- c("household_size", "consumption", "bedrooms", "washing_1", "washing_2", "washing_3")

# Remove outliers and classes with too less occurences
df_classification <- df_classification[which(df_classification$household_size < 6 & df_classification$consumption < 20000),]
rownames(df_classification) <- NULL

df_classification$household_size <- as.factor(df_classification$household_size)

# Divide in training and test dataset
sample_size <- floor(0.67 * nrow(df_classification))

train_index <- sample(seq_len(nrow(df_classification)), size = sample_size)

train_data <- df_classification[train_index,]
test_data <- df_classification[-train_index,]

# SVM training
svm_model <- svm(household_size ~ ., data=train_data, scale = TRUE, gamma=0.25, cost=20)

# SVM testing
pred <- predict(svm_model, test_data[,-1])

# Get metrics
confusionMatrix(data = pred, reference = test_data$household_size)

# 3.5.3
# Combine residential and SME area data
data$home_sme_area <- ifelse(is.na(data$home_floor_area) & !is.na(data$premise_area), data$premise_area, data$home_floor_area)

# Remove outliers since knn is sensitive to these
data_index <- which((data$Code == 1 & data$y_consum < 20000) | data$Code == 2)

# Build classification dataframe and normalize features
df_classification <- na.omit(data.frame(data[data_index,]$Code, scale(data[data_index,]$y_consum), scale(data[data_index,]$home_sme_area)))
colnames(df_classification) <- c("Code", "consumption", "area")
rownames(df_classification) <- NULL

# Divide in training and test dataset
sample_size <- floor(0.67 * nrow(df_classification))

train_index <- sample(seq_len(nrow(df_classification)), size = sample_size)

train_data <- df_classification[train_index, -1]
test_data <- df_classification[-train_index, -1]

train_labels <- df_classification[train_index, 1]
test_labels <- df_classification[-train_index, 1]

# Run knn
pred <- knn(train_data, test_data, cl = train_labels, k = 11)

# Get metrics
confusionMatrix(data = pred, reference = as.factor(test_labels))

# Classify new observations
df_to_classify <- na.omit(data.frame(scale(data[data$Code == 3,]$y_consum), scale(data[data$Code == 3,]$home_sme_area)))
colnames(df_to_classify) <- c("consumption", "area")

# Predict if household or SME
pred <- knn(df_classification[, -1], df_to_classify, cl = df_classification[, 1], k = 11)
