# Report

All values following are rounded to the first decimal place.

## 3.2 Get to know the data

### 1.	How many residential households, SMEs, and 'other' are contained in the data set?

| | Initially allocated | With consumption data |
| :---------------- | ----------------: | ----------------: |
| Residential | 4,225 | 3,639 |
| SME | 485 |  427 |
| Other | 1,735 | 570 |
| Sum | 6,445 | 4,636

The following analyses are based only on participants with reported consumption data as the project's main target is the performance assessment of smart meters.

### 2.	What is the fraction of residential households, SMEs and ‘other’ which have electric heating systems installed? For which proportion of households/SMEs is data regarding this characteristic missing? Report the results in a table.

| | Questions included |
| :---------------- | ----------------: |
| Residential | Q 470, Q 4701 |
| SME | Q 4551 |

| | Electric heating | Yes | No | N/A |
| :---------------- | ----------------: | ----------------: | ----------------: | ----------------: |
| Residential | | 48.3 | 33.9 | 17.8 |
| SME |	| 40.0 | 28.0 | 32.0 |
| Other	| | 20.5 | 13.5 | 66.0 |

All values are displayed in per cent. Includes space and water heating. Logical or-operators are put to use as we deem every residential and other using one of the latter as an electric heating user (derived from the fact that within the survey these heating measures are partially aggregated, as can be seen in column 173).

### 3. What is the proportion of residential households that have children?

22.5% of residential households have children.

| | Questions included |
| :---------------- | ----------------: |
| Residential | Q ? (Column 15), Q 43111 |

### 4. On average, how many people live in a residential household? Report the standard deviation, median, and variance as well.

| | Questions included |
| :---------------- | ----------------: |
| Residential | Q 420, Q 43111 |

| | Mean | Median | Variance | Std. Dev. |
| :------ | ------: | ------: | ------: | ------: |
| Residential | 4.2 | 4 | 1.5 | 1.2 |

### 5. Based on annual turnover, how many SMEs are ‘small’ enterprises and how are ‘medium’ sized?

Small and medium sized enterprise definition according to the EU Commission: 
https://ec.europa.eu/regional_policy/sources/conferences/state-aid/sme/smedefinitionguide_en.pdf

| | Questions included |
| :---------------- | ----------------: |
| SME | Q 6201 |

Everything below or equal answer code 8 is considered a small and everything above 8 to 12 a medium company according to above definition. Large companies are not included as this is a survey targeted at SMEs.

| | Company size |
| :------------- |-------------: |
| Small | 97 |
| Medium | 11 |
| NA / refused | 319 |

Obviously a large proportion of those enterprises surveyed did not want to disclose their annual turnover.

### 6. Provide a visual plot of the different business categories SMEs fall into.

!Placeholder: insert pychart with category distribution!

### 7. Summarize the level of satisfaction SMEs have with the electricity market in Ireland.

| | Questions included |
| :---------------- | ----------------: |
| SME | Q 5512 |

Summarize over the entirety of related questions and divide by the number of related questions (Q 5512 consists of eight questions in total).

| | Mean of means | Median of means | Standard deviation of means |
| :------ | ------: | ------: | ------: |
| Residential | 3 | 2.9 | 0.7 |

A mean of 3 can be interpreted as 'medium' satisfied, according to the scale.

Median of means (here: 2.9) is slightly lower compared to mean of means which implies there is a weak tendency towards higher levels of satisfaction, falsified by some highly negative outliers.

Standard deviation of means is at 0.7 here.

## 3.3 Investigate the data

### 1. Calculate the mean, variance and standard deviation of the yearly electricity consumption of all SMEs, households, and ‘others’, respectively. Report the results in a table.

| | Mean | Variance | Standard deviation |
| :------------- | ----------: | -----------------: | -----------------: |
| Residential | 8,712 | 19,007,356.2 | 4359.7 |
| SME | 36,547.1 |  1,331,332,711 | 36,487.4 |
| Other | 13,092.5 | 676,390,893 | 26,007.5 |

### 2. Calculate the mean, variance and standard deviation of the yearly consumption for residential households by household size. Interpret the results in 3-4 lines – what do these summary statistics tell you about the data set?

Household size is based on number of people living in the household. Household sizes are categorized into small (1-3 pax), medium (4-5 pax) and large (>6 pax.).

| | Mean | Variance | Standard deviation |
| :------------- | -----------------: | -----------------: | -----------------: |
| Small | 8,829.9 | 12,536,348 | 3,540.7 |
| Medium | 11,407.3 |  15,110,904 | 3,887.3 |
| Large | 13,719.3 | 23,568,691 | 4,854.8 |

Mean increases with increasing household size which is to be expected. Interestingly though, the standard deviation also increases with household sizes - this implies that larger households have a more heterogeneous energy consumption.

### 3. For the following tasks in this subsection, please use the data set with the halfhourly load profiles. Create a sub-sample with 100 randomly selected residential households and 100 randomly selected SMEs and:

Emergent observation beforehand: In both categories (residentials and SMEs), a decline of ca. 33% in monthly energy consumption can be observed during the seasons spring to autumn.

#### (a) Plot the average monthly energy consumption of the 100 randomly selected residential households included in the sub-sample over one year. Interpret the plot.

![Average monthly energy consumption of residential households](/img/avgMonthlyResidential.png)

We can see a sharp decline in energy consumption during the spring months and a strong increase in energy consumption during the autumn months. The reason for this observation is assumed to be the increased time of household participants staying at home during the colder and darker months of the year.

#### (b) Plot the average monthly energy consumption of the 100 random SMEs included in the sub-sample over one year. Interpret the plot.

![Average monthly energy consumption of SMEs](/img/avgMonthlySME.png)

We can see a sharp decline in energy consumption during the spring months and a strong increase in energy consumption during the autumn months. The reason for this observation is assumed to be the increased heating within enterprise premises during the colder and darker months of the year.