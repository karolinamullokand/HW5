HW5
Study group: Karolina Mullokand & Jermaine

```
library(plotly)
library(dplyr)
library(ggplot2)
library(tidyverse)
setwd("/Users/karolinamullokand/Desktop/ECO_B2000")
load("Household_Pulse_data_ph4c2.RData")
```

```
# Sample: With a focus on indivduals in their prime working years, my objective is to better understand marriage how marriage, anxiety and income are interrelated
# Setting up Variables
# creating midpoints for income levels
Household_Pulse_data$income_midpoint <- fct_recode(Household_Pulse_data$INCOME,
                                                   "12500" = "HH income less than $25k",
                                                   "30000" = "HH income $25k - $34.9k",
                                                   "40000" = "HH income $35k - 49.9",
                                                   "62500" = "HH income $50k - 74.9",
                                                   "82500" = "HH income $75 - 99.9",
                                                   "125000" = "HH income $100k - 149",
                                                   "175000" = "HH income $150 - 199",
                                                   "225000" = "HH income $200k +",
                                                   NULL = "NA")
```

```
Household_Pulse_data$income_midpoint <- as.numeric(levels(Household_Pulse_data$income_midpoint))[Household_Pulse_data$income_midpoint]
summary(Household_Pulse_data$income_midpoint )
```

# filtering age to represent prime working years 25-55

```
Household_Pulse_data$Age <- 2024 - Household_Pulse_data$TBIRTH_YEAR
filtereddata <- Household_Pulse_data %>%
  filter(MS %in% c("married", "never") &
           ANXIOUS %in% c("no anxiety over past 2 wks", "nearly every day anxiety") &
           Age >= 25 & Age <= 55)
```

# at te confidence level of 90% I hypothesize There is a significant positive correlation between marital status and household income leading to lower levels of stress

```
#model1
model <- lm(income_midpoint  ~ Age + MS + ANXIOUS , data = filtereddata)
confint(model, level = 0.90)
t_test_result <- t.test(income_midpoint ~ ANXIOUS, data = filtereddata)
t_test_result$p.value
t_test_result$conf.int #format(p_value, scientific = FALSE)
summary(model)
```

# subset in order to plot

```
NNobs <- length(filtereddata$income_midpoint)
set.seed(12345) # just so you can replicate and get same "random" choices
graph_obs <- (runif(NNobs) < 0.1) # so something like just 1/10 as many obs
dat_graph <-subset(filtereddata,graph_obs)
modelsummary(model, stars = TRUE, gof_map = c("nobs", "r.squared"))
ggplot(dat_graph, aes(x = interaction(ANXIOUS,MS), y = income_midpoint, color = MS)) +
  geom_jitter(shape = 16, alpha = 0.7, size = 3) +
  ylim(0, 120000)
```





















