# Downloading and loading required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)

final.data <- read_rds("data/output/final_ma_data.rds")

# Question 1: distribution of plan counts by county over time 
## Filter data to remove SNPs, 800-series plans, and prescription drug only plans
filtered.data <- final.data %>%
    filter(
        snp == "No",
        !(planid >= 800 & planid < 900),
        !is.na(partc_score)
    )

## Count plans by county and year
plan.counts <- filtered.data %>%
    group_by(fips, year) %>%
    summarise(plan_count = n(), .groups = "drop")

## Boxplot of plan counts over time
q1 <- ggplot(plan.counts, aes(x = as.factor(year), y = plan_count)) +
    geom_boxplot() +
    labs(x = "Year",
             y = "Number of Plans per County") +
    theme_minimal()
print(q1)

# Question 2: distribution of star ratings in 2010, 2012, and 2015
## Filter data for the years 2010, 2012, and 2015
star_ratings_data <- filtered.data %>%
    filter(year %in% c(2010, 2012, 2015))

## Create a bar graph for the distribution of star ratings
q2 <- ggplot(star_ratings_data, aes(x = as.factor(partc_score), fill = as.factor(year))) +
    geom_bar(position = "dodge") +
    labs(x = "Star Rating",
         y = "Count",
         fill = "Year") +
    theme_minimal() +
    ggtitle("Distribution of Star Ratings in 2010, 2012, and 2015")
print(q2)

# Question 3: Avg benchmark payment from 2010 to 2015 
## Filter data for the years 2010 through 2015
benchmark_data <- filtered.data %>%
    filter(year >= 2010 & year <= 2015) %>% 
    group_by(year) %>%
    summarise(avg_benchmark = mean(ma_rate, na.rm = TRUE), .groups = "drop")

## Line plot of average benchmark payment over time
q3 <- ggplot(benchmark_data, aes(x = year, y = avg_benchmark)) +
    geom_line() +
    geom_point() +
    labs(x = "Year",
         y = "Average Benchmark Payment",
         title = "Average Benchmark Payment (2010-2015)") +
    theme_minimal()
print(q3)

## Calculate the rise in average benchmark payment
payment_rise <- benchmark_data %>%
    summarise(rise = max(avg_benchmark) - min(avg_benchmark)) %>%
    pull(rise)

cat("The average benchmark payment has risen by", round(payment_rise, 2), "over the years.")

# Question 4: Avg share of Medicaid over time
## Filter data for the years 2010 through 2015 and calculate average share of Medicare Advantage
medicare_share_data <- filtered.data %>%
    filter(year >= 2010 & year <= 2015) %>%
    group_by(year) %>%
    summarise(avg_share_ma = mean(ma_share, na.rm = TRUE), .groups = "drop")

## Line plot of average share of Medicare Advantage over time
q4 <- ggplot(medicare_share_data, aes(x = year, y = avg_share_ma)) +
    geom_line() +
    geom_point() +
    labs(x = "Year",
         y = "Average Share of Medicare Advantage",
         title = "Average Share of Medicare Advantage (2010-2015)") +
    theme_minimal()
print(q4)

save.image("submission1/results/hwk4_workspace.Rdata")