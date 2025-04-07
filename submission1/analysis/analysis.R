if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)

final.data <- read_rds("data/output/final_ma_data.rds")

# Question 1: distribution of plan counts by county over time 
# Filter data to remove SNPs, 800-series plans, and prescription drug only plans
filtered.data <- final.data %>%
    filter(
        snp == "No",
        !(planid >= 800 & planid < 900),
        !is.na(partc_score)
    )

# Count plans by county and year
plan.counts <- filtered.data %>%
    group_by(fips, year) %>%
    summarise(plan_count = n(), .groups = "drop")

# Boxplot of plan counts over time
q1 <- ggplot(plan.counts, aes(x = as.factor(year), y = plan_count)) +
    geom_boxplot() +
    labs(x = "Year",
             y = "Number of Plans per County") +
    theme_minimal()
print(q1)


save.image("submission1/results/hwk4_workspace.Rdata")


