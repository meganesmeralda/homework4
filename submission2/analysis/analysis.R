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
final.data <- final.data %>%
  mutate(ma_share = (avg_enrolled / avg_eligibles) * 100)

# Calculate the average share by year
average_share_by_year <- final.data %>%
  group_by(year) %>%
  summarize(avg_share = mean(ma_share, na.rm = TRUE))
# Plot the average share over time from 2010 to 2015
q4 <- ggplot(average_share_by_year, aes(x = year, y = avg_share)) +
  geom_line(color = "black") +
  geom_point(color = "black") +
  labs(
    title = "Average Share of Medicare Advantage Enrollment",
    x = "Year",
    y = "Average Share of Medicare Advantage Enrollment (%)"
  ) +
  theme_minimal()
print(q4)

# Estimating ATEs
data_2010 <- final.data %>%
             filter(!is.na(avg_enrollment) & year==2010 & !is.na(partc_score)) %>%
  distinct(contractid, planid, county, .keep_all = TRUE)

# Question 5: Underlying Star Rating
## Calculate raw ratings 
data_2010 <- data_2010 %>%
  mutate(raw_rating=rowMeans(
    cbind(breastcancer_screen, rectalcancer_screen, cv_diab_cholscreen, glaucoma_test,
          monitoring, flu_vaccine, pn_vaccine, physical_health, mental_health,
          osteo_test, physical_monitor, primaryaccess, osteo_manage,
          diab_healthy, bloodpressure, ra_manage, copd_test, bladder,
          falling, nodelays, doctor_communicate, carequickly, customer_service,                    
          overallrating_care, overallrating_plan, complaints_plan, appeals_timely,
          appeals_review, leave_plan, audit_problems, hold_times, info_accuracy,
          ttyt_available),
    na.rm=T)) %>%
    select(contractid, planid, fips, avg_enrollment, state, county, raw_rating, partc_score,
         avg_eligibles, avg_enrolled, premium_partc, partd, risk_ab, Star_Rating,
         bid, avg_ffscost, ma_rate, plan_type) %>% 
    mutate(mkt_share = avg_enrollment/avg_eligibles, 
          HMO=str_detect(plan_type, "HMO"))
colnames(data_2010)

## Calculate the rounded star ratings 
rounded_data_2010 <- data_2010 %>%
  mutate(rounded_30=ifelse(raw_rating>=2.75 & raw_rating<3.00 & Star_Rating==3.0,1,0), 
         rounded_35=ifelse(raw_rating>=3.25 & raw_rating<3.50 & Star_Rating==3.5,1,0),
         rounded_40=ifelse(raw_rating>=3.75 & raw_rating<4.00 & Star_Rating==4.0,1,0),
         rounded_45=ifelse(raw_rating>=4.25 & raw_rating<4.50 & Star_Rating==4.5,1,0), 
         rounded_50=ifelse(raw_rating>=4.75 & raw_rating<5.00 & Star_Rating==5.0,1,0)) %>%
  group_by(Star_Rating) %>% 
  filter(Star_Rating %in% c(3, 3.5, 4, 4.5, 5)) %>% 
  summarize(count_30=sum(rounded_30), 
            count_35=sum(rounded_35), 
            count_40=sum(rounded_40), 
            count_45=sum(rounded_45),
            count_50=sum(rounded_50))%>% 
  mutate(rounded_up=count_30 + count_35 + count_40 + count_45 + count_50) %>% 
  select(Star_Rating, rounded_up)

kable(rounded_data_2010)
# Display the table using kable
library(knitr)
kable(rounded_data_2010, format = "markdown", col.names = c("Star Rating", "Rounded Up Count"), 
    caption = "Rounded Up Star Ratings in 2010")


# Question 6: Estimate of the effect of receiving a 3-star versus a 2.5 star rating on enrollments and 3.5


rm(list=c("final.data"))
save.image("submission2/results/hwk4_workspace.Rdata")
