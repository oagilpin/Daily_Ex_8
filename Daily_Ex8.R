# Name: Olivia Gilpin
# Date: 2025-02-19
# Purpose: This script is for Day 08 exercises in R. 

#1
library(tidyverse)

url <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv'
covid <- read_csv(url)

#2
df <- data.frame(region = state.region, 
                 State = state.name, 
                 stateabbr = state.abb)

#3
covid_with_state_info <- covid %>%
  left_join(df, by = c("state" = "State"))

#4
covid_processed <- covid_with_state_info %>%
  select(state, region, date, cases, deaths) %>%
  mutate(date = as.Date(date))  
head(covid_processed)

#5
covid_cases_long <- covid_processed %>%
  select(region, date, cases) %>%
  pivot_longer(cols = cases, names_to = "metric", values_to = "value") %>%
  mutate(metric = "Cases")

covid_deaths_long <- covid_processed %>%
  select(region, date, deaths) %>%
  pivot_longer(cols = deaths, names_to = "metric", values_to = "value") %>%
  mutate(metric = "Deaths")

covid_combined <- bind_rows(covid_cases_long, covid_deaths_long)

#6
ggplot(covid_combined, aes(x = as.Date(date), y = value, color = metric)) +
  geom_line(linewidth = 1) +
  facet_wrap(~region + metric, scales = "free_y", ncol = 4) +  
  labs(
    title = "Cumulative Cases and Deaths: Region",
    subtitle = "COVID-19 Data: NY-Times",
    x = "Date",
    y = "Yearly Cumulative Count",
    color = "Metric"
  ) +
  scale_color_manual(values = c("Cases" = "green", "Deaths" = "purple")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray", linewidth = .75),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 4)  
  )

covid_processed_cleaned <- covid_processed %>%
  filter(!is.na(region))

covid_cases_long <- covid_processed_cleaned %>%
  select(region, date, cases) %>%
  pivot_longer(cols = cases, names_to = "metric", values_to = "value") %>%
  mutate(metric = "Cases")

covid_deaths_long <- covid_processed_cleaned %>%
  select(region, date, deaths) %>%
  pivot_longer(cols = deaths, names_to = "metric", values_to = "value") %>%
  mutate(metric = "Deaths")

covid_combined <- bind_rows(covid_cases_long, covid_deaths_long)

ggplot(covid_combined, aes(x = as.Date(date), y = value, color = metric)) +
  geom_line(linewidth = 1) +
  facet_wrap(~region + metric, scales = "free_y", ncol = 4) +  
  labs(
    title = "Cumulative Cases and Deaths: Region",
    subtitle = "COVID-19 Data: NY-Times",
    x = "Date",
    y = "Yearly Cumulative Count",
    color = "Metric"
  ) +
  scale_color_manual(values = c("Cases" = "green", "Deaths" = "purple")) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray", linewidth = .75),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 4)  
  )

