---
title: "Exercise 3"
output: pdf_document: default
header-includes: \usepackage{float}
date: "2024-04-01" 
--- 


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)


install.packages("arrow")
library("arrow") 
library("dplyr") 
library("ggplot2") 
install.packages("gender")
library("gender")
install.packages("wru")
library("wru") 
install.packages("lubridate")
library("lubridate") 
library(readr)
library(tidyverse)

```

```{r}
data_folder <- "C:\\Users\\Aasna\\Downloads\\672_project_data\\"
app_data <- read_parquet(paste0(data_folder, "app_data_sample.parquet"))
relationship_data <- read_csv(paste0(data_folder, "edges_sample.csv"))

```


```{r}
app_data

```

```{r}

relationship_data

```

```{r}
# Extract unique first names for gender determination
unique_first_names <- app_data %>%
  distinct(examiner_first_name = examiner_name_first)

# Assign gender to these names
gender_info <- unique_first_names %>%
  rowwise() %>%
  mutate(gender_assignment = gender(examiner_first_name, method = "ssa")) %>%
  unnest(c(gender_assignment)) %>%
  select(examiner_first_name, gender, proportion_female)

gender_info <- gender_info %>%
  select(examiner_first_name, gender)

app_data <- app_data %>%
  left_join(gender_info, by = c("examiner_name_first" = "examiner_first_name"))


rm(unique_first_names, gender_info)
gc()

app_data
```


```{r}

# Isolate last names for race estimation
unique_last_names <- app_data %>%
  select(last_name = examiner_name_last) %>%
  distinct()

race_predictions <- predict_race(unique_last_names, surname.only = TRUE) %>%
  as_tibble()

# Determine the predominant race for each surname
race_assignment <- race_predictions %>%
  rowwise() %>%
  mutate(dominant_race = case_when(
    pred.asi >= max(pred.bla, pred.his, pred.oth, pred.whi) ~ "Asian",
    pred.bla >= max(pred.asi, pred.his, pred.oth, pred.whi) ~ "Black",
    pred.his >= max(pred.asi, pred.bla, pred.oth, pred.whi) ~ "Hispanic",
    pred.oth >= max(pred.asi, pred.bla, pred.his, pred.whi) ~ "Other",
    TRUE ~ "White"
  ))

app_data <- app_data %>%
  left_join(race_assignment, by = "examiner_name_last")


rm(unique_last_names, race_predictions, race_assignment)
gc()

app_data
```



```{r}

# Process dates to calculate tenure
tenure_details <- app_data %>%
  mutate(
    start_date = ymd(filing_date),
    end_date = ymd(appl_status_date),
    tenure_days = as.integer(difftime(end_date, start_date, units = "days"))
  ) %>%
  group_by(examiner_id) %>%
  summarise(tenure = mean(tenure_days, na.rm = TRUE))

app_data <- app_data %>%
  left_join(tenure_details, by = "examiner_id")


rm(tenure_details)
gc()

app_data
```
```{r}

workgroup_A = filter(app_data, str_starts(examiner_art_unit, "176"))
workgroup_B = filter(app_data, str_starts(examiner_art_unit, "162"))


summary(workgroup_A)
summary(workgroup_B)

```
```{r}
# Gender distribution in the selected workgroups
ggplot(workgroup_A, aes(x = factor(gender))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Gender Distribution in Workgroup A") +
  theme_minimal()

ggplot(workgroup_B, aes(x = factor(gender))) +
  geom_bar(fill = "firebrick") +
  labs(title = "Gender Distribution in Workgroup B")

  
```
