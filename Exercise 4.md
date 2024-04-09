---
title: "Exercise 4"
output: 
  pdf_document: 
    default
header-includes: 
  \usepackage{float}
date: "2024-04-09"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library("arrow")
library("tidyverse")
library("gender")
library(dplyr)
```


```{r cars}
data_path <- "C:\\Users\\Aasna\\Downloads\\672_project_data\\"
applications <- read_parquet(paste0(data_path, "app_data_sample.parquet"))
edges <- read_csv(paste0(data_path, "edges_sample.csv"))
```



```{r pressure, echo=FALSE}
applications
```

```{r}
edges
```

## add the following variables for examiners: Gender, Race, Tenure


## Gender
```{r}
# get a list of first names without repetitions
# get a list of first names without repetitions
examiner_names <- applications %>% 
  distinct(examiner_name_first)

examiner_names
```

```{r}
# get a table of names and gender
examiner_names_gender <- examiner_names %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(
    examiner_name_first = name,
    gender,
    proportion_female
  )

examiner_names_gender
```



```{r}
# remove extra colums from the gender table
examiner_names_gender <- examiner_names_gender %>% 
  select(examiner_name_first, gender)

# joining gender back to the dataset
applications <- applications %>% 
  left_join(examiner_names_gender, by = "examiner_name_first")

# cleaning up
rm(examiner_names)
rm(examiner_names_gender)
gc()

applications
```


## Race
```{r}
library(wru)

examiner_surnames <- applications %>% 
  select(surname = examiner_name_last) %>% 
  distinct()

examiner_surnames
```


```{r}
examiner_race <- predict_race(voter.file = examiner_surnames, surname.only = T) %>% 
  as_tibble()
examiner_race
```


```{r}
examiner_race <- examiner_race %>% 
  mutate(max_race_p = pmax(pred.asi, pred.bla, pred.his, pred.oth, pred.whi)) %>% 
  mutate(race = case_when(
    max_race_p == pred.asi ~ "Asian",
    max_race_p == pred.bla ~ "black",
    max_race_p == pred.his ~ "Hispanic",
    max_race_p == pred.oth ~ "other",
    max_race_p == pred.whi ~ "white",
    TRUE ~ NA_character_
  ))

examiner_race
```

```{r}
# removing extra columns
examiner_race <- examiner_race %>% 
  select(surname,race)

applications <- applications %>% 
  left_join(examiner_race, by = c("examiner_name_last" = "surname"))

rm(examiner_race)
rm(examiner_surnames)
gc()
```


```{r}
applications
```


## Tenure

```{r}
library(lubridate) # to work with dates

examiner_dates <- applications %>% 
  select(examiner_id, filing_date, appl_status_date) 

examiner_dates
```

```{r}
examiner_dates <- examiner_dates %>% 
  mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date)))
```


```{r}
examiner_dates <- examiner_dates %>% 
  group_by(examiner_id) %>% 
  summarise(
    earliest_date = min(start_date, na.rm = TRUE), 
    latest_date = max(end_date, na.rm = TRUE),
    tenure_days = interval(earliest_date, latest_date) %/% days(1)
    ) %>% 
  filter(year(latest_date)<2018)

examiner_dates
```

```{r}
applications <- applications %>% 
  left_join(examiner_dates, by = "examiner_id")

rm(examiner_dates)
gc()

applications
```

 

```{r}

group_219 = applications[substr(applications$examiner_art_unit, 1,3)==219,]
group_162 = applications[substr(applications$examiner_art_unit, 1,3)==162,]
summary(group_219)
```


```{r}
summary(group_162)
```



```{r}
# get examiner ids to use as nodes
examiner_ids = distinct(subset(applications, select=c(examiner_art_unit, examiner_id)))
examiner_ids$workgroup = substr(examiner_ids$examiner_art_unit, 1,3)
examiner_ids = examiner_ids[examiner_ids$workgroup==162 | examiner_ids$workgroup==219,]

# merge with edges dataframe to get final edge table
data.f = merge(x=edges, y=examiner_ids, by.x="ego_examiner_id", by.y="examiner_id", all.x=TRUE)
data.f = data.f %>% rename(ego_art_unit=examiner_art_unit, ego_workgroup=workgroup)
data.f = drop_na(data.f)

data.f = merge(x=data.f, y=examiner_ids, by.x="alter_examiner_id", by.y="examiner_id", all.x=TRUE)
data.f = data.f %>% rename(alter_art_unit=examiner_art_unit, alter_workgroup=workgroup)
data.f = drop_na(data.f)

```



```{r}
# get unique ego and alter nodes
ego_nodes = subset(data.f, select=c(ego_examiner_id,ego_art_unit, ego_workgroup)) %>% rename(examiner_id=ego_examiner_id,art_unit=ego_art_unit,workgroup=ego_workgroup)
alter_nodes = subset(data.f, select=c(alter_examiner_id,alter_art_unit, alter_workgroup))%>% rename(examiner_id=alter_examiner_id,art_unit=alter_art_unit,workgroup=alter_workgroup)
nodes = rbind(ego_nodes, alter_nodes)
nodes = distinct(nodes)
nodes = nodes %>% group_by(examiner_id) %>% summarise(examiner_id=first(examiner_id), art_unit=first(art_unit), workgroup=first(workgroup))
library(igraph)
# creating network
advice_net = graph_from_data_frame(d=data.f, vertices=nodes, directed=TRUE)
```
###Create variable for application processing time ‘app_proc_time’

Exercise 4

```{r}

# filter for examiner ids to consider (nodes)
applications_2 <- applications %>%
      filter(examiner_id %in% nodes$examiner_id)

# Convert to class date
applications_2 <- applications_2 %>% 
  mutate(patent_issue_date = as.Date(patent_issue_date, format = "%YYYY-%mm-%dd"))
applications_2 <- applications_2 %>% 
  mutate(abandon_date = as.Date(abandon_date, format = "%YYYY-%mm-%dd"))

app_proc_time <- c()
for (i in 1:nrow(applications_2)){
  # if patent issue date is NA, use abandon date
  if (is.na(applications[i,11])){
    #app_proc_time[i] = interval(applications_2[i,2] ,applications_2[i,11] )
    app_proc_time[i] = applications_2[i,12] - applications_2[i,2]
  }
  else{
    #app_proc_time[i] = interval(applications_2[i,2] ,applications_2[i,12] )
    app_proc_time[i] = applications_2[i,11] - applications_2[i,2]
  }
  
}

applications_2$app_proc_time = app_proc_time
```

### join with nodes dataset: add gender, race, app_proc_time, tenure and other relevant variables
```{r}
# remove columns not needed for regression from the applications table
applications_2 <- applications_2 %>% 
  select(examiner_id, gender, race, app_proc_time,tenure_days)
# remove NaN values
applications_2<- na.omit(applications_2)
applications_2 <- subset(applications_2,!duplicated(applications_2$examiner_id))
# group by examiner id to get average app_proc time for each examiner
#applications_2 <- applications_2%%group_by(applications_2$examiner_id)%%summarise_at(vars(applications_2$app_proc_date,applications_2$tenure_days),mean)
#applications_3 <- aggregate(applications_2$app_proc_time, list(applications_2$examiner_id), FUN=mean) 
# joining gender back to the dataset
nodes <- nodes %>% 
  left_join(applications_2, by = "examiner_id")
```

### add centrality values as columns in nodes dataframe
```{r}
Degree <- degree(advice_net, v=V(advice_net))
nodes$Degree <- Degree
Betweenness <- betweenness(advice_net)
nodes$Betweenness <- Betweenness
Closeness <- closeness(advice_net)
nodes$Closeness <- Closeness
Eigenvector <- evcent((advice_net))$vector
nodes$Eigenvector <- Eigenvector

nodes <- na.omit(nodes)
```

### Use linear regression models `lm()` to estimate the relationship between centrality and `app_proc_time`, Does this relationship differ by examiner gender? Include Interaction term

```{r}
library('stargazer')
# create linear regression
lr <- lm(unlist(app_proc_time) ~ Degree +Closeness + Betweenness + Eigenvector +
           tenure_days+ art_unit + workgroup, nodes)
# view model summary
summary(lr)
#stargazer(lr, type="html")

```



```{r}
# create linear regression
lr2 <- lm(unlist(app_proc_time) ~ Degree +Closeness + Betweenness + Eigenvector +
           tenure_days + art_unit + workgroup + Degree*race + Degree*gender + Betweenness*race + Betweenness*gender, nodes)
# view model summary
summary(lr2)
#stargazer(lr2, type="html")
```
Findings from the Regression Analyses
Model without Interaction Terms (lr):

- None of the predictors were statistically significant, as indicated by their p-values being well above the conventional threshold of 0.05.
- The coefficients for Degree, Closeness, Betweenness, and Eigenvector suggest their isolated effects on app_proc_time, but given the lack of significance, no conclusive relationships are established.
- The Residual standard error and the low Adjusted R-squared value suggest that the model does not fit the data well, explaining only a small fraction of the variance in app_proc_time.

Model with Interaction Terms (lr2):

- This model attempted to account for the interactions between centrality measures and race and gender, hypothesizing these demographic factors might modify the impact of centrality on processing times.
- Similar to the first model, the predictors, including the interaction terms, were not statistically significant.
- The Adjusted R-squared is negative, indicating that the model is overfitted with the number of predictors and interaction terms not justified by the data.
One of the coefficients (Betweenness:racewhite) could not be defined due to singularities, suggesting possible issues with multicollinearity or redundant variables.

Implications for the USPTO:
Centrality's Role: The lack of significant findings suggests that centrality, at least in isolation or in simple interaction with demographic variables, may not be a strong predictor of processing times. This could imply that factors not included in the model, such as the complexity of applications, external resources, or organizational support, play more critical roles.

Demographic Factors: The attempt to include race and gender interactions with centrality measures did not reveal significant effects. This could suggest that the efficiency and effectiveness of examiners, as measured by processing times, are not strongly differentiated by these demographic factors in the context of their network centrality. 
