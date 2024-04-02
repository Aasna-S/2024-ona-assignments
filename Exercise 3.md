---
title: "Exercise 3"
output: 
  pdf_document: 
    default
header-includes: 
  \usepackage{float}
date: "2024-04-01" 
--- 

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library("arrow") 
library("dplyr") 
library("ggplot2") 
library("gender") 
library("wru") 
library("lubridate") 
library("readr")
library("tidyverse")
library("igraph")
library("ggraph")
library("dplyr")
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
#GENDER 


unique_first_names <- app_data %>%
  distinct(examiner_first_name = examiner_name_first)

safe_gender <- function(name) {
  tryCatch({
    result <- gender(name, method = "ssa")
    if (nrow(result) == 0) return(NA)  
    return(result$gender[1])  
  }, error = function(e) {
    NA  
  })
}

gender_info <- unique_first_names %>%
  rowwise() %>%
  mutate(gender_assignment = safe_gender(examiner_first_name)) %>%
  ungroup()  
app_data <- app_data %>%
  left_join(gender_info, by = c("examiner_name_first" = "examiner_first_name"))


rm(unique_first_names, gender_info)
gc()

app_data
```

```{r}
#RACE
examiner_surnames <- app_data %>% 
  select(surname = examiner_name_last) %>% 
  distinct()


race_predictions <- predict_race(examiner_surnames, surname.only = T) %>%
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
  left_join(race_assignment, by = c("examiner_name_last" = "surname"))

rm(unique_last_names, race_predictions, race_assignment)
gc()

app_data
```


```{r}
#TENURE

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

# Select the workgroups of interest
workgroup_A = filter(app_data, str_starts(examiner_art_unit, "176"))
workgroup_B = filter(app_data, str_starts(examiner_art_unit, "162"))


summary(workgroup_A)
summary(workgroup_B)

```
```{r}

examiner_ids = distinct(subset(app_data, select=c(examiner_art_unit, examiner_id)))
examiner_ids$workgroup = substr(examiner_ids$examiner_art_unit, 1,3)
examiner_ids = examiner_ids[examiner_ids$workgroup==162 | examiner_ids$workgroup==176,]

data.f = merge(x=relationship_data, y=examiner_ids, by.x="ego_examiner_id", by.y="examiner_id", all.x=TRUE)
data.f = data.f %>% rename(ego_art_unit=examiner_art_unit, ego_workgroup=workgroup)
data.f = drop_na(data.f)

data.f = merge(x=data.f, y=examiner_ids, by.x="alter_examiner_id", by.y="examiner_id", all.x=TRUE)
data.f = data.f %>% rename(alter_art_unit=examiner_art_unit, alter_workgroup=workgroup)
data.f = drop_na(data.f)

```

```{r}
ego_nodes = subset(data.f, select=c(ego_examiner_id,ego_art_unit, ego_workgroup)) %>% rename(examiner_id=ego_examiner_id,art_unit=ego_art_unit,workgroup=ego_workgroup)
alter_nodes = subset(data.f, select=c(alter_examiner_id,alter_art_unit, alter_workgroup))%>% rename(examiner_id=alter_examiner_id,art_unit=alter_art_unit,workgroup=alter_workgroup)
nodes = rbind(ego_nodes, alter_nodes)
nodes = distinct(nodes)
nodes = nodes %>% group_by(examiner_id) %>% summarise(examiner_id=first(examiner_id), art_unit=first(art_unit), workgroup=first(workgroup))

advice_net = graph_from_data_frame(d=data.f, vertices=nodes, directed=TRUE)
```



```{r}

degree_centrality <- degree(advice_net, v=V(advice_net))
betweenness_centrality <- betweenness(advice_net)
closeness_centrality <- closeness(advice_net)

centrality_scores <- cbind(degree_centrality, betweenness_centrality,closeness_centrality)

head(centrality_scores)

centrality_scores = data.frame(centrality_scores)

```

```{r}
app_data$degree_centrality <- centrality_scores[match(app_data$examiner_id, row.names(centrality_scores)), "degree"]
app_data$betweenness_centrality <- centrality_scores[match(app_data$examiner_id, row.names(centrality_scores)), "betweenness"]
app_data$closeness_centrality <- centrality_scores[match(app_data$examiner_id, row.names(centrality_scores)), "closeness"]

```

```{r}

V(advice_net)$size <- degree(advice_net)
V(advice_net)$color <- ifelse(V(advice_net)$name %in% c("specific", "node", "names"), "red", "blue")


ggraph(advice_net, layout = "kk") +
  geom_edge_link() +
  geom_node_point(aes(size = size, color = color), show.legend = TRUE)
```

