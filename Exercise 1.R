
library(dplyr)
library(stringr)
library(tidyr)
library(igraph)
library(ggplot2)


linkedin_data <- read.csv("Connections.csv", sep=",")


### contact analysis by current employer

employer_counts <- linkedin_data %>%
  group_by(Company) %>%
  summarise(Total = n()) %>%
  arrange(desc(Total))

print(employer_counts)

grand_total <- sum(employer_counts$Total)

print(paste("Grand total of contacts:", grand_total))

### creation of nodes and edges for network analysis

linkedin_data$UniqueName <- with(linkedin_data, paste(First.Name, substring(Last.Name, 1, 1), sep = "_"))

linkedin_data <- linkedin_data %>%
  mutate(UniqueID = row_number())


unique_nodes <- linkedin_data %>%
  distinct(UniqueID, UniqueName, Company)

updated_linkedin_data <- linkedin_data %>%
  inner_join(unique_nodes, by = c("UniqueName", "Company"))


connections <- unique_nodes %>%
  group_by(Company) %>%
  filter(n() > 1) %>%
  summarise(Pairs = list(combn(UniqueID, 2, simplify = FALSE))) %>%
  unnest(Pairs) %>%
  ungroup() %>%
  mutate(Start = sapply(Pairs, `[`, 1),
         End = sapply(Pairs, `[`, 2)) %>%
  select(Start, End)

print(connections)


network_graph <- graph_from_data_frame(d = connections, vertices = unique_nodes, directed = TRUE)
plot(network_graph, vertex.label = V(network_graph)$UniqueName)

### Network Visualization with 'McGill' affiliation 

unique_nodes <- unique_nodes %>%
  mutate(IsMcGill = ifelse(str_detect(Company, "McGill"), "Yes", "No"))
unique_nodes$UniqueID <- as.character(unique_nodes$UniqueID)


mcgill_graph <- graph_from_data_frame(d = connections, vertices = unique_nodes, directed = FALSE)
mcgill_layout <- as.data.frame(layout_with_fr(mcgill_graph))
names(mcgill_layout) <- c("x", "y")
mcgill_layout$UniqueID <- V(mcgill_graph)$name


mcgill_layout <- mcgill_layout %>%
  left_join(unique_nodes %>% select(UniqueID, IsMcGill), by = "UniqueID")

connections$Start <- as.character(connections$Start)
connections$End <- as.character(connections$End)


edge_coordinates <- connections %>%
  left_join(mcgill_layout %>% select(UniqueID, x_origin = x, y_origin = y), by = c("Start" = "UniqueID")) %>%
  left_join(mcgill_layout %>% select(UniqueID, x_destination = x, y_destination = y), by = c("End" = "UniqueID"))


unique_nodes <- unique_nodes %>%
  mutate(AffiliationColor = ifelse(IsMcGill == "Yes", "green", "purple")) 
unique_nodes$UniqueID <- as.character(unique_nodes$UniqueID)


new_graph_layout <- as.data.frame(layout_nicely(mcgill_graph))
names(new_graph_layout) <- c("x", "y")
new_graph_layout$UniqueID <- V(mcgill_graph)$name

new_graph_layout <- new_graph_layout %>%
  left_join(unique_nodes %>% select(UniqueID, AffiliationColor), by = "UniqueID")


new_edge_coordinates <- connections %>%
  left_join(new_graph_layout %>% select(UniqueID, x_origin = x, y_origin = y), by = c("Start" = "UniqueID")) %>%
  left_join(new_graph_layout %>% select(UniqueID, x_destination = x, y_destination = y), by = c("End" = "UniqueID"))


ggplot() +
  geom_segment(data = new_edge_coordinates, aes(x = x_origin, y = y_origin, xend = x_destination, yend = y_destination), color = "gold", arrow = arrow(type = "closed", length = unit(0.15, "inches")), lineend = 'round') +
  geom_point(data = new_graph_layout, aes(x = x, y = y, color = AffiliationColor), size = 6, shape = 21, fill = "white") +
  scale_color_manual(values = c("green" = "green", "purple" = "purple"), labels = c("Yes", "No")) + 
  theme_minimal() +
  theme(legend.position = "right") 
  labs(title = "Linkedin Network Visualization", color = "McGill Affiliation") 
