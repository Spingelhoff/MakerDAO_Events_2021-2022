library(tidyverse)
library(tidygraph)
library(ggraph)
library(visNetwork)
library(RColorBrewer)

#clean data from extracted from google bigquery
data <- as_tibble(
  read.csv(
    file = 'dai_events_may2021to2022.csv',
    header = TRUE,
    colClasses = c("Date", "character", "character", "character",
                  "character", "numeric", "character"))) %>% 
  mutate(value =
    ifelse (percision == 'wad', value/10e18,
      ifelse (percision == 'ray', value/10e27,
        ifelse (percision == 'rad', value/10e45, NaN)))) %>% 
  select(-percision)

#check data if you'd like
data

#filter events to pull out only DAI transaction data
dai_transactions <- data %>% 
  filter(makerdao_smartcontract == 'DAI' & 
    makerdao_function == 'transfer')

#check data again
dai_transactions

#create node list (list of nodes with corresponding IDs)
node_list <- dai_transactions %>% 
  distinct(sender) %>% 
  full_join(distinct(dai_transactions, reciever), 
    by = c("sender" = "reciever")) %>% 
  rowid_to_column("id")

#check node list
node_list

#create edge list with count as weight and add value column
edge_list <- dai_transactions %>% 
  group_by(sender, reciever) %>% 
  summarize(weight = n(), amount = sum(value)) %>% 
  ungroup()
  
#check edge list
edge_list

#convert edge list addresses into id specified in node_list
edge_list_byid <- edge_list %>%
  left_join(node_list, by = "sender") %>%
  rename(from = id) %>%
  left_join(node_list, by = c("reciever" = "sender")) %>%
  rename(to = id) %>%
  select(from, to, weight, amount)

#check converted edge_list then remove original
edge_list_byid

#using the node_list and edge_list_byid we can create a
#graph object
graph <- tbl_graph(nodes = node_list, edges = edge_list_byid, 
          directed = TRUE)

#cluster nodes to find communities and centralities and rewrite graph object
graph <- graph %>%
  activate(nodes) %>%
  mutate(community = group_infomap(weights = weight)) %>%
  mutate(centrality = centrality_eigen(weights = weight, directed = TRUE))
  
graph

#filter graph by centrality to reduce size by trimming data to visualize, value amount filtered to remove low value actors
#note this also reduces community sizing as low value actors are excluded
filtered_graph <- graph %>%
  activate(edges) %>%
  filter(amount > quantile(amount, 0.50), weight > 100) %>%
  activate(nodes) %>%
  filter(centrality > quantile(centrality, 0.999)) %>%
  filter(!node_is_isolated())

filtered_graph

#network is too large to be visualized so it will be contracted on its communities, weight and value will be summed
contracted_graph <- filtered_graph %>%
  activate(nodes) %>%
  convert(to_contracted, community) %>%
  activate(nodes) %>%
  mutate(community_size = map_dbl(.orig_data, ~ nrow(.x))) %>%
  activate(edges) %>%
  mutate(contracted_weight = map_dbl(.orig_data, ~ sum(.x$weight)), 
         contracted_amount = map_dbl(.orig_data, ~ sum(.x$amount)))

contracted_graph

#graph network
network_graph <- ggraph(contracted_graph, layout = 'igraph', algorithm = 'kk') +
  geom_edge_link(aes(edge_alpha = contracted_weight, edge_colour = contracted_amount)) +
  scale_edge_colour_continuous(high = '#000000', low = '#56B1F7') +
  geom_node_point(aes(size = sqrt(community_size), colour = centrality_degree())) +
  scale_colour_continuous(high = '#000000', low = '#56B1F7')

network_graph

#subset graph on value transacted to visualize high value connections (and importantly, visualize readable labels)

subset_graph <- contracted_graph %>%
  activate(edges) %>%
  top_n(50, contracted_amount) %>%
  activate(nodes) %>%
  filter(!node_is_isolated())

subset_graph

subset_network_graph <- ggraph(subset_graph, layout = 'igraph', algorithm = 'kk') +
  geom_edge_link(aes(edge_alpha = contracted_weight, edge_colour = contracted_amount)) +
  scale_edge_colour_continuous(high = '#000000', low = '#56B1F7') +
  geom_node_point(aes(size = sqrt(community_size), colour = centrality_degree())) +
  scale_colour_continuous(high = '#000000', low = '#56B1F7') +
  geom_node_text(aes(label = community), position = position_nudge(x = 0.2, y = 0.2))

subset_network_graph

#graph community 56

community_56_graph <- graph %>%
  activate(nodes) %>%
  filter(community == 56) %>%
  activate(edges) %>%
  filter(amount > quantile(amount, 0.99)) %>%
  activate(nodes) %>%
  filter(!node_is_isolated())

community_56_graph

community_56_network_graph <- ggraph(community_56_graph, layout = 'igraph', algorithm = 'graphopt') +
  geom_edge_link(color = 'sky blue') +
  geom_node_point(aes(size = sqrt(centrality)))

community_56_network_graph

#prepare data for visualization of community 56 in order to display addresses (sender) in a readable way

#create color palette for groups
palette <- as_tibble(brewer.pal(6, 'Set3'))

palette

vis_edge_list <- community_56_graph %>%
  activate(edges) %>%
  arrange(desc(to)) %>%
  as_tibble() %>%
  mutate(arrows = 'to') %>%
  rename(title = amount, value = weight)

vis_edge_list

vis_node_list <- community_56_graph %>%
  activate(nodes) %>%
  mutate(grouping = group_infomap()) %>%
  as_tibble() %>%
  select(-id, -community) %>%
  rowid_to_column('id') %>%
  rename(title = sender, value = centrality) %>%
  mutate(label = "", color = case_when(grouping == 1 ~ '#FB8072',
                                       grouping == 2 ~ '#FFFFB3',
                                       grouping == 3 ~ '#BEBADA',
                                       grouping == 4 ~ '#8DD3C7',
                                       grouping == 5 ~ '#80B1D3',
                                       grouping == 6 ~ '#FDB462'))

vis_node_list

#graph dynamic network with prepared data
dynamic_network_graph <- visNetwork(vis_node_list, vis_edge_list, main = 
                                    "Transaction Network of Top 1% Most Influential Addresses Using DAI") %>%
  visOptions(highlightNearest = list(enabled = TRUE, degree = list(from = 0, to = 1))) %>%
  visInteraction(dragView = FALSE) %>%
  visLayout(randomSeed = 11)

dynamic_network_graph

visSave(dynamic_network_graph, "Most_Influential_Addresses_Using_Dai_Network_Graph.html")
