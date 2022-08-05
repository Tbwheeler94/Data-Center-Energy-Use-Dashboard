buildIndustryTrendsLeaseCloudNetworkPlot <- function() {
  
  #create list of network links
  full_network_links <- data_sheet_company_raw %>% 
    select('company_name', 'provider_1', 'provider_2', 'provider_3', 'provider_4', 'provider_5', 'provider_6', 'provider_7', 'provider_8', 'provider_9', 'provider_10') %>% 
    pivot_longer(!company_name, names_to = 'provider', values_to = "name_of_provider") %>% 
    select('company_name', 'name_of_provider') %>% 
    na_if("") %>%
    na.omit %>% 
    rename("from" = company_name, "to" = name_of_provider)
  
  #calculate size of nodes, based on the number of relationships they have
  number_of_leasee_relationships <- full_network_links %>% count(to)
  number_of_leasor_relationships <- full_network_links %>% count(from) %>% rename(to = from)
  
  node_size <- rbind(number_of_leasee_relationships, number_of_leasor_relationships) %>% group_by(to) %>% summarize(n = sum(n))
    
  #find list of companies that are both leasors and leasees
  leasers_and_leasees <- intersect(full_network_links$to, full_network_links$from)
  
  #find full list of unique nodes in network
  full_network_nodes <- data.frame(id = c(full_network_links$to, full_network_links$from)) %>% 
    distinct() %>% 
    mutate(leaser.type = ifelse(id %in% full_network_links$from, 1, 2)) %>% 
    mutate(leaser.type = ifelse(id %in% leasers_and_leasees, 3, leaser.type)) %>% 
    mutate(leaser.type.name = case_when(leaser.type == 1 ~ "Lessor Only",
                                        leaser.type == 2 ~ "Lessee Only",
                                        leaser.type == 3 ~ "Lessor and Lessee")) %>% 
    left_join(node_size, by = c("id" = "to")) %>% 
    replace(is.na(.), 1) %>% 
    rename(size = n) %>% 
    mutate(size = size * 10)
  
  #formatting network graph
  
  vis.nodes <- full_network_nodes
  vis.links <- full_network_links
  
  vis.nodes$shape  <- "dot"  
  vis.nodes$shadow <- TRUE # Nodes will drop shadow
  vis.nodes$title  <- vis.nodes$leaser.type.name # Text on click
  vis.nodes$label  <- vis.nodes$id # Node label
  vis.nodes$size   <- vis.nodes$size # Node size
  vis.nodes$font.size <- c(35)
  vis.nodes$color.background <- c("slategrey", "tomato", "gold")[full_network_nodes$leaser.type]
  vis.nodes$color.border <- "black"
  vis.nodes$color.highlight.background <- "orange"
  vis.nodes$color.highlight.border <- "darkred"
  
  vis.links$arrows <- "from"
  
  #set up legend
  
  lnodes <- data.frame(label = c("Lessor Only", "Lessee Only", "Lessor and  \n Lessee"),
                       shape = c("circle"), color = c("slategrey", "tomato", "gold"))
  
  ledges <- data.frame(color = c("black", "black"),
                       label = c("leased to", "leased \n from"), arrows =c("to", "from"))
  
  #plot
  
  visNetwork(vis.nodes, vis.links) %>% 
    visInteraction(navigationButtons = TRUE) %>% 
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visPhysics(solver = "forceAtlas2Based",
               forceAtlas2Based = list(gravitationalConstant = -100)) %>%
    visGroups(groupname = "Lessor Only", color = "slategrey") %>%
    visGroups(groupname = "Lessee Only", color = "tomato") %>%
    visGroups(groupname = "Lessor and Lessee", color = "gold") %>%
    visLegend(addEdges = ledges, addNodes = lnodes, useGroups = FALSE, width = .1) 
  
}