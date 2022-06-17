buildIndustryTrendsLeaseCloudNetworkPlot <- function() {
  
  full_network_links <- data_sheet_company_raw %>% 
    select('company_name', 'provider_1', 'provider_2', 'provider_3', 'provider_4', 'provider_5', 'provider_6', 'provider_7', 'provider_8', 'provider_9', 'provider_10') %>% 
    pivot_longer(!company_name, names_to = 'provider', values_to = "name_of_provider") %>% 
    select('company_name', 'name_of_provider') %>% 
    na_if("") %>%
    na.omit %>% 
    rename("from" = company_name, "to" = name_of_provider)
  
  leasers_and_leasees <- intersect(full_network_links$to, full_network_links$from)
  
  full_network_nodes <- data.frame(id = c(full_network_links$to, full_network_links$from)) %>% 
    distinct() %>% 
    mutate(leaser.type = ifelse(id %in% full_network_links$from, 1, 2)) %>% 
    mutate(leaser.type = ifelse(id %in% leasers_and_leasees, 3, leaser.type)) %>% 
    mutate(leaser.type.name = case_when(leaser.type == 1 ~ "Leasor",
                                        leaser.type == 2 ~ "Leasee",
                                        leaser.type == 3 ~ "Leasor/Leasee"))
  
  vis.nodes <- full_network_nodes
  vis.links <- full_network_links
  
  vis.nodes$shape  <- "dot"  
  vis.nodes$shadow <- TRUE # Nodes will drop shadow
  vis.nodes$title  <- vis.nodes$leaser.type.name # Text on click
  vis.nodes$label  <- vis.nodes$id # Node label
  #vis.nodes$size   <- vis.nodes$audience.size # Node size
  #vis.nodes$borderWidth <- 2 # Node border width
  
  vis.nodes$color.background <- c("slategrey", "tomato", "gold")[full_network_nodes$leaser.type]
  vis.nodes$color.border <- "black"
  vis.nodes$color.highlight.background <- "orange"
  vis.nodes$color.highlight.border <- "darkred"
  vis.links$arrows <- "middle"
  
  visNetwork(vis.nodes, vis.links)
  
}