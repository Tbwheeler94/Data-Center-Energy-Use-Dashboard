buildIndustryTrendsLeaseCloudNetworkPlot <- function(full_network_links) {
  
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
    mutate(leaser.type.name = ifelse(size > 1, 
                                     paste0(leaser.type.name, "<br>", size, " Relationships", "<br>", "<button id='network_to_single_company_analysis' class='shiny-bound-input action-button'>", "Click Here", "</button>", " To Learn More About ", id), 
                                     paste0(leaser.type.name, "<br>", size, " Relationship", "<br>", "<button id='network_to_single_company_analysis' class='shiny-bound-input action-button'>", "Click Here", "</button>", " To Learn More About ", id))) %>% 
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
  vis.links$color <- c("lightskyblue", "darkkhaki")[full_network_links$category]
  vis.links$width <- 5
  
  #set up legend
  
  lnodes <- list(
    list(label = "Lessor Only", shape = "icon", 
         icon = list(code = "f111", size = 45, color = "slategrey")),
    list(label = "Lessee Only", shape = "icon", 
         icon = list(code = "f111", size = 45, color = "tomato")),
    list(label = "Lessor and \n Lessee", shape = "icon", margin = list(bottom = 25),
         icon = list(code = "f111", size = 45, color = "gold")),
    list(label = "Size Scaled To \n # of Relationships", shape = "icon", margin = list(bottom = 25),
         icon = list(code = "f140", size = 45)))
  
  ledges <- data.frame(color = c("darkkhaki", "lightskyblue"),
                       label = c("cloud", "colocation"), arrows =c("to", "to"))
  
  #plot
  
  visNetwork(vis.nodes, vis.links) %>% 
    visInteraction(navigationButtons = TRUE) %>% 
    visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
    visPhysics(solver = "forceAtlas2Based",
               forceAtlas2Based = list(gravitationalConstant = -100)) %>%
    visGroups(groupname = "Lessor Only", color = "slategrey") %>%
    visGroups(groupname = "Lessee Only", color = "tomato") %>%
    visGroups(groupname = "Lessor and Lessee", color = "gold") %>%
    visLegend(addEdges = ledges, addNodes = lnodes, useGroups = FALSE, width = .1) %>%
    addFontAwesome()
  
}