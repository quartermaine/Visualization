#### Lab 6



###### Assignment 1

#Import-Preporcessing

library(ggraph)
library(igraph)
library(visNetwork)
library(seriation)

edges <- read.delim("trainData.dat", header = FALSE, sep  = " ")
nodes <- read.delim("trainMeta.dat", header = FALSE, sep = " ")


nodes$id <- rownames(nodes)
colnames(nodes) <- c("bombers", "b_group", "id")
colnames(edges) <- c("temp", "from", "to", "value")
edges$temp <- NULL
graph <- graph.data.frame(edges, directed = T)
degree_value <- degree(graph)
nodes$value <- degree_value[match(nodes$id, names(degree_value))]
nodes <- na.omit(nodes) # removing non connected nodes
nodes$label<-nodes$bombers
#### a.Basic
visNetwork(nodes=nodes,edges=edges,main = "Network of people invloved in Madrid Bombing")

#### b. Color groups
nodes$group<-nodes$b_group

visNetwork(nodes=nodes,edges=edges,main = "Network of people invloved in Madrid Bombing")

#### c. Strength
strength(graph)
visNetwork(nodes=nodes,edges=edges,main = "Network of people invloved in Madrid Bombing")

#### d. , e.  Repulsion & Highlight 


visNetwork(nodes = nodes, edges = edges, main = "Network of people invloved in Madrid Bombing") %>% 
  visGroups(groupname = "0", color = "blue") %>% 
  visGroups(groupname = "1", color = "orange") %>% 
  visEdges(arrows = "from") %>%
  visOptions(collapse = TRUE,
             selectedBy = "group",
             nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visPhysics(solver= "repulsion") %>% 
  visLegend() %>% addFontAwesome()


#### e. Hightlight nodes of length one

visNetwork(nodes = nodes, edges = edges) %>% 
  visGroups(groupname = "0", color = "blue") %>% 
  visGroups(groupname = "1", color = "orange") %>% 
  visEdges(arrows = "to") %>%
  visOptions(highlightNearest = list(enabled =TRUE,  algorithm = "hierarchical",
                                     degree = 1), 
             collapse = TRUE,
             selectedBy = "group",
             nodesIdSelection = TRUE) %>%
  visPhysics(solver= "repulsion") %>% 
  visLegend() %>% addFontAwesome()

#### 2

visNetwork(nodes = nodes, edges = edges) %>% 
  visGroups(groupname = "0", color = "blue") %>% 
  visGroups(groupname = "1", color = "orange") %>% 
  visEdges(arrows = "to") %>%
  visOptions(highlightNearest = list(enabled =TRUE,  algorithm = "hierarchical",
                                      degree = list(from = 1, to = 2)), 
             collapse = TRUE,
             selectedBy = "group",
             nodesIdSelection = TRUE) %>%
  visPhysics(solver= "repulsion") %>% 
  visLegend() %>% addFontAwesome()

#### 3 

graph_for_clusters <- graph.data.frame(edges, directed = FALSE)
clusters <- cluster_edge_betweenness(graph_for_clusters, directed = T)
nodes$clusters <- clusters$membership

visNetwork(nodes = nodes, edges = edges, main = "Network of people invloved in Madrid Bombing") %>% 
  visEdges(arrows = "to") %>%
  visOptions(highlightNearest = list(enabled =TRUE,  algorithm = "hierarchical",
                                     degree = list(from = 1, to = 2)), 
             collapse = TRUE,
             selectedBy = "group",
             nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visPhysics(solver= "repulsion") %>% 
  visLegend() %>% addFontAwesome() %>%visIgraphLayout()

#### 4

clusters <- cluster_edge_betweenness(graph_for_clusters)
nodes$clusters <- clusters$membership
netm <- get.adjacency(graph_for_clusters, sparse=F)
colnames(netm) <- nodes$label
rownames(netm) <- nodes$label
rowdist<-dist(netm)

order1<-seriate(rowdist, "HC")
ord1<-get_order(order1)
reordmatr<-netm[ord1,ord1]

plot_ly(z=~reordmatr, x=~colnames(reordmatr), 
        y=~rownames(reordmatr), type="heatmap") %>% layout(title = "Heatmap to find clusters among the bombing suspects")


###### Assignment 2 


oilcoal<-read.csv("Oilcoal.csv",header=T,sep=";",stringsAsFactors = F)
oilcoal<-oilcoal[,c(1:5)]
oilcoal$Coal<-as.numeric(gsub(",",".",oilcoal$Coal))
oilcoal$Oil<-as.numeric(gsub(",",".",oilcoal$Oil))
oilcoal$Marker.size<-as.numeric(gsub(",",".",oilcoal$Marker.size))
#Visualize data in Plotly as an animated bubble chart of Coal versus Oil 
#in which the bubble size corresponds to the country size. 
#List several noteworthy features of the investigated animation.

###1

base<-oilcoal%>%plot_ly(x=~Coal,y=~Oil,size=~Marker.size,text=~Country,hoverinfo="text")%>%
  add_markers(color=~Country,frame=~Year,ids=~Country)%>%
  animation_opts(8,redraw = F)%>%animation_slider(
    currentvalue = list(prefix = "YEAR ", font = list(color="black")))

###2
oilcoal %>% filter(Country %in% c("France", "Germany")) %>% plot_ly(x=~Coal, y=~Oil, frame =~Year, type = 'scatter', text = ~Country, mode = 'markers') %>% 
  animation_opts(100, easing = "cubic", redraw = F) %>% layout(title="Timeline of Consumption of Oil France and Germany")

###3
oilcoal$oilprop<-100*oilcoal$Oil/(oilcoal$Oil+oilcoal$Coal)

oilcoal$oilprop_0<-0

melt_oil<-gather(oilcoal, condition, measurement,oilprop,oilprop_0)


melt_oil %>%  plot_ly(x=~Country, y=~measurement, frame =~Year, type = 'scatter', mode='lines', text = ~Country,  color = ~Country, line = list(width = 20)) %>%
  animation_opts(300, easing = "cubic", redraw = F) %>% layout(title="Timeline of energy in terms of oil consumption")

####Another PLot that frame=Country
melt_oil %>%filter(condition=="oilprop")%>% 
  plot_ly(y = ~measurement, x = ~Year, color =~Country,mode="lines+markers",frame = ~Country)%>%
  hide_legend() %>%
  animation_opts(redraw = FALSE,easing = "cubic")


###4

melt_oil %>%  plot_ly(x=~Country, y=~measurement, frame =~Year, type = 'scatter', mode='lines', text = ~Country,  color = ~Country, line = list(width = 20)) %>%
  animation_opts(300, easing = "elastic", redraw = F) %>% layout(title="Timeline of energy in terms of oil consumption")

####Another PLot that frame=Country
melt_oil %>%filter(condition=="oilprop")%>% 
  plot_ly(y = ~measurement, x = ~Year, color =~Country,mode="lines+markers",frame = ~Country)%>%
  hide_legend() %>%
  animation_opts(redraw = FALSE,easing = "elastic")



###5
oilcoal_c <- oilcoal[, c("Country", "Year", "Coal")]
oilcoal_tour <- oilcoal_c %>%spread(Country, Coal)
oilcoal_scale <- rescale(oilcoal_tour[, 2:9])
#oilcoal_scale<-as.data.frame(lapply(oilcoal_tour[, 2:9],rescale))


rownames(oilcoal_scale) <- oilcoal_tour$Year
colnames(oilcoal_scale) <- names(oilcoal_tour)[-1]

set.seed(12345)
tour <- new_tour(oilcoal_scale, grand_tour(), NULL)
#tour<- new_tour(mat, guided_tour(cmass), NULL)

steps <- c(0, rep(1/15, 200))


Projs<-lapply(steps, function(step_size){  
  step <- tour(step_size)
  if(is.null(step)) {
    .GlobalEnv$tour<- new_tour(oilcoal_scale, guided_tour(cmass), NULL)
    step <- tour(step_size)
  }
  step
}
)

# projection of each observation
tour_dat <- function(i) {
  step <- Projs[[i]]
  proj <- center(oilcoal_scale %*% step$proj)
  data.frame(x = proj[,1], y = proj[,2], state = rownames(oilcoal_scale))
}
# projection of each variable's axis
proj_dat <- function(i) {
  step <- Projs[[i]]
  data.frame(
    x = step$proj[,1], y = step$proj[,2], variable = colnames(oilcoal_scale)
  )
}
stepz <- cumsum(steps)
# tidy version of tour data
tour_dats <- lapply(1:length(steps), tour_dat)
tour_datz <- Map(function(x, y) cbind(x, step = y), tour_dats, stepz)
tour_dat <- dplyr::bind_rows(tour_datz)

# tidy version of tour projection data
proj_dats <- lapply(1:length(steps), proj_dat)
proj_datz <- Map(function(x, y) cbind(x, step = y), proj_dats, stepz)
proj_dat <- dplyr::bind_rows(proj_datz)

ax <- list(
  title = "", showticklabels = FALSE,
  zeroline = FALSE, showgrid = FALSE,
  range = c(-1.1, 1.1)
)
# for nicely formatted slider labels
options(digits = 3)
tour_dat <- highlight_key(tour_dat, ~state, group = "A")
tour <- proj_dat %>%
  plot_ly(x = ~x, y = ~y, frame = ~step, color = I("black")) %>%
  add_segments(xend = 0, yend = 0, color = I("gray80")) %>%
  add_text(text = ~variable) %>%
  add_markers(data = tour_dat, text = ~state, ids = ~state, hoverinfo = "text") %>%
  layout(xaxis = ax, yaxis = ax, title = "Animated tour of the coal consumption by country")#%>%animation_opts(frame=0, transition=0, redraw = F)
tour






