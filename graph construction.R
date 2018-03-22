library(igraph)
library(plyr)
library(poweRlaw)
library(grid)
library(lattice)
library(Rmisc)
library(ggplot2)
library(data.table)
library(MASS)
library(survival)
library(fitdistrplus)
library(logspline)
library(stringi)
library(readr)

#############################################################################################################
#import the selected consumer data (store data) here
store_data <- read_csv("~/Desktop/research/consumer data/R files/Cal stores/2014/consumer/5029028.csv", 
                     col_names = c('trip_code_uc','upc','upc_ver_uc','quantity','total_price_paid','coupon_value','deal_flag_uc'))

#create product graph of store
temp1 <- matrix(nrow = 0, ncol = 2)
#split datasets based on trip_code_uc
temp <- split(store_data, f = store_data$trip_code_uc)
#add edges to edgelist
for (trip in 1:length(temp)){
  temp2 <- temp[[trip]]$upc
  if (length(temp2) > 1) {
    temp1 <- rbind(temp1, t(combn(temp2, 2)))
  }
}

#convert to data frame
edgelist <- as.data.frame(temp1)
#create the graph
G <-  graph.data.frame(edgelist, directed = FALSE)
#assign weights of 1 to edges
E(G)$weight <- 1
#remove self edges and merge duplicate edges into weights
G <- simplify(G, remove.loops = TRUE, edge.attr.comb=list(weight="sum"))
#filter edge weights according to a criterion
G <- delete.edges(G, which(E(G)$weight < 3))
G <- delete_vertices(G, degree(G) == 0)
#convert graph into edgelist with weights
edgelist <- cbind(as_edgelist(G, names = TRUE), E(G)$weight, "Undirected")
edgelist[,1:2] <- as.character(as.numeric(edgelist[,1:2]))
#rename column names to Source and Target to use in Gephi
colnames(edgelist) <- c('Source','Target','Weight','Type')
#address for the table to be written at
address <- "~/Desktop/research/consumer data/R files/Cal stores/2014/graphs/edgelist.tsv"
write.table(edgelist, address, row.names = FALSE, sep = ",")
#creating node labels
node_list <- as.data.frame(vertex_attr(G))
colnames(node_list) <- c('upc')
node_list$upc <- as.character(node_list$upc)
node_list$upc <- as.numeric(node_list$upc)
#keep upc ver == 1
product_temp <- products[products$upc_ver_uc == 1,c(1,6,7)]
product_temp <- products[products$upc_ver_uc == 1,c(1,8,9)]
#product_temp$upc <- as.character(product_temp$upc)
node_list <- merge(node_list, product_temp, by.x = "upc", by.y = "upc", all.x = TRUE, all.y = FALSE)
node_list$upc <- as.factor(node_list$upc)
node_list$upc <- as.character(node_list$upc)
#address for the table to be written at
address <- "~/Desktop/research/consumer data/R files/Cal stores/2014/graphs/nodelist.tsv"
colnames(node_list) <- c('Id','Label','product_group_descr')
write.table(node_list, address, row.names = FALSE, sep = ",")
#remove the created objects to free memory
rm(temp1)
rm(temp2)
rm(temp)
#############################################################################################################
#############################################################################################################
#############################################################################################################
# in this analysis we calculate the effect of removing nodes on the graph connectivity by counting the 
# components after removing each node
# using the purchasing graph G for a store
weighted_degree <- strength(G, weights = E(G)$weights)
temp <- sort(weighted_degree, decreasing = TRUE)
degree <- degree(G)
temp <- sort(degree, decreasing = TRUE)
# we can use different measures to sort nodes for removal
between <- betweenness(G, directed = FALSE, weights = E(G)$weights)
temp <- sort(between, decreasing = TRUE)
close <- closeness(G,  weights = E(G)$weights)
temp <- sort(close, decreasing = TRUE)

component_count <- data.frame(matrix(ncol = 2, nrow = vcount(G)))
colnames(component_count) <- c('component','GCC')
tempG <- G
# compare it to a random graph
ER <- erdos.renyi.game(vcount(G), ecount(G), type=c("gnm"))
tempG <- ER
temp <- order(weighted_degree, decreasing = TRUE)

for (i in 1:length(temp)){
  tempG <- delete.vertices(tempG, names(temp[i]))
  #tempG <- delete.vertices(ER, temp[1:i])
  component_count$component[i] = components(tempG)$no
  component_count$GCC[i] = components(tempG)$csize[1]/vcount(tempG)
}
Component_Node_Removal <- ggplot(component_count, aes(y = GCC, x = seq(1,length(component_count$component)))) +
#Component_Node_Removal <- ggplot(component_count, aes(y = component, x = seq(1,length(component_count$component)))) +
  geom_point() +
  theme_bw() +
  #scale_y_continuous(limits = c(0,1), breaks = seq(0,1,length.out = 6)) +
  #scale_x_continuous(limits = c(0,1)) + 
#  ggtitle("Number of Components vs. Node Removal
#          Ordered by Closeness Centrality") +
  xlab("Number of Nodes Removed") +
  ylab("Number of Components")
Component_Node_Removal
address <- "~/Desktop/research/consumer data/plots/new plots/component GCC wdegree"
pdf(address, width=6.5, height=6)
print(Component_Node_Removal)
dev.off()


#############################################################################################################
#############################################################################################################
#############################################################################################################


#############################################################################################################
# plot section
# Degree to Weighted Degree Ratio
degree_full <- mapply(c, weighted_degree, degree, SIMPLIFY=FALSE)
temp <- data.frame(matrix(ncol = 1, nrow = length(degree_full)))
colnames(temp) <- "DWDR"   # Degree to Weighted Degree Ratio
for (i in 1:length(degree_full)){
  temp$DWDR[i] = degree_full[[i]][2] / degree_full[[i]][1]
}
DWDR <- ggplot(temp, aes(x = DWDR)) +
  stat_ecdf() +
  theme_bw() +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,length.out = 6)) +
  scale_x_continuous(limits = c(0,1)) + 
  ggtitle("Degree to Weighted Degree Ratio Distribution") +
  xlab("Degree to Weighted Degree Ratio") +
  ylab("CDF")
DWDR

# edge weight distribution 
weight_fig <- ggplot(as.data.frame(E(G)$weight), aes(x = E(G)$weight)) +
  stat_ecdf() +
  theme_bw() +
  scale_y_continuous(limits = c(0,1), breaks = seq(0,1,length.out = 6)) +
  scale_x_log10() + 
  ggtitle("Edge Weight Distribution") +
  xlab("Weight") +
  ylab("CDF")
weight_fig

# degree distribution
degree_fig <- ggplot(as.data.frame(degree(G)), aes(x = degree(G))) +
  stat_ecdf()
degree_fig_data <- ggplot_build(degree_fig)$data[[1]]
#degree_fig <- ggplot(degree_fig_data, aes(x = x, y = 1 - y)) +
degree_fig <- ggplot(as.data.frame(degree(G)), aes(x = degree(G))) +
  stat_ecdf() +
  #geom_step() +
  theme_bw() +
  scale_y_log10() +
  scale_x_log10() + 
  ggtitle("Degree Distribution") + 
  xlab("Degree") +
  ylab("CDF")
degree_fig

# degree with weight distribution (CCDF)
weighted_degree_fig <- ggplot(as.data.frame(strength(G, weights = E(G)$weights)), aes(x = strength(G))) +
  stat_ecdf()
weighted_degree_fig_data <- ggplot_build(weighted_degree_fig)$data[[1]]
weighted_degree_fig <- ggplot(weighted_degree_fig_data, aes(x = x, y = 1 - y)) +
  geom_step() + 
  theme_bw() +
  scale_y_log10() +
  scale_x_log10() + 
  ggtitle("Weighted Degree Distribution") + 
  xlab("Weighted Degree") +
  ylab("CDF")
weighted_degree_fig

#############################################################################################################
# generating same degree distribution random graph
deg <- degree(G)
random_G <- sample_degseq(deg, method = c('simple.no.multiple'))
random_G <- sample_degseq(deg, method = c('vl'))
# comparing clustering coefficient of random graph with same degree distribition vs. experimental groph
transitivity(G)
transitivity(random_G)
#############################################################################################################
# fitting power law to degree distribution
deg <- degree(G)
deg_dist <- data.frame(k = 0:max(deg), P_k = degree_distribution(G))
# plotting the distribution
degree_fig <- ggplot(deg_dist, aes(x = k, y = P_k)) +
  geom_point() +
  theme_bw() +
  scale_y_log10() +
  scale_x_log10() + 
  ggtitle("Degree Distribution") + 
  xlab("Degree = k") +
  ylab("P_k")
degree_fig
# testing kolmogrov test for power law
#estimate x_min
est <- estimate_xmin(data.frame(deg))
data_pl$xmin <- est$xmin
data_pl$pars <- est$pars



#############################################################################################################
#selecting top products
top_10p <- tail(sort(degree(G)), vcount(G)* 0.2)
top_upc <- names(which(degree(G) > min(top_10p)))
length(top_upc)
#import movemnet file for same store
store_sales <- X2323912
colnames(store_sales) <- c('store_code_uc','upc','weeek_end','units','prmult','price','feature','display')
#sum the sales for each products throughout the year
store_sales <- data.table(store_sales)
store_sales_agg <- store_sales[, units_total:= sum(units), by = upc]
store_sales_agg <- store_sales_agg[!(duplicated(store_sales_agg[,2])),]
store_sales_agg_sorted <- store_sales_agg[order(-store_sales_agg$units_total),c(2,9)]
top_upc_sales <- store_sales_agg_sorted$upc[1:length(top_upc)]
#compare tops sellers of scanner data vs consumer data
top_products <- intersect(top_upc, top_upc_sales)
#creating sales from scanner attribute for nodes 
store_sales_agg_sorted$upc <- as.numeric(store_sales_agg_sorted$upc)
node_list <- merge(node_list, store_sales_agg_sorted, by.x = "upc", by.y = "upc", all.x = TRUE, all.y = FALSE)
node_list$upc <- as.factor(node_list$upc)
node_list$upc <- as.character(node_list$upc)
#address for the table to be written at
address <- "~/Desktop/research/consumer data/R files/Cal stores/2014/graphs/nodelist.tsv"
colnames(node_list) <- c('Id','Label','product_group_descr')
write.table(node_list, address, row.names = FALSE, sep = ",")

#############################################################################################################
# upc_matrix is the count of each upc purchased by every household in store S through year Y
# Here we calculate the entropy of products through trips for each household in order to find the major products
# for that household
# N = Number of households

#upc_matrix <- final_matrix
#class(upc_matrix)<- 'numeric'
N = 270
entropy <- data.frame(household_code = numeric(N))
entropy$household_code <- upc_matrix[,1]
#remove household code column
upc_matrix <- upc_matrix[,-1]
# count is the count of different UPCs household H has purchased in store S through year Y
entropy$count <- rowSums(upc_matrix != 0)
entropy$sum <- rowSums(upc_matrix)
upc_portion <- upc_matrix/entropy$sum
log_portion <- log(upc_portion)
log_portion[log_portion[,]==-Inf] <- 0
entropy$entropy <- rowSums(-upc_portion*log_portion)/log(entropy$count)
entropy$entropy[is.na(entropy$entropy)] = 0
entropy$threshold <- entropy$count

upc_entropy_plot <- ggplot(upc_matrix, aes(x = X1)) + stat_ecdf()
upc_entropy_plot
upc_entropy_plot <- ggplot(entropy, aes(x = count)) + stat_ecdf() +
  theme_bw() +
  #theme(axis.text.x = element_text(size = 7.5, angle = 45,  vjust=1, hjust=1)) +
  #theme(axis.title.y = element_text(size = 10)) +
  scale_y_continuous(limits = c(0,1)) +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) +
  scale_x_continuous(limits = c(0, 1000)) +
  xlab("UPC Entropy") +
  ylab("CDF") +
  ggtitle("UPC Entropy Distribution Over All Trips in 2014")
upc_entropy_plot
#address <- '~/Desktop/research/consumer data/R files/Cal stores/2014/plots/entropy.png'
#savePlot(address)
#measuring the centrality of nodes in product graph
graph <- G
store_betweenness <- betweenness(graph, v = V(graph), directed = FALSE, weights = E(graph)$weight)
top_between <- products[products$upc %in% as.numeric(names(tail(sort(store_betweenness),10))),]
store_degree <- strength(graph, v = V(graph), weights = E(graph)$weight)
top_degree <- products[products$upc %in% as.numeric(names(tail(sort(store_degree),10))),]
store_closeness <- closeness(graph, v = V(graph), weights = E(graph)$weight)
top_closeness <-  products[products$upc %in% as.numeric(names(tail(sort(store_closeness),10))),]
store_page_rank <- page_rank(graph, v = V(graph), directed = FALSE, weights = E(graph)$weight)
top_page_rank <- products[products$upc %in% as.numeric(names(tail(sort(store_page_rank$vector),10))),]

# community of nodes in graph G
community <- cluster_louvain(G, weights = E(G)$weight)
community <- cluster_walktrap(G, weights = E(G)$weight)
community <- membership(community)
address <- "~/Desktop/research/consumer data/R files/Cal stores/2014/graphs/Community_walk.tsv"
temp <- matrix(community)
length(unique(community))
temp2 <- matrix(as.numeric(attributes(community)$names))
Community <- cbind(temp2, temp)
colnames(Community) <- c('node','community')
write.table(Community, address, row.names = FALSE, sep = ",")

# cosine similarity of household top category purchase behaviour
cos.sim <- function(ix) 
{
  A = X[ix[1],]
  B = X[ix[2],]
  return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
}   
X <- customer
n <- 100
cmb <- expand.grid(i=1:n, j=1:n) 
C <- matrix(apply(cmb,1,cos.sim),n,n)

#############################################################################################################
# create a purchasing data for each household in a specific store
#merge household into store data
temp <- trips_2014[,1:2]
store_data <- merge(store_data, temp, by.x = "trip_code_uc", by.y = "trip_code_uc", all.x = TRUE, all.y = FALSE)
#create the graph for each household
household_code_list <- unique(store_data$household_code)
store_data_copy <- store_data
counter <- 0
par(mfrow = c(10,27))
household_product_removal_effect <- data.frame(matrix(ncol = 7, nrow = 0))
colnames(household_product_removal_effect) <- c("household_code", "upc", "component_count", "degree", "closeness", "betweenness", "weighted_degree")

for (i in household_code_list){
  counter <- counter + 1
  print(counter)
  store_data <- store_data_copy
  store_data <- store_data[store_data$household_code == i,]
  
  #skip if there is only one product for a household
  if (length(store_data[,1]) == 1) next
  #create graphs from this data and store the edgelist + nodelist
  #create product graph of store
  temp1 <- matrix(nrow = 0, ncol = 2)
  #split datasets based on trip_code_uc
  temp <- split(store_data, f = store_data$trip_code_uc)
  #add edges to edgelist
  for (trip in 1:length(temp)){
    temp2 <- temp[[trip]]$upc
    if (length(temp2) > 1) {
      temp1 <- rbind(temp1, t(combn(temp2, 2)))
    }
  }
  
  #convert to data frame
  edgelist <- as.data.frame(temp1)
  #create the graph
  G <-  graph.data.frame(edgelist, directed = FALSE)
  #assign weights of 1 to edges
  E(G)$weight <- 1
  #remove self edges and merge duplicate edges into weights
  G <- simplify(G, remove.loops = TRUE, edge.attr.comb=list(weight="sum"))
  #filter edge weights according to a criterion
  #G <- delete.edges(G, which(E(G)$weight < 3))
  #G <- delete_vertices(G, degree(G) == 0)
  #convert graph into edgelist with weights
  edgelist <- cbind(as_edgelist(G, names = TRUE), E(G)$weight, "Undirected")
  edgelist[,1:2] <- as.character(as.numeric(edgelist[,1:2]))
  #skip if there is no edge in the graph
  if (length(edgelist[,1]) == 0) next
  #rename column names to Source and Target to use in Gephi
  colnames(edgelist) <- c('Source','Target','Weight','Type')
  #address for the table to be written at
  address <- paste("~/Desktop/research/consumer data/R files/Cal stores/2014/graphs/store2116298/edgelist/edgelist",i,".tsv",sep = "")
  write.table(edgelist, address, row.names = FALSE, sep = ",")
  #creating node labels
  node_list <- as.data.frame(vertex_attr(G))
  colnames(node_list) <- c('upc')
  node_list$upc <- as.character(node_list$upc)
  ###
  #this block of code counts the number of compunents after removing each node 
  G_copy <- G
  product_list <- unique(node_list$upc)
  #c2<-0
  for (j in product_list){
   # c2<-c2+1
    #print(c2)
    G <- G_copy
    G <- delete.vertices(G, v = j)
    household_product_removal_effect <- rbind(household_product_removal_effect, data.frame(household_code = i, 
                                              upc = as.character(j), component_count = count_components(G), 
                                              degree = degree(G_copy, v = j), 
                                              closeness = closeness(G_copy, vids = j, weights = E(G_copy)$weights),
                                              betweenness = betweenness(G_copy, v = j, weights = E(G_copy)$weights, directed = FALSE),
                                              weighted_degree = strength(G_copy, vids = j, weights = E(G_copy)$weights)))
                                              #pagerank = page_rank(G_copy, vids = j, weights = E(G_copy)$weights, directed = FALSE)))
  }
  ###
  
  node_list$upc <- as.numeric(node_list$upc)
  #keep upc ver == 1
  product_temp <- products[products$upc_ver_uc == 1,c(1,6,7)]
  product_temp <- products[products$upc_ver_uc == 1,c(1,8,9)]
  #product_temp$upc <- as.character(product_temp$upc)
  node_list <- merge(node_list, product_temp, by.x = "upc", by.y = "upc", all.x = TRUE, all.y = FALSE)
  node_list$upc <- as.factor(node_list$upc)
  node_list$upc <- as.character(node_list$upc)
  #address for the table to be written at
  address <- paste("~/Desktop/research/consumer data/R files/Cal stores/2014/graphs/store2116298/nodelist/nodelist",i,".tsv",sep = "")
  colnames(node_list) <- c('Id','Label','product_group_descr')
  write.table(node_list, address, row.names = FALSE, sep = ",")
  #plot graph
  #plot(G)
}

#analysis of change in component number vs. various centralities
address <- "~/Desktop/research/consumer data/R files/Cal stores/2014/graphs/store2116298/household_product_removal_effect.tsv"
write.table(household_product_removal_effect, address, row.names = FALSE, sep = ",")
household_product_removal_effect_t <- as.data.table(household_product_removal_effect)
household_product_removal_effect_t <- household_product_removal_effect_t[, comp := min(component_count), by = household_code]
household_product_removal_effect_t$change_in_comp_count <- household_product_removal_effect_t$component_count - household_product_removal_effect_t$comp
cor(household_product_removal_effect_t[,3:9])

household_product_removal_effect_t <- as.data.frame(household_product_removal_effect_t)

#plot the change in number of components cdf by removal of products
change_in_comp_count_plot <- ggplot(household_product_removal_effect_t, aes(x = change_in_comp_count)) + stat_ecdf() +
  theme_bw() +
  scale_y_continuous(limits = c(0,1)) +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) +
  scale_x_continuous(limits = c(0, 10)) +
  xlab("Change in Number of Components") +
  ylab("CDF") +
  ggtitle("Change in Number of Components Over All Trips in one store")
change_in_comp_count_plot


