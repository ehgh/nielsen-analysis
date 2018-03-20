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

#############################################################################################################
#importing the files
for (i in 2014:2014){
  address <- paste("~/Desktop/research/consumer data/KiltsPanelData/nielsen_extracts 5/HMS/", i, "/Annual_Files/panelists_" , i, ".tsv", sep = "")
  assign(paste("panelists_",i, sep = ""), read.table(address, header = TRUE, sep = "\t"))
}
for (i in 2014:2014){
  address <- paste("~/Desktop/research/consumer data/KiltsPanelData/nielsen_extracts 5/HMS/", i, "/Annual_Files/trips_" , i, ".tsv", sep = "")
  assign(paste("trips_",i, sep = ""), read.table(address, header = TRUE, sep = "\t"))
}
for (i in 2013:2013){
  address <- paste("~/Desktop/research/consumer data/KiltsPanelData/nielsen_extracts 5/HMS/", i, "/Annual_Files/purchases_" , i, ".tsv", sep = "")
  assign(paste("purchases_",i, sep = ""), read.table(address, header = TRUE, sep = "\t"))
}

#products1 <- read.table("~/Desktop/research/consumer data/KiltsPanelData/nielsen_extracts 4/HMS/Master_Files/Latest/products.tsv" , sep = "\t", quote = "", as.is = !StringsAsFactor, skipNul = TRUE)
#products1 <- read.table("~/Desktop/research/consumer data/nielsen_extracts/RMS/Master_Files/Latest/products.tsv" , header = TRUE, sep = "\t", quote = "", as.is = !StringsAsFactor, skipNul = TRUE)
#############################################################################################################
#combining trips and purchases
for (i in 2014:2014){
  #merging lists (Note: some trips may not include any purchases in the purchases list and here we exclude them to create products graph)
  assign(paste("trips_purchases_",i, sep = ""), merge(eval(parse(text = paste("trips_", i, sep = ""))), eval(parse(text = paste("purchases_", i, sep = ""))), by.x = "trip_code_uc", by.y = "trip_code_uc", all.x = FALSE, all.y = TRUE))
  #combining upc+upc_ver_uc into upc_unique
  assign(paste("trips_purchases_",i, sep = ""), transform(eval(parse(text = paste("trips_purchases_", i, sep = ""))), upc_unique = paste(upc, upc_ver_uc, sep = "")))
  assign(paste("trips_purchases_",i, sep = ""), transform(eval(parse(text = paste("trips_purchases_", i, sep = ""))), upc_unique = as.character(upc_unique)))
  assign(paste("trips_purchases_",i, sep = ""), transform(eval(parse(text = paste("trips_purchases_", i, sep = ""))), upc_unique = as.numeric(upc_unique)))
  #assign(paste("trips_purchases_data_",i, sep = ""), eval(parse(text = paste("trips_purchases_",i, "[,c(1:9,12:16)]", sep = ""))))
  #assign(paste("trips_purchases_data_",i, sep = ""), eval(parse(text = paste("trips_purchases_",i, "[,c(1:2,5,7,16)]", sep = ""))))
}
#############################################################################################################
#calculating number of edges in products graph
temp <- trips_purchases_2014$trip_code_uc
#temp <- trips_purchases_zip_956_2014$trip_code_uc
temp = table(temp)
temp = data.frame(temp)
temp<- transform(temp, temp = as.numeric(temp))
temp<- list(unlist(temp$Freq)*(unlist(temp$Freq)-1)/2)
product_graph_edge_count <- sum(temp[[1]])
average_productPerTrip <- mean(temp$Freq)
#############################################################################################################
#scanner data 
#import stores data
stores_2014 <- read.table("scanner data/stores_2014.tsv", header = TRUE, sep = "\t")
stores_2013 <- read.table("scanner data/stores_2013.tsv", header = TRUE, sep = "\t")
#list of stores in California
cal_stores <- unique(stores_2014$store_code_uc[which(stores_2014$store_zip3 > 899 & stores_2014$store_zip3 < 962)])
cal_stores_2013 <- unique(stores_2013$store_code_uc[which(stores_2013$store_zip3 > 899 & stores_2013$store_zip3 < 962)])
#write.table(cal_stores, "scanner data/cal_stores.tsv", col.names = FALSE, row.names = FALSE)
write.table(cal_stores_2013, "scanner data/cal_stores_2013.tsv", col.names = FALSE, row.names = FALSE)
#############################################################################################################
#creating products graph
#remove NA store_zip3 rows
listname <- paste("trips_purchases_zip_", i, sep = "")
assign(listname, eval(parse(text = paste("trips_purchases_",i,"[!is.na(trips_purchases_",i,"$store_zip3),]", sep = ""))))
#filter a specific zipcode
for (zip in 956){
  listname <- paste("trips_purchases_zip_",zip,"_", i, sep = "")
  assign(listname, eval(parse(text = paste("trips_purchases_zip_",i,"[trips_purchases_zip_",i,"$store_zip3 == ", zip, ",]", sep = ""))))
}
##filter stores in california
#trips_purchases_2014_cal <- trips_purchases_zip_2014[trips_purchases_zip_2014$store_zip3 > 899 & trips_purchases_zip_2014$store_zip3 < 962, ]
#filter stores in california with existing sales info
trips_purchases_2014_cal <- trips_purchases_zip_2014[trips_purchases_zip_2014$store_code_uc %in% cal_stores , ]
#removing stores with less than N products in their purchasing graph
temp <- trips_purchases_2014_cal
temp <- temp[!duplicated(temp[,c("store_code_uc","upc_unique")]),]
temp <- as.data.table(temp)
temp <- temp[, count:= .N, by = store_code_uc]
list <- unique(temp$store_code_uc[temp$count > 1000])
trips_purchases_2014_cal_2 <- trips_purchases_2014_cal[trips_purchases_2014_cal$store_code_uc %in% list, ]
#counting the number of common products between stores in purchasing behavior
#listing the products of each store
store_product_list_cal = list()
for (i in 1:length(list)){
  temp_2 <- unique(trips_purchases_2014_cal_2$upc_unique[trips_purchases_2014_cal_2$store_code_uc == list[i]])
  store_product_list_cal <- c(store_product_list_cal, list(temp_2))
  print(i)
}
#filtering stores with common products more than 500
temp <- matrix(nrow = 0, ncol = 7)
for (i in 1:(length(list)-1)){
  for (j in (i+1):length(list)){
    if(i != j){
      k <- length(intersect(store_product_list_cal[[i]],store_product_list_cal[[j]]))
      if (k > 500){
        temp <- rbind(temp, c(i,j,k, list[i], list[j], length(store_product_list_cal[[i]]), length(store_product_list_cal[[j]])))
      }
    }
  }
  print(i)
}
common_stores <- temp
common_stores <- cbind(common_stores, common_stores[,3]/common_stores[,6],common_stores[,3]/common_stores[,7])
#now select desired stores and go to next line for graph or import corresponding store sales file for sales data(code around line 700+)
#############################################################################################################
#separating list based on store_code_uc
listname <- paste("trips_purchases_zip_",zip,"_", i,"$store_code_uc", sep = "")
#zip 956 stores
store_code_list <- unique(eval(parse(text = listname)))
store_code_list <- unique(trips_purchases_zip_2014$store_code_uc[trips_purchases_zip_2014$store_zip3 == 956])
#some of cal stores with high common products
store_code_list <- unique(c(common_stores[common_stores[,3] > 1020 ,5], common_stores[common_stores[,3] > 1020, 4]))
i <- 2014
for (store_code in store_code_list){
  if (which(store_code_list == store_code)%%10 == 0){
    print(store_code)
  }
  #filtering the store_code data
  #for zip 956 next 2 lines
  #listname <- paste("trips_purchases_zip_",zip,"_store_",store_code,"_", i, sep = "")
  #assign(listname, eval(parse(text = paste("trips_purchases_zip_",i,"[trips_purchases_zip_",i,"$store_code_uc == ", store_code, ",]", sep = ""))))
  #for selected cal store zips
  listname <- paste("trips_purchases_zip_store_",store_code,"_", i, sep = "")
  assign(listname, eval(parse(text = paste("trips_purchases_",i,"[trips_purchases_",i,"$store_code_uc == ", store_code, ",]", sep = ""))))
  #check if there is only one purchase with one product to continue to next store
  #if (length(eval(parse(text = listname)) == 1))
  #  next
  #create product graph edgelist
  #this line is not required!
  #trip_code_list <- unique(eval(parse(text = paste(listname, "$trip_code_uc", sep = ""))))
  #parse by trip_code and then add edges from the parsed list  
  temp1 <- matrix(nrow = 0, ncol = 2)
  #split datasets based on trip_code_uc
  temp <- split(eval(parse(text = listname)), f = eval(parse(text = paste(listname, "$trip_code_uc", sep = ""))))
  #add edges to edgelist
  for (trip in 1:length(temp)){
      temp2 <- temp[[trip]]$upc_unique
      if (length(temp2) > 1) {
        temp1 <- rbind(temp1, t(combn(temp2, 2)))
      }
  }
  
  print(store_code)
  
  listname <- paste(listname, "_edgelist", sep = "" )
  #convert to data frame
  assign(listname, as.data.frame(temp1))
  #counting the number of duplicate rows as weight
  temp <- ddply(eval(parse(text = listname)),.(V1,V2),nrow)
  #create the graph
  assign(paste(listname, "_graph", sep = "" ), graph.data.frame(eval(parse(text = listname)), directed = FALSE))
  #assign weights of 1 to edges
  eval(parse(text = paste("E(", listname, "_graph)$weight <- 1", sep = "" )))
  #remove self edges and merge duplicate edges into weights
  assign(paste(listname, "_graph", sep = "" ), simplify(eval(parse(text = paste(listname, "_graph", sep = "" ))), remove.loops = TRUE, edge.attr.comb=list(weight="sum")))
  #convert graph into edgelist with weights
  assign(listname, cbind(as_edgelist(eval(parse(text = paste(listname, "_graph", sep = "" ))), names = TRUE), E(eval(parse(text = paste(listname, "_graph", sep = "" ))))$weight, "Undirected"))
  #rename column names to Source and Target to use in Gephi
  eval(parse(text = paste("colnames(", listname, ") <- c('Source','Target','Weight','Type') ", sep = "" )))
  #address for the table to be written at
  address <- paste("~/Desktop/research/consumer data/R files/zip 956 store resulotion graphs/", listname, ".tsv", sep = "")
  write.table(eval(parse(text = listname)), address, row.names = FALSE, sep = ",")
  #remove the created objects to free memory
  rm(temp1)
  rm(temp2)
  rm(temp)
  listname <- paste("trips_purchases_zip_",zip,"_store_",store_code,"_", i, "_edgelist", sep = "")
  rm(list = listname)
  listname <- paste("trips_purchases_zip_",zip,"_store_",store_code,"_", i, sep = "")
  rm(list = listname)
  listname <- paste("trips_purchases_zip_",zip,"_store_",store_code,"_", i, "_edgelist_graph", sep = "" )
  rm(list = listname)
}

#############################################################################################################
#temporary code for importing some stored graphs
trips_purchases_zip_store_2323912_2014_edgelist <- read.table("zip 956 store resulotion graphs/trips_purchases_zip_store_2323912_2014_edgelist.tsv", header = TRUE, sep = ",")
trips_purchases_zip_store_4187623_2014_edgelist <- read.table("zip 956 store resulotion graphs/trips_purchases_zip_store_4187623_2014_edgelist.tsv", header = TRUE, sep = ",")
trips_purchases_zip_store_2323912_2014_graph <- graph.data.frame(trips_purchases_zip_store_2323912_2014_edgelist, directed = FALSE)
E(trips_purchases_zip_store_2323912_2014_graph)$weight <- trips_purchases_zip_store_2323912_2014_edgelist$Weight
trips_purchases_zip_store_4187623_2014_graph <- graph.data.frame(trips_purchases_zip_store_4187623_2014_edgelist, directed = FALSE)
E(trips_purchases_zip_store_4187623_2014_graph)$weight <- trips_purchases_zip_store_4187623_2014_edgelist$Weight

#############################################################################################################
#combining products and purchases
for (i in 2014:2014){
  #merging lists (Note: some trips may not include any purchases in the purchases list and here we exclude them to create products graph)
  assign(paste("products_purchases_",i, sep = ""), merge(products, eval(parse(text = paste("purchases_", i, sep = ""))), by.x = c("upc","upc_ver_uc"), by.y = c("upc","upc_ver_uc"), all.x = FALSE, all.y = TRUE))
  #counting the purchases in each product category/department/module and sort them
  assign(paste("departments_",i, sep = ""), data.frame(sort(table(eval(parse(text = paste("products_purchases_",i, "$department_descr", sep = "")))), decreasing = TRUE)))
  assign(paste("groups_",i, sep = ""), data.frame(sort(table(eval(parse(text = paste("products_purchases_",i, "$product_group_descr", sep = "")))), decreasing = TRUE)))
  assign(paste("modules_",i, sep = ""), data.frame(sort(table(eval(parse(text = paste("products_purchases_",i, "$product_module_descr", sep = "")))), decreasing = TRUE)))
  assign(paste("groups_",i, sep = ""), table(eval(parse(text = paste("products_purchases_",i, "$product_group_descr", sep = "")))))
  assign(paste("modules_",i, sep = ""), table(eval(parse(text = paste("products_purchases_",i, "$product_module_descr", sep = "")))))
  #plot ecdf of purchases per product department/module/group
  department_plot =  
    ggplot(eval(parse(text = paste("departments_",i, sep = ""))), aes(y = cumsum(Freq/sum(Freq)), x = seq(1:length(Var1)))) +
    geom_step() +
    geom_segment(aes(x = 0.2*10, y = 0.1, xend = 0.2*10, yend = 0.9), color = "red") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 10, angle = 45,  vjust=1, hjust=1),axis.text.y = element_text(size = 13)) +
    theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,length.out = 6)) +
    scale_x_continuous(breaks = seq(1:length(eval(parse(text = paste("departments_",i,"$Var1", sep = ""))))),  labels = eval(parse(text = paste("departments_",i,"$Var1", sep = "")))) +
    #ggtitle(paste("Cumulative distribution of purchases per product departments\n ordered by contribution in year ", i, sep = ""))
    xlab("Product departments ordered by highest contribution first") +
    ylab("Cumulative distribution of purchases\n per product departments")
  address <- paste("~/Desktop/research/consumer data/plots/CDF_department_", i, ".pdf", sep = "")
  pdf(address, width=6.5, height=6)
  print(department_plot)
  dev.off()
  
  L <- length(eval(parse(text = paste("modules_",i,"$Var1", sep = ""))))
  #Number of the points to be shown on x axis
  N <- 30
  modules_plot =  
    ggplot(eval(parse(text = paste("modules_",i, sep = ""))), aes(y = cumsum(Freq/sum(Freq)), x = seq(1:length(Var1)))) +
    geom_step() +
    geom_segment(aes(x = 0.2*L, y = 0.1, xend = 0.2*L, yend = 0.9), color = "red") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 10, angle = 45,  vjust=1, hjust=1),axis.text.y = element_text(size = 13)) +
    theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,length.out = 6)) +
    #labeling the x-axis by sampled modules
    #scale_x_continuous(breaks = eval(parse(text = "seq(from = 1, to = L , length.out = N)")),  labels = eval(parse(text = paste("modules_",i,"$Var1[seq(from = 1, to = L, length.out = N)]", sep = "")))) +
    #labeling the x-axis by normalized interval [0,1]
    scale_x_continuous(breaks = eval(parse(text = "seq(from = 1, to = L , length.out = N)")),  labels = round(seq(0,L,length.out = N), digits = 0)) +
    #ggtitle(paste("Cumulative distribution of purchases per product modules\n ordered by contribution in year ", i, sep = ""))
    xlab("Product modules ordered by highest contribution first") +
    ylab("Cumulative distribution of purchases per product modules")
  address <- paste("~/Desktop/research/consumer data/plots/CDF_modules_", i, ".pdf", sep = "")
  pdf(address, width=6, height=6)
  print(modules_plot)
  dev.off()
  
  L <- length(eval(parse(text = paste("groups_",i,"$Var1", sep = ""))))
  #Number of the points to be shown on x axis
  N <- 30
  groups_plot =  
    ggplot(eval(parse(text = paste("groups_",i, sep = ""))), aes(y = cumsum(Freq/sum(Freq)), x = seq(1:length(Var1)))) +
    geom_step() +
    geom_segment(aes(x = 0.2*L, y = 0.1, xend = 0.2*L, yend = 0.9), color = "red") +
    theme_bw() +
    theme(axis.text.x = element_text(size = 10, angle = 45,  vjust=1, hjust=1),axis.text.y = element_text(size = 13)) +
    theme(axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15)) +
    scale_y_continuous(limits = c(0,1), breaks = seq(0,1,length.out = 6)) +
    #labeling the x-axis by sampled groups
    #scale_x_continuous(breaks = eval(parse(text = "seq(from = 1, to = L , length.out = N)")),  labels = eval(parse(text = paste("groups_",i,"$Var1[seq(from = 1, to = L, length.out = N)]", sep = "")))) +
    #labeling the x-axis by normalized interval [0,1]
    scale_x_continuous(breaks = eval(parse(text = "seq(from = 1, to = L , length.out = N)")),  labels = round(seq(0,L,length.out = N), digits = 0)) +
    #ggtitle(paste("Cumulative distribution of purchases per product groups\n ordered by contribution in year ", i, sep = ""))
    xlab("Product groups ordered by highest contribution first") +
    ylab("Cumulative distribution of purchases per product groups")
  address <- paste("~/Desktop/research/consumer data/plots/CDF_groups_", i, ".pdf", sep = "")
  pdf(address, width=6, height=6)
  print(groups_plot)
  dev.off()
}
#############################################################################################################
#############################################################################################################
#finding difference in product graph for different stores
#importing the graphs
#importing the files
store_list_graph_stat <- as.data.frame(store_code_list[1:215])
for (i in store_code_list[1:215]){
#for (i in c(2073128)){
  address <- paste("~/Desktop/research/consumer data/R files/zip 956 store resulotion graphs/trips_purchases_zip_956_store_", i, "_2014_edgelist.tsv" , sep = "")
  assign(paste("trips_purchases_zip_956_store_", i, "_2014_edgelist", sep = ""), read.table(address, header = TRUE, sep = ","))
  assign(paste("trips_purchases_zip_956_store_", i, "_2014_edgelist", sep = ""), as.data.frame(eval(parse(text = paste("trips_purchases_zip_956_store_", i, "_2014_edgelist[,1:3]", sep = "")))))
  assign(paste("trips_purchases_zip_956_store_", i, "_2014_graph", sep = ""), graph.data.frame(eval(parse(text = paste("trips_purchases_zip_956_store_", i, "_2014_edgelist", sep = ""))), directed = FALSE))
  rm(list = paste("trips_purchases_zip_956_store_", i, "_2014_edgelist", sep = ""))
  store_list_graph_stat$vertices[store_list_graph_stat$store_code_list == i] = vcount(eval(parse(text = paste("trips_purchases_zip_956_store_", i, "_2014_graph", sep = ""))))
  store_list_graph_stat$edges[store_list_graph_stat$store_code_list == i] = ecount(eval(parse(text = paste("trips_purchases_zip_956_store_", i, "_2014_graph", sep = ""))))
  store_list_graph_stat$diff[store_list_graph_stat$store_code_list == i] = ecount(difference(trips_purchases_zip_956_store_821690_2014_graph,eval(parse(text = paste("trips_purchases_zip_956_store_", i, "_2014_graph", sep = "")))))/17283
}
####### watch out !!!!!!! this will erase graph objects
#removing the graph objects created
for (i in store_code_list[1:215]){
  rm(list = paste("trips_purchases_zip_956_store_", i, "_2014_graph", sep = ""))
}

store_code_list_with_salesNgraph <- store_code_list[store_code_list %in% cal_stores]

#edge count of pairwise difference graph and pairwise common nodes of graphs
store_list_graph_stat_all <- matrix(nrow = 215, ncol = 215)
store_list_graph_stat_node <- matrix(nrow = 215, ncol = 215)

for(i in 1:215){
  for(j in c(1:215)){
    store_list_graph_stat_node[i,j] = length(intersect(eval(parse(text = paste("vertex_attr(trips_purchases_zip_956_store_", store_code_list[i], "_2014_graph)$name", sep = ""))),eval(parse(text = paste("vertex_attr(trips_purchases_zip_956_store_", store_code_list[j], "_2014_graph)$name", sep = "")))))/store_list_graph_stat$vertices[i]
    #store_list_graph_stat_all[i,j] = ecount(difference(eval(parse(text = paste("trips_purchases_zip_956_store_", store_code_list[i], "_2014_graph", sep = ""))),eval(parse(text = paste("trips_purchases_zip_956_store_", store_code_list[j], "_2014_graph", sep = "")))))/store_list_graph_stat$edges[i]
  }
  print(i)
}
# this part of code is just temporary (can be deleted)
temp <- ecount(difference(trips_purchases_zip_store_2323912_2014_edgelist_graph,trips_purchases_zip_store_2762443_2014_edgelist_graph))
temp <- ecount(difference(trips_purchases_zip_store_2323912_2014_edgelist_graph,trips_purchases_zip_store_6345418_2014_edgelist_graph))
temp <- ecount(difference(trips_purchases_zip_store_2323912_2014_edgelist_graph,trips_purchases_zip_store_5310749_2014_edgelist_graph))
temp <- ecount(difference(trips_purchases_zip_store_2323912_2014_edgelist_graph,trips_purchases_zip_store_4299341_2014_edgelist_graph))
temp <- ecount(difference(trips_purchases_zip_store_2323912_2014_edgelist_graph,trips_purchases_zip_store_4187623_2014_edgelist_graph))
temp <- difference(trips_purchases_zip_store_4187623_2014_edgelist_graph,trips_purchases_zip_store_2323912_2014_edgelist_graph)
temp1<- trips_purchases_zip_store_4187623_2014_graph
temp2<- trips_purchases_zip_store_2323912_2014_graph
E(temp1)$weight <- 1
E(temp2)$weight <- 1
temp<- difference(temp1,temp2)
vcount(temp)
ecount(temp)/ecount(temp1)
temp<- difference(temp1,temp)
ecount(temp)
temp3 <- graph.intersection(temp1, temp2, keep.all.vertices = FALSE)
temp3 <-delete.vertices(temp3, which(degree(temp3) == 0))
address <- "~/Desktop/research/consumer data/problem statement/Images/intersectgraph.pdf"
pdf(address, width=6, height=6)
print(plot(temp3  , vertex.label= NA,vertex.size = 3))
dev.off()

#finding stores with large common products in the graph
temp <- which(store_list_graph_stat_node > 0.5 & store_list_graph_stat_node < 0.6 , arr.ind = T)
temp <- as.data.frame(temp)
#adding the size and store code of graphs as new columns to temp
temp$vertice1 <- store_list_graph_stat$vertices[temp$row] 
temp$vertice2 <- store_list_graph_stat$vertices[temp$col] 
temp$storecode1 <- store_list_graph_stat$`store_code_list[1:215]`[temp$row] 
temp$storecode2 <- store_list_graph_stat$`store_code_list[1:215]`[temp$col] 
temp <- temp[temp$storecode1 %in% store_code_list_with_salesNgraph,]
temp <- temp[temp$storecode2 %in% store_code_list_with_salesNgraph,]
temp2 <- temp[temp[,c(3,4)] > 12,]

write.table(as.data.table(store_list_graph_stat_node), "~/Desktop/research/consumer data/R files/store_list_graph_stat_node.tsv", sep = ",", col.names = FALSE, row.names = FALSE)
temp <- read.table("~/Desktop/research/consumer data/R files/store_list_graph_stat_all.tsv", sep = ",", header = FALSE)

store_list_graph_CC <- matrix(nrow = 215, ncol = 1)

for(j in c(1:215)){
  store_list_graph_CC[j] = transitivity(eval(parse(text = paste("trips_purchases_zip_956_store_", store_code_list[i], "_2014_graph", sep = ""))))
}
ggplot(data = as.data.frame(table(store_list_graph_stat_all1)), aes(x = Var1)) + geom_histogram(aes(y = ..count..))
ggplot(data = as.data.frame(table(store_list_graph_CC)), aes(x = Var1)) + geom_histogram(aes(y = ..count..))

#############################################################################################################
#combining upc and upc_ver_uc in products list
products1 <-products
assign("products1", transform(products1, upc_unique = paste(upc, upc_ver_uc, sep = "")))
assign("products1", transform(products1, upc_unique = as.character(upc_unique)))
assign("products1", transform(products1, upc_unique = as.numeric(upc_unique)))
#measuring the centrality of nodes in product graph
graph <- eval(parse(text = paste("trips_purchases_zip_956_store_", i, "_2014_graph", sep = "")))
store_betweenness <- betweenness(graph, v = V(graph), directed = FALSE, weights = E(graph)$weight)
top_between <- products1[products1$upc_unique %in% names(tail(sort(store_betweenness),10)),]
store_degree <- strength(graph, v = V(graph), weights = E(graph)$weight)
top_degree <- products1[products1$upc_unique %in% names(tail(sort(store_degree),10)),]
store_closeness <- closeness(graph, v = V(graph), weights = E(graph)$weight)
top_closeness <- products1[products1$upc_unique %in% names(tail(sort(store_closeness),10)),]
store_page_rank <- page_rank(graph, v = V(graph), directed = FALSE, weights = E(graph)$weight)
top_page_rank <- products1[products1$upc_unique %in% names(tail(sort(store_page_rank$vector),10)),]
#store_ <- (graph, v = V(graph), weights = E(graph)$weight)
#top_ <- products1[products1$upc_unique %in% names(tail(sort(store_),10)),]


#############################################################################################################
#creating the bipartite household product graph
#############################################################################################################
#applying pareto rule..keeping top 20% of modules
assign(paste("modules_",i, sep = ""), data.frame(sort(table(eval(parse(text = paste("products_purchases_",i, "$product_module_code", sep = "")))), decreasing = TRUE)))
L <- length(eval(parse(text = paste("modules_",i,"$Freq", sep = ""))))
assign(paste("modules_",i,"_20P", sep = ""), eval(parse(text = paste("modules_",i, "[floor(0.2*L):floor(0.21*L),]", sep = ""))))
assign(paste("Products_",i,"_20P", sep = ""), eval(parse(text = paste("products[products$product_module_code %in% modules_",i,"_20P$Var1 ,c(1:3,5)]", sep = ""))))
assign(paste("Products_",i,"_20P", sep = ""), transform(eval(parse(text = paste("Products_",i,"_20P", sep = ""))), upc_unique = paste(upc, upc_ver_uc, sep = "")))
assign(paste("Products_",i,"_20P", sep = ""), transform(eval(parse(text = paste("Products_",i,"_20P", sep = ""))), upc_unique = as.character(upc_unique)))
assign(paste("Products_",i,"_20P", sep = ""), transform(eval(parse(text = paste("Products_",i,"_20P", sep = ""))), upc_unique = as.numeric(upc_unique)))
assign(paste("trips_purchases_",i,"_20P", sep = ""), eval(parse(text = paste("trips_purchases_",i,"[trips_purchases_",i,"$upc_unique %in% Products_",i,"_20P$upc_unique ,]", sep = ""))))

household_rand <- unique(trips_purchases_2014_20P$household_code)
household_rand <- household_rand[1:3]
assign(paste("trips_purchases_",i,"_20P1", sep = ""), eval(parse(text = paste("trips_purchases_",i,"_20P[trips_purchases_",i,"_20P$household_code %in% household_rand,]", sep = ""))))


assign(paste("household_product_",i, sep = ""), eval(parse(text = paste("trips_purchases_",i,"_20P1[,c(2,16)]", sep = ""))))
assign(paste("household_product_",i, sep = ""), eval(parse(text = paste("ddply(household_product_",i,",.(household_code, upc_unique),nrow)", sep = ""))))
#assign(paste("household_product_",i, sep = ""), eval(parse(text = paste("sapply(household_product_",i,",as.character)", sep = ""))))
assign(paste("household_store_",i, sep = ""), eval(parse(text = paste("trips_purchases_",i,"_20P1[,c(2,5)]", sep = ""))))
assign(paste("household_store_",i, sep = ""), eval(parse(text = paste("ddply(household_store_",i,",.(household_code, store_code_uc),nrow)", sep = ""))))
assign(paste("household_store_",i, sep = ""), eval(parse(text = paste("household_store_",i,"[!(household_store_",i,"$store_code_uc==0),]", sep = ""))))
#assign(paste("household_store_",i, sep = ""), eval(parse(text = paste("sapply(household_store_",i,",as.character)", sep = ""))))
assign(paste("household_product_graph",i, sep = ""), eval(parse(text = paste("rbind(household_product_",i,",household_store_",i,")", sep = ""))))

write.csv(household_product_2014, "~/Desktop/research/consumer data/R files/household_product_2014.csv", sep = ",", col.names = c("household_code","upc","weight"), row.names = FALSE)
write.csv(household_store_2014, "~/Desktop/research/consumer data/R files/household_store_2014.csv", sep = ",", col.names = TRUE, row.names = FALSE)


graph <- graph.data.frame(household_store_2014, directed = FALSE)
E(graph)$weight <- household_store_2014$V1
V(graph)$type <- V(graph)$name %in% household_store_2014[,1]

graph <- graph.data.frame(household_product_2014, directed = FALSE)
E(graph)$weight <- household_product_2014$V1
V(graph)$type <- V(graph)$name %in% household_product_2014[,1]

l <- layout.bipartite(graph)
plot(graph, layout = l[, c(2,1)], vertex.color="red",vertex.size = 1,
     edge.width = 2.5, vertex.label = NA)

#############################################################################################################
#filtering pareto products
PP <- read.table('~/Desktop/research/consumer data/R files/zip 956 store resulotion graphs/trips_purchases_zip_956_store_8376197_2014_edgelist.tsv', header = TRUE, sep = ",")
assign("PP", eval(parse(text = paste("PP[PP$Source %in% Products_",i,"_20P$upc_unique & PP$Target %in% Products_",i,"_20P$upc_unique,]", sep = ""))))
PP$SourceName <- Products_2014_20P[match(PP$Source, Products_2014_20P$upc_unique),3]
PP$TargetName <- Products_2014_20P[match(PP$Target, Products_2014_20P$upc_unique),3]
PP$SourceName1 <- Products_2014_20P[match(PP$Source, Products_2014_20P$upc_unique),4]
PP$TargetName1 <- Products_2014_20P[match(PP$Target, Products_2014_20P$upc_unique),4]
PP <- PP[,c(5,6,3,4)]
PP <- ddply(PP,.(SourceName1,TargetName1),summarise, Weight = sum(Weight))
PP <- PP[!(PP$SourceName1==PP$TargetName1),]
PP$Type <- "Undirected"
colnames(PP) <- c('Source','Target','Weight','Type')
write.csv(PP, "~/Desktop/research/consumer data/R files/PP1.csv", sep = ",", row.names = FALSE)
#############################################################################################################
#product entropy for users
temp <- as.data.table(trips_purchases_2014)
#entropy of products in trips
temp1 <- temp[ , count := sum(quantity), by = list(trip_code_uc, upc_unique)]
temp1_1 <- temp1[ , countTotal := sum(quantity), by = list(trip_code_uc)]
temp1_2 <- temp1_1[!duplicated(temp1_1[,c(1,16)]),]
temp1_3 <- temp1_2[ , C := .N, by = trip_code_uc]
temp1_3$frac <- temp1_3$count/temp1_3$countTotal
temp1_4 <- temp1_3[ , entropy := -sum(frac*log(frac))/log(C), by = trip_code_uc]
temp1_4_sample <- temp1_4[1:100,]
temp1_5 <- temp1_4[!duplicated(temp1_4[,1]),]
temp1_5$entropy[is.nan(temp1_5$entropy)] <- 0
temp1_5$entropy[temp1_5$entropy>1] <- 1
#plot trip entropy distribution
trip_entropy =
  ggplot(temp1_5, aes(x = entropy)) +
  stat_ecdf() +
  theme_bw() +
  #theme(axis.text.x = element_text(size = 7.5, angle = 45,  vjust=1, hjust=1)) +
  #theme(axis.title.y = element_text(size = 10)) +
  scale_y_continuous(limits = c(0,1)) +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) +
  scale_x_continuous(limits = c(0, 1)) +
  xlab("Trip Entropy") +
  ylab("CDF") +
  ggtitle("Trip Entropy Distribution Over All Trips in 2014")
trip_entropy
address <- "~/Desktop/research/consumer data/plots/Trip Entropy Distribution Over All Trips in 2014.pdf"
pdf(address, width=6, height=6)
print(trip_entropy)
dev.off()
rm(trip_entropy)
#product distribution KL Difference from uniform for each household

#product entropy
temp2 <- temp[ , count := sum(quantity), by = list(household_code, trip_code_uc, upc_unique)]
temp2_1 <- temp2[ , productCountTotal := sum(quantity), by = list(household_code, upc_unique)]
temp2_1$frac <- temp2_1$count/temp2_1$productCountTotal
temp2_2 <- temp2_1[(!duplicated(temp2_1[,c(1,2,17)])) & count != 0 ,]
temp2_3 <- temp2_2[ , entropy := -sum(frac*log(frac)), by = list(household_code, upc_unique)]
temp2_4 <- temp2_3[ , productEntropy := mean(entropy), by = upc_unique]
temp2_5 <- temp2_4[!duplicated(temp2_4[,16]) ,]


View(temp2_4[1:20,])

#plotting distribution
costumer_product_entropy_plot =  
  ggplot(temp2_5, aes(x = productEntropy)) +
  stat_ecdf() +
  theme_bw() +
  #theme(axis.text.x = element_text(size = 7.5, angle = 45,  vjust=1, hjust=1)) +
  #theme(axis.title.y = element_text(size = 10)) +
  scale_y_continuous(limits = c(0,1)) +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) +
  scale_x_continuous(limits = c(0, 1)) +
  xlab("Product Entropy") +
  ylab("CDF") +
  ggtitle("Constumers Product Entropy Distribution Over All Stores in 2014")
costumer_product_entropy_plot
address <- "~/Desktop/research/consumer data/plots/product Entropy Distribution Over All Stores in 2014.pdf"
pdf(address, width=6, height=6)
print(costumer_product_entropy_plot)
dev.off()
rm(costumer_product_entropy_plot)
#fit distribution to inter trip time
descdist(costumer_product_entropy$entropy)
fit.lognorm <- fitdist(costumer_product_entropy$entropy , "lnorm")
plot(fit.lognorm)
fit.weibull <- fitdist(costumer_product_entropy$entropy , "weibull")
plot(fit.weibull)

#product store entropy
temp2 <- temp[ , count := sum(quantity), by = list(household_code, store_code_uc, upc_unique)]
temp2_1 <- temp2[ , productCountTotal := sum(quantity), by = list(household_code, upc_unique)]
temp2_1$frac <- temp2_1$count/temp2_1$productCountTotal
temp2_2 <- temp2_1[(!duplicated(temp2_1[,c(2,5,16)])) & count != 0 ,]
temp2_3 <- temp2_2[ , entropy := -sum(frac*log(frac)), by = list(household_code, upc_unique)]
temp2_4 <- temp2_3[ , productEntropy := mean(entropy), by = upc_unique]
temp2_5 <- temp2_4[!duplicated(temp2_4[,16]) ,]


View(temp2_4[1:20,])

#plotting distribution
costumer_product_store_entropy_plot =  
  ggplot(temp2_5, aes(x = productEntropy)) +
  stat_ecdf() +
  theme_bw() +
  #theme(axis.text.x = element_text(size = 7.5, angle = 45,  vjust=1, hjust=1)) +
  #theme(axis.title.y = element_text(size = 10)) +
  scale_y_continuous(limits = c(0,1)) +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) +
  scale_x_continuous(limits = c(0, 1)) +
  xlab("Product Entropy") +
  ylab("CDF") +
  ggtitle("Constumers Product Per Store Entropy Distribution Over All Stores in 2014")
costumer_product_store_entropy_plot
address <- "~/Desktop/research/consumer data/plots/store product Entropy Distribution Over All Stores in 2014.pdf"
pdf(address, width=6, height=6)
print(costumer_product_store_entropy_plot)
dev.off()
rm(costumer_product_store_entropy_plot)


#fit distribution to inter trip time
descdist(costumer_product_entropy$entropy)
fit.lognorm <- fitdist(costumer_product_entropy$entropy , "lnorm")
plot(fit.lognorm)
fit.weibull <- fitdist(costumer_product_entropy$entropy , "weibull")
plot(fit.weibull)


#############################################################################################################
#scanner data for certain store code 
#sales per product
#store1
address <- "~/Desktop/research/consumer data/R files/zip 956 store resulotion graphs/trips_purchases_zip_956_store_4559094_2014_edgelist.tsv"
#another store2
address <- "~/Desktop/research/consumer data/R files/zip 956 store resulotion graphs/trips_purchases_zip_store_4187623_2014_edgelist.tsv"
address <- "~/Desktop/research/consumer data/R files/zip 956 store resulotion graphs/trips_purchases_zip_store_2323912_2014_edgelist.tsv"
store_49 <- read.table(address, header = TRUE, sep = ",")
store_49_G <- graph.data.frame(store_49, directed = FALSE)
vcount(store_49_G)
sum(store_49$Weight)
#store1
address <- "~/Desktop/research/consumer data/R files/Full_Store_Code_4559094 (Retail_Scanner).tsv"
#store2
address <- "~/Desktop/research/consumer data/R files/scanner data/4187623.tsv"
address2 <- "~/Desktop/research/consumer data/R files/scanner data/2323912.tsv"
store_49_S <- read.table(address, header = FALSE, sep = "\t")
store_49_S2 <- read.table(address2, header = FALSE, sep = "\t")
colnames(store_49_S) <- c("store_code_uc","upc","week_end", "units","prmult","price","feature","display")
colnames(store_49_S2) <- c("store_code_uc","upc","week_end", "units","prmult","price","feature","display")
#merge two data frames to find common products between two stores
temp <- as.data.table(store_49_S[store_49_S$upc %in% unique(store_49_S2$upc),])
temp <- as.data.table(store_49_S2[store_49_S2$upc %in% unique(store_49_S$upc),])
temp <- temp[temp$upc %in% substr(vertex_attr(temp3)$name, 1, nchar(vertex_attr(temp3)$name)-1),]
#checking the sale on specific products over time
upc_list <- neighbors(temp1, v = c("34000056311","34000476031"))$name
upc_list <- upc_list[5:10]
upc_list <- upc_list[1:2]
upc_list <- c("34000056311" ,"34000476031")# ,upc_list)
upc_list <- substr(upc_list, 1, nchar(upc_list)-1)
upc_sale <- store_49_S2[store_49_S2$upc %in% upc_list,]
upc_sale$upc <- as.factor(upc_sale$upc)
upc_sale <- transform(upc_sale, week_end = as.Date(as.character(week_end), "%Y%m%d", origin = "2014-01-01"))
upc_sale$upc <- revalue(upc_sale$upc, c("3400005631"="product 1", "3400047603"="product 2"))
upc_sale$upc <- revalue(upc_sale$upc, c("3400005631"="product 2", "3400047603"="product 3"))
upc_sale$upc <- revalue(upc_sale$upc, c("8523928407"="product 8", "8523931157"="product 6","37003062070"="product 7","2240000522"="product 1","88491212965"="product 5","3450015129"="product 4"))


costumer_product_entropy_plot =  
  ggplot(upc_sale, aes(x = week_end, y = units, color = upc)) + 
  geom_point(size = 3) +
  theme_bw() +
  scale_y_continuous(limits = c(0,200)) +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) +
  scale_x_date(date_breaks = "2 month", date_minor_breaks = "1 week", date_labels = "%B") +
  xlab("Week end") +
  ylab("Sales in units") +
  ggtitle("Unit sales by week for neighboring products
          in a sample store in 2014")
costumer_product_entropy_plot
address <- "~/Desktop/research/consumer data/problem statement/Images/sales4.pdf"
pdf(address, width=6, height=6)
print(costumer_product_entropy_plot)
dev.off()


length(unique(store_49_S$upc))
store_49_S <- transform(store_49_S, week_end = as.Date(as.character(week_end), "%Y%m%d", origin = "2014-01-01"))

store_49_S <- as.data.table(store_49_S)
store_49_S_1 <- store_49_S[ , count := .N, by = week_end]
store_49_S_1$count <- store_49_S_1$count/length(unique(store_49_S$upc))
store_49_S_1 <- store_49_S_1[!duplicated(store_49_S_1[,3]) ,]

costumer_product_entropy_plot =  
  ggplot(store_49_S_1, aes(x = week_end, y = count)) +
  geom_point() +
  theme_bw() +
  scale_y_continuous(limits = c(0,1)) +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) +
  scale_x_date(date_breaks = "2 month", date_minor_breaks = "1 week", date_labels = "%B") +
  xlab("Week end") +
  ylab("Fraction of products sold during the week") +
  ggtitle("")
costumer_product_entropy_plot
address <- "~/Desktop/research/consumer data/plots/Fraction of products sold by weeks of 20141.pdf"
address <- "~/Desktop/research/consumer data/problem statement/Images/f1.pdf"

pdf(address, width=6, height=6)
print(costumer_product_entropy_plot)
dev.off()
rm(costumer_product_entropy_plot)
#############################################################################################################
temp <- as.data.table(store_49_S)
temp <- merge(temp, products, by.x = "upc", by.y = "upc", all.x = FALSE, all.y = FALSE)
#histogram of the number of weeks per year every product module has been sold 
temp <- temp[!duplicated(temp[,c(3,11)]),]
temp <- temp[, count:= .N, by= product_module_code]
temp <- temp[!duplicated(temp$product_module_code),]
length(unique(temp$product_module_code))
#remove 'VIDEO PRODUCTS PRERECORDED' and 'MAGAZINES SELECTED TITLES' 
temp <- temp[!(temp$product_module_code %in% c(8900,8902)),]
plot(table(temp$count))
#histogram of weeks that modules with low number of week sales has been sold during those weeks
temp1 <- temp[count < 10,]
plot(table(temp1$week_end))
list <- temp1$upc[temp1$week_end == 20140104]
temp_2 <- products[products$upc %in% list,]
#histogram of the number of weeks per year every product upc has been sold 
temp <- as.data.table(store_49_S)
temp <- merge(temp, products, by.x = "upc", by.y = "upc", all.x = FALSE, all.y = FALSE)
temp <- temp[!duplicated(temp[,c(1,3)]),]
temp <- temp[, count:= .N, by= upc]
temp <- temp[!duplicated(temp$upc),]
length(unique(temp$upc))
plot(table(temp$count))
#histogram of weeks that modules with low number of week sales has been sold during those weeks
temp1 <- temp[count < 2,]
plot(table(temp1$week_end))
#convert weekend to date
temp <- as.data.table(store_49_S2)
temp <- merge(temp, products, by.x = "upc", by.y = "upc", all.x = FALSE, all.y = FALSE)
temp <- transform(temp, week_end = as.Date(as.character(week_end), "%Y%m%d"))
temp <- temp[, window:= max(week_end)-min(week_end), by= upc]
temp <- temp[, start:= min(week_end), by= upc]
temp <- temp[!duplicated(temp$upc),]
plot(table(temp$start))
plot(table(temp$window))
plot(temp$start, temp$window)
costumer_product_entropy_plot =  
  ggplot(temp, aes(x = start, y = window)) +
  geom_point() +
  theme_bw() +
  #scale_y_continuous(limits = c(0,1)) +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) +
  #scale_x_continuous(limits = c(20140101, 20141231)) +
  xlab("Date the product is introduced to the store") +
  ylab("Number of days product sold in the store") +
  ggtitle("sales time window vs. product 
introduction date for a common products 
of two sample stores in year 2014")
costumer_product_entropy_plot
address <- "~/Desktop/research/consumer data/problem statement/Images/scatter211.pdf"
pdf(address, width=6, height=6)
print(costumer_product_entropy_plot)
dev.off()
rm(costumer_product_entropy_plot)

#############################################################################################################
#creating the sales file for a store from the list of files that includes sales of that store in scanner dataset
temp <- read.table("~/Desktop/research/consumer data/R files/scanner data/store_code_4559094/file_names.txt", header = FALSE)
temp[] <- lapply(temp, as.character)
index <- which(nchar(temp$V1) == 93)
temp$V1[index] <- gsub('^(.{79})(.*)$', '\\1aaa\\2', temp$V1[index])
file_names <- sapply(temp, substring, 84, 96)
store_code_4559094 <- data.frame()
for (i in 1:length(file_names)){
  address <- paste("~/Desktop/research/consumer data/R files/scanner data/store_code_4559094/Ehsan/",file_names[i], sep = "")
  temp <- read.table(address, header = FALSE, col.names = c("store_code_uc","upc","week_end","units","prmult","price"))
  store_code_4559094 <- rbind(store_code_4559094, temp)
  
}

#############################################################################################################
#calculating correlation between sales of every pair of products of a store
R <- unique(store_49_S$week_end)
C <- unique(store_49_S$upc)[1:1000]
C <- vertex_attr(store_49_G, "name")
C <- substr(C, 1, nchar(C)-1)
SalesCorrMat <- matrix(NA, nrow = length(R), ncol = length(C))
SalesCorrMat <- as.table(SalesCorrMat)
rownames(SalesCorrMat) <- R
colnames(SalesCorrMat) <- C
store_49_S_temp <- store_49_S[store_49_S$upc %in% C,]
for (i in R){
  for (j in C){
    temp <- store_49_S_temp$units[store_49_S_temp$week_end == i & store_49_S_temp$upc == j]
    if (length(temp) == 1){
      SalesCorrMat[as.character(i),as.character(j)] <- temp
    }
  }
  print(i)
}
SalesCorr <- cor(SalesCorrMat, use = "pairwise.complete.obs")
highCorr <- which(SalesCorr>0.9 &SalesCorr<1 , arr.ind = T)
highCorupc1 <- C[highCorr[,1]]
highCorupc2 <- C[highCorr[,2]]
highCorupc1 <- paste(highCorupc1,"1",sep = "")
highCorupc2 <- paste(highCorupc2,"1",sep = "")
connection <- c(FALSE)
verNames <- vertex_attr(store_49_G, "name")
for (i in 1:length(highCorupc1)){
  if (highCorupc1[i] %in% verNames & highCorupc2[i] %in% verNames){
    connection <- rbind(connection, c(i, are.connected(store_49_G, highCorupc1[i],highCorupc2[i])))
  }
}

connect <- connection[connection[,2]==1,1]
upc <- cbind(highCorupc1[connect], highCorupc2[connect])

store_49_S <- transform(store_49_S, week_end = as.Date(as.character(week_end), "%Y%m%d", origin = "2014-01-01"))

temp <- store_49_S[store_49_S$upc %in%  c(as.numeric(substr(upc[3,], 1, nchar(upc[3,])-1))),]
products[products$upc %in%  c(as.numeric(substr(upc[20,], 1, nchar(upc[20,])-1))),]

costumer_product_entropy_plot =  
  ggplot(temp, aes(x = week_end, y = units, color = upc)) + 
  geom_point(size = 3) +
  theme_bw() +
  scale_y_continuous(limits = c(0,250)) +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) +
  scale_x_date(date_breaks = "2 month", date_minor_breaks = "1 week", date_labels = "%B") +
  xlab("Week end") +
  ylab("Sales in units") +
  ggtitle("Unit sales by week for neighboring products
          in a sample store in 2014")
costumer_product_entropy_plot
address <- "~/Desktop/research/consumer data/plots/axe-nail.pdf"
pdf(address, width=6, height=6)
print(costumer_product_entropy_plot)
dev.off()

#############################################################################################################
#############################################################################################################

gc()


temp <- as.data.table(purchases_2014)
temp2 <- temp5[, count:= .N, by = list(trip_code_uc, upc, upc_ver_uc)]
temp3 <- table(temp2$count)
temp3
temp2[temp2$count==49,2]
products[products$upc == 7980199470,]
length(which(products$product_module_code>445 & products$product_module_code<468))

temp4 <- products$upc[which(products$product_module_code>445 & products$product_module_code<468 | products$product_module_code==750)]
temp5 <- temp[!(temp$upc %in% temp4),]
