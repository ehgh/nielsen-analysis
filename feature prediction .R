#feature prediction
#merge all different hours employment into 1 and remove families without male or female head
for (i in 2011:2014){
  assign(paste("panelists_",i, sep = ""), eval(parse(text = paste("panelists_", i, "[!(panelists_", i, "$male_head_employment == 0),]", sep = ""))))
  eval(parse(text = paste("panelists_", i, "$male_head_employment[which(panelists_", i, "$male_head_employment == 2 | panelists_", i, "$male_head_employment == 3)] <- 1", sep = "")))
  assign(paste("panelists_",i, sep = ""), eval(parse(text = paste("panelists_", i, "[!(panelists_", i, "$female_head_employment == 0),]", sep = ""))))
  eval(parse(text = paste("panelists_", i, "$female_head_employment[which(panelists_", i, "$female_head_employment == 2 | panelists_", i, "$female_head_employment == 3)] <- 1", sep = "")))
}
#find common panelists throughout years
common_panelists <- panelists_2011$household_code
for (i in 2012:2014){
  common_panelists <- intersect(common_panelists, eval(parse(text = paste("panelists_", i, "$household_code", sep = ""))))  
}
#filtering panelists according to common panelists
for (i in 2011:2014){
  assign(paste("panelists_",i, sep = ""), eval(parse(text = paste("panelists_", i, "[panelists_", i, "$household_code %in% common_panelists,]", sep = ""))))
}
#filtering trips according to common panelists
for (i in 2011:2014){
  assign(paste("trips_",i, sep = ""), eval(parse(text = paste("as.data.table(trips_", i, "[trips_", i, "$household_code %in% common_panelists,])", sep = ""))))
}
#filtering trips_purchases according to common panelists
for (i in 2011:2012){
  assign(paste("trips_purchases_",i, sep = ""), eval(parse(text = paste("as.data.table(trips_purchases_", i, "[trips_purchases_", i, "$household_code %in% common_panelists,])", sep = ""))))
}
#count the number of trips for each household and the amount of spendature per year for that household
for (i in 2011:2014){
  assign(paste("trips_sum_",i, sep = ""), eval(parse(text = paste("trips_", i, "[,list(total = sum(total_spent), num = .N), by = household_code]", sep = ""))))
}
#count the average of quantities for each household and the amount of total coupon per year for that household
for (i in 2011:2012){
  assign(paste("trips_purchases_sum_",i, sep = ""), eval(parse(text = paste("trips_purchases_", i, "[,list(coupon = sum(coupon_value), quantity = mean(quantity)), by = household_code]", sep = ""))))
}

#merge trip_sums to calculate change in spendature
assign("trips_sum_11_12", merge(eval(parse(text = paste("trips_sum_", "2011", sep = ""))), eval(parse(text = paste("trips_sum_", "2012", sep = ""))), by = "household_code",suffixes = c("2011","2012")))
trips_sum_11_12$total_spent_change <- (trips_sum_11_12$total2012/trips_sum_11_12$num2012) - (trips_sum_11_12$total2011/trips_sum_11_12$num2011)
#merge trip_purchases_sums to calculate change in coupon and quantity
assign("trips_purchases_sum_11_12", merge(eval(parse(text = paste("trips_purchases_sum_", "2011", sep = ""))), eval(parse(text = paste("trips_purchases_sum_", "2012", sep = ""))), by = "household_code",suffixes = c("2011","2012")))
trips_purchases_sum_11_12$quantity_change <- trips_purchases_sum_11_12$quantity2012 - trips_purchases_sum_11_12$quantity2011
trips_purchases_sum_11_12$coupon_change <- trips_purchases_sum_11_12$coupon2012 - trips_purchases_sum_11_12$coupon2011
#merge panelists to calculate change in employment status
assign("panelists_13_14", merge(eval(parse(text = paste("panelists_", "2013", sep = ""))), eval(parse(text = paste("panelists_", "2014", sep = ""))), by = "household_code",suffixes = c("2013","2014")))
assign("panelists_13_14", transform(eval(parse(text = "panelists_13_14")), employment = paste(male_head_employment2013, male_head_employment2014, female_head_employment2013, female_head_employment2014, sep = "")))
assign("panelists_13_14", transform(eval(parse(text = "panelists_13_14")), employment = as.character(employment)))
assign("panelists_13_14", transform(eval(parse(text = "panelists_13_14")), employment = as.numeric(employment)))
assign("panelists_13_14", eval(parse(text = paste("panelists_13_14[,c(1,116)]", sep = ""))))
assign("trips_sum_11_12", eval(parse(text = paste("trips_sum_11_12[,c(1,6)]", sep = ""))))
assign("trips_purchases_sum_11_12", eval(parse(text = paste("trips_purchases_sum_11_12[,c(1,6,7)]", sep = ""))))
#merge change in spendature and change in employment into one dataframe
assign("panelists_13_14", merge(eval(parse(text = "panelists_13_14")), eval(parse(text = "trips_sum_11_12")), by = "household_code"))
assign("panelists_13_14", merge(eval(parse(text = "panelists_13_14")), eval(parse(text = "trips_purchases_sum_11_12")), by = "household_code"))
#separate files for change in employment of only male/female head
apanelists_13_14 <-  panelists_13_14[which(panelists_13_14$employment %in% c("1111","1911","1999","9111","9199")),]
apanelists_13_14 <-  panelists_13_14[which(panelists_13_14$employment %in% c("1111","1119","9919","1191","9991")),]
table(apanelists_13_14$employment)

apanelists_13_14 <- as.table(apanelists_13_14)
#plot change in purchasin quantity for change in employment groups
employment_plot =  
  ggplot(apanelists_13_14, aes(x = quantity_change, color = household_code)) +
  stat_ecdf() +
  theme_bw() +
  theme(axis.text.x = element_text(size = 7.5, angle = 45,  vjust=1, hjust=1)) +
  theme(axis.title.y = element_text(size = 10)) +
  scale_y_continuous(limits = c(0,1)) +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) +
  scale_x_continuous(limits = c(-0.25,0.25)) +
  xlab("Average quantity change from 2011 to 2012") +
  ylab("CDF") +
  ggtitle(paste("CDF of average quantity change from 2011 to 2012\n grouped by change in male head emplyment\n status", "", sep = ""))
employment_plot
address <- paste("~/Desktop/research/consumer data/plots/CDF_employment_quantity_male_change1_", i, ".pdf", sep = "")
pdf(address, width=6, height=6)
print(employment_plot)
dev.off()
rm(employment_plot)

#plot change in expenditure for change in employment groups
employment_plot =  
  ggplot(panelists_13_14, aes(total_spent_change, color = employment)) +
  stat_ecdf() +
  theme_bw() +
  theme(axis.text.x = element_text(size = 7.5, angle = 45,  vjust=1, hjust=1)) +
  theme(axis.title.y = element_text(size = 10)) +
  scale_y_continuous(limits = c(0,1)) +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) +
  #scale_x_continuous(limits = c(-0.25,0.25)) +
  xlab("Total expenditure change from 2011 to 2012") +
  ylab("CDF") +
  ggtitle(paste("CDF of total expenditure change from 2011 to 2012\n grouped by change in male head emplyment\n status", "", sep = ""))
employment_plot
address <- paste("~/Desktop/research/consumer data/plots/CDF_employment_total_spent_male_change1_", i, ".pdf", sep = "")
pdf(address, width=6, height=6)
print(employment_plot)
dev.off()
rm(employment_plot)
panelists_2014_weka <- panelists_2014[,c(5,8,10:17,20,23,26)]
panelists_2014_weka <- panelists_2014[,c(1,6,9,10,11,12,13,14,15)]
panelists_2013_weka <- panelists_2013[,c(1,6,9,10,11,12,13,14,15)]
write.csv(panelists_2013_weka, "~/Desktop/research/consumer data/weka files/panelists_2013_weka.csv", sep = ",", col.names = TRUE, row.names = FALSE)
write.csv(panelists_2014_weka, "~/Desktop/research/consumer data/weka files/panelists_2014_weka.csv", sep = ",", col.names = TRUE, row.names = FALSE)
w = cor(panelists_2014_weka, use = 'pairwise.complete.obs')
w = cor(panelists_2014_weka)
w[abs(w[,9]) > 0.3,9]
w[abs(w) > 0.5 & w < 1]
w = cor(panelists_2013_weka, use = 'pairwise.complete.obs')
w = cor(panelists_2013_weka)
w[abs(w[,9]) > 0.1,9]
w[,c(9,10)]


#############################################################################################################
#Finding the statistics of purchasing in trips_purchases file
#Basket size distribution
temp <- as.data.table(trips_purchases_2014[,c(1,2,3,4,5,7,16)])
temp2 <- temp[ , basket := .N, by = trip_code_uc]
temp3 <- temp2[!duplicated(temp2[,1]),]
#this will do the same as above but only keeps counts and not rows
temp4 <- count(temp, c('trip_code_uc'))
#plotting teh distribution
all_stores_basket_plot =  
  ggplot(temp3, aes(x = basket, color = household_code)) +
  stat_ecdf() +
  theme_bw() +
  #theme(axis.text.x = element_text(size = 7.5, angle = 45,  vjust=1, hjust=1)) +
  #theme(axis.title.y = element_text(size = 10)) +
  scale_y_continuous(limits = c(0,1)) +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) +
  scale_x_continuous(limits = c(1, 100)) +
  xlab("Basket Size") +
  ylab("CDF") +
  ggtitle("Basket Size Distribution Over All Stores in 2014")
all_stores_basket_plot
address <- "~/Desktop/research/consumer data/plots/Basket Size Distribution Over All Stores in 2014.pdf"
pdf(address, width=6, height=6)
print(all_stores_basket_plot)
dev.off()
rm(all_stores_basket_plot)
#check what distribution fits the data most
temp5 <- temp3$basket[temp3$basket<100]
descdist(temp5)
fit.lognorm <- fitdist(temp5 , "lnorm")
plot(fit.lognorm)
fit.weibull <- fitdist(temp5 , "weibull")
plot(fit.weibull)


#############################################################################################################
#Finding inter-trip time
household_list <- unique(trips_2014$household_code)
temp3 <- matrix(nrow = 0, ncol = 1)
for (i in 1:length(household_list)){
  if (i%%1000 == 0){
    print(i)
  }
  temp <- trips_2014$purchase_date[trips_2014$household_code == household_list[i]]
  temp2 <- strptime(sort(temp), format = "%Y-%m-%d")
  temp3 <- rbind(temp3, as.matrix(round(diff(temp2)/86400, digits = 0)))
}
inter_trip_time <- as.data.frame(temp3)
#similar code as above but faster..needs some fixing
temp <- as.data.table(trips_2014[,c(2,3)])
temp2 <- temp[ , inter_trip_time := round(diff(strptime(sort(purchase_date), format = "%Y-%m-%d"))/86400, digits = 0), by = household_code]
V1 <- temp2$inter_trip_time
inter_trip_time <- as.data.frame(V1)
#plotting distribution
inter_trip_time_plot =  
  ggplot(inter_trip_time, aes(x = V1)) +
  stat_ecdf() +
  theme_bw() +
  #theme(axis.text.x = element_text(size = 7.5, angle = 45,  vjust=1, hjust=1)) +
  #theme(axis.title.y = element_text(size = 10)) +
  scale_y_continuous(limits = c(0,1)) +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) +
  scale_x_continuous(limits = c(1, 122)) +
  xlab("Inter Trip Time") +
  ylab("CDF") +
  ggtitle("Inter Trip Time Distribution Over All Stores in 2014")
inter_trip_time_plot
address <- "~/Desktop/research/consumer data/plots/Inter Trip Time Distribution Over All Stores in 2014.pdf"
pdf(address, width=6, height=6)
print(inter_trip_time_plot)
dev.off()
rm(inter_trip_time_plot)
#fit distribution to inter trip time
inter_trip_time <- as.numeric(inter_trip_time$V1)
descdist(inter_trip_time)
fit.lognorm <- fitdist(inter_trip_time , "lnorm")
plot(fit.lognorm)
fit.weibull <- fitdist(inter_trip_time , "weibull")
plot(fit.weibull)
#number of costumers per store
temp <- as.data.table(trips_2014[trips_2014$store_code_uc != 0,])
temp2 <- temp[ , count := .N, by = list(store_code_uc, purchase_date)]
temp3 <- temp2[,c(3,5,10)]
temp4 <- temp3[!duplicated(temp3),]
count <- temp4$count
costumers_day_store <- as.data.frame(count)
#plotting distribution
costumers_day_store_plot =  
  ggplot(costumers_day_store, aes(x = count)) +
  stat_ecdf() +
  theme_bw() +
  #theme(axis.text.x = element_text(size = 7.5, angle = 45,  vjust=1, hjust=1)) +
  #theme(axis.title.y = element_text(size = 10)) +
  scale_y_continuous(limits = c(0,1)) +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) +
  scale_x_continuous(limits = c(1, 20)) +
  xlab("Constumers Per Day Per Store") +
  ylab("CDF") +
  ggtitle("Constumers Per Day Per Store Distribution Over All Stores in 2014")
costumers_day_store_plot
address <- "~/Desktop/research/consumer data/plots/costumers day store Distribution Over All Stores in 2014.pdf"
pdf(address, width=6, height=6)
print(costumers_day_store_plot)
dev.off()
rm(costumers_day_store_plot)
#fit distribution to inter trip time
descdist(costumers_day_store$count)
fit.lognorm <- fitdist(costumers_day_store , "lnorm")
plot(fit.lognorm)
fit.weibull <- fitdist(costumers_day_store$count , "weibull")
plot(fit.weibull)
