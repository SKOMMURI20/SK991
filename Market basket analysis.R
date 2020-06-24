library(data.table)
library(arules)

#Reading the data
e_transactions <- read.transactions("ElectronidexTransactions2017.csv")
dim(e_transactions)
#printing the transaction data
inspect(e_transactions)

#Frequency plot
itemFrequencyPlot(e_transactions, topN = 20, type = 'absolute', col = rainbow(4))

#Running the Apriori Algorithm
analysis_rule <- apriori(e_transactions, parameter = list(support = 0.01, confidence = 0.7, minlen = 2))
length(analysis_rule)
summary(analysis_rule)
#Checking for redundancy
is.redundant(analysis_rule)
inspect(analysis_rule[1:10])
#selecting the data in a sorted way based on lift value
inspect(head(sort(analysis_rule, by="lift"),5))

#Selecting the redundant data
subset_rules <- which(colSums(is.subset(analysis_rule, analysis_rule)) > 1) # get subset rules in vector
length(subset_rules) 

#Removing the redundant data
subset_analysis_rule <- analysis_rule[-subset_rules]
length(subset_analysis_rule)
inspect(subset_analysis_rule)

#Selecting the top 10 values of the non-redundant data
top10_Rules <- head(subset_analysis_rule, n = 20, by = "lift")
inspect(top10_Rules)

library(arulesViz)
# Filter rules with confidence greater than 0.7 or 70%
Filter_plot1 <- analysis_rule[quality(analysis_rule)$confidence>0.7]   #useless information because of redundancy
Filter_plot <- subset_analysis_rule[quality(subset_analysis_rule)$confidence>0.6] #Filtered non-redundant data with confidence = 60%

#Plot Filtered data
plot(Filter_plot,method="two-key plot", jitter = 0)
plotly_arules(Filter_plot) #interactive plot

#Graph plot to visualize the relationship between items
plot(subset_analysis_rule, method = "graph",  engine = "htmlwidget")
plot(top10_Rules, method="graph", engine = "htmlwidget")

#Paracoord plot for indicating the highest demand product with realtionship and parametric values
plot(top10_Rules, method="paracoord")
plot(top10_Rules, method="paracoord", control=list(reorder=TRUE))

#Doubledecker plot just too show the realtionship of between only 1 transaction
oneRule <- sample(analysis_rule, 1)
plot(oneRule, method="doubledecker", data = e_transactions)
