library(readr)
library(dplyr)
library(arules)
library(tidyr)

data <- read_csv("transaction_history.csv")
data$CustomerID=as.factor(data$CustomerID)

### Cleaning the data
# 1- Removing letters "C" & "A" at the beginning of InvoiceNo, as InvoiceNo is a number and does not contain letters
a=grep("^[A-Z]", data$InvoiceNo)
data$tempo=data$InvoiceNo
data$tempo[a]=substr(data$tempo[a], 2, 8)
data$InvoiceNo=data$tempo
data=data[-9]

# 2- Removing the negative quantities
data=subset(data, Quantity>=0)

# 3- Bring description to lower case 
data$Description=tolower(data$Description)

# I can keep the NA in CustomerID, as I do not use CustomerID in my analyse

# 4- removing discount from dataframe as they are not actual transactions
data=subset(data, StockCode!="D")

# Which product is found the most in transactions?
B=summarise(group_by(data, StockCode), Count=length(StockCode))
B=B[order(B$Count, decreasing = TRUE),]
C=summarise(group_by(data, StockCode, Description), Count=length(Description))
subset(C, C$StockCode==B$StockCode[1])

# Which country has the most transactions?
B=summarise(group_by(data, Country), Count=length(InvoiceNo))
B=B[order(B$Count, decreasing = TRUE),]
B$Country[1]

# What is the most profitable product in transactions?
data$paid=data$Quantity*data$UnitPrice
B=summarise(group_by(data, StockCode),times_perchased=length(StockCode), profitability=sum(paid))
B=B[order(B$profitability, decreasing = TRUE),]
subset(B, B$StockCode==B$StockCode[c(1,2)])[c(1,2)]
# ‘DOT (description = DOTCOM POSTAGE’) seems to indicate the amount spent by the customer on postage. Postage isn’t a 
# direct indicator of sales and might skew the amount spent across cohorts. So, I take the second item as the most profitable
subset(B, B$StockCode==B$StockCode[2])[c(1,2)]

# Which customer has visited the most?
B=summarise(group_by(data, CustomerID), Count=length(unique(InvoiceDate)))
B=B[order(B$Count, decreasing = TRUE),]
B=na.omit(B)
B$CustomerID[1]

rm(B,C)

# Removing the columns which are not necessary for this analysis #
data=data[,c(1,2)]

# Number of items in the shop
n_item=length(unique(data$StockCode))

# Changing StockCode coloumn name to item
names(data)[2]="item"

# Creating transaction format datafile
write.csv(data, "donneesTransac.csv", row.names = F)

trans <- read.transactions(
  file = "donneesTransac.csv",
  format = "single",
  sep = ",", header = T,
  cols=c("InvoiceNo","item"),
  rm.duplicates = T
)

rm(data) 

inspect(trans[1:5])

# Visualizing the 25 first items with highest frequencies (support)
itemFrequencyPlot(trans, topN=25)

# Which product is found the most in transactions?
sort(itemFrequency(trans), decreasing = TRUE)[1]

# Association Rules
assoc_rules <- apriori(trans, parameter = list(supp=0.024, conf=0.7,maxlen=5))
Apriori_df = inspect(assoc_rules)
inspect(sort(assoc_rules,by="lift"))
summary(assoc_rules)

