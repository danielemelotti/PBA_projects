## %%% ## ## %%% ## ## %%% ## ## %%% ## ## %%% ## ## %%% ## ## %%% ## ## %%% ## 
## PBA Assignment #1                                                         ##
## %%% ## ## %%% ## ## %%% ## ## %%% ## ## %%% ## ## %%% ## ## %%% ## ## %%% ## 

# Import Dataset
raw <- read.csv("/Users/J1Yoo/Documents/local_rScript/online_retail.csv")

str(raw) # Print the structure of the data set.
head(raw) # Print the first 6 observations in the data set.

# Convert InvoiceDate to a Date Object
raw$date <- as.Date(raw$InvoiceDate, format = "%m/%d/%y")
data <- subset(raw, subset = raw$date >= as.Date("2011/07/01") & 
                 raw$date <= as.Date("2011/08/31"))
str(data)
summary(data$date)
list(min = min(data$date),
     max = max(data$date)) # Display the min. & max. values in InvoiceDate after subsetting

length(unique(data$InvoiceNo)) # 3,664: Print the number of transactions made in Jul & Aug of 2011.

summary(data$UnitPrice) # Print the summary statistics of the UnitPrice

# 1) Compute the mean of Quantity and UnitPrice using a for loop
colNum = c(4, 6)
i = 1
for(i in 1:2){
  print(paste(names(data[colNum[i]]),
      mean(data[[colNum[i]]])))
}

# 2) Determine the types of each column using a for loop
for(i in 1:length(names(data))){
  print(class(data[[i]]))
}

# 3) Compute the number of unique values in each column using a for loop
for(i in 1:length(names(data))){
  print(length(unique(data[[i]])))
}

# Subsetting the data
head(data) # Take a look again at the dataset. What is the name of the variable we will need to use for subsetting?
unique(data$Country) # Print the unique values in the character vector, Country.

subdata <- subset(x = data, subset = Country %in% c("United Kingdom", "Netherlands", "Australia"))
unique(subdata$Country) # Print the unique values in Country after subsetting.

round(mean(subdata$UnitPrice), digits = 3) # Print the average of UnitPrice rounded up to 3 decimal points.
round(sd(subdata$UnitPrice), digits = 3) # Print the standard deviation of UnitPrice rounded up to 3 decimal points.
length(unique(subdata$InvoiceNo)) # Print the number of transactions made in these countries in Jul & Aug of 2011.
length(unique(subdata$CustomerID)) # Print the number of customers who lived in these countries from Jul & Aug of 2011.

unique(subdata$InvoiceNo) # Print and take a look at the InvoiceNo.
table(nchar(subdata$InvoiceNo)) # The number of characters of the InvoiceNo for refunds are 7 (due to the C), not 6.

refunds <- subset(x = data, subset = nchar(data$InvoiceNo) == 7) # Subset and filter the data for the refunds.
head(refunds)
length(unique(refunds$CustomerID)) - 1 # Print the number of customers who made at least one refund. -> Here, I am subtracting 1 from the length to account for NA.

cust_refund <- unique(refunds$CustomerID)

nonMember <- subset(x = data, subset = is.na(data$CustomerID)) # Subset data for those that purchased without logging in.
head(nonMember)
nonMember$Sales <- nonMember$Quantity * nonMember$UnitPrice # Generate a variable called Sales.

sum(nonMember$Sales) # Total sales amount from those that purchased without logging in.
length(unique(nonMember$InvoiceNo))

#### Extra Credit
head(data)
data$Sales <- data$Quantity * data$UnitPrice # Generate a variable called Sales.

exDat <- aggregate(data$Sales, by = list(months(data$date), data$CustomerID), sum); exDat
exDat <- subset(x = exDat, Group.1 == "July"); head(exDat)
head(exDat[order(exDat$x, decreasing = T),], 5)

# End of Code