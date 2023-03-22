library(ggplot2)
library(readxl)
getwd()
# set the working directory to where the dataset is located
setwd("C:/Users/User/Desktop/portfolio DataSceince/proj/R projects")

# read the CSV file into a dataframe
cafe_ocean <- read_excel("Cafe_Ocean.xlsx")

# correct the column names
names(cafe_ocean) <- c("Date", "Bill_Number", "Item_Desc", "Time", "Quantity", "Rate", "Tax", "Discount", "Total", "Category")
View(cafe_ocean)

# Group the data by Category and Item_Desc and sum up the Quantity for each item within each category
cat_item_qty <- aggregate(Quantity ~ Category + Item_Desc, data = cafe_ocean, FUN = sum)

# Sort the items by Quantity in descending order within each category
cat_item_qty <- cat_item_qty[order(cat_item_qty$Category, -cat_item_qty$Quantity),]

# Extract the most famous item within each category
most_famous_items <- aggregate(Item_Desc ~ Category, data = cat_item_qty, FUN = function(x) x[1])

# Rename the column
names(most_famous_items) <- c("Category", "Most_Famous_Item")
# Calculate the total cost for each item (including tax and discount)
cafe_ocean$Total_Cost <- cafe_ocean$Total + cafe_ocean$Tax - cafe_ocean$Discount

# Group the data by Item_Desc and calculate the total cost for each item
item_cost <- aggregate(Total_Cost ~ Item_Desc, data = cafe_ocean, FUN = sum)

# Sort the items by Total_Cost in descending order
item_cost <- item_cost[order(-item_cost$Total_Cost),]

# Show the top 1 item
costliest_item <- item_cost[1, "Item_Desc"]

# Group the data by Category and Item_Desc and calculate the total cost for each item within each category
cat_item_cost <- aggregate(Total_Cost ~ Category + Item_Desc, data = cafe_ocean, FUN = sum)

# Sort the items by Total_Cost in descending order within each category
cat_item_cost <- cat_item_cost[order(cat_item_cost$Category, -cat_item_cost$Total_Cost),]

# Extract the costliest item within each category
costliest_items <- aggregate(Item_Desc ~ Category, data = cat_item_cost, FUN = function(x) x[1])

# Rename the column
names(costliest_items) <- c("Category", "Costliest_Item")

# Calculate the total revenue for each category
category_revenue <- aggregate(Total ~ Category, data = cafe_ocean, FUN = sum)

# Calculate the percentage of revenue for each category
category_revenue$Percent_Revenue <- category_revenue$Total / sum(category_revenue$Total) * 100

# Create a bar plot
ggplot(category_revenue, aes(x = Category, y = Percent_Revenue, fill = Category)) +
  geom_bar(stat = "identity") +
  xlab("Category") +
  ylab("Percent Revenue") +
  ggtitle("Category-wise Share of Revenue")


# Calculate the total revenue for each category
category_revenue <- aggregate(Total ~ Category, data = cafe_ocean_df, FUN = sum)

# Calculate the percentage of revenue for each category
category_revenue$Percent_Revenue <- category_revenue$Total / sum(category_revenue$Total) * 100

# Create a pie chart
ggplot(category_revenue, aes(x = "", y = Percent_Revenue, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(fill = "Category") +
  ggtitle("Category-wise Share of Revenue") +
  theme_void()

# Convert the Date column to a Date format
cafe_ocean$Date <- as.Date(cafe_ocean$Date, format = "%m/%d/%Y")

# Calculate the total sales for each day
daily_sales <- aggregate(Total ~ Date, data = cafe_ocean, FUN = sum)

# Create a line plot

ggplot(daily_sales, aes(x = Date, y = Total)) +
  geom_line() +
  xlab("Date") +
  ylab("Total Sales") +
  ggtitle("Day-wise Sales Trends")

# Create a bar plot
ggplot(daily_sales, aes(x = Date, y = Total)) +
  geom_bar(stat = "identity") +
  xlab("Date") +
  ylab("Total Sales") +
  ggtitle("Day-wise Sales Trends")


# Create a new column for month
cafe_ocean$Month <- format(cafe_ocean$Date, "%m-%Y")

# Count the number of bills generated each month
monthly_traffic <- aggregate(Bill_Number ~ Month, data = cafe_ocean, FUN = length)

# Create a bar plot

ggplot(monthly_traffic, aes(x = Month, y = Bill_Number)) +
  geom_bar(stat = "identity") +
  xlab("Month") +
  ylab("Number of Bills") +
  ggtitle("Monthly Customer Traffic")

# Count the number of bills generated each day
daily_traffic <- aggregate(Bill_Number ~ Date, data = cafe_ocean, FUN = length)

# Create a line plot
ggplot(daily_traffic, aes(x = Date, y = Bill_Number)) +
  geom_line() +
  xlab("Date") +
  ylab("Number of Bills") +
  ggtitle("Daily Customer Traffic")

# Create a new column for hour
cafe_ocean$Hour <- format(cafe_ocean$Time, "%H")

# Count the number of bills generated each hour
hourly_traffic <- aggregate(Bill_Number ~ Hour, data = cafe_ocean, FUN = length)

# Create a bar plot
ggplot(hourly_traffic, aes(x = Hour, y = Bill_Number)) +
  geom_bar(stat = "identity") +
  xlab("Hour") +
  ylab("Number of Bills") +
  ggtitle("Hourly Customer Traffic")


# Load the arules package
library(arules)

# Convert the Item_Desc column to a factor
cafe_ocean$Item_Desc <- as.factor(cafe_ocean$Item_Desc)

# Create a transactions object
trans <- as(cafe_ocean, "transactions")

# Use the apriori algorithm to find frequent itemsets
itemsets <- apriori(trans, parameter = list(support = 0.01, confidence = 0.5))

# Use the inspect function to view the frequent itemsets
inspect(itemsets)

# Use the arulesViz package to create a scatterplot matrix of the frequent itemsets
library(arulesViz)
plot(itemsets, method = "scatterplot",jitter=0)



