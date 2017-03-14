library(ggplot2)
library(plyr)
library(dplyr)
library(caret)

train <- read.csv("Train_item.csv")
test <- read.csv("Test_item.csv")

#add a column
test$Item_Outlet_Sales <- 1

#combine the datasets
combi <- rbind(train, test)


#relabel fat content into low fat and regular
combi$Item_Fat_Content <- revalue(combi$Item_Fat_Content,
                                  c("LF" = "Low Fat", "low fat" = "Low Fat", "reg" = "Regular"))


#create a new variable Item_Type_Combined to categorise items into Food, Drinks and Non-Consumables
combi$Item_Type_Combined <- substr(combi$Item_Identifier, 1, 2)


#check item fat content vs item type combined and replace fat content of non-food items
table(combi$Item_Type_Combined, combi$Item_Fat_Content)
table(combi$Item_Type, combi$Item_Fat_Content)
levels(combi$Item_Fat_Content) <- c(levels(combi$Item_Fat_Content), "None")
combi <- within(combi, Item_Fat_Content[Item_Type == "Household" | Item_Type == "Health and Hygiene" | Item_Type == "Others"] <- "None")


#create a new variable for outlet's years in existence and remove Establish Year
combi$Year <- as.factor(2013 - combi$Outlet_Establishment_Year)
combi <- subset(combi, select = -Outlet_Establishment_Year)


#Check median weights by factor level of Item_Type and impute missing values with median
impute.fun <- function(x, fun) replace(x, which(is.na(x)), fun(x, na.rm=TRUE))
combi <- combi %>% 
  group_by(Item_Type) %>%
  mutate(Item_Weight = impute.fun(Item_Weight, median))


#Impute missing values with the median for every factor grouped by Outlet and Item Type
cols <- c("Outlet_Type", "Item_Type")
combi$Item_Visibility <- with(combi, as.numeric(replace(Item_Visibility, Item_Visibility == 0, NA)))
combi <- combi %>% 
  group_by(Outlet_Type, Item_Type) %>% 
  mutate(Item_Visibility = impute.fun(Item_Visibility, median)) %>%
  as.data.frame()


#visualize Item_MRP with a density plot
ggplot(data = combi, aes(x = Item_MRP)) +
  geom_density(color = "blue", adjust = 1/5) +
  scale_x_continuous(breaks = seq(0,270, 25))

#create new factor level MRP_level 
combi$MRP_Level <- as.factor(ifelse(
  combi$Item_MRP < 70, "Low",
  ifelse(combi$Item_MRP < 136, "Medium",
         ifelse(combi$Item_MRP < 203, "High", "Very High"))
))

#Inspect Outlet size vs outlet type
table(combi$Outlet_Size, combi$Outlet_Type)

#fill all size of grocery stores as small
combi <- within(combi, Outlet_Size[Outlet_Size == "" & Outlet_Type == "Grocery Store"] <- "Small")


#visualize Outlet size and Outlet_Identifier vs Sales
ggplot(combi[1:nrow(train),], aes(x = Outlet_Type, y = Item_Outlet_Sales, fill = Outlet_Size)) +
  geom_boxplot()

ggplot(combi[1:nrow(train),], aes(x = Outlet_Identifier, y = Item_Outlet_Sales, fill = Outlet_Size)) +
  geom_boxplot()


#classify Outlet 45 and 17 as "small"
combi <- within(combi, Outlet_Size[Outlet_Identifier == "OUT017" | Outlet_Identifier == "OUT045"] <- "Small")


#remove dependent and identifier variables
my_data <- subset(combi, select = -c(Item_Outlet_Sales, Item_Identifier, Outlet_Identifier))

#since PCA only works on numerical variables, check variable class
str(my_data)

#convert factors into numerical using one hot encoding
library(dummies)

#create a dummy data frame
new_my_data <- dummy.data.frame(my_data, names = c('Item_Fat_Content', 'Item_Type', 'Outlet_Size', 'Outlet_Location_Type', 'Outlet_Type', 'Year', 'MRP_Level', 'Item_Type_Combined'))
str(new_my_data)

#divide the data in train and test
train.data <- new_my_data[1:nrow(train),]
test.data <- new_my_data[-(1:nrow(train)),]

train.data$Item_Outlet_Sales <- train$Item_Outlet_Sales
test.data$Item_Outlet_Sales <- 1

## MODEL BUILDING 
#set seed
set.seed(100)


#set up cross validation
xgbTrControl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 2,
  verboseIter = FALSE,
  returnData = FALSE,
  allowParallel = TRUE
)

#set up tuning grid
xgbGrid <- expand.grid(
  nrounds = c(250),
  max_depth = c(2,3,4),
  eta = c(0.1),
  gamma = c(4,5),
  colsample_bytree = c(0.5),
  min_child_weight = c(1,2),
  subsample = c(0.6)
)


#fit XGB model
xgbTrain <- train(
  x = train.data,
  y = train$Item_Outlet_Sales,
  trControl = xgbTrControl,
  tuneGrid = xgbGrid,
  method = "xgbTree"
)

#check the best tuning parameters
head(xgbTrain$results[with(xgbTrain$results, order(RMSE)),], 1)

#predict on test data
yhatxgb <- predict(xgbTrain, newdata = test.data)

#create submission
sample <- read.csv("SampleSubmission.csv")
final.sub <- data.frame(Item_Identifier = sample$Item_Identifier, 
                        Outlet_Identifier = sample$Outlet_Identifier, 
                        Item_Outlet_Sales = yhatxgb)
write.csv(final.sub, "Big_Mart.csv", row.names=F)

## TEST RMSE - 1154, Rank - 175