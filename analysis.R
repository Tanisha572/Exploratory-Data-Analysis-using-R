#Read data from csv file
data <- read.csv("myCall feedback_Apr2020.csv", header=TRUE, fileEncoding="UTF-8-BOM")
View(data)
attrs <- ncol(data) #number of columns
records <- nrow(data) #number of rows

sprintf("No. of records: %d",records)
sprintf("No. of attributes: %d",attrs)
class(data)

#as all attributes except Longitude and Latitude are nominal and ordinal attributes,
#we'll convert them as factors to explicitly discretize them
#doing this prevents summary to be drawn in an incorrect way, i.e. 5 point summary doesn't make
#sense in discrete and nominal/ordinal attributes

#not making rating a factor as mean median etc operations are meaningful
data$Operator = as.factor(data$Operator)
data$In.Out.Travelling = as.factor(data$In.Out.Travelling)
data$Call.Drop.Category = as.factor(data$Call.Drop.Category)
data$Network.Type = as.factor(data$Network.Type)
data$State.Name = as.factor(data$State.Name)

# ----------------------------HANDLING MISSING VALUES---------------------------------------------------------


#first, identifying the missing values
#and modifying these values so that they can be detected as missing values by R
#i.e. change values like Unknown to missing values NA


#getting all possible values for each attribute (except lat and long) and checking if all values are valid
operatorVal = unique(data$Operator)
inOutTravellingVal = unique(data$In.Out.Travelling)
networkTypeVal = unique(data$Network.Type)
ratingVal = unique(data$Rating)
callDropCatVal = unique(data$Call.Drop.Category)
stateVal = unique(data$State.Name)

print(operatorVal)
print(inOutTravellingVal)
print(networkTypeVal)
print(ratingVal)
print(callDropCatVal)
print(stateVal)

#records having invalid latitude and longitude
print(nrow(data[data$Latitude == data$Longitude, 1:8]))


#attributes:
  
#   #operator :           no missing values
#   #In Out Travelling :  no missing values
#   #Network type :       missing values marked as Unknown
#   #Rating :             no missing values
#   #Call Drop Category : no missing values
#   #Latitude :   missing values marked as -1
#   #Longitude :  missing values marked as -1
#   #State Name : missing values already marked as NA

print(typeof(data$Longitude))

#replacing all missing values with NA 
#state has already missing values marked as NA
data$Network.Type[data$Network.Type == "Unknown"]<-NA
data$Longitude[data$Longitude == as.double(-1) | data$Longitude == as.double(0)]<-NA
data$Latitude[data$Latitude == as.double(-1) | data$Latitude == as.double(0)]<-NA

#print(data[is.na(data$Longitude), 1:8])
View(data)

#to visualize the missing values and how it is distributed with the data, use mice package
library(mice)

summary(data)
dev.new(width=15, height=5, unit="in")
md.pattern(data, rotate.names = TRUE)


#replacing missing values with most occurring value in that attribute

networkTypeReplace = names(sort(table(data$Network.Type), decreasing = TRUE)[1])
sprintf("Most frequently occurring value for Network Type attribute : %s", networkTypeReplace)
data$Network.Type[is.na(data$Network.Type)] <- networkTypeReplace


View(data)


#---1. Rank the operators based on user satisfaction ---------------------------------------------

sortedRating = names(sort(table(data$Rating), decreasing = TRUE))
sortedRating

library(hash)

operatorStats = hash()


#getting mean rating for all operators

for(x in operatorVal)
  operatorStats[[x]] = mean(data$Rating[which(data$Operator == x)], trim = 0, na.rm = FALSE)
  
sprintf("operators sorted based on their avergae ratings")
sort(values(operatorStats), decreasing = TRUE)