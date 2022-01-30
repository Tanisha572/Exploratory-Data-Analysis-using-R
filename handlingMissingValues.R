#Read data from csv file
data <- read.csv("myCall feedback_Apr2020.csv", header=TRUE, fileEncoding="UTF-8-BOM")
View(data)
attrs <- ncol(data) #number of columns
records <- nrow(data) #number of rows

sprintf("No. of records: %d",records)
sprintf("No. of attributes: %d",attrs)
class(data)


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
md.pattern(data)


#replacing missing values with most occurring value in that attribute

networkTypeReplace = names(sort(table(data$Network.Type), decreasing = TRUE)[1])
sprintf("Most frequently occurring value for Network Type attribute : %s", networkTypeReplace)
data$Network.Type[is.na(data$Network.Type)] <- networkTypeReplace

stateReplace = names(sort(table(data$State.Name), decreasing = TRUE)[1])
sprintf("Most frequently occurring value for State Name attribute : %s", stateReplace)
data$State.Name[is.na(data$State.Name)] <- stateReplace

View(data)