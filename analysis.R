#Read data from csv file
library(ggplot2)
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

ggplot(data, aes(Network.Type, fill = Network.Type)) + geom_bar() + theme_wsj()+ scale_colour_wsj("colors6")


#replacing missing values with most occurring value in that attribute

networkTypeReplace = names(sort(table(data$Network.Type), decreasing = TRUE)[1])
sprintf("Most frequently occurring value for Network Type attribute : %s", networkTypeReplace)
data$Network.Type[is.na(data$Network.Type)] <- networkTypeReplace


#getting frequency plots for all nominal attributes
library(ggplot)
install.packages("ggthemes") # Install 

ggplot(data, aes(Operator, fill = Operator)) + geom_bar() + theme_wsj()+ scale_colour_wsj("colors6")
ggplot(data, aes(In.Out.Travelling, fill = In.Out.Travelling)) + geom_bar() + theme_wsj()+ scale_colour_wsj("colors6")
ggplot(data, aes(Call.Drop.Category, fill = Call.Drop.Category)) + geom_bar() + theme_wsj()+ scale_colour_wsj("colors6")
ggplot(data, aes(State.Name, fill = State.Name)) + geom_bar() + theme_wsj()+ scale_colour_wsj("colors6") + coord_flip()

#getting the new plot for network type as well
ggplot(data, aes(Network.Type, fill = Network.Type)) + geom_bar() + theme_wsj()+ scale_colour_wsj("colors6")

ggplot() + 
  geom_boxplot(aes(y = data$Rating)) + 
  scale_x_discrete( ) +
  ylim(c(1,5)) +
  labs(title = "Rating by customers for April 2020",
       y = "Rating") + theme_wsj()+ scale_colour_wsj("colors6")


#getting updated summary on the data
summary(data)
View(data)
write.csv(data, "cleaned-data.csv", row.names = FALSE)


#---1. Rank the operators based on user satisfaction ---------------------------------------------

sortedRating = names(sort(table(data$Rating), decreasing = TRUE))
sortedRating

operatorStats = matrix(data = NA, nrow = 6, ncol = 3, dimnames = list(c(levels(operatorVal)), c("avgRating", "#ratings=1", "#ratings=5")))

#getting mean rating, #low ratings and #high ratings for all operators
i = 1
for(x in operatorVal){
   avgRating = mean(data$Rating[which(data$Operator == x)], trim = 0, na.rm = FALSE)
   lowRating = NROW(which(data$Operator == x & data$Rating == 1))
   highRating = NROW(which(data$Operator == x & data$Rating == 5))
   operatorStats[i,] = c(avgRating, lowRating, highRating)
   i = i +1
}
  
print(operatorStats)

sprintf("operators sorted based on their average ratings")
avgRating = sort(operatorStats[,1], decreasing = TRUE)

sprintf("operators sorted based on their #ratings = 1")
lowRating = sort(operatorStats[,2], decreasing = TRUE)

sprintf("operators sorted based on their #ratings = 5")
highRating = sort(operatorStats[,3], decreasing = TRUE)

#plotting the respective ratings and #records
library(lattice)
barchart(avgRating)
barchart(lowRating)
barchart(highRating)

#--------------2. Which operator has the most call drops -------------------------------------------


callDropRate = c()
for(x in operatorVal)
    callDropRate = append(callDropRate, NROW(which(data$Operator == x & data$Call.Drop.Category == "Call Dropped")))

#appending the callDropRate column onto the operatorStats
operatorStats = cbind(operatorStats, callDropRate)

sprintf("operators sorted based on their call drop rate")
callDropRank = sort(operatorStats[,4], decreasing = TRUE)
callDropRank
#plotting the graph
barchart(callDropRank)

#----------3. Finding relation between user satisfaction and frequency band of the network----------

data<-read.csv("cleaned-data.csv", header=TRUE)
# Get the subset of the dataset with only rating and network type columns
df3 = data[,c('Network.Type', 'Rating','Call.Drop.Category')]

# Plot a pie chart of popularity of different types of networks
networkCount = table(df3[,'Network.Type'])
networkCountDf = data.frame(networkCount)
colnames(networkCountDf)[1] <- 'NetworkType'
piepercent = as.array(round(100*networkCount/sum(networkCount),1))

pielabels = piepercent
for (i in 1:dim(piepercent)){
  pielabels[i] = paste(as.character(pielabels[i]),"%", sep="")
}

ggplot(networkCountDf, aes(x = "", y = piepercent, fill = NetworkType)) +
  geom_col(color = "black") +
  geom_label(aes(label = pielabels), color = c("white", "white", "white"),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  ggtitle("Pie Chart representing popularity of network type")+
  guides(fill = guide_legend(title = "NetworkType")) +
  scale_fill_manual(values=c('#3e065c','#cd2051','#ffa600')) +
  coord_polar(theta = "y") + 
  theme_void()+  theme(plot.title = element_text(hjust = 0.5))


# Getting the mean rating for the 3 network types
mean(df3[df3$Network.Type == '2G','Rating'])
mean(df3[df3$Network.Type == '3G','Rating'])
mean(df3[df3$Network.Type == '4G','Rating'])

# Get weighted frequencies of ratings within each network type
networkVsRating = table(df3[,c('Network.Type', 'Rating')])
networkVsRatingDf = data.frame(networkVsRating)
networkVsRatingDf

probs_table = networkVsRating
for (i in 1:dim(probs_table)[1])
  probs_table[i,] = probs_table[i,] / sum(probs_table[i,])
probs_table

probDf = data.frame(probs_table)
rating = probDf[,'Rating']
network = probDf[,'Network.Type']
probs = probDf[,'Freq']

# Plotting the bar graph of weighted frequencies
ggplot(probDf,aes(x=rating,y=probs, fill=network)) +
  geom_bar(stat="identity", width=0.8, position=position_dodge(0.85), alpha=1.0) +
  ggtitle("Comparison of different network types based on rating") + 
  xlab("Rating")+
  ylab("Weighted Frequencies")+
  scale_fill_manual(values=c('#3e065c','#cd2051','#ffa600'))+
  labs(fill="NetworkType")

# Plotting percentage bar graph to see distribution of ratings within each network.
ggplot(networkVsRatingDf,aes(x=Network.Type,y=Freq, fill=Rating)) +
  geom_bar(stat="identity", width=0.5,position="fill") +
  ggtitle("Percentage of different ratings for each network type") + 
  xlab("Network Type")+
  ylab("Percentage Frequency")+
  scale_fill_manual(values=c("#d43d51","#ed9568","#ffdfaa","#b0b561","#488f31"))+
  labs(fill="Rating")


# Analysing the impact of network type on CallDropCat
networkVsCallDrop = table(df3[,c('Network.Type', 'Call.Drop.Category')])
networkVsCallDropDf = data.frame(networkVsCallDrop)

# Plot percentage bar graph of different ratings within each network type
ggplot(networkVsCallDropDf,aes(x=Network.Type,y=Freq, 
                               fill=Call.Drop.Category)) +
  geom_bar(stat="identity", width=0.5, alpha=1.0,position="fill") +
  ggtitle("Percentage barchart for comparing call drop quality for different network types") + 
  xlab("Network type")+
  ylab("Percentage")+
  scale_fill_manual(values=c("#004c6d","#2a99b9","#62efff"))+
  labs(fill="CallDropCat")


#------4(a) Finding any association between location type & rating---------------------------------
df4 <- data[,c('Operator','In.Out.Travelling','Rating','Call.Drop.Category')]
colnames(df4)[2] = 'LocType'
colnames(df4)[4] = 'CallDropCat'
df4

df4a = df4[,c('LocType','Rating')]
df4a_freq = data.frame(table(df4a))

freq = df4a_freq[,'Freq']
rating = df4a_freq[,'Rating']
locType = df4a_freq[,'LocType']

# Draw a combined bar plot
ggplot(df4a_freq,aes(x=rating,y=freq, fill=locType)) +
  geom_bar(stat="identity", width=0.8, alpha=0.7,position=position_dodge(0.85)) +
  ggtitle("Comparison of location types on the basis of rating") + 
  xlab("Rating")+
  ylab("Frequencies")+
  labs(fill="LocType")

# Function to compute weighted frequencies
getWeightedFreq <- function(frequencies){
  weighted_freq = frequencies
  for (i in 1:dim(weighted_freq)[1])
    weighted_freq[i,] = weighted_freq[i,] / sum(weighted_freq[i,])
  print(weighted_freq)
  data.frame(weighted_freq)
}

df4a_weighted = getWeightedFreq(table(df4a))
weighted_freq = df4a_weighted[,'Freq']

# Plot combined bar graphs with weighted frequencies
ggplot(df4a_weighted,aes(x=rating,y=weighted_freq, fill=locType)) +
  geom_bar(stat="identity", width=0.8, alpha=0.7,position=position_dodge(0.85)) +
  ggtitle("Comparison of Weighted Frequencies") + 
  xlab("Rating")+
  ylab("Weighted Frequencies")+
  labs(fill="LocType")

# Plot a percentage bar graph for further interpretability
ggplot(df4a_freq,aes(x=locType,y=freq, fill=rating)) +
  geom_bar(stat="identity", width=0.5,position="fill") +
  ggtitle("Percentage Stacked barchart") + 
  xlab("LocType")+
  ylab("Percentage Frequency")+
  scale_fill_manual(values=c("#d43d51","#ed9568","#ffdfaa","#b0b561","#488f31"))+
  labs(fill="Rating")



#------4(b)Finding any association between location type & call drops-------------------------------
locTypeVsCallDrop = table(df4[,c('LocType','CallDropCat')])
locTypeVsCallDrop

locTypeVsCallDropDf = data.frame(locTypeVsCallDrop)

# Plot a stacked barchat with LocType on X axis
ggplot(locTypeVsCallDropDf,aes(x=locTypeVsCallDropDf$LocType,y=locTypeVsCallDropDf$Freq, 
                               fill=locTypeVsCallDropDf$CallDropCat)) +
  geom_bar(stat="identity", width=0.5, alpha=1.0,position="fill") +
  ggtitle("Percentage barchart for comparing call drop quality for different location status") + 
  xlab("LocType")+
  ylab("Percentage")+
  scale_fill_manual(values=c("#004c6d","#2a99b9","#62efff"))+
  labs(fill="CallDropCat")


#-------5.Comparison of operators in terms of indoor, outdoor and travelling network quality--------

# Indoor Quality for different operators in terms of CallDropCat  
indoorTable = table(df4[df4$LocType=='Indoor',c('Operator','CallDropCat')])
indoorTableDf = data.frame(indoorTable)
indoorTable

ggplot(indoorTableDf, aes(x=Operator,y=Freq, fill=CallDropCat)) +
  geom_bar(stat="identity", width=0.5, alpha=1.0,position="fill") +
  ggtitle("Percentage Stacked barchart for comparing indoor call quality for different operators") + 
  xlab("Operator")+
  ylab("Percentage Frequency")+
  scale_fill_manual(values=c("#004c6d","#2a99b9","#62efff"))+
  labs(fill="Calldrop category")


# Outdoor Quality for different operators in terms of CallDropCat
outdoorTable = table(df4[df4$LocType=='Outdoor',c('Operator','CallDropCat')])
outdoorTableDf = data.frame(outdoorTable)
outdoorTable

ggplot(outdoorTableDf, aes(x=outdoorTableDf[,'Operator'],y=outdoorTableDf[,'Freq'], fill=outdoorTableDf[,'CallDropCat'])) +
  geom_bar(stat="identity", width=0.5, alpha=1.0,position="fill") +
  ggtitle("Percentage Stacked barchart for comparing Outdoor call quality for different operators") + 
  xlab("Operator")+
  ylab("Percentage Frequency")+
  scale_fill_manual(values=c("#004c6d","#2a99b9","#62efff"))+
  labs(fill="Calldrop category")

# Travelling Quality for different operators in terms of CallDropCat
travelTable = table(df4[df4$LocType=='Travelling',c('Operator','CallDropCat')])
travelTableDf = data.frame(travelTable)
travelTable
ggplot(travelTableDf, aes(x=travelTableDf[,'Operator'],y=travelTableDf[,'Freq'], fill=travelTableDf[,'CallDropCat'])) +
  geom_bar(stat="identity", width=0.5, alpha=1.0,position="fill") +
  ggtitle("Percentage Stacked barchart for comparing Travelling call quality for different operators") + 
  xlab("Operator")+
  ylab("Percentage Frequency")+
  scale_fill_manual(values=c("#004c6d","#2a99b9","#62efff"))+
  labs(fill="Calldrop category")

#----------8. Call Quality in Various Regions of Country----------
library(sf)
library(tidyverse)
library(ggplot2)
library(plyr)

india_official <- st_read("India-Map/India_State_Boundary.shp")
head(india_official)

regionTable = data[complete.cases(data), ] # remove all NA value rows
head(regionTable)
pnts_sf <- st_as_sf(regionTable, coords = c('Longitude', 'Latitude'), crs = st_crs(4326))
pnts_trans <- st_transform(pnts_sf, 2163)
map_trans <- st_transform(india_official, 2163)

pnts_intersections <- pnts_sf %>% mutate(
  ID = as.integer(st_intersects(pnts_trans, map_trans)))

aggr_pnts <- pnts_intersections %>% group_by(ID) %>%
  summarise_at(vars(Rating), list(name = mean))

aggr_pnts_df <- as.data.frame(aggr_pnts)
india_official_df <- as.data.frame(india_official)
india_official_df$ID <- 1:37

res <- merge(x = india_official_df,y = aggr_pnts_df, by="ID", all.x = TRUE)

india_official$RATING <- res$name

library(viridis)
ggplot(india_official) + 
  geom_sf(aes(fill=RATING)) +
  scale_fill_viridis(discrete=FALSE)