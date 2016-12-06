# install.packages('ggmap')
library(ggmap)
# install.packages('maps')
library(maps)
# install.packages('mapdata')
library(mapdata)
# install.packages('plyr')
library(plyr)
# install.packages('stringr')
library(stringr)
# install.packages('ggplot2')
library(ggplot2)
# install.packages('readr')
library(readr)



##################################################################################
###################################CLEAN / COMPILE DATA###########################
##################################################################################

##### UDATA
udata <- read_delim("~/R/Movie Lens/u.data", "\t", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
### Put Headings on udata
udataHeadings <- c('User ID','Movie ID','Rating','Seconds After 1/1/1970 00:00:00')
names(udata) <- udataHeadings
### Unix Time - Add Time Stamp to udata
str(udata$`Seconds After 1/1/1970 00:00:00`)
udata$'Time Stamp' <- as.POSIXct(udata$`Seconds After 1/1/1970 00:00:00`,origin = '1970-01-01', tz="EST")


##### UUSER
uuser <- read_delim("~/R/Movie Lens/u.user", "|", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
### Put Headings on uuser
uuserHeadings <- c('User ID','Age','Gender','Occupation','Zip Code')
names(uuser) <- uuserHeadings


##### UITEM
uitem <- read_delim("~/R/Movie Lens/u.item", "|", escape_double = FALSE, col_names = FALSE, trim_ws = TRUE)
### Put Headings on uitem
uitemHeadings <- c('Movie ID','Title','Release Date','Release Year','URL','unknown genre','Action','Adventure',
                   'Animation',"Children's",'Comedy','Crime','Documentary','Drama','Fantasy','Film-Noir',
                   'Horror','Musical','Mystery','Romance','Sci-Fi','Thriller','War','Western')
names(uitem) <- uitemHeadings
### Separate Year from Title in uitem
uitem$`Release Year`<- str_sub(uitem$Title,-5,-2)
### Return only Title in Title (Year) column
uitem$Title <- gsub('.{7}$', '', uitem$Title)


##### ZIPS
zips <- read_csv("~/R/Movie Lens/zips.csv")
### Change Headings in zips
zipsHeadings <- c('Zip Code','State Abbreviation', 'Latitude', 'Longitude', 'City', 'State')
names(zips) <- zipsHeadings
### Get quotes out of zips
zips$`State Abbreviation` <- str_sub(zips$`State Abbreviation`,2,-2)
zips$Latitude <- str_sub(zips$Latitude,2,-2)
zips$Longitude <- str_sub(zips$Longitude,2,-2)
zips$City <- str_sub(zips$City,2,-2)
zips$State <- str_sub(zips$State,2,-2)
### Make Latitude & Longitude numeric
zips$Latitude <- as.numeric(as.character(zips$Latitude))
zips$Longitude <- as.numeric(as.character(zips$Longitude))



##### Merging Datasets
dataAndUser = merge(udata,uuser,by="User ID")
dataAndUserAndItem = merge(dataAndUser,uitem,by="Movie ID")
completeData = merge(dataAndUserAndItem,zips,by="Zip Code",all.x=TRUE)


# write.csv(completeData, file = "C:/Users/makhalil/Documents/R/Movie Lens.csv"





###############################################################################
###############################SUMMARY TABLE###################################
###############################################################################

##############################################################################
# lapply - list of inputs, function <-------------- try this out, from Callie#
##############################################################################

#Make everything numeric
ratingsData$Gender <- as.factor(as.character(ratingsData$Gender))
ratingsData$Action <- as.numeric(as.character(ratingsData$Action))
ratingsData$Adventure <- as.numeric(as.character(ratingsData$Adventure))
ratingsData$Animation <- as.numeric(as.character(ratingsData$Animation))
ratingsData$`Children's`<- as.numeric(as.character(ratingsData$`Children's`))
ratingsData$Comedy <- as.numeric(as.character(ratingsData$Comedy))
ratingsData$Crime <- as.numeric(as.character(ratingsData$Crime))
ratingsData$Documentary <- as.numeric(as.character(ratingsData$Documentary))
ratingsData$Drama <- as.numeric(as.character(ratingsData$Drama))
ratingsData$Fantasy <- as.numeric(as.character(ratingsData$Fantasy))
ratingsData$`Film-Noir` <- as.numeric(as.character(ratingsData$`Film-Noir`))
ratingsData$Horror <- as.numeric(as.character(ratingsData$Horror))
ratingsData$Musical <- as.numeric(as.character(ratingsData$Musical))
ratingsData$Mystery <- as.numeric(as.character(ratingsData$Mystery))
ratingsData$Romance <- as.numeric(as.character(ratingsData$Romance))
ratingsData$`Sci-Fi` <- as.numeric(as.character(ratingsData$`Sci-Fi`))
ratingsData$Thriller <- as.numeric(as.character(ratingsData$Thriller))
ratingsData$War <- as.numeric(as.character(ratingsData$War))
ratingsData$Western <- as.numeric(as.character(ratingsData$Western))

# Complete Data for Action Genre
actionData <- subset(completeData, completeData$Action == 1)
# Average Ratings of Action by Gender
actionRatings <- ddply(actionData,.(Gender),function(x) mean(x$Rating))
# ratingsData shows average ratings for all Genres
ratingsData <-actionRatings

adventureData <- subset(completeData, completeData$Adventure == 1)
adventureRatings <- ddply(adventureData,.(Gender),function(x) mean(x$Rating))
# Merge data
ratingsData <- merge(ratingsData,adventureRatings,by="Gender")

animationData <- subset(completeData, completeData$Animation == 1)
animationRatings <- ddply(animationData,.(Gender),function(x) mean(x$Rating))
ratingsData <- merge(ratingsData,animationRatings, by='Gender')

childrensData <- subset(completeData, completeData$`Children's` == 1)
childrensRatings <- ddply(childrensData,.(Gender),function(x) mean(x$Rating))
ratingsData <- merge(ratingsData,childrensRatings, by='Gender')

comedyData <- subset(completeData, completeData$Comedy == 1)
comedyRatings <- ddply(comedyData,.(Gender),function(x) mean(x$Rating))
ratingsData <- merge(ratingsData,comedyRatings, by='Gender')

crimeData <- subset(completeData, completeData$Crime == 1)
crimeRatings <- ddply(crimeData,.(Gender),function(x) mean(x$Rating))
ratingsData <- merge(ratingsData,crimeRatings, by='Gender')

documentaryData <- subset(completeData, completeData$Documentary == 1)
documentaryRatings <- ddply(documentaryData,.(Gender),function(x) mean(x$Rating))
ratingsData <- merge(ratingsData,documentaryRatings, by='Gender')

dramaData <- subset(completeData, completeData$Drama == 1)
dramaRatings <- ddply(dramaData,.(Gender),function(x) mean(x$Rating))
ratingsData <- merge(ratingsData,dramaRatings, by='Gender')

fantasyData <- subset(completeData, completeData$Fantasy == 1)
fantasyRatings <- ddply(fantasyData,.(Gender),function(x) mean(x$Rating))
ratingsData <- merge(ratingsData,fantasyRatings, by='Gender')

filmnoirData <- subset(completeData, completeData$`Film-Noir` == 1)
filmnoirRatings <- ddply(filmnoirData,.(Gender),function(x) mean(x$Rating))
ratingsData <- merge(ratingsData,filmnoirRatings, by='Gender')

horrorData <- subset(completeData, completeData$Horror == 1)
horrorRatings <- ddply(horrorData,.(Gender),function(x) mean(x$Rating))
ratingsData <- merge(ratingsData,horrorRatings, by='Gender')

musicalData <- subset(completeData, completeData$Musical == 1)
musicalRatings <- ddply(musicalData,.(Gender),function(x) mean(x$Rating))
ratingsData <- merge(ratingsData,musicalRatings, by='Gender')

mysteryData <- subset(completeData, completeData$Mystery == 1)
mysteryRatings <- ddply(mysteryData,.(Gender),function(x) mean(x$Rating))
ratingsData <- merge(ratingsData,mysteryRatings, by='Gender')

romanceData <- subset(completeData, completeData$Romance == 1)
romanceRatings <- ddply(romanceData,.(Gender),function(x) mean(x$Rating))
ratingsData <- merge(ratingsData,romanceRatings, by='Gender')

scifiData <- subset(completeData, completeData$`Sci-Fi` == 1)
scifiRatings <- ddply(scifiData,.(Gender),function(x) mean(x$Rating))
ratingsData <- merge(ratingsData,scifiRatings, by='Gender')

thrillerData <- subset(completeData, completeData$Thriller == 1)
thrillerRatings <- ddply(thrillerData,.(Gender),function(x) mean(x$Rating))
ratingsData <- merge(ratingsData,thrillerRatings, by='Gender')

warData <- subset(completeData, completeData$War == 1)
warRatings <- ddply(warData,.(Gender),function(x) mean(x$Rating))
ratingsData <- merge(ratingsData,warRatings, by='Gender')

westernData <- subset(completeData, completeData$Western == 1)
westernRatings <- ddply(westernData,.(Gender),function(x) mean(x$Rating))
ratingsData <- merge(ratingsData,westernRatings, by='Gender')

ratingsDataHeadings <- c('Gender','Action','Adventure','Animation',"Children's",'Comedy','Crime','Documentary','Drama',
                         'Fantasy','Film-Noir','Horror','Musical','Mystery','Romance','Sci-Fi','Thriller','War','Western')
names(ratingsData) <- ratingsDataHeadings


###########################################################################################
###################################RATINGS DATA BAR GRAPH##################################
###########################################################################################
# install.packages('reshape2')
library(reshape2)

meltedRatings <- melt(ratingsData, id="Gender")
meltedRatingsHeadings <- c('Gender','Genre','Rating')
names(meltedRatings) <- meltedRatingsHeadings
ggplot(meltedRatings, aes(x=Genre, y=Rating, fill=Gender)) + geom_bar(stat="identity", position = "dodge") +
  ggtitle("Movie Rating by Genre") + ylab("Rating") + theme(plot.title = element_text(hjust = .5))

###########################################################################################
############################################HEAT MAP#######################################
###########################################################################################

us_map <- get_map(location='united states', zoom=4, maptype="roadmap",source='google',
                  color='color')
ggmap(us_map,extent="device") +
  geom_density2d(data = completeData, aes(x=Longitude, y=Latitude),size=0.5) +
  stat_density2d(data = completeData, aes(x=Longitude, y=Latitude, fill= ..level.., alpha=0.5),
                 size = 0.01, bins = 25, geom = "polygon") + 
  scale_fill_gradient(low = "blue",high = "red", guide= FALSE) +
  scale_alpha(range = c(0,0.25), guide = FALSE)
