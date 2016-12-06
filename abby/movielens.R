setwd("C:/Users/abmarkowitz/Documents")

#import file with user id, movie id, rating, timestamp, then name columns.
ratingsdata <- read.table("u.data.txt", header=FALSE)
summary(ratingsdata)
names(ratingsdata) <- c("userID", "movieID", "rating", "timestamp")

#import file with movie title, year, genre, then name columns.
moviedata <- read.delim("u.item.txt", sep="|", header=FALSE)
names(moviedata) <- c("movieID", "movie title", "release date", "video release date", "IMDb URL", "unknown", "Action", 
                      "Adventure", "Animation", "Childrens", "Comedy", "Crime", "Documentary", "Drama", "Fantasy", 
                      "Film-Noir", "Horror", "Musical", "Mystery", "Romance", "Sci-Fi", "Thriller", "War", "Western")

#join movie title/year/genre from moviedata to ratingsdata. compare movieID field to make sure valid to merge on.
summary(ratingsdata$movieID)
summary(moviedata$movieID)
datamerge1 <- merge(ratingsdata, moviedata, by="movieID")
summary(datamerge1)

#import file with reviewer gender, age, zip code, then name columns
setwd("C:/Users/abmarkowitz/Documents")
reviewerdata <- read.delim("u.reviewer.txt", sep="|", header=FALSE)
names(reviewerdata) <- c("userID", "age", "gender", "occupation", "zip")

#merge reviewer data with rest of data
summary(datamerge1$userID)
summary(reviewerdata$userID)
datamerge2 <- merge(datamerge1, reviewerdata, by="userID")
summary(datamerge2)

#import zip code data, clean quotations
zipdata <- read.csv("zips.csv", header=TRUE)
names(zipdata) <- c("zip", "state ab", "lat", "long", "city", "state")
zipdata <- as.data.frame(sapply(zipdata, function(x) gsub("\"", "", x)))
summary(zipdata)

#merge zips data with rest of data.
library(dplyr)
datamerge2$zip <- as.character(datamerge2$zip)
zipdata$zip <- as.character(zipdata$zip)
datamerge2$zip <- as.factor(datamerge2$zip)
zipdata$zip <- as.factor(zipdata$zip)
zipdata$long <- as.numeric(as.character(zipdata$long))
zipdata$lat <- as.numeric(as.character(zipdata$lat))
finaldata <- left_join(datamerge2, zipdata, by="zip")

#drop info dont need
finaldata$`video release date`<- NULL
finaldata$occupation <- NULL
finaldata$city <- NULL
finaldata$`state ab`<- NULL
finaldata$`IMDb URL`<- NULL

#create year variable based off of release date, drop release data
finaldata$year <- as.character(finaldata$`release date`)
finaldata$year <- substring(finaldata$year,8,11)
finaldata$year <- as.numeric(finaldata$year)
finaldata$`release date`<- NULL

#summary table of average review by genre for each gender
genre <- c("Unknown", "Action", "Adventure", "Animated", "Children", "Comedy", "Crime", "Documentary", "Drama", 
           "Fantasy", "Film_Noir", "Horror", "Musical", "Mystery", "Romantic", "SciFi", "Thriller", "War", "Western") 
mreviews <- c()
freviews <- c()

for (i in 9:17){ 
mreviewer_df <- filter(finaldata, finaldata[i]==1, gender=="M") 
mreviews[(i-8)] <- c(mean(mreviewer_df[["rating"]])) 
 } 
male_ratings <- cbind(mreviews, genre) 

for (i in 9:17){ 
freviewer_df <- filter(finaldata, finaldata[i]==1, gender=="F") 
freviews[(i-8)] <- c(mean(freviewer_df[["rating"]])) 
 } 
female_ratings <- cbind(freviews, genre)

ggdata <- data.frame(cbind(male_ratings, female_ratings))[-4] 
ggdata <- ggdata[c(2,1,3)] 
colnames(ggdata) <- c("Genre", "Male", "Female") 
ggdata$Female <- as.numeric(as.character(ggdata$Female)) 
ggdata$Male <- as.numeric(as.character(ggdata$Male)) 

# visualiation - melt data to put into ggplot 
graph_data <- melt(ggdata, id="Genre") 
colnames(graph_data) <- c("Genre", "Gender", "Rating") 
ggplot(graph_data, aes(x=Genre, y=Rating, fill=Gender)) +  
geom_bar(stat="identity", position = "dodge") +  
ggtitle("Average Rating by Genre and Gender") + 
ylab("Average Rating") + theme(plot.title = element_text(hjust = .5)) 

#generate a heat map in ggmap for the entire country by long/lat of count of reviews.
#hotter if more reviews from that area, cooler if less
library(ggplot2)
library(ggmap)
library(RColorBrewer)
library(reshape2)
mymap <- get_map(location='United States', zoom=4, maptype= 'roadmap', source='google', color='color')
ggmap(mymap, extent="device") + geom_density2d(data = finaldata, aes(x = finaldata$long, y = finaldata$lat), size = 0.3) + 
                                  stat_density2d(data = finaldata, aes(x = finaldata$long, y = finaldata$lat, fill = ..level.., alpha =.5), 
                                  geom='polygon', size = 0.01, bins = 16) + 
                                  scale_fill_gradient(low = "pink", high = "red", guide=FALSE) + scale_alpha(range = c(0, 0.3), guide = FALSE)

#export to csv
write.csv(finaldata, file="Movie Ratings Final Data")

