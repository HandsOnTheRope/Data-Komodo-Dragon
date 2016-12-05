#libraries

install.packages ("stringr")
library(stringr)
install.packages("dplyr")
library (dplyr)
library(ggplot2)
install.packages("reshape")
library(reshape)
install.packages("doBy")
library(doBy)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("ggmap")
library(ggmap)
install.packages("plyr")
library(plyr)
library(stringr)


#Reading in Data
UData<- read.delim ("C:/Users/mmigdalovich/Documents/movielens/ml-100k/u.data", header = FALSE)
UItem<- read.delim ("C:/Users/mmigdalovich/Documents/movielens/ml-100k/u.item", header = FALSE, sep="|")
UUser<- read.delim ("C:/Users/mmigdalovich/Documents/movielens/ml-100k/u.user", header = FALSE, sep="|")
UGenre<- read.delim ("C:/Users/mmigdalovich/Documents/movielens/ml-100k/u.genre", header = FALSE, sep="|")
ZipData <- read.csv("C:/Users/mmigdalovich/Documents/movielens/ml-100k/zips.csv")

#Labeling Data (itemid--> movie id)
names(UData) <- c("userid","movieid","rating", "timestamp") 
names(UItem) <- c("movieid","movietitle","releasedate", "videoreleasedate", "Imdburl", "unknown", "action", "adventure","animation", "childrens","comedy","crime", "documentary", "drama", "fantasy", "film-noir", "horror", "musical","mystery,","romance", "sci-fi", "thriller", "war", "western")
names(UUser) <- c("userid","age","gender", "occupation", "zipcode") 
names(ZipData) <- c("zipcode","stateabbreviations","latitude","longtitude", "city", "state") 
TotalMerg <-TotalMerg[-11]

#Merging Data
Merg1 <- merge(UUser, UData, by="userid", all.x=TRUE)
Merg2 <- merge(Merg1, UItem, by="movieid", all.x=TRUE)
TotalMerg <-merge(Merg2, ZipData, by="zipcode", all.x=TRUE)
summary(TotalMerg)

#Data Cleaning:

#clean TimeStamp
TotalMerg$timestamp <- as.Date(as.POSIXct(TotalMerg$timestamp,origin="1970-01-01"))

names(TotalMerg)[20] <- "film_noir"
names(TotalMerg)[23] <- "mystery"
names(TotalMerg)[25] <- "sci_fi"


#clean up Releasedate
TotalMerg$MOVIEYEAR <- str_sub(TotalMerg$movietitle, -5,-2)
TotalMerg$movietitle <-str_sub(TotalMerg$movietitle, end= -8)

#deleting imdb & movie release date
TotalMerg <-TotalMerg[-10]
TotalMerg <-TotalMerg[-10]

#zips
TotalMerg$zipcode <-as.character(TotalMerg$zipcode)
TotalMerg$zipcode <-as.factor(TotalMerg$zipcode)

#Longitude
del <- colwise(function(TotalMerg) str_replace_all(TotalMerg, '"',""))
TotalMerg <-del(TotalMerg)

#Change from Factor to number
TotalMerg$Longitude <- as.numeric(TotalMerg$longtitude)
TotalMerg$Latitude <- as.numeric(TotalMerg$latitude)

#Table: Displays average Review for each genre by Gender

#new dataframe
ratingsgendergenre <- select(TotalMerg,userid,rating, gender, unknown, action, 
                             adventure, animation, childrens, comedy, crime, 
                             documentary, drama, fantasy, film_noir, horror, 
                             musical, mystery, romance, sci_fi, thriller, 
                             war, western)
#combine all genre names
genre <- c("unknown", "action", 
          "adventure", "animation", "childrens", "comedy", "crime", 
          "documentary", "drama", "fantasy", "film_noir", "horror", 
          "musical", "mystery", "romance", "sci_fi", "thriller", 
          "war", "western")

#vectors, average ratings

#male
m_final <-c()
f_final <- c()
for (z in 4:22) {
  m_all_df <-filter(ratingsgendergenre, ratingsgendergenre[z]==1, gender=="M")
  m_final[(z-4)] <- c(mean(m_all_df[["rating"]])) 
}
maleratings <- cbind(m_final,genre)

#female
for (z in 4:22) {
  f_all_df <-filter(ratingsgendergenre, ratingsgendergenre[z]==1, gender=="F")
  f_final[(z-4)] <- c(mean(f_all_df[["rating"]])) }

femaleratings <- cbind(f_final,genre)

#combine into 1 dataframe

MF_DF <-data.frame(cbind(maleratings, femaleratings))[-2]

#reorder
MF_DF <- MF_DF[c(3,1,2)]

#rename
names(MF_DF) <- c("Genre", "Male", "Female")

MF_DF$Female <-as.numeric(as.character(MF_DF$Female))
MF_DF$Male <-as.numeric(as.character (MF_DF$Male))

#"melt" data for plotting ** not sorting by gender**
Graphdata<- melt(MF_DF,id="Genre")
colnames(Graphdata) <-c("Genre", "Gender", "Rating")
ggplot(Graphdata, aes(x= Genre, y=Rating, fill=Gender))+
  geom_bar(stat="identity", position="dodge")+ggtitle("Average Movie Rating by Genre and Gender")+
  ylab("Average Rating")+theme(plot.title=element_text(hjust =.5))
                             

#Heatmap by Longitude and Latitude based on Number of Total Reviews

USMAP <-get_map(location='United States', zoom=4, maptype="roadmap",
                source='google', color='color')


ggmap(USMAP, extent ="device")+
  geom_density2d(data=TotalMerg, aes(x=Longitude, y=Latitude), size=.3)+
  stat_density2d(data= TotalMerg, aes(x=Longitude, y=Latitude, fill = ..level..,alpha=.5),
                size=0.01, bins = 16, geom ="polygon") +
  scale_fill_gradient(low="darkturquoise", high ="blue4", guide= FALSE) +
  scale_alpha(range = c(0,0.3), guide=FALSE)
            
