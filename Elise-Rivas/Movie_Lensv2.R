# Join and create a dataframe with UserID, MovieID,
# MovieTitle, MovieYear, Rating, Timestamp, Title, 
# Genre, Gender of Reviewer, Age of Reviewer, 
# Zip Code, State, Longitute, Lattitude

# Create a summary table for display of average review by genre for each gender. Visualize that table.
# Create a heatmap by long/lat for total count of reviews using a national map.	
# Also, export your newly joined dataframe to a csv. 
# Primary libraries you will have to call will include plyr or dplyr as well as ggmap.

library(plyr)
library(dplyr)
library(ggplot2)
library(ggmap)
library(stringr)
library(doBy)
library(reshape2)
library(RColorBrewer)

# Load in data
setwd("C:/Users/elrivas/Documents/Trainings/R Training/movielens/ml-100k")

ratings <- read.table("u.data", sep="\t")
zips <- read.csv("zips.csv", header=TRUE)
user_info <- read.delim("user_info.user", sep="|", header=FALSE)
movie_info <- read.delim("u.item", sep="|", header=FALSE)

### Clean data and assign variable names ###

# ratings
names(ratings) <- c("User_ID", "Movie_ID", "Rating", "Timestamp")
ratings$Timestamp <- as.Date(as.POSIXct(ratings$Timestamp, origin="1970-01-01"))
# movie_info, delete column 4 which is empty
movie_info <- movie_info[-4]
names(movie_info) <- c("Movie_ID", "Title", "Video_Release_Date",
                       "IMDB_URL", "Unknown", "Action", 
                       "Adventure", "Animated",
                       "Children", "Comedy", "Crime", 
                       "Documentary", "Drama",
                       "Fantasy", "Film_Noir", 
                       "Horror", "Musical", "Mystery",
                       "Romantic", "SciFi", "Thriller", 
                       "War", "Western")

movie_info$Movie_Year <- str_sub(movie_info$Title,-5,-2)
movie_info$Title <- str_sub(movie_info$Title, end=-8)
#user_info
names(user_info) <- c("User_ID", "Age", "Gender", "Occupation", "Zip")
user_info$Zip <- as.character(user_info$Zip)
user_info$Zip <- as.factor(user_info$Zip)
#zips
names(zips) <- c("Zip", "State_Abbr", "Lat", "Long", "City", "State")
zips <- as.data.frame(sapply(zips, function(x) gsub("\"", "", x)))
zips[] <- lapply(zips, as.character)
zips[,c(1,2,5,6)] <- lapply(zips[,c(1,2,5,6)], as.factor)
zips[,c(3,4)] <- lapply(zips[,c(3,4)], as.numeric)

### Merging ###

# Adding City, State, Lat, and Long to user_info from zips
user_info <- left_join(user_info, zips, by = "Zip")
# Adding movie_info to ratings
ratings <- left_join(ratings, movie_info, by = "Movie_ID")
# Adding user_info to ratings
ratings <- left_join(ratings, user_info, by = "User_ID")
# Omit uneeded variabls
ratings <- ratings[,c(-6, -7, -30, -32, -35)]
# Reorder columns
ratings <- ratings[c(1,2,5,25,3,4,6:24,27,26,28,31,30,29)]
# export rating to csv
write.csv(ratings, file="ratings.csv")

### Table visualizing average review for each genre by gender ###

# New data frame with needed variables
ratingsgg <- select(ratings, User_ID, Rating, Gender, Unknown, Action, Adventure,
                    Animated, Children, Comedy, Crime, Documentary, Drama,
                    Fantasy, Film_Noir, Horror, Musical, Mystery, Romantic, SciFi,
                    Thriller, War, Western)
# Vector of genre names
genre <- c("Unknown", "Action", "Adventure",
           "Animated", "Children", "Comedy", "Crime", "Documentary", "Drama",
           "Fantasy", "Film_Noir", "Horror", "Musical", "Mystery", "Romantic", "SciFi",
           "Thriller", "War", "Western")
# Vectors for storing average ratings
m_review <- c()
f_review <- c()
for (i in 3:22){
  mreviewer_df <- filter(ratingsgg, ratingsgg[i]==1, Gender=="M")
  m_review[(i-3)] <- c(mean(mreviewer_df[["Rating"]]))
}
male_ratings <- cbind(m_review, genre)
for (i in 3:22){
  freviewer_df <- filter(ratingsgg, ratingsgg[i]==1, Gender=="F")
  f_review[(i-3)] <- c(mean(freviewer_df[["Rating"]]))
}
female_ratings <- cbind(f_review, genre)
# Combine
ggdata <- data.frame(cbind(male_ratings, female_ratings))[-4]
ggdata <- ggdata[c(2,1,3)]
colnames(ggdata) <- c("Genre", "Male", "Female")
ggdata$Female <- as.numeric(as.character(ggdata$Female))
ggdata$Male <- as.numeric(as.character(ggdata$Male))
# Melt data to put into ggplot
graph_data <- melt(ggdata, id="Genre")
colnames(graph_data) <- c("Genre", "Gender", "Rating")
ggplot(graph_data, aes(x=Genre, y=Rating, fill=Gender)) + 
  geom_bar(stat="identity", position = "dodge") + 
  ggtitle("Average Rating by Genre and Gender") +
  ylab("Average Rating") + theme(plot.title = element_text(hjust = .5))

### Heatmap by long and lat for total # of reviews ###

us_map <- get_map(location='United States', zoom=4, maptype = "roadmap",
                       source='google',color='color')

  ggmap(us_map, extent = "device") + 
  geom_density2d(data = ratings, aes(x = Long, y = Lat), size = 0.3) + 
  stat_density2d(data = ratings, aes(x = Long, y = Lat, fill = ..level.., alpha=.5), 
                 size = 0.01, bins = 16, geom = "polygon") + 
  scale_fill_gradient(low = "darkturquoise", high = "blue4", guide = FALSE) + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)

