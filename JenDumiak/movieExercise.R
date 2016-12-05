# 12/01/16

################################### Read in libraries ###############################

library(ggplot2)
library(ggmap)
library(dplyr)
library(doBy)
library(stringr)
library(ggthemes)
library(reshape)

# This data set consists of:
#     100,000 ratings (1-5) from 943 users on 1682 movies. 
#     Each user has rated at least 20 movies. 
#     Simple demographic info for the users (age, gender, occupation, zip)

# Join and create a dataframe with UserID, MovieID, MovieTitle, MovieYear, Rating, Timestamp, Genre, Gender of Reviewer, Age of Reviewer, Zip Code, State, Longitute, Latitude

################### Read in 4 data sets we need and clean them  ####################

############################# Lat/ long zipcode data ###############################

zip <- read.csv("C:/Users/jdumiak/Documents/Movie/zips.csv", header = TRUE)
# Need Zip Code, State, Longitute, Latitude from here
zip <- zip[,c(1,3,4,6)]
colnames(zip) <- c("Zipcode", "Lat", "Long", "State")

# Clean up the data for later use
zip$Zipcode <- as.character(zip$Zipcode)
# Take out " " from zip code
zip <- as.data.frame(sapply(zip, function(x) gsub("\"", "", x)))
zip$Lat <- as.numeric(as.character(zip$Lat))
zip$Long <- as.numeric(as.character(zip$Long))

# See null data
sapply(zip,function(x) sum(is.na(x)))


########################### User and Rating Dataser ############################

maindat <- read.delim("C:/Users/jdumiak/Documents/Movie/u.data", header=FALSE, sep ="\t")
# Need UserID, itemID, Rating, Timestamp from here
colnames(maindat) <- c("UserID", "MovieID", "Rating","Timestamp")

# Clean, make characters for later easy use 
maindat$UserID <- as.character(maindat$UserID)
maindat$MovieID <- as.character(maindat$MovieID)

# See null data
sapply(maindat,function(x) sum(is.na(x)))


############################## Info on movie and genre ##############################

movieinfo <- read.delim("C:/Users/jdumiak/Documents/Movie/u.item", header=FALSE, sep = "|")
# Need MovieID, MovieTitle, MovieYear, Genre from here
movieinfo <- movieinfo[,c(1:3,6:24)]
colnames(movieinfo) <- c("MovieID", "MovieTitle", "MovieYear", "Unknown", "Action", "Adventure", "Animation", "Children", "Comedy", "Crime", "Documentary","Drama","Fantasy", "Film-Noir", "Horror", "Musical", "Mystery", "Romance", "Sci-Fi", "Thriller", "War", "Western")

# Clean the data and make characters/factor the genre

# Remove trailing spaces on MovieTitle
movieinfo$MovieTitle <- trimws(as.character(movieinfo$MovieTitle), "r")
# Clean up one title
movieinfo$MovieTitle[1412] <- as.character(str_sub(movieinfo$MovieTitle[1412], start = 1, end =57))

# Extract release year from MovieTitle, one title is unknown will become na when year is forced to be numeric
movieinfo$MovieYear <- as.numeric(str_sub(movieinfo$MovieTitle, start = -5, end = -2))
movieinfo$MovieYear[which(is.na(movieinfo$MovieYear))] <- ""

# Remove the year from the MovieTitle
movieinfo$MovieTitle <- as.character(str_sub(movieinfo$MovieTitle, start = 1, end = -7))

# Factor genre
movieinfo[4:22] <- lapply(movieinfo[4:22], factor)

# See missing data
sapply(movieinfo,function(x) sum(is.na(x)))

############################### User info #####################################

userinfo <- read.delim("C:/Users/jdumiak/Documents/Movie/u.user", header=FALSE, sep = "|")
# UserID, Gender of Reviewer, Age of Reviewer, zip code
userinfo <- userinfo[,c(1:3,5)]
colnames(userinfo) <- c("UserID","Age","Gender", "Zipcode")

# make zipcode a character for later
userinfo$Zipcode <- as.character(userinfo$Zipcode)

# See missing data
sapply(userinfo,function(x) sum(is.na(x)))

############################## Merge the data together ##########################

# First merge maindat with userinfo on UserID
movieFinal <- merge(x = maindat, y = userinfo, by = "UserID", all = TRUE)
# Now merge with movieinfo on MovieID
movieFinal <- merge(x = movieFinal, y = movieinfo, by = "MovieID", all = TRUE)
# Merge with ziplatlon on Zipcode
movieFinal <- merge(x = movieFinal, y = zip, by = "Zipcode", all.x = TRUE)

############################# Export to a csv file #############################

write.csv(movieFinal, file = "C:/Users/jdumiak/Documents/Movie/movieReviewCleaned.csv")

################################# Summary Table ###############################

#	Create a summary table for display of average review by genre for each gender
# Loop through genre filter by genre and gender

genre <- c("Unknown", "Action", "Adventure", "Animation", "Children", "Comedy", "Crime", "Documentary","Drama","Fantasy", "Film-Noir", "Horror", "Musical", "Mystery", "Romance", "Sci-Fi", "Thriller", "War", "Western")

# Initialize vectors for ratings and gender
mRatings <- c("M")
mGender <- c("M")
fRatings <- c("F")
fGender <- c("F")

for (i in 10:28) {
  # Subset male data
    MaleSubset <- filter(movieFinal, movieFinal[i] == 1, Gender == "M")
    mRatings[i-9] <- c(mean(MaleSubset[["Rating"]]))
    mGender[i-9] <- "M"
    
    # Subset female data
    FemaleSubset <- filter(movieFinal, movieFinal[i] == 1, Gender == "F")
    fRatings[i-9] <- c(mean(FemaleSubset[["Rating"]]))
    fGender[i-9] <- "F"
}

# Combine data
# Combine columns first
female <- cbind(fRatings, genre, fGender)
male <- cbind(mRatings, genre, mGender)

# Combine into rows and make a final summary data frame
genreByGender <- data.frame(rbind(male, female))

colnames(genreByGender) <- c("Rating", "Genre", "Gender")

genreByGender$Rating <- as.numeric(as.character(genreByGender$Rating))

#	Visualize that table
ggplot(genreByGender, aes(x= Genre, y = Rating, fill=Gender)) + geom_bar(stat="identity", position = "dodge", width = .7, alpha = .8) + ylab("Average Movie Rating") + xlab("Genre by Gender") + ggtitle("Average Movie Rating by Genre and Gender") + theme_economist() + scale_colour_economist() + coord_flip() #+ theme(panel.background=element_rect(fill='grey99')) + theme(plot.background = element_rect(fill='darkseagreen1')) 


################################ Heat Map ####################################

# Create a heatmap by lat/long for total count of reviews using a national map

# Create the map 
map <- get_map(location='united states', zoom=4, maptype = "terrain",
             source='google',color='color')

# Ratings point plot
ggmap(map) + geom_point(
  aes(x=Long, y=Lat, show_guide = TRUE, colour=Rating), 
  data=movieFinal, alpha=.8, na.rm = T)  + 
  scale_color_gradient(low="yellow", high="red")

# Plot heatmap with ggmap
ggmap(map, extent = "device") + 
  geom_density2d(data = movieFinal, aes(x = Long, y = Lat), size = 0.3) + 
  stat_density2d(data = movieFinal, aes(x = Long, y = Lat, fill = ..level.., alpha = .95), size = 0.01,bins = 16, geom = "polygon") + scale_fill_gradient(low = "yellow", high = "red") +scale_alpha(range = c(0, 0.3), guide = FALSE)

# No Map plot              
ggplot(movieFinal,aes(x=Long,y=Lat))+
  stat_density2d(aes(alpha=..level..), geom="polygon") +
  scale_alpha_continuous(limits=c(0,0.2),breaks=seq(0,0.2,by=0.025))+
  geom_point(colour="red",alpha=0.02)+ ylab("Latitude") + xlab("Longitude") + theme_bw()  + ggtitle("Reviews by Location in the United States")

# Trying to get this one to run
ggmap(map) + geom_tile(data = movieFinal, aes(x = Long, y = Lat, alpha = Rating),fill = 'red') + theme(axis.title.y = element_blank(), axis.title.x = element_blank()) + scale_fill_gradient(low="green", high="red")




