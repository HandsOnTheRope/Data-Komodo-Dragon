# Movie Reviews


# Don't forget the 1980s pop culture reference.

# Using data from the University of Minnesota, the goal of this script is to do the following:

# 1. Create a dataframe with UserID, MovieID, MovieTitle, MovieYear, Rating, Timestamp, Title, Genre, Gender of Reviewer, Age of Reviewer, Zip Code, State, Longitute, Lattitude. Export this dataframe as a .csv file.

# 2. Visualize a summary table that displays average review by genre for each gender.

# 3. Create a heatmap by long/lat for total count of reviews using a national map.


#################### Upload Relevant Libraries ######################

library(dplyr)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)



#################### Upload Relevant Data ##########################

# Rating Information
    data <- read.delim("C:/Users/michnelson/Desktop/Analytics Training/R Exercise/ml-100k/u.data",
                       header = FALSE)
    colnames(data) <- c("userID", "movieID", "rating", "timestamp")

    data$userID <- as.character(data$userID)
    data$movieID <- as.character(data$movieID)

    sapply(item,function(x) sum(is.na(x)))
    
# Movie Information
    
    item <- read.delim("C:/Users/michnelson/Desktop/Analytics Training/R Exercise/ml-100k/u.item",
                       header = FALSE, sep = "|")
    
    colnames(item) <- c("movieID","movie_title", "release_date", "video_release_date",
                        "IMDb_URL", "unknown", "action", "adventure", "animation", 
                        "childrens", "comedy", "crime", "documentary", "drama", "fantasy",
                        "film_noir", "horror", "musical", "mystery", "romance", "sci_fi",
                        "thriller", "war", "western")
    
    # Remove release year from Movie Title
    item$movieID <- as.character(item$movieID)
    item$movie_title <- as.character(item$movie_title)
    item$movie_title <- substring(item$movie_title, 1, nchar(item$movie_title) - 7)
    
    # Convert release date to release year
    item$movie_year <- as.character(item$release_date)
    item$movie_year <- as.numeric(substring(item$movie_year,8,11))
    
    # Make all genres factors
    item[6:25] <- lapply(item[6:25], factor)

    sapply(item,function(x) sum(is.na(x)))
    which(is.na(item$movie_year))
    item$movie_year[267] <- ""
    
    
# Reviewer Information
    
    user <- read.delim("C:/Users/michnelson/Desktop/Analytics Training/R Exercise/ml-100k/u.user",
                       header = FALSE, sep = "|")
    
    colnames(user) <- c("userID", "age", "gender", "occupation", "zip_code")
    
    user$zip_code <- as.character(user$zip_code)
    
    
    sapply(user,function(x) sum(is.na(x)))
    

# Zip Code Information
    zip <- read.csv("C:/Users/michnelson/Desktop/Analytics Training/R Exercise/ml-100k/zips.csv")
    
    colnames(zip) <- c("zip_code", "state_abb", "latitude", "longitude", "city", "state_long")

    zip$zip_code <- as.character(zip$zip_code)
    
    zip <- as.data.frame(sapply(zip, function(x) gsub("\"", "", x)))

    zip$latitude <- as.numeric(as.character(zip$latitude))
    zip$longitude <- as.numeric(as.character(zip$longitude))
     

    
    sapply(zip,function(x) sum(is.na(x)))
    
    
####################  Merge Datasets Into One Dataframe ###########################


data <- merge( x = data, y = item, by = "movieID", all.x = TRUE)

data <- merge( x = data, y = user, by = "userID", all.x = TRUE)

data <- merge( x = data, y = zip, by = "zip_code", all.x = TRUE)


# Collect only relevant data
reviewdata <- select(data, userID, movieID, movie_title, rating, timestamp, gender, age, zip_code, state_abb, longitude, latitude, unknown, action, adventure, animation, childrens, comedy, crime, documentary, drama, fantasy, film_noir, horror, musical, mystery, romance, sci_fi, thriller, war, western)

# Export data as .csv
write.csv(reviewdata, file = "C:/Users/michnelson/Desktop/Analytics Training/R Exercise/Movie_Reviews.csv")


################### Summary Table #########################


# Create new dataset with relevant information
      ggrating <- select(reviewdata, rating, gender, unknown, action, adventure, animation, childrens, comedy, crime, documentary, drama, fantasy, film_noir, horror, musical, mystery, romance, sci_fi, thriller, war, western)
    
# Create vector of genre names
      genre <- c("Unknown", "Action", "Adventure", "Animation", 
                 "Children's", "Comedy", "Crime", "Documentary", "Drama", "Fantasy",
                 "Flm Noir", "Horror", "Musical", "Mystery", "Romance", "Sci Fi",
                 "Thriller", "War", "Western")

      
      
# Collect a dataset of average male ratings by genre (colnames = "Rating", "Genre" "Gender")

      malereviews <- c("M")  # place holder for average ratings vector
      male <- c("M")        # place holder for gender vector
    
    # For Loop that filters all data by genre for males, collects their average rating per genre in malereviews vector, and adds another "M" to male vector.
      for (i in 2:21) {
        
        maleviewer <- filter(ggrating, ggrating[i] == 1, gender == "M" )
        
        malereviews[(i-2)] <- c(mean(maleviewer[["rating"]]))
        
        male[(i-2)] <- c("M")
        
      }
    
    # Combine three vectors into one matrix
      maleratings <- cbind(malereviews, genre, male)


# Collect a vector of average female ratings by genre

      femalereviews <- c("F")  # place holder for average ratings vector
      female <- c("F")        # place holder for gender vector

    # For Loop that filters all data by genre for males, collects their average rating per genre in malereviews vector, and adds another "M" to male vector.
      for (i in 2:21) {
          
          femaleviewer <- filter(ggrating, ggrating[i] == 1, gender == "F" )
          
          femalereviews[(i-2)] <- c(mean(femaleviewer[["rating"]]))
          
          female[(i-2)] <- c("F")
          
        }
    
    # Combine three vectors into one matrix
      femaleratings <- cbind(femalereviews, genre, female)
    


    # Combine both matrices into one dataset
      genregenderdata <- data.frame(rbind(maleratings, femaleratings))
      
      colnames(genregenderdata) <- c("rating", "genre", "gender")
      
      genregenderdata$rating <- as.numeric(as.character(genregenderdata$rating))

    
    # Create bar graph with X = Genre, Y = Average Rating, Separated by Gender
      ggplot(data = genregenderdata, aes(y= rating, x= genre, fill = gender)) +
        geom_bar(position = "dodge", stat = "identity",
                 width = .3,
                 alpha = .5) +
                 labs(title = "Movie Preferences by Gender",
                      x = "Genre",
                      y = "Average Rating") +
                scale_fill_manual(values = c("red", "dark green"))



######################## Heat Map ################################


# Create a heat map that demonstrates the concentration of movie raters by geographic location
    
      us_map <- get_map(location='united states', zoom=4, maptype = "roadmap",
                        source='google',color='color') 
      
      ggmap(us_map, extent = "device") + 
        geom_density2d(data = reviewdata, aes(x = longitude, y = latitude), size = 0.3) + 
        stat_density2d(data = reviewdata, aes(x = longitude, y = latitude, fill = ..level.., alpha = .95), 
        size = 0.01,bins = 16, geom = "polygon") + scale_fill_gradient(low = "dodgerblue", high = "deeppink") + 
        scale_alpha(range = c(0, 0.3), guide = FALSE)
      