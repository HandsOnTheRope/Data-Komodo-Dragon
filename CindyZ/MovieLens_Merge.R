#R-Training
#Cleaning and Merging Datasets 
#Movie Lens Datasets
#11/30/16

#Sections:

# 1.Set up
# 2.Loading the data
# 3.Cleaning the data 
# 4.Merging the dataframes
# 5.Summary StatistiCS
# 6.Heat Map
# 7.Export to CSV



############# SET UP ############# 
#Install packages if don't have them
#install.packages("plyr")
#install.packages("ggmap")
#install.packages("stringr")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("varhandle")
#install.packages("data.table")
#install.packages("ggplot2")

#Load packages
library(party)
library(rpart)
library(rattle)
library(rpart)  
library(rpart.plot)
library(RColorBrewer)
library(caret)
library(lattice)
library(e1071)
library(pROC)
library(ROCR)
library(ggplot2)
library(tree)
library(randomForest)
library(gridExtra)
library(rmarkdown)
library(plyr)
library(ggmap)
library(stringr)
library(dplyr)
library(tidyr)
library(varhandle)
library(data.table)

############# LOADING THE DATA ############# 

#Dataset 1: U.data contains user id, movie id, rating, timestamp
u_data <- read.delim("C:/Users/INPUT OWN PATH/Trainings/R_Studio/Data/u.data",
                        header = FALSE)
summary(u_data)
  
#Dataset 2: U.item contains movie id, movie title, release data, genre
u_item <- read.delim("C:/Users/INPUT OWN PATH/Trainings/R_Studio/Data/u.item",
                       header = FALSE, sep = "|")
summary(u_item)
  
#Dataset 3: U.user contains user id, age, gender, zip code
u_user <- read.delim("C:/Users/INPUT OWN PATH/Trainings/R_Studio/Data/u.user",
                       header = FALSE, sep = "|")
summary(u_user)
  
#Dataset 3: U.user contains user id, age, gender, zip code
zips_csv <- read.csv(
    "C:/Users/INPUT OWN PATH/Trainings/R_Studio/Data/zips.csv",
    header = TRUE
)
summary(zips_csv)

############# CLEANING THE DATA ############# 

#DATASET 1: U_Data
  #Adding in labels
  names(u_data) <- c("id","movie_id","rating", "timestamp")


#DATASET 2: U_Item
  #Adding in labels
  names(u_item) <- c("movie_id", "movie_title", "release_date", "video_release_date", "url", "unknown", "action",
                     "adventure", "animation", "children", "comedy", "crime", "documentary", "drama", "fantasy", "film_noir",
                     "horror", "musical", "mystery", "romance", "sci_fi", "thriller", "war", "western")
  str(u_item)

  #Splitting out the date to the year (last 4 digits)
    #Change date into a character
    u_item$date_chr <- as.character.Date(u_item$release_date)
   
    separate(u_item, date_chr, c("day", "month", "year"), "-", fill = "right")
    #Note: One missing date at row 267, filled in from the right. It remains blank.
  

#DATASET 3: U_User
  #Adding in labels
  names(u_user) <- c("id","age","gender", "ocupation", "zipcode")
  
#DATASET 4: zips_csv
  #Adding in labels
  names(zips_csv) <- c("zipcode", "state_abbrev", "lat", "long", "city", "state ") 
  
  #Change lat and long to numeric from factor
    #Remove quotation marks
    del <- colwise(function(zips_csv) str_replace_all(zips_csv, '"', ""))
    zips_csv2 <- del(zips_csv)
    
    #Chaning to numeric
    zips_csv2$lat <- as.numeric(zips_csv2$lat)
    zips_csv2$long <- as.numeric(zips_csv2$long)
  

############# MERGING DATASETS ############# 
    
  #Merging u_data and u_item
  merge_temp1 <- merge(u_data,u_item,by="movie_id")
  
  #Merging in u_user
  merge_temp2 <- merge(merge_temp1,u_user,by="id")
  
  #Merging in zips_csv
  merge_all <- merge(merge_temp2, zips_csv2,by="zipcode", all.x=TRUE) 
    #Note: Need to left join so that unmatched ones are also added
  merge_all
  
  
############# SUMMARY STATISTICS ############# 
  
  
summary(merge_all)
  
#Summary table for display of average review by genre for each gender & Visualize
  
  #First attempt
  # Table3 <- with(merge_all, aggregate(merge_all$rating, list(gender, unknown, action),FUN=mean))
  # Table3
  
  #Note: Issue with summary table: movie label categories are not exclusive. 
    #Tables are counting 0,0, 0,1, 1,0, 1,1 as different groups if only looking at two genres
    #In actuality, want categories with any 1 to be summed up, regardless of multiple categories 
  #Solution: Create separate columns with drama rating, action rating, ect. and then find the means of those columns
    
  #Creating loop to create all the genre rating columns
    for(i in names(merge_all[,c(10:28)])){
      merge_all[[paste(i, 'r', sep="_")]] <- as.numeric(ifelse(merge_all[[i]]==1, merge_all$rating, "na"))
      #Note: Needed to put in na instead of 0's because didn't want 0 to be included in the mean. Ratings are only for 1-5.
      #Then needed to make sure casted it as a numeric so could calculate mean
    }
    
  #Summary check of newly created columns
  summary(merge_all[,c(38:56)])
    
    
  #Aggregate table
  #Resource: http://stackoverflow.com/questions/21982987/mean-per-group-in-a-data-frame
  mean_table <- aggregate(merge_all[, 38:56], list(merge_all$gender), mean, na.rm = TRUE) 
  mean_table
 
    
#----------------------------------------------------------------------------------------------------------------------
    
    # Output:
    # Group.1 unknown_r action_r adventure_r animation_r children_r comedy_r  crime_r documentary_r  drama_r fantasy_r film_noir_r
    # 1       F     3.500 3.484013    3.517988    3.627136   3.426971 3.424021 3.556299      3.614973 3.662246  3.201102    3.740260
    # 2       M     3.125 3.479228    3.499246    3.557471   3.320000 3.382972 3.654049      3.691769 3.696957  3.220425    3.973294
    # horror_r musical_r mystery_r romance_r sci_fi_r thriller_r    war_r western_r
    # 1 3.263993  3.640083  3.560122  3.655685 3.497908   3.496068 3.781179  3.514825
    # 2 3.298058  3.472665  3.664208  3.607072 3.577072   3.512927 3.826328  3.637896
    
  
#----------------------------------------------------------------------------------------------------------------------
    
#Visualizing summary table in bar plot
    #Resources: Melt - https://www.r-bloggers.com/using-r-barplot-with-ggplot2/
      #Renaming labels: http://www.cookbook-r.com/Manipulating_data/Renaming_levels_of_a_factor/

    # Testing what kind of data frame can be plotted
    # df <- data.frame(genre = c("unknown_r","unknown_r", "action_r", "action_r", "animation_r", "animation_r"), 
    #                  gender = c("male", "female","male", "female","male", "female"), 
    #                  avg_rating =   c(3.1,3.5,3.5,3.5,3.6,3.8))
    # qplot(x=genre, y=avg_rating, fill=gender, data=df, geom="bar") +
    #   geom_bar(stat="identity")
    
    #Reshape the data so that genres become a column
    melted <- melt(mean_table, id.vars=c("Group.1"))
    
    #Clean up labels
    names(melted) <- c("gender","genre","avg_rating")
    
    #Clean up factor labels of genre so it doesn't have '_r' at the end when graphing
    levels(melted$genre) <- gsub("_r", "", levels(melted$genre))
    levels(melted$genre)
   
    
 #Bar Plot
    ggplot(melted, aes(factor(genre), avg_rating, fill = gender, xlabel="Movie Genres")) + 
      geom_bar(stat="identity", position = "dodge") + 
      scale_fill_brewer(palette = "Set3") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      xlab("Movie Genres") +
      ylab("Average Rating")+
      ggtitle("Average Movie Rating by Genre and Gender")
    
    
############# HEAT MAP ############# 
#Heat map of entire country, by long and lat of count of reviews (more reviews = hotter)
#     #Resources:
#       #https://blog.dominodatalab.com/geographic-visualization-with-rs-ggmaps
    
    us_map <- get_map(location='united states', zoom=4, maptype = "roadmap",
                      source='google',color='color') 
    ggmap(us_map, extent = "device") + 
      geom_density2d(data = merge_all, aes(x = long, y = lat), size = 0.3) + 
      stat_density2d(data = merge_all, aes(x = long, y = lat, fill = ..level.., alpha = .95), 
                     size = 0.01,bins = 16, geom = "polygon") + scale_fill_gradient(low = "#e8f442", high = "#f44242") + 
      scale_alpha(range = c(0, 0.3), guide = FALSE)
    
    
############# EXPORT TO CSV ############# 
    
#Set working directory
  setwd("C:/Users/INPUT OWN PATH/Trainings/R_Studio/Data") 

write.csv(merge_all, file = "MovieLens_Merged_DF.csv")
    
    
#**************************************************************************************************************************    