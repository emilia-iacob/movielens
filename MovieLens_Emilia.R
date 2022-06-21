
### STEP 1 - COLLECTING DATA


if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

# Creating a "ratings" dataset
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

# Creating a "movies" dataset
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

# Joining together the ratings and movies datasets
movielens <- left_join(ratings, movies, by = "movieId")

# Saving movielens object to a file called "movielens.rda"
save(movielens, file = "rdas/movielens.rda")

#readLines("ml-10M100K/ratings.dat", n=10)
readLines("ml-10M100K/movies.dat", n=10)

getwd()


### STEP 2 - EXPLORING DATA

# Reviewing the edx dataset structure
str(edx)

# Let's see the number of unique users and the number of unique movies that are rated in the edx data set
edx %>% summarize (unique_users = n_distinct(userId), unique_movies = n_distinct(movieId))

# Let's create a histogram showing the distribution of ratings in the edx data set
hist(edx$rating, xlab = "Ratings")

# Let's now see the actual number of movies rated for each of the ratings in the edx data set
table(edx$rating)

# Let's see what are the top 10 movies by number of ratings
edx %>% group_by(title) %>% summarize(n_ratings = n()) %>% arrange(desc(n_ratings)) %>% head()

# Let's see what are the top ratings by number of movies rated
edx %>% group_by(rating) %>% summarize(n_ratings = n()) %>% arrange(desc(n_ratings)) %>% head()


### STEP 3 - PREPARING DATA - Creating edx train and validation datasets

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") 
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Making sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Adding rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# Removing all the data sets created except the training edx and validation sets
rm(dl, ratings, movies, test_index, temp, movielens, removed)

#Creating and using the RMSE function to compare the movie rating predictions to the true 
# ratings in the validation set
RMSE <- function(true_ratings, predicted_ratings) {
  sqrt(mean((true_ratings - predicted_ratings)^2))
}




