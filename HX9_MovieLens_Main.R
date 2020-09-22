# Instructions ---------------------------------------------------------------------------------------------------------

# If you are reading this code, here are some tips to make your life slightly easier:
# - This code is divided in sections roughly corresponding to the sections in the report.
# - CTRL + SHIFT + O shows/hides the section outline of the script, making it easier to navigate.
# - You can show/hide individual sections by clicking in the down arrow on the left.
# - CTRL + ALT + T runs an entire section.
# - CTRL + ENTER runs a line of code or the current selection and moves the cursor to the next line.
# - Some parts of this code can take more than a few minutes to run; there will be comments to alert 
#   you when appropriate.
# - The entire code takes 3-5 hours to run on a typical personal laptop.
# - Some parts of this code many require giving R access to additional memory and use of the hard drive to run; 
#   there will be comments to alert you when appropriate.

# Housekeeping ---------------------------------------------------------------------------------------------------------
# Install and load libraries; Run code provided on course to load dataset and to create the training ("edx") dataset 
# and validation ("validation") dataset

# Install and load libraries

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knirt", repos = "http://cran.us.r-project.org")
if(!require(Matrix)) install.packages("Matrix", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(knitr)
library(Matrix)
library(matrixStats)

# Download MovieLens dataset
# WARNING: may take several minutes to run

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Create training and validation datasets

suppressWarnings(set.seed(1, sample.kind="Rounding"))
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

# Remove temporary variables

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Quiz: MovieLens Dataset ----------------------------------------------------------------------------------------------
# Responses to questions on the edX course quiz

# Question 1 - number of rows and columns

edx %>% nrow()
edx %>% ncol()

# Question 2 - number of ratings

edx$rating %>% table()

# Question 3 - number of unique movies

edx$movieId %>% n_distinct()

# Question 4 - number of unique users

edx$userId %>% n_distinct()

# Question 5 - number of ratings per genre

edx %>% 
  summarize(Drama = sum(str_detect(genres,'Drama')),
            Comedy = sum(str_detect(genres,'Comedy')),
            Thriller = sum(str_detect(genres,'Thriller')),
            Romance = sum(str_detect(genres,'Romance')))

# Question 6 - number of ratings per movie

edx %>% 
  summarize(`Forrest Gump` = sum(str_detect(title,'Forrest Gump')),
            `Jurassic Park` = sum(str_detect(title,'Jurassic Park\\s\\(\\d{4}\\)')), # RegEx to avoid movie sequels
            `Pulp Fiction` = sum(str_detect(title,'Pulp Fiction')),
            `Shawshank Redemption` = sum(str_detect(title,'Shawshank Redemption')),
            `Speed 2` = sum(str_detect(title,'Speed 2'))) %>% 
  pivot_longer(cols = everything(),
               names_to = 'Movie',
               values_to = '# of ratings') %>% 
  arrange(desc(`# of ratings`))

# Question 7 - order of given ratings

edx$rating %>% 
  table() %>% 
  as.data.frame() %>% 
  arrange(desc(Freq))

# Question 8 - half stars vs full stars ratings

edx %>% 
  summarize(full_star = sum(rating %in% c(1,2,3,4,5)),
            half_star = sum(rating %in% c(0.5,1.5,2.5,3.5,4.5))) %>% 
  pivot_longer(cols = everything(),
               names_to = 'type of rating',
               values_to = '# of ratings') %>% 
  arrange(desc(`# of ratings`))


# Data preparation -----------------------------------------------------------------------------------------------------

# split the edx object into a train and a test sets

index <- createDataPartition(edx$rating, times = 1, p = 0.2, list = FALSE)
test <- edx[index,]
train <- edx[-index,]
keep <-
  test %>% 
  semi_join(train, by = "movieId") %>% 
  semi_join(train, by = "userId")

remove <-
  anti_join(test,keep,
            by = c('movieId', 'userId'))

# create train and test sets

train <- bind_rows(train,remove)
test <- keep

# remove temporary variables

rm(index,keep,remove)

# create ratings object (info strictly related to each rating event) from the train set

ratings <- 
  train %>% 
  # select columns strictly related to each rating
  select(userId,movieId,rating,timestamp) %>% 
  # create date variable and drop timestamp
  mutate(date = as_date(as_datetime(timestamp))) %>%
  select(-timestamp)
           
# initialize movies object, with individual information for each movie from the train set

movies <- 
  train %>% 
  select(movieId,title,genres,rating) %>% 
  # split title (year) into title and year variables
  mutate(year = as.integer(str_extract(title,'(?<=\\()\\d{4}(?=\\))')),
         title = str_replace(title,'\\s\\(\\d{4}\\)$','')) %>% 
  # aggregate into individual movies with basic statistics
  group_by(movieId,title,year,genres) %>%
  summarise("average rating" = mean(rating),
            "number of ratings" = n(),
            .groups = 'drop')

# initialize users object, with individual information for each user from the train set

users <-
  train %>% 
  select(userId,rating) %>% 
  # aggregate into individual users with basic statistics
  group_by(userId) %>% 
  summarise("average rating" = mean(rating),
            "number of ratings" = n(),
            .groups = 'drop')

# create genres object, with information for each of the different genres combinations

genres.long <- 
  train %>% 
  select(genres) %>% 
  distinct() %>%
  separate(col = "genres", 
           into = paste("genre #", as.character(c(1:20))), 
           remove = FALSE, sep = "\\|", fill = 'right') %>% 
  pivot_longer(cols = contains("genre #"),
               names_to = "genre #",
               values_to = "genre",
               values_drop_na = TRUE) %>% 
  select(- `genre #`)

genres.wide <-
  long %>% 
  mutate("true" = TRUE) %>% 
  pivot_wider(names_from = "genre",
              values_from = "true",
              values_fill = FALSE)

genres.column.order <- names(genres.wide)[-1]

genres <- 
  list(long = genres.long,
       wide = genres.wide,
       column.order = genres.column.order)


# create function for RMSE calculation

RMSE = function(observations,predictions){sqrt(mean((observations-predictions)^2))}

# create function to limit range of predictions to 0.5 to 5 stars

limitRange = function(predictions,min,max){
  predictions[predictions > max] <- max
  predictions[predictions < min] <- min
  predictions
}

# initialize lists of models, parameters and predictions for model optimization

models <- list()
parameters <- list()
predictions <- list(train = select(train,userId,movieId,rating), # predictions on the train set from each model
                    test = select(test,userId,movieId,rating), # predictions on the test set from each model
                    validation = select(validation,userId,movieId,rating)) # predictions on the validation set from each model
results <- list(train = data.frame(),
                test = data.frame(),
                validation = data.frame())

# remove temporary variables

rm(genres.long,genres.wide)

# Benchmark - global average -------------------------------------------------------------------------------------------

# calculate global average from the train set

parameters$`global average` <- mean(ratings$rating)

# create model based on global average - receives data frame with userId and movieId and returns the global average

models$`global average` <- function(df){rep(parameters$`global average`, nrow(df))}

# calculate predictions on train and test sets

predictions$train$`global average` <- models$`global average`(select(train,userId,movieId))
predictions$test$`global average` <- models$`global average`(select(test,userId,movieId))

# calculate RMSE and save to results object

results$train <- 
  data.frame(modelId = 1,
             RMSE = RMSE(train$rating,predictions$train$`global average`),
             description = "Global average (benchmark)")

results$test <-
  data.frame(modelId = 1,
             RMSE = RMSE(test$rating,predictions$test$`global average`),
             description = "Global average (benchmark)")

# Simple movie effect --------------------------------------------------------------------------------------------------

# calculate simple movie effect from train set and include in movies object

movie.effect <-
  train %>% 
  # calculate residuals from benchmark model
  mutate(residual = rating - parameters$`global average`) %>% 
  # calculate mean residuals for each movie
  group_by(movieId) %>% 
  summarise("simple movie effect" = mean(residual),
            .groups = 'drop')

movies <-
  movies %>% 
  left_join(movie.effect, by = "movieId")

# create model with simple movie effect

models$`simple movie effect` <-
  function(df){
    df %>% 
      # get movie effect for each movie from the movies object
      left_join(select(movies,movieId,`simple movie effect`), by = "movieId") %>%
      # make prediction
      mutate(prediction = parameters$`global average` + `simple movie effect`) %>% 
      pull(prediction)
  }

# calculate predictions on train and test sets

predictions$train$`simple movie effect` <- models$`simple movie effect`(train)
predictions$test$`simple movie effect` <- models$`simple movie effect`(test)

# calculate RMSE and save to results object

results$train <- 
  results$train %>% 
  bind_rows(data.frame(modelId = 2,
                       RMSE = RMSE(train$rating,predictions$train$`simple movie effect`),
                       description = "model 1 + simple movie effect"))

results$test <- 
  results$test %>% 
  bind_rows(data.frame(modelId = 2,
                       RMSE = RMSE(test$rating,predictions$test$`simple movie effect`),
                       description = "model 1 + simple movie effect"))

# remove temporary objects

rm(movie.effect)

# Regularized movie effect ---------------------------------------------------------------------------------------------

# find optimal lambda for regularization

lambda <- seq(0,10,.1)

rmse <- sapply(lambda, function(lambda){
  
  # get regularized movie effect for each movie with candidate lambda
  reg.movie.effect <-
    movies %>% 
    mutate("regularized movie effect" = `simple movie effect` * `number of ratings`/(`number of ratings` + lambda)) %>% 
    select(movieId, `regularized movie effect`)
  
  # calculate predictions on test set with candidate lambda
  predictions <-
    # get userId and movieId from test set
    test %>% 
    select(userId,movieId) %>% 
    # include regularized movie effect for each movie
    left_join(reg.movie.effect, by = "movieId") %>% 
    # make predictions
    mutate(prediction = parameters$`global average` +`regularized movie effect`) %>% 
    pull(prediction)
  
  # return RMSE
  RMSE(test$rating,predictions)
})

# include optimal lambda in the list of optimization parameters

parameters$`lambda (movie effect)` <- lambda[which.min(rmse)]

# include regularized movie effect with selected lambda into movies object

movies <- 
  movies %>% 
  mutate("regularized movie effect" = 
           `simple movie effect` * `number of ratings`/(`number of ratings` + parameters$`lambda (movie effect)`))

# create model with regularized movie effect

models$`regularized movie effect` <-
  function(df){
    df %>% 
      # get movie effect for each movie from the movies object
      left_join(select(movies,movieId,`regularized movie effect`), by = "movieId") %>%
      # make prediction
      mutate(prediction = parameters$`global average` + `regularized movie effect`) %>% 
      pull(prediction)
  }

# calculate predictions on train and test sets

predictions$train$`regularized movie effect` <- models$`regularized movie effect`(train)
predictions$test$`regularized movie effect` <- models$`regularized movie effect`(test)

# calculate RMSE and save to results object

results$train <-
  results$train %>%
  bind_rows(data.frame(modelId = 3,
                       RMSE = RMSE(train$rating,predictions$train$`regularized movie effect`),
                       description = "model 1 + regularized movie effect"))

results$test <-
  results$test %>%
  bind_rows(data.frame(modelId = 3,
                       RMSE = RMSE(test$rating,predictions$test$`regularized movie effect`),
                       description = "model 1 + regularized movie effect"))

# remove temporary objects

rm(lambda,rmse)

# Simple user effect ---------------------------------------------------------------------------------------------------

# calculate simple user effect from train set and include in users object

user.effect <-
  train %>% 
  # include regularized movie effect for each movie
  left_join(select(movies,movieId,`regularized movie effect`), by = 'movieId') %>% 
  # calculate residuals from regularized movie effect model
  mutate(prediction = parameters$`global average` + `regularized movie effect`,
         residual = rating - prediction) %>% 
  # calculate mean residuals for each user
  group_by(userId) %>% 
  summarise("simple user effect" = mean(residual),
            .groups = 'drop')

users <-
  users %>% 
  left_join(user.effect, by = "userId")

# create model with simple user effect

models$`simple user effect` <-
  function(df){
    df %>% 
      # get regularized movie effect for each movie from movies object
      left_join(select(movies,movieId,`regularized movie effect`), by = "movieId") %>% 
      # get user effect for each user from the users object
      left_join(select(users,userId,`simple user effect`), by = "userId") %>%
      # make prediction
      mutate(prediction = parameters$`global average` + `regularized movie effect` + `simple user effect`) %>% 
      mutate(prediction = limitRange(prediction, 0.5, 5)) %>% 
      pull(prediction)
  }

# calculate predictions on train and test sets

predictions$train$`simple user effect` <- models$`simple user effect`(train)
predictions$test$`simple user effect` <- models$`simple user effect`(test)

# calculate RMSE and save to results object

results$train <-
  results$train %>%
  bind_rows(data.frame(modelId = 4,
                       RMSE = RMSE(train$rating,predictions$train$`simple user effect`),
                       description = "model 3 + simple user effect"))

results$test <-
  results$test %>%
  bind_rows(data.frame(modelId = 4,
                       RMSE = RMSE(test$rating,predictions$test$`simple user effect`),
                       description = "model 3 + simple user effect"))

# remove temporary objects

rm(user.effect)

# Regularized user effect ----------------------------------------------------------------------------------------------

# find optimal lambda for regularization

lambda <- seq(0,10,.1)

rmse <- sapply(lambda, function(lambda){
  
  # get regularized user effect for each user with candidate lambda
  reg.user.effect <-
    users %>% 
    mutate("regularized user effect" = `simple user effect` * `number of ratings`/(`number of ratings` + lambda)) %>% 
    select(userId, `regularized user effect`)
  
  # calculate predictions on test set with candidate lambda
  predictions <-
    # get userId and movieId from test set
    test %>% 
    select(userId,movieId) %>% 
    # include regularized movie effect for each movie
    left_join(select(movies,movieId,`regularized movie effect`), by = "movieId") %>% 
    # include regularized user effect for each user
    left_join(reg.user.effect, by = "userId") %>% 
    # make predictions
    mutate(prediction = parameters$`global average` +`regularized movie effect` + `regularized user effect`) %>% 
    mutate(prediction = limitRange(prediction, 0.5, 5)) %>% 
    pull(prediction)
  
  # return RMSE
  RMSE(test$rating,predictions)
})

# include optimal lambda in the list of optimization parameters

parameters$`lambda (user effect)` <- lambda[which.min(rmse)]

# include regularized user effect with selected lambda into users object

users <- 
  users %>% 
  mutate("regularized user effect" = 
           `simple user effect` * `number of ratings`/(`number of ratings` + parameters$`lambda (user effect)`))

# create model with regularized movie effect

models$`regularized user effect` <-
  function(df){
    df %>% 
      # get regularized movie effect for each movie from the movies object
      left_join(select(movies,movieId,`regularized movie effect`), by = "movieId") %>%
      # get regularized user effect for each user from the users object
      left_join(select(users,userId,`regularized user effect`), by = "userId") %>% 
      # make prediction
      mutate(prediction = parameters$`global average` + `regularized movie effect` + `regularized user effect`) %>% 
      pull(prediction)
  }

# calculate predictions on train and test set

predictions$train$`regularized user effect` <- models$`regularized user effect`(train)
predictions$test$`regularized user effect` <- models$`regularized user effect`(test)

# calculate RMSE and save to results object

results$train <-
  results$train %>%
  bind_rows(data.frame(modelId = 5,
                       RMSE = RMSE(train$rating,predictions$train$`regularized user effect`),
                       description = "model 3 + regularized user effect"))

results$test <-
  results$test %>%
  bind_rows(data.frame(modelId = 5,
                       RMSE = RMSE(test$rating,predictions$test$`regularized user effect`),
                       description = "model 3 + regularized user effect"))

# remove temporary objects

rm(lambda,rmse)

# Genre effect (drama only) --------------------------------------------------------------------------------------------
# WARNING: may take several minutes to run

# get each user's reaction to drama and non-drama movies

user.drama.effect <-
  # prediction from the previous model on the train set
  predictions$train %>% 
  select(userId,movieId,rating,`regularized user effect`) %>% 
  # include genres column from movies object
  left_join(select(movies,movieId,genres), by = "movieId") %>% 
  # add residual and classify movies into drama and non-drama
  mutate(residual = rating - `regularized user effect`,
         is.drama = str_detect("Drama",genres)) %>% 
  # calculate each user's reactions to both drama and non-drama movies
  group_by(userId) %>% 
  summarise("mean residual" = mean(residual),
            "drama movies" = sum(is.drama),
            "non-drama movies" = sum(!is.drama),
            "mean residual (drama)" = mean(residual[is.drama]),
            "mean residual (non-drama)" = mean(residual[!is.drama]),
            .groups = 'drop') %>% 
  # remove NA values
  replace_na(list("mean residual (drama)" = 0,
                  "mean residual (non-drama)" = 0))

# find optimal lambda for regularization

lambda <- seq(0,100,1)

rmse <- sapply(lambda, function(lambda){
  
  # get regularized drama genre effect for each user with current lambda
  reg.drama.effect <- 
    user.drama.effect %>% 
    transmute(userId,
              "regularized drama effect" = `mean residual (drama)` * `drama movies`/(`drama movies` + lambda),
              "regularized non-drama effect" = `mean residual (non-drama)` * `non-drama movies`/(`non-drama movies` + lambda)) %>% 
    # replace NAs with 0
    replace_na(list("regularized drama effect" = 0,
                    "regularized non-drama effect" = 0))
  
  # calculate predictions on test set with candidate lambda
  predictions <-
    # get predictions on test set with previous model
    predictions$test %>% 
    select(userId,movieId,`regularized user effect`) %>%  
    # include genres information from movies object
    left_join(select(movies,movieId,genres), by = "movieId") %>% 
    # classify movies into drama and non-drama and add appropriate genre effect
    mutate(is.drama = str_detect(genres, "Drama")) %>% 
    # include each user's reaction to drama and non-drama movies
    left_join(reg.drama.effect, by = "userId") %>% 
    # create new prediction
    mutate(prediction = `regularized user effect` + if_else(is.drama, `regularized drama effect`, `regularized non-drama effect`)) %>% 
    pull(prediction)
  
  # return RMSE
  RMSE(test$rating,predictions)
})

# include optimal lambda in the list of optimization parameters

parameters$`lambda (drama effect)` <- lambda[which.min(rmse)]

# get regularized drama and non-drama effects for selected lambda and save to users object

reg.drama.effect <- 
  user.drama.effect %>% 
  transmute(userId,
            "regularized drama effect" = `mean residual (drama)` * 
              `drama movies`/(`drama movies` + parameters$`lambda (drama effect)`),
            "regularized non-drama effect" = `mean residual (non-drama)` * 
              `non-drama movies`/(`non-drama movies` + parameters$`lambda (drama effect)`)) %>% 
  # replace NAs with 0
  replace_na(list("regularized drama effect" = 0,
                  "regularized non-drama effect" = 0))

users <- 
  left_join(users, reg.drama.effect, by = "userId")

# create model including the regularized genre effect (drama)

models$`regularized drama/non-drama effect` <-
  function(df){
    df %>% 
      select(userId,movieId,genres) %>% 
      # include parameters necessary for calculation of predictions
      left_join(select(users,userId,`regularized user effect`,`regularized drama effect`,
                       `regularized non-drama effect`), by = "userId") %>% 
      left_join(select(movies,movieId,`regularized movie effect`), by = "movieId") %>% 
      left_join(select(genres$wide,genres,Drama), by = "genres") %>% 
      # calculate predictions
      mutate(prediction = parameters$`global average` + `regularized user effect` + `regularized movie effect` +
               if_else(Drama, `regularized drama effect`, `regularized non-drama effect`)) %>% 
      mutate(prediction = limitRange(prediction, 0.5, 5)) %>% 
      pull(prediction)
  }

# make predictions on the train and test sets with current model

predictions$train$`regularized drama/non-drama effect` <- models$`regularized drama/non-drama effect`(train)
predictions$test$`regularized drama/non-drama effect` <- models$`regularized drama/non-drama effect`(test)

# calculate RMSE and update results object

results$train <-
  results$train %>%
  bind_rows(data.frame(modelId = 6,
                       RMSE = RMSE(train$rating,predictions$train$`regularized drama/non-drama effect`),
                       description = "model 5 + regularized drama/non-drama effects"))

results$test <-
  results$test %>%
  bind_rows(data.frame(modelId = 6,
                       RMSE = RMSE(test$rating,predictions$test$`regularized drama/non-drama effect`),
                       description = "model 5 + regularized drama/non-drama effects"))

# remove temporary variables

rm(user.drama.effect, rmse, reg.drama.effect)

# Genre effect (Principal Component Analysis) --------------------------------------------------------------------------
# WARNING: may take several minutes to run
# WARNING: may require access to additional memory to run - in this case, run following line:
# memory.limit(15000)

# run Principal Component Analysis on movie genres

genres$principal.components <-
  movies %>% 
  select(movieId,genres) %>% 
  left_join(genres$wide, by = "genres") %>% 
  select(-genres) %>% 
  column_to_rownames("movieId") %>% 
  prcomp()

# get mean residuals for each user from reference model on the train set

residuals.users <-
  # get residuals from reference model (model 5: reg user effect)
  predictions$train %>% 
  select(userId,movieId,rating,`regularized user effect`) %>% 
  mutate(residual = rating - `regularized user effect`) %>% 
  # get mean residuals for each user
  group_by(userId) %>% 
  summarise("mean residual" = mean(residual),
            .groups = 'drop')

# get mean residuals for each user / genre combination

residuals.users.genres <-
  # get residuals from reference model (model 5: reg user effect)
  predictions$train %>% 
  select(userId,movieId,rating,`regularized user effect`) %>% 
  mutate(residual = rating - `regularized user effect`) %>% 
  # include mean residual for each user and subtract from residual on each entry
  left_join(residuals.users, by = "userId") %>% 
  mutate("residual deviation" = residual - `mean residual`) %>% 
  # make individual entries for each individual genre, in long format
  left_join(select(movies,movieId,genres), by = "movieId") %>% 
  left_join(genres$long, by = "genres") %>% 
  # get simple genre effect for each user/genre combination
  group_by(userId,genre) %>% 
  summarise("number of ratings" = n(),
            "simple genre effect" = mean(`residual deviation`),
            .groups = 'drop')

# find best lambda for regularization

lambda <- c(0:10,seq(15,50,5),seq(60,100,10),seq(120,200,20))

rmse <- sapply(lambda, function(lambda){
  
  # calculate regularized genre effect for each user/genre combination and current lambda value
  users.genres.df <-
    residuals.users.genres %>% 
    mutate("regularized genre effect" = `simple genre effect` * 
             `number of ratings`/(`number of ratings` + lambda))
  
  # get matrix with regularized genre effect for each user
  users.genres.matrix <-
    users.genres.df %>% 
    select(userId, genre, `regularized genre effect`) %>% 
    pivot_wider(names_from = "genre",
                values_from = "regularized genre effect",
                values_fill = 0) %>% 
    column_to_rownames("userId") %>% 
    select(genres$column.order) %>% 
    as.matrix()
  
  # calculate matrix with each user's regularized average reaction to each genre principal component
  users.pc.matrix <-
    users.genres.matrix %*% genres$principal.components$rotation
  
  # get prediction correction for each user/movie combination via principal components
  correction <-
    test %>% 
    select(userId,movieId) %>% 
    mutate(correction = rowSums(users.pc.matrix[as.character(userId),] * 
                                  genres$principal.components$x[as.character(movieId),])) %>% 
    as.data.frame()
  
  # make prediction on test set with calculated correction
  prediction <-
    test %>% 
    # get variables necessary for prediction with current model and current value of lambda
    left_join(select(users,userId,`regularized user effect`), by = "userId") %>% 
    left_join(select(movies,movieId,`regularized movie effect`), by = "movieId") %>% 
    left_join(correction, by = c("userId","movieId")) %>% 
    # make predictions
    mutate(prediction = parameters$`global average` + `regularized movie effect` + 
             `regularized user effect` + correction) %>% 
    mutate(prediction = limitRange(prediction, 0.5, 5)) %>% 
    pull(prediction)
  
  # calculate RMSE
  RMSE(test$rating, prediction)

})

# update list of parameters with the selected lambda

parameters$`lambda (genre effect)` <- lambda[which.min(rmse)]

# calculate regularized genre effect for each user/genre combination and current lambda value

users.genres.df <-
  residuals.users.genres %>% 
  mutate("regularized genre effect" = `simple genre effect` * 
           `number of ratings`/(`number of ratings` + parameters$`lambda (genre effect)`))

# get matrix with regularized genre effect for each user

users.genres.matrix <-
  users.genres.df %>% 
  select(userId, genre, `regularized genre effect`) %>% 
  pivot_wider(names_from = "genre",
              values_from = "regularized genre effect",
              values_fill = 0) %>% 
  column_to_rownames("userId") %>% 
  select(genres$column.order) %>% 
  as.matrix()

# calculate matrix with each user's regularized average reaction to each genre principal component

users.pc.matrix <-
  users.genres.matrix %*% genres$principal.components$rotation

# create df with each user's regularized genre effect calculated through PCA

users.pc.df <-
  users.pc.matrix %>% 
  as.data.frame() %>% 
  unite(col = "genre effect (PCA)", sep = "|") %>% 
  rownames_to_column("userId") %>% 
  mutate(userId = as.numeric(userId)) %>% glimpse
  
# include regularized genre effect (PCA) for each user into users object

users <- 
  left_join(users,users.pc.df, by = "userId")

# create model for predictions including regularized genre effect calculated through PCA

models$`regularized genre effect (PCA)` <- function(df){
  # create matrix with user's reactions for each genre PC, same user order as the input data frame
  users.genres.pc <-
    # get user's reactions to each PC
    df %>% 
    select(userId) %>% 
    left_join(select(users,userId,`genre effect (PCA)`), by = "userId") %>% 
    select(-userId) %>% 
    separate(col = `genre effect (PCA)`, into = paste("PC",1:20), sep = "\\|", 
             remove = TRUE, convert = TRUE) %>% 
    as.matrix()
  
  # create matrix with movie's decomposition into genres PCs, same movie order as the input data frame
  movies.genres.pc <-
    genres$principal.components$x[as.character(df$movieId),]
  
  # create predictions
  df %>% 
    # include appropriate columns for prediction
    select(userId,movieId) %>% 
    left_join(select(movies,movieId,`regularized movie effect`), by = 'movieId') %>% 
    left_join(select(users,userId,`regularized user effect`), by = 'userId') %>% 
    # calculate correction due to genre effect
    mutate("genre effect" = rowSums(users.genres.pc * movies.genres.pc)) %>% 
    # make prediction
    mutate(prediction = parameters$`global average` + `regularized movie effect` +
             `regularized user effect` + `genre effect`) %>% 
    mutate(prediction = limitRange(prediction, 0.5, 5)) %>% 
    pull(prediction)
}

# make predictions on the train and test sets

predictions$train$`regularized genre effect (PCA)` <- models$`regularized genre effect (PCA)`(train)
predictions$test$`regularized genre effect (PCA)` <- models$`regularized genre effect (PCA)`(test)

# calculate RMSE and include in results object

results$train <-
  results$train %>%
  bind_rows(data.frame(modelId = 7,
                       RMSE = RMSE(train$rating, predictions$train$`regularized genre effect (PCA)`),
                       description = "model 5 + regularized genre effect through PCA"))

results$test <-
  results$test %>%
  bind_rows(data.frame(modelId = 7,
                       RMSE = RMSE(test$rating, predictions$test$`regularized genre effect (PCA)`),
                       description = "model 5 + regularized genre effect through PCA"))

# remove temporary objects

rm(residuals.users,residuals.users.genres,users.genres.df,
   users.genres.matrix,users.pc.df,users.pc.matrix)

# Movie age effect -----------------------------------------------------------------------------------------------------
# WARNING: may take several minutes to run
# WARNING: may require access to additional memory to run - in this case, run following line:
# memory.limit(15000)

# get global mean residual from previous model on the train set

global.mean.residual <- 
  predictions$train %>% 
  select(userId,movieId,rating,`regularized genre effect (PCA)`) %>% 
  mutate(residual = rating - `regularized genre effect (PCA)`) %>% 
  summarise("mean residual" = mean(residual)) %>% 
  pull(`mean residual`)

# get movie age at the time of each rating and residuals from previous model

age.effect <-
  # gather variables required for calculations
  predictions$train %>% 
  select(userId,movieId,rating,`regularized genre effect (PCA)`) %>% 
  mutate(residual = rating - `regularized genre effect (PCA)`) %>% 
  left_join(select(movies,movieId,year), by = "movieId") %>% 
  left_join(select(ratings,userId,movieId,date), by = c("userId","movieId")) %>% 
  # calculate years since release at time of rating
  mutate("movie age" = limitRange(year(date) - year, 0, 100)) %>% 
  # get simple age effect (mean deviation from global average residual for each age)
  group_by(`movie age`) %>% 
  summarise("number of ratings" = n(),
            "number of movies" = n_distinct(movieId),
            "simple movie age effect" = mean(residual - global.mean.residual),
            .groups = 'drop')

# find best lambda for regularization

lambda <- c(0:20, seq(22,30,2), seq(35,60,5), seq(70,100,10), seq(200,1000,100))

rmse <- sapply(lambda, function(lambda){
  predictions$test %>% 
    select(userId,movieId,`regularized genre effect (PCA)`) %>% 
    # incorporate variables required to apply age effect
    left_join(select(test,userId,movieId,timestamp), by = c("userId","movieId")) %>% 
    left_join(select(movies,movieId,year), by = "movieId") %>% 
    # get date from timestamp
    mutate(date = as_date(as_datetime(timestamp))) %>% 
    # calculate movie age at time of rating
    mutate("movie age" = limitRange(year(date) - year, 0, 100)) %>% 
    # incorporate appropriate variables for each movie age to calculate prediction
    left_join(select(age.effect,`movie age`,`number of ratings`,`simple movie age effect`), 
              by = "movie age") %>% 
    # make prediction
    mutate(prediction = `regularized genre effect (PCA)` + 
             `simple movie age effect` * `number of ratings`/(`number of ratings` + lambda)) %>% 
    mutate(prediction = limitRange(prediction, .5, 5)) %>% 
    # calculate rmse
    summarise(rmse = RMSE(test$rating, prediction)) %>% 
    pull(rmse)
})

# save selected lambda to parameters object

parameters$`lambda (movie age effect)` <- lambda[which.min(rmse)]

# calculate movie age effect with selected lambda and save to parameters object
  
age.effect <-
  age.effect %>% 
  mutate("regularized movie age effect" = `simple movie age effect` * 
           `number of ratings` / (`number of ratings` + parameters$`lambda (movie age effect)`))

parameters$age.effect <- age.effect

# create model for predictions including the movie age effect

models$`movie age effect` <- function(df){
  # create matrix with user's reactions for each genre PC, same user order as the input data frame
  users.genres.pc <-
    # get user's reactions to each PC
    df %>% 
    select(userId) %>% 
    left_join(select(users,userId,`genre effect (PCA)`), by = "userId") %>% 
    select(-userId) %>% 
    separate(col = `genre effect (PCA)`, into = paste("PC",1:20), sep = "\\|", 
             remove = TRUE, convert = TRUE) %>% 
    as.matrix()
  
  # create matrix with movie's decomposition into genres PCs, same movie order as the input data frame
  movies.genres.pc <-
    genres$principal.components$x[as.character(df$movieId),]
  
  # create predictions
  df %>% 
    # get variables necessary for calculation
    transmute(userId,movieId,date = as_date(as_datetime(timestamp))) %>% 
    left_join(select(users,userId,`regularized user effect`), by = "userId") %>% 
    left_join(select(movies,movieId,year,`regularized movie effect`), by = "movieId") %>% 
    mutate("movie age" = limitRange(year(date) - year, 0, 100)) %>% 
    left_join(select(parameters$age.effect,`movie age`,`regularized movie age effect`), by = "movie age") %>% 
    # calculate correction due to genre effect
    mutate("genre effect" = rowSums(users.genres.pc * movies.genres.pc)) %>% 
    # make prediction
    mutate(prediction = parameters$`global average` + `regularized movie effect` + `regularized user effect` +
             `genre effect` + `regularized movie age effect`) %>% 
    mutate(prediction = limitRange(prediction, 0.5, 5)) %>% 
    pull(prediction)
}

# make prediction on train and test sets and save results to results object

predictions$train$`movie age effect` <- models$`movie age effect`(train)
predictions$test$`movie age effect` <- models$`movie age effect`(test)

results$train <-
  results$train %>%
  bind_rows(data.frame(modelId = 8,
                       RMSE = RMSE(train$rating, predictions$train$`movie age effect`),
                       description = "model 7 + regularized movie age effect"))
results$test <-
  results$test %>%
  bind_rows(data.frame(modelId = 8,
                       RMSE = RMSE(test$rating, predictions$test$`movie age effect`),
                       description = "model 7 + regularized movie age effect"))

# remove temporary variables
rm(global.mean.residual,age.effect,lambda,rmse)

# Collaborative filter - IBCF ------------------------------------------------------------------------------------------
# WARNING: may take upwards of one hour to run

# define some useful functions for sparse matrices

sparse.colSums <- function(M){t(M) %*% matrix(1, nrow = nrow(M), ncol = 1)}

sparse.colMeans <- function(M){t(M) %*% matrix(1/nrow(M), nrow = nrow(M), ncol = 1)}

sparse.colCenter <- function(M){t(t(M) - as.numeric(sparse.colMeans(M)))}

sparse.correlationCommonRows <- function(M,N){
  # deviations from mean
  norm.M <- sparse.colCenter(M)
  norm.N <- sparse.colCenter(N)
  # sum of squared errors for each pair of columns form first and second matrices
  sse.M <- t(norm.M^2) %*% !near(N,0) # sse of each column in M matched with non-zero entries of each column of N
  sse.N <- t(norm.N^2) %*% !near(M,0) # sse of each column in N matched with non-zero entries of each column of M
  # correlation matrix
  (t(norm.M) %*% norm.N) / sqrt(sse.M * t(sse.N))
}

# create model that performs training on the train set and makes predictions on the df set passed as input

models$`similar movies effect` <- function(df){
  
  # calculate mean residuals for each user using previous model
  
  mean.user.residuals <-
    predictions$train %>% 
    select(userId,rating,`movie age effect`) %>% 
    # get mean residual per user
    group_by(userId) %>% 
    summarise("mean residual" = mean(rating - `movie age effect`),
              .groups = 'drop')
  
  # create sparse matrix with each rating's deviation from mean user residual
  
  residuals.matrix <-
    # join prediction from previous model with mean residual for each user
    predictions$train %>% 
    select(userId,movieId,rating,`movie age effect`) %>%
    mutate(residual = rating - `movie age effect`) %>% 
    left_join(mean.user.residuals, by = "userId") %>% 
    # calculate how each residual deviates from mean user residual
    transmute(userId,movieId,
              "deviation from mean" = residual - `mean residual`) %>% 
    # spread into sparse matrix
    (function(df){sparseMatrix(i = df$userId,
                               j = df$movieId,
                               x = df$`deviation from mean`,
                               dimnames = list(1:max(df$userId),
                                               1:max(df$movieId)))})
  
  # create partitions of users and movies
  
  numMovies <- 1000 # number of movies used as reference
  popularMovies <- top_n(x = movies, n = numMovies, wt = `number of ratings`) %>% pull(movieId) # movies used as reference
  numUserPartitions <-  7 # number of partitions in which to divide complete set of users
  numMoviePartitions <- 10 # number of partitions in which to divide complete set of movies
  partitionGrid <- expand.grid(1:numUserPartitions,1:numMoviePartitions) # grid with all partitions
  
  partitions <- list("users" = createFolds(y = users$userId,
                                           k = numUserPartitions,
                                           list = FALSE),
                     "movies" = createFolds(y = movies$movieId,
                                            k = numMoviePartitions,
                                            list = FALSE))
  
  # get predictions for each pair of users and movies partitions 
  # (trained on the train set and saved when matching pair is in the data frame passed as input to the model)
  
  collaborative.effect <-
    mapply(partitionGrid[,1],
           partitionGrid[,2],
           SIMPLIFY = FALSE,
           FUN = function(userPartition,moviePartition){
             
             # subset of the general matrix, with only the users in the current partition and the most popular movies
             M <- residuals.matrix[users$userId[which(partitions$users == userPartition)],
                                   popularMovies]
             
             # subset of the general matrix, with only movies and users in the current partitions
             U <- residuals.matrix[users$userId[which(partitions$users == userPartition)],
                                   movies$movieId[which(partitions$movies == moviePartition)]]
             
             # correlation between the movies in the current partition (U) and the most popular movies (M), as rated by users in current partition
             C <- sparse.correlationCommonRows(M,U)
             C[is.na(C)] <- 0
             
             # predictions for the users in the current partition and movies in the current partition
             Y <- t(t(M %*% C) / sparse.colSums(abs(C)))
             Y[is.na(Y)] <- 0
             
             # check which entries in the input data frame set will be filled with this user and movie partitions
             prediction.grid <-
               df %>%
               select(userId,movieId) %>%
               filter(userId %in% as.numeric(users$userId[which(partitions$users == userPartition)]),
                      movieId %in% as.numeric(as.character(movies$movieId[which(partitions$movies == moviePartition)]))) %>%
               as.matrix()
             
             # add predictions from collaborative filter into prediction matrix
             predictions.current.partition <- Y[matrix(as.character(prediction.grid), ncol = 2, byrow = FALSE)] %>% matrix(ncol = 1)
             prediction.matrix <- matrix(c(prediction.grid,predictions.current.partition),byrow=FALSE,ncol=3)
           })
  
  # calculate the correction based on each user's evaluations to similar movies
  
  similar.movies.effect <-
    collaborative.effect %>%
    plyr::rbind.fill.matrix() %>%
    as.data.frame() %>%
    transmute("userId" = `1`,
              "movieId" = `2`,
              "similar movies effect" = `3`)
  
  # make predictions on the data frame informed as input to the model
  
  predictions <-
    df %>% 
    left_join(similar.movies.effect, by = c("userId", "movieId")) %>% 
    mutate("previous prediction" = models$`movie age effect`(df)) %>% 
    left_join(mean.user.residuals, by = "userId") %>% 
    mutate(prediction = `previous prediction` + `similar movies effect` + `mean residual`) %>% 
    mutate(prediction = limitRange(prediction, 0.5, 5)) %>% 
    pull(prediction)
}

# make predictions on the train and test sets using model created above

predictions$train$`similar movies effect` <- models$`similar movies effect`(train)
predictions$test$`similar movies effect` <- models$`similar movies effect`(test)

# calculate RMSE ans save to results object

results$train <-
  results$train %>%
  bind_rows(data.frame(modelId = 9,
                       RMSE = RMSE(train$rating, predictions$train$`similar movies effect`),
                       description = "model 8 + similar movies effect"))

results$test <-
  results$test %>%
  bind_rows(data.frame(modelId = 9,
                       RMSE = RMSE(test$rating, predictions$test$`similar movies effect`),
                       description = "model 8 + similar movies effect"))

# Performance evaluation on the validation set -------------------------------------------------------------------------
# WARNING: may take upwards of one hour to run

# make predictions on the validation set
  
predictions$validation$`global average` <- models$`global average`(validation)
predictions$validation$`simple movie effect` <- models$`simple movie effect`(validation)
predictions$validation$`regularized movie effect` <- models$`regularized movie effect`(validation)
predictions$validation$`simple user effect` <- models$`simple user effect`(validation)
predictions$validation$`regularized user effect` <- models$`regularized user effect`(validation)
predictions$validation$`regularized drama/non-drama effect` <- models$`regularized drama/non-drama effect`(validation)
predictions$validation$`regularized genre effect (PCA)` <- models$`regularized genre effect (PCA)`(validation)
predictions$validation$`movie age effect` <- models$`movie age effect`(validation)
predictions$validation$`similar movies effect` <- models$`similar movies effect`(validation)
  
# evaluate RMSE on the validation set and save to results object

results$validation <- 
  data.frame(modelId = 1, RMSE = RMSE(validation$rating, predictions$validation$`global average`),
             description = "Global average (benchmark)") %>% 
  bind_rows(data.frame(modelId = 2, RMSE = RMSE(validation$rating, predictions$validation$`simple movie effect`),
                       description = "model 1 + simple movie effect")) %>% 
  bind_rows(data.frame(modelId = 3, RMSE = RMSE(validation$rating, predictions$validation$`regularized movie effect`),
                       description = "model 1 + regularized movie effect")) %>% 
  bind_rows(data.frame(modelId = 4, RMSE = RMSE(validation$rating, predictions$validation$`simple user effect`),
                       description = "model 3 + simple user effect")) %>% 
  bind_rows(data.frame(modelId = 5, RMSE = RMSE(validation$rating, predictions$validation$`regularized user effect`),
                       description = "model 3 + regularized user effect")) %>% 
  bind_rows(data.frame(modelId = 6, RMSE = RMSE(validation$rating, predictions$validation$`regularized drama/non-drama effect`),
                       description = "model 5 + regularized drama/non-drama effects")) %>% 
  bind_rows(data.frame(modelId = 7, RMSE = RMSE(validation$rating, predictions$validation$`regularized genre effect (PCA)`),
                       description = "model 5 + regularized genre effect through PCA")) %>% 
  bind_rows(data.frame(modelId = 8, RMSE = RMSE(validation$rating, predictions$validation$`movie age effect`),
                       description = "model 7 + regularized movie age effect")) %>% 
  bind_rows(data.frame(modelId = 9, RMSE = RMSE(validation$rating, predictions$validation$`similar movies effect`),
                       description = "model 8 + similar movies effect"))

# Detailed results for sample users ------------------------------------------------------------------------------------

# generate a test set with 10 users and every movie they have not rated in the train and test sets
random.users.set <-
  # create grid of movies and users
  expand.grid(sample(x = users$userId, size = 10), movies$movieId) %>% 
  rename("userId" = "Var1", "movieId" = "Var2") %>% 
  # remove entries already present in the train and test sets
  anti_join(select(train,userId,movieId), by = c("userId","movieId")) %>% 
  anti_join(select(train,userId,movieId), by = c("userId","movieId")) %>% 
  # include timestamp corresponding to last day in the train set and genres corresponding to each movie
  mutate(timestamp = max(train$timestamp)) %>% 
  left_join(select(movies,movieId,genres), by = "movieId")

# generate predictions for the set of random users
random.users.predictions <-
  random.users.set %>% 
  # get prediction from each of the models
  transmute(userId,movieId,
            `global average` = models$`global average`(random.users.set),
            `simple movie effect` = models$`simple movie effect`(random.users.set),
            `regularized movie effect` = models$`regularized movie effect`(random.users.set),
            `simple user effect` = models$`simple user effect`(random.users.set),
            `regularized user effect` = models$`regularized user effect`(random.users.set),
            `regularized drama/non-drama effect` = models$`regularized drama/non-drama effect`(random.users.set),
            `regularized genre effect (PCA)` = models$`regularized genre effect (PCA)`(random.users.set),
            `movie age effect` = models$`movie age effect`(random.users.set),
            `similar movies effect` = models$`similar movies effect`(random.users.set))

# save predictions to predictions object
predictions$`sample users` <-
  random.users.predictions

# remove temporary variables
rm(random.users.set,random.users.predictions)

# Save variables for use in the report ---------------------------------------------------------------------------------

save.image(paste(getwd(),"/HX9_MovieLens.RData", sep=""))
