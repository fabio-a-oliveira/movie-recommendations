# This script downloads the required dataset and saves it so that the 
# Shiny Web App can access it directly. However, it only needs to be run once 
# and is not referenced in the app.R script.

# Housekeeping -----------------------------------------------------------------

if(!require(tidyverse)) install.packages("tidyverse", 
                                         repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", 
                                          repos = "http://cran.us.r-project.org")

library(tidyverse)
library(data.table)

# Download MovieLens dataset ---------------------------------------------------

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- 
  fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
        col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- 
  str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

movies <- 
  as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

ratings <- left_join(ratings, movies, by = "movieId")

# Remove temporary objects
rm(movies,dl)

# Format ratings object --------------------------------------------------------
ratings <- 
  ratings %>% 
  mutate(year = as.integer(str_extract(title,'(?<=\\()\\d{4}(?=\\))')),
         title = str_replace(title,'\\s\\(\\d{4}\\)$','')) %>% 
  select(userId,movieId,rating,title,year,genres)

# Create movies object ---------------------------------------------------------

all.movies <- 
  ratings %>% 
  group_by(movieId,title,genres) %>% 
  summarise("number of ratings" = n(),
            "average rating" = mean(rating),
            .groups = 'drop') %>% 
  select(movieId,title,`average rating`,`number of ratings`, genres)

popular.movies <- 
  all.movies %>% 
  top_n(n = 1000, wt = `number of ratings`) %>% 
  arrange(desc(`number of ratings`))

best.movies <-
  all.movies %>% 
  filter(`number of ratings` > 100) %>% 
  top_n(n = 1000, wt = `average rating`) %>% 
  arrange(desc(`average rating`))

movies <- 
  list(selection = full_join(popular.movies,best.movies),
       popular = popular.movies$movieId,
       best = best.movies$movieId)

rm(all.movies,popular.movies,best.movies)

# Create users object ----------------------------------------------------------

all.users <-
  ratings %>% 
  group_by(userId) %>% 
  summarise("average rating" = mean(rating),
            "number of ratings" = n(),
            .groups = 'drop')

active.users <-
  all.users %>% 
  top_n(5000,`number of ratings`)

users <-
  list(all = all.users,
       active = active.users)

rm(all.users,active.users)

# Find correlation between movies in selection ---------------------------------

correlations <- 
  ratings %>% 
  filter(userId %in% users$active$userId,
         movieId %in% movies$selection$movieId) %>% 
  left_join(select(users$all, userId, `average rating`), by = "userId") %>% 
  transmute(userId,movieId,
            residual = rating - `average rating`) %>% 
  pivot_wider(names_from = "movieId",
              values_from = "residual",
              values_fill = 0) %>% 
  column_to_rownames("userId") %>% 
  as.matrix() %>% 
  cor()

movies$correlations <- 
  correlations %>% 
  round(2)*100 %>% 
  as.integer()

rm(correlations)

# Save objects used by the app -------------------------------------------------

save(movies, file = "MovieRecommenderApp/movies.RData")

# Sandbox ----------------------------------------------------------------------

movies$selection %>% 
  mutate("quality modifier" = `average rating`^2/25,
         "popularity modifier" = `number of ratings` / 
           max(`number of ratings`)) %>% 
  select(title, `quality modifier`,`popularity modifier`) %>% 
  slice_sample(n = 5,
               weight_by = 
                 (TRUE + FALSE * `quality modifier`) *
                 (TRUE + FALSE * `popularity modifier`),
               replace = FALSE)


movies$selection %>% 
  arrange(title)

arrange(movies$selection,title)$title



movies$selection %>% 
  filter(str_detect(title,"Star Wars")) %>% 
  select(movieId,title)
