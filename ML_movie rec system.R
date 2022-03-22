## install/import packages
install.packages("recommenderlab")
library(recommenderlab)
library(ggplot2)
library(data.table)
library(reshape2)

## retrieve data & check it out
mdata <- movies
rdata <- ratings

str(mdata)
summary(mdata)
head(mdata)

str(rdata)
summary(rdata)
head(rdata)

## create usable genre matrix
m_genre <- as.data.frame(mdata$genres, stringsAsFactors = FALSE)
m_genre2 <- as.data.frame(tstrsplit(m_genre[,1],'[|]', 
                                    type.convert = TRUE), stringsAsFactors = FALSE)
colnames(m_genre2) <- c(1:10)

list_genre <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")
genre_mat1 <- matrix(0,10330,18)
genre_mat1[1,] <- list_genre
colnames(genre_mat1) <- list_genre

for (index in 1:nrow (m_genre2)) {
  for (col in 1:ncol(m_genre2)) {
    gen_col = which(genre_mat1[1,] == m_genre2[index,col])
    genre_mat1[index + 1, gen_col] <- 1
  }
}
genre_mat2 <- as.data.frame(genre_mat1[-1,], stringsAsFactors = FALSE) #remove first row (genre list)

for (col in 1:ncol(genre_mat2)) {
  genre_mat2[,col] <- as.integer(genre_mat2[,col]) #convert characters to integers
}
str(genre_mat2)

## create a search matrix
search_matrix <- cbind(mdata[,1:2], genre_mat2[])
head(search_matrix)

## convert ratings to sparse matrix
rating_matrix <- dcast(rdata, userId~movieId, value.var = "rating", na.rm = FALSE)
rating_matrix <- as.matrix(rating_matrix[,-1]) #remove userIds
rating_matrix <- as(rating_matrix, "realRatingMatrix") #convert using recommenderlab
rating_matrix

## overview of parameters
rec_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
names(rec_model)
lapply(rec_model, "[[", "description")

## implement a single model - item based collaborative filtering (IBCF)
rec_model$IBCF_realRatingMatrix$parameters

## explore similarity data
similarity_matrix <- similarity(rating_matrix[1:4,],
                                method = "cosine",
                                which = "users") #users
as.matrix(similarity_matrix)
image(as.matrix(similarity_matrix), main = "User's Similarities") 

movie_similarity <- similarity(rating_matrix[,1:4],
                                method = "cosine",
                                which = "items")
as.matrix(movie_similarity)
image(as.matrix(movie_similarity), main = "Movies Similarity")

## extract the most unique ratings & create a table
rating_values <- as.vector(rating_matrix@data)
unique(rating_values)

table_of_ratings <- table(rating_values)
table_of_ratings

## lets look at the most viewed movies
movie_views <- colCounts(rating_matrix) #counts views
table_views <- data.frame(movie = names(movie_views),
                          views = movie_views) #df of views
table_views <- table_views[order(table_views$views,
                                 decreasing = TRUE), ] #sort by views
table_views$title <- NA
for (index in 1:10325) {
  table_views[index,3] <- as.character(subset(mdata,
              mdata$movieId == table_views[index, 1])$title)
}
table_views[1:6,]

## visualize w/ a bar plot
ggplot(table_views[1:6, ],  aes(x = title, y = views)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label=views), vjust = -0.3, size = 3.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  ggtitle("Total Views of the Top Films")

## heat map of movie ratings
image(rating_matrix[1:20,1:25], axes = FALSE, 
      main = "Heat map of the first 25 rows and columns")

## now we shall do some data prep - set min ratings to 50
movie_ratings <- rating_matrix[rowCounts(rating_matrix) > 50,
                               colCounts(rating_matrix) > 50]
movie_ratings

#trim down to the top users
minimum_movies <- quantile(rowCounts(movie_ratings), 0.98)
minimum_users <- quantile(colCounts(movie_ratings), 0.98)

#heat map of top movies vs. top users
image(movie_ratings[rowCounts(movie_ratings) > minimum_movies,
      colCounts(movie_ratings) > minimum_users],
      main = "Heat map of the top users & movies")

## average ratings per user distribution
average_ratings <- rowMeans(movie_ratings)
qplot(average_ratings, fill=I("steelblue"), col=I("red")) +
  ggtitle("Distribution of the average rating per user")

#we need to remove bias from the model - normalize the data
normalized_ratings <- normalize(movie_ratings)
sum(rowMeans(normalized_ratings) > 0.00001)


image(normalized_ratings[rowCounts(normalized_ratings) >
                           minimum_movies, 
                         colCounts(normalized_ratings) >
                           minimum_users],
      main = "Normalized Ratings of the Top Users")

## data binarization!!
binary_minimum_movies <- quantile(rowCounts(movie_ratings), .95)
binary_minimum_users <- quantile(colCounts(movie_ratings), .95)
#movies_watched <- binarize(movie_ratings, minRating = 1)

good_rated_films <- binarize(movie_ratings, minRating = 3)
image(good_rated_films[rowCounts(movie_ratings) >
                         binary_minimum_movies,
                       colCounts(movie_ratings) > 
                         binary_minimum_users],
      main = "Heat map of the top users and movies")

## collaborative filtering system (similarities between people)
#start with splitting into training (80%) and test (20%) sets
sampled_data <- sample(x = c(TRUE, FALSE),
                       size = nrow(movie_ratings),
                       replace = TRUE,
                       prob = c(0.8,0.2))
training_data <- movie_ratings[sampled_data, ]
testing_data <- movie_ratings[!sampled_data, ]

## building the rec system
recommendation_system <- recommenderRegistry$get_entries(dataType =
                                                    "realRatingMatrix")
recommendation_system$IBCF_realRatingMatrix$parameters

recommend_model <- Recommender(data = training_data,
                               method = "IBCF",
                               parameter = list(k = 30))
recommend_model
class(recommend_model)

## explore the model
model_info <- getModel(recommend_model)
class(model_info$sim)
dim(model_info$sim)
top_items <- 20
image(model_info$sim[1:top_items, 1:top_items],
      main = "Heat map of the first rows and columns")

## sum of rows & columns w/ similarities > 0
sum_rows <- rowSums(model_info$sim > 0)
table(sum_rows)

sum_cols <- colSums(model_info$sim > 0)
qplot(sum_cols, fill = I("steelblue"), col = I("red")) +
  ggtitle("Distribution of the column count")

## create top recs for each user
top_rec <- 10
predicted_rec <- predict(object = recommend_model,
                         newdata = testing_data,
                         n = top_rec)
predicted_rec

#lets look at recs for the first user
user1 <- predicted_rec@items[[1]]
movies_user1 <- predicted_rec@itemLabels[user1]
movies_user2 <- movies_user1
for (index in 1:10) {
  movies_user2[index] <- as.character(subset(mdata,
                                            mdata$movieId ==
                                            movies_user1[index])$title)
}
movies_user2

#matrix w/ recs for each user
rec_matrix <- sapply(predicted_rec@items,
              function(x){as.integer(colnames(movie_ratings)[x])})
rec_matrix[,1:4]
