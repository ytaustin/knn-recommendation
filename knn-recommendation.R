library(tidyverse)
library(lubridate)
library(caret)




movies <- read_tsv("C:/Users/taoya/Desktop/MSCS6520/Homework/homework4/movies.dat")
userratings <- read_tsv("C:/Users/taoya/Desktop/MSCS6520/Homework/homework4/user_ratedmovies.dat")





movies<-mutate(movies, movieID=id)
movie_ratings<-inner_join(userratings, movies, key="movieID")


average_rating<-group_by(movie_ratings,movieID,title)%>%summarize(count = n(),avg_rating = mean(rating))
ggplot(data = average_rating, mapping = aes(avg_rating))+geom_histogram(binwidth = 0.5)
ggsave("rating_distribution.jpg")
ggplot(data = average_rating, mapping = aes(log1p(count)))+geom_density()
ggsave("number_rating_density.jpg")
ggplot(data = average_rating, mapping = aes(log1p(count), avg_rating))+geom_point()
ggsave("number_vs_avg.jpg")


set.seed(5647)
trainSize = floor(0.8*nrow(movie_ratings))
trainIndex = sample(seq_len(nrow(movie_ratings)),size = trainSize)
train <-movie_ratings[trainIndex,]
test<- movie_ratings[-trainIndex,]

#baseline, use overall average rating to predict the rating by each user

overall_avg_rating = mean(train$rating)
test_predictions_baseline <- mutate(test, overall_avg_rating= overall_avg_rating)
postResample(test_predictions_baseline$rating, test_predictions_baseline$overall_avg_rating)

# model_1, use average rating per movie to predict the rating by each user 
predicted_rating_model_1 <-group_by(train, movieID, title) %>% summarize(avg_rating= mean(rating))
test_prediction_model_1 <-inner_join(test,predicted_rating_model_1,key = "movieID")
postResample(test_prediction_model_1$rating, test_prediction_model_1$avg_rating)


# model_2, use average rating per movie for movies >=10 ratings to predict the rating by each user 
predicted_rating_model_2 <-group_by(train, movieID, title) %>% summarize(avg_rating= mean(rating), count = n())
predicted_rating_model_2 <-filter(predicted_rating_model_2, count>=10)
test_prediction_model_2 <-inner_join(test,predicted_rating_model_2,key = "movieID")
postResample(test_prediction_model_2$rating, test_prediction_model_2$avg_rating)



# model_3, use average rating per movie for movies >=50 ratings to predict the rating by each user 
predicted_rating_model_3 <-group_by(train, movieID, title) %>% summarize(avg_rating= mean(rating), count = n())
predicted_rating_model_3 <-filter(predicted_rating_model_3, count>=50)
test_prediction_model_3 <-inner_join(test,predicted_rating_model_3,key = "movieID")
postResample(test_prediction_model_3$rating, test_prediction_model_3$avg_rating)


# model_4, use average rating per movie for movies >=100 ratings to predict the rating by each user 
predicted_rating_model_4 <-group_by(train, movieID, title) %>% summarize(avg_rating= mean(rating), count = n())
predicted_rating_model_4 <-filter(predicted_rating_model_4, count>=100)
test_prediction_model_4 <-inner_join(test,predicted_rating_model_4,key = "movieID")
postResample(test_prediction_model_4$rating, test_prediction_model_4$avg_rating)



