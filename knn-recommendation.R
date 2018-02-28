library(tidyverse)
library(lubridate)
library(caret)




movies <- read_tsv("C:/Users/taoya/Desktop/MSCS6520/Homework/homework4/movies.dat")
userratings <- read_tsv("C:/Users/taoya/Desktop/MSCS6520/Homework/homework4/user_ratedmovies.dat")





movies<-mutate(movies, movieID=id)

movie_ratings<-inner_join(userratings, movies, key="movieID")






save.image("movies")
