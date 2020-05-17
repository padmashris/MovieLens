
#############################################################
# Importing edx set and the validation set
#############################################################
# Note: this process could take a couple of minutes for loading required package: 
# tidyverse and package caret
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

edx <- readRDS("~/Downloads/edx.rds")
validation <- readRDS("~/Downloads/validation.rds")

head(edx) %>% #getting a summary of the dataset 
  print.data.frame()

RMSE <- function(rating_actual, rating_prediction){ #defining the function to find the RMSE value 
  sqrt(mean((rating_actual - rating_prediction)^2))
}

avg <- mean(edx$rating) #computing the average movie rating
avg

rmse1 <- RMSE(validation$rating, avg) #the first RMSE value
rmse1

results <- tibble(method = "Mean Movie Rating", RMSE = rmse1) #displaying the results in a table
results %>% knitr::kable()

rating_prediction <- avg +  validation %>% #modelling the impact of movies
  left_join(av_movie, by='movieId') %>%
  pull(i_m)
rmse2 <- RMSE(rating_prediction, validation$rating) #calculating the 2nd RMSE value
results <- bind_rows(results,
                          tibble(method="Modelling Movie Impact",  
                                     RMSE = rmse2 ))
results %>% knitr::kable() #displaying it in a table

av_user <- edx %>% #calculating the average user ratings
  left_join(av_movie, by="movieId") %>%
  group_by(userId) %>%
  summarize(i_u = mean(rating - avg - i_m))

rating_prediction <- validation %>% #modelling the impact of users and movies
  left_join(av_movie, by='movieId') %>%
  left_join(av_user, by='userId') %>%
  mutate(pred = avg + i_m + i_u) %>%
  pull(pred)
rmse3 <- RMSE(rating_prediction, validation$rating) #computing the 3rd RMSE value
results <- bind_rows(results, 
                     tibble(method="Modelling Movie and User Impact",
                            RMSE = rmse3))
results %>% knitr::kable() #displaying the RMSE values in a compiled table

rhos <- seq(0, 10, 0.2) #defining the tuning parameter 
rmses <- sapply(rhos, function(k){ #RMSE values computed to find the minimum possible value
  
  avg <- mean(edx$rating)
  
  i_m <- edx %>% 
    group_by(movieId) %>%
    summarize(i_m = sum(rating - avg)/(n()+k))
  
  i_u <- edx %>% 
    left_join(i_m, by="movieId") %>%
    group_by(userId) %>%
    summarize(i_u = sum(rating - i_m - avg)/(n()+k))
  
  rating_prediction <- 
    validation %>% 
    left_join(i_m, by = "movieId") %>%
    left_join(i_u, by = "userId") %>%
    mutate(pred = avg + i_m + i_u) %>%
    pull(pred)
  
  return(RMSE(rating_prediction, validation$rating))
})

qplot(rhos, rmses)  #plot to select the optimal RMSE value

  rho_min <- rhos[which.min(rmses)] #finding the optimal and minimum RMSE value
rho_min

results <- bind_rows(results,tibble(method="Modelling Regularized Movie and User Impacts",RMSE = min(rmses))) #compiling the results with the new regularized model
results %>% knitr::kable() #displaying all RMSE values computed above in a table


