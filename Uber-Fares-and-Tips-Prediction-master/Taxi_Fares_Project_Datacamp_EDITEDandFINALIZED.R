
library(tidyverse)
library(tree)
library(ggmap)
library(viridis)
library(randomForest)
library(lubridate)
library(partykit)
library(rpart)

# Reading in the taxi data
taxi <- read_csv("~/Desktop/R/DataCamp Taxi Project/taxi.csv")
head(taxi)

# Renaming the location variables,
# dropping any journeys with zero fares and zero tips,
# and creating the total variable as the log sum of fare and tip
taxi <- taxi %>% rename(long = pickup_longitude)
taxi <- taxi %>% rename(lat = pickup_latitude)

colnames(taxi)

#The reason to define total as the the log() of fare_amount + tip_amount is that :
#by taking the log we remedy the effect of outliers by making really large numbers smaller.
taxi <- taxi %>% filter(fare_amount > 0 | tip_amount > 0)
taxi <- taxi %>% mutate(total = log(fare_amount+tip_amount))


# Reducing the data to taxi trips starting in Manhattan
# Manhattan is bounded by the rectangle with 
# latitude from 40.70 to 40.83 and 
# longitude from -74.025 to -73.93

taxi <- taxi  %>% 
  filter(between(lat, 40.70, 40.83) &
           between(long, -74.025, -73.93))


# Retrieving a stored map object which originally was created by
# manhattan <- get_map("manhattan", zoom = 12, color = "bw")
manhattan <- readRDS("~/Desktop/R/DataCamp Taxi Project/manhattan.rds")


# Drawing a density map with the number of journey start locations
ggmap(manhattan, darken = 0.5) +
   scale_fill_viridis(option = 'plasma') +
   geom_bin2d(aes(x = long, y = lat),data = taxi, bins = 60, alpha = 0.6) +
   labs(x="long",y="lat")


# Fitting a tree to lat and long
fitted_tree <- tree(total ~lat+long, data = taxi)

plot(fitted_tree)
text(fitted_tree)


#######_____FINDINGS______########################################################################
#6. It's time. More predictors.
#The tree above looks a bit frugal, it only includes one split: 
#It predicts that trips where lat < 40.7237 are more expensive, which makes sense as it is downtown Manhattan. 
#But that's it. It didn't even include long as tree deemed that it didn't improve the predictions. 
#Taxi drivers will need more information than this and any driver paying for your data-driven insights would be disappointed with that. 
#Let's start by adding some more predictors related to the time the taxi trip was made.
#################################################################################################



# Draw a diagram of the tree structure
# Generate the three new time variables
taxi <- taxi %>% 
    mutate(hour = hour(ymd_hms(pickup_datetime))
           ,wday = wday(pickup_datetime, label = TRUE)
           ,month = month(pickup_datetime, label = TRUE))

str(taxi)

# Fitting a tree with total as the outcome and 
# lat, long, hour, wday, and month as predictors
fitted_tree <- tree(total ~ long + hour + wday + month + lat, data = taxi)

fitted_tree1 <- rpart(total ~ long + hour + wday + month + lat, data = taxi)
# draw a diagram of the tree structure
plot(fitted_tree)
text(fitted_tree)
summary(fitted_tree)

plot(as.party(fitted_tree1))
rsq.rpart(fitted_tree1)



#######_____FINDINGS______########################################################################
#One tree is not enough
#The regression tree has not changed after including the three time variables. 
#This is likely because latitude is still the most promising first variable to split the data on, and after that split, 
#the other variables are not informative enough to be included. 
#A random forest model, where many different trees are fitted to subsets of the data, 
#may well include the other variables in some of the trees that make it up.
#################################################################################################



# Fitting a random forest
fitted_forest <- randomForest(total ~ lat + long + hour 
                              + wday + month, data = taxi,
                             ntree = 80, sampsize = 10000)

# Printing the fitted_forest object
plot(fitted_forest)
text(fitted_forest)
summary(fitted_forest)

#######_____FINDINGS______########################################################################
#fitted_forest output shows Mean of squared residuals (the average of the squared errors the model makes) 
#If you scroll up and check the summary of fitted_tree you'll find Residual mean deviance which is the same number. 
#If you compare these numbers, you'll see that fitted_forest has a slightly lower error. 
#Neither predictive model is that good, in statistical terms, they explain only about 3% of the variance.
#Now, let's take a look at the predictions of fitted_forest projected back onto Manhattan.
#################################################################################################

# Extracting the prediction from fitted_forest
taxi$pred_total <- fitted_forest$predicted

head(taxi)

# Plotting the predicted mean trip prices from according to the random forest
ggmap(manhattan, darken = 0.5) +
   scale_fill_viridis(option = 'plasma') +
   stat_summary_2d(aes(x = long, y = lat, z = pred_total)
                   ,fun = mean
                   ,data = taxi, bins = 60, alpha = 0.6) +
   labs(x="long",y="lat")


# Function that returns the mean *if* there are 15 or more datapoints
mean_if_enough_data <- function(x) { 
    ifelse( length(x) >= 15, mean(x), NA) 
}

# Plotting the mean trip prices from the data
ggmap(manhattan, darken = 0.5) +
   scale_fill_viridis(option = 'plasma') +
   stat_summary_2d(aes(x = long, y = lat, z = total)
                   ,fun = mean_if_enough_data
                   ,data = taxi, bins = 60, alpha = 0.6) +
   labs(x="long",y="lat")


# Where are people spending the most on their taxi trips?
spends_most_on_trips <- "downtown" # "uptown" or "downtown"


