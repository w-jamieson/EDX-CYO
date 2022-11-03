##Analysis of video game sales data##

# Install if required and load packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(arules)) install.packages("arules", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(randomForest)
library(arules)

# Download and read the vieao game sales csv file
dl <- tempfile()
download.file("https://github.com/w-jamieson/EDX-CYO/raw/main/Video_Games_Sales_as_at_22_Dec_2016.csv", dl)

gamesales <- read.csv(file = dl)

# Examining the data
str(gamesales)
head(gamesales)

## Data includes metadata along with sales by region, critic & user scores for 16719 games (16 columns)
## Some data such as critic/user scores/counts, developer and rating is missing

# Total Global Sales stats
nrow(gamesales)
sum(gamesales$Global_Sales)
mean(gamesales$Global_Sales)
sd(gamesales$Global_Sales)
max(gamesales$Global_Sales)
## A small number of the many games make up the bulk of the sales


# Games by genre
# Number of titles
ggplot(gamesales, aes(x = Genre)) +
  geom_bar() + ggtitle("Number of Titles")
gamesales %>%
  count(Genre)
# Total sales
ggplot(gamesales, aes(x = Genre, y = Global_Sales)) + geom_bar(stat = "identity") + ggtitle("Total Sales")
gamesales %>% 
  group_by(Genre) %>% 
  summarise(TotalSales = sum(Global_Sales))
# Average sales
ggplot(gamesales, aes(x = Genre, y = Global_Sales)) + stat_summary(fun="mean", geom = "bar") + ggtitle("Average Sales")
gamesales %>% 
  group_by(Genre) %>% 
  summarise(AvgSales = mean(Global_Sales))
# Standard deviation of sales
ggplot(gamesales, aes(x = Genre, y = Global_Sales)) + stat_summary(fun="sd", geom = "bar") + ggtitle("Standard Deviation of Sales")
gamesales %>% 
  group_by(Genre) %>% 
  summarise(SdSales = sd(Global_Sales))
# Summary
gamesales %>% 
  group_by(Genre) %>% 
  summarise(TotalSales = sum(Global_Sales), AvgSales = mean(Global_Sales), SdSales = sd(Global_Sales)) %>%
  arrange(., desc(TotalSales))

# Games by platform
# Number of titles
ggplot(gamesales, aes(x = Platform)) +
  geom_bar() + ggtitle("Number of Titles")
gamesales %>%
  count(Platform)
# Total sales
ggplot(gamesales, aes(x = Platform, y = Global_Sales)) + geom_bar(stat = "identity") + ggtitle("Total Sales")
gamesales %>% 
  group_by(Platform) %>% 
  summarise(TotalSales = sum(Global_Sales))
# Average sales
ggplot(gamesales, aes(x = Platform, y = Global_Sales)) + stat_summary(fun="mean", geom = "bar") + ggtitle("Average Sales")
gamesales %>% 
  group_by(Platform) %>% 
  summarise(AvgSales = mean(Global_Sales))
# Standard deviation of sales
ggplot(gamesales, aes(x = Platform, y = Global_Sales)) + stat_summary(fun="sd", geom = "bar")  + ggtitle("Standard Deviation of Sales")
gamesales %>% 
  group_by(Platform) %>% 
  summarise(SdSales = sd(Global_Sales))
#Summary
gamesales %>% 
  group_by(Platform) %>% 
  summarise(TotalSales = sum(Global_Sales), AvgSales = mean(Global_Sales), SdSales = sd(Global_Sales)) %>%
  arrange(., desc(TotalSales))

# Games by publisher
# Number of titles
ggplot(gamesales, aes(x = Publisher)) +
  geom_bar() + ggtitle("Number of Titles")
head(gamesales %>%
       count(Publisher))
# Total sales
ggplot(gamesales, aes(x = Publisher, y = Global_Sales)) + geom_bar(stat = "identity") + ggtitle("Total Sales")
gamesales %>% 
  group_by(Publisher) %>% 
  summarise(TotalSales = sum(Global_Sales))
# Average sales
ggplot(gamesales, aes(x = Publisher, y = Global_Sales)) + stat_summary(fun="mean", geom = "bar") + ggtitle("Average Sales")
gamesales %>% 
  group_by(Publisher) %>% 
  summarise(AvgSales = mean(Global_Sales))
# Standard deviation of sales
ggplot(gamesales, aes(x = Publisher, y = Global_Sales)) + stat_summary(fun="sd", geom = "bar") + ggtitle("Standard Deviation of Sales")
gamesales %>% 
  group_by(Publisher) %>% 
  summarise(SdSales = sd(Global_Sales))
#Summary
gamesales %>% 
  group_by(Publisher) %>% 
  summarise(TotalSales = sum(Global_Sales), AvgSales = mean(Global_Sales), SdSales = sd(Global_Sales)) %>%
  arrange(., desc(TotalSales))

# Games by year
# Number of titles
ggplot(gamesales, aes(x = Year_of_Release)) +
  geom_bar() + ggtitle("Number of Titles")
gamesales %>%
  count(Year_of_Release)
# Total sales
ggplot(gamesales, aes(x = Year_of_Release, y = Global_Sales)) + geom_bar(stat = "identity") + ggtitle("Total Sales")
gamesales %>% 
  group_by(Year_of_Release) %>% 
  summarise(TotalSales = sum(Global_Sales))
# Average sales
ggplot(gamesales, aes(x = Year_of_Release, y = Global_Sales)) + stat_summary(fun="mean", geom = "bar") + ggtitle("Average Sales")
gamesales %>% 
  group_by(Year_of_Release) %>% 
  summarise(AvgSales = mean(Global_Sales))
# Standard deviation of sales
ggplot(gamesales, aes(x = Year_of_Release, y = Global_Sales)) + stat_summary(fun="sd", geom = "bar") + ggtitle("Standard Deviation of Sales")
gamesales %>% 
  group_by(Year_of_Release) %>% 
  summarise(SdSales = sd(Global_Sales))
#Summary
gamesales %>% 
  group_by(Year_of_Release) %>% 
  summarise(TotalSales = sum(Global_Sales), AvgSales = mean(Global_Sales), SdSales = sd(Global_Sales)) %>%
  arrange(., desc(TotalSales))
## Year with most sales is 2008

# Yearly game sales by genre
ggplot(gamesales, aes(x = Year_of_Release, y = Global_Sales, fill = Genre)) + geom_bar(stat = "identity") + ggtitle("Yearly Sales by Genre")
# Yearly game sales by platform
ggplot(gamesales, aes(x = Year_of_Release, y = Global_Sales, fill = Platform)) + geom_bar(stat = "identity") + ggtitle("Yearly Sales by Platform")


#Sales data prior to 1996 is highly statistically different to future years. Also data appears incomplete post 2016. Filtering to years between these values.
gamesales <- gamesales %>% 
  filter(Year_of_Release >= 1996) %>%
  filter(Year_of_Release < 2016) 


ggplot(gamesales, aes(x = Year_of_Release, y = Global_Sales)) + geom_bar(stat = "identity") + ggtitle("Total Sales")

# Also Wii Sports can be seen as extreme outlier relative to other sales. Removing this title as it as unlikely to support predictive modelling.
gamesales <- gamesales %>% 
    filter(Name != "Wii Sports") 

# Filter out missing developers
gamesales <- gamesales %>% 
  filter(Developer != "") 
head(gamesales)

# Distribution of sales data
gamesales %>%
  ggplot(aes(NA_Sales)) + geom_histogram(binwidth = 1) + ggtitle("North America")
gamesales %>%
  ggplot(aes(JP_Sales)) + geom_histogram(binwidth = 1) + ggtitle("Japan")
gamesales %>%
  ggplot(aes(EU_Sales)) + geom_histogram(binwidth = 1) + ggtitle("Europe")
gamesales %>%
  ggplot(aes(Other_Sales)) + geom_histogram(binwidth = 1) + ggtitle("Other")
gamesales %>%
  ggplot(aes(Global_Sales)) + geom_histogram(binwidth = 1) + ggtitle("Global")
# Converting sales to log to create normal distribution assumption
gamesales <- gamesales %>%
  mutate(Global_Sales_log = log(Global_Sales))
gamesales %>%
    ggplot(aes(Global_Sales_log)) + geom_histogram(binwidth = 1) + ggtitle("Global log normal")

# Add categorical bins of global for classification prediction models
gamesales <- gamesales %>%
  mutate(Global_Sales_range = discretize(Global_Sales, method = "frequency", breaks = 10))
head(gamesales)
table(gamesales$Global_Sales_range)

#create train & test sets for data
set.seed(1,sample.kind="Rounding")
test_index <- createDataPartition(y = gamesales$Global_Sales_log, times = 1, p = 0.2, list = FALSE) 
train_set <- gamesales[-test_index,]
test_set <- gamesales[test_index,]

test_set <- test_set %>% 
  semi_join(train_set, by = "Genre") %>%
  semi_join(train_set, by = "Platform") %>%
  semi_join(train_set, by = "Publisher") %>%
  semi_join(train_set, by = "Developer")

# Function to calculate RMSE
RMSE <- function(true_sales, predicted_sales){
  sqrt(mean((true_sales - predicted_sales)^2))
}

# Model assuming same sames for all games
# Average
mu_hat <- mean(train_set$Global_Sales)
mu_hat

naive_rmse <- RMSE(test_set$Global_Sales, mu_hat)

rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

rmse_results %>% knitr::kable()

# Model based on on genre effect
mu <- mean(train_set$Global_Sales) 
genre_avgs <- train_set %>% 
  group_by(Genre) %>% 
  summarize(b_g = mean(Global_Sales - mu))

predicted_sales <- mu + test_set %>% 
  left_join(genre_avgs, by='Genre') %>%
  .$b_g

model_1_rmse <- RMSE (test_set$Global_Sales,predicted_sales)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Genre Effect Model",
                                     RMSE = model_1_rmse))

rmse_results %>% knitr::kable()

# Model based on genre and platform effect
platform_avgs <- train_set %>% 
  left_join(genre_avgs, by='Genre') %>%
  group_by(Platform) %>%
  summarize(b_p = mean(Global_Sales - mu - b_g))

predicted_sales <- test_set %>% 
  left_join(genre_avgs, by='Genre') %>%
  left_join(platform_avgs, by='Platform') %>%
  mutate(pred = mu + b_g + b_p) %>%
  .$pred

model_2_rmse <- RMSE (test_set$Global_Sales, predicted_sales)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Genre + Platform Effects Model",  
                                     RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()

# Model based on genre, platform & publisher effect
publisher_avgs <- train_set %>% 
  left_join(genre_avgs, by='Genre') %>%
  left_join(platform_avgs, by='Platform') %>%
  group_by(Publisher) %>%
  summarize(b_u = mean(Global_Sales - mu - b_g - b_p))

predicted_sales <- test_set %>% 
  left_join(genre_avgs, by='Genre') %>%
  left_join(platform_avgs, by='Platform') %>%
  left_join(publisher_avgs, by='Publisher') %>%
  mutate(pred = mu + b_g + b_p + b_u) %>%
  .$pred

model_3_rmse <- RMSE (test_set$Global_Sales, predicted_sales)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Genre + Platform + Publisher Effects Model",  
                                     RMSE = model_3_rmse ))
rmse_results %>% knitr::kable()

# Model based on genre, platform, publisher & developer effect
developer_avgs <- train_set %>% 
  left_join(genre_avgs, by='Genre') %>%
  left_join(platform_avgs, by='Platform') %>%
  left_join(publisher_avgs, by='Publisher') %>%
  group_by(Developer) %>%
  summarize(b_d = mean(Global_Sales - mu - b_g - b_p - b_u))

predicted_sales <- test_set %>% 
  left_join(genre_avgs, by='Genre') %>%
  left_join(platform_avgs, by='Platform') %>%
  left_join(publisher_avgs, by='Publisher') %>%
  left_join(developer_avgs, by='Developer') %>%
  mutate(pred = mu + b_g + b_p + b_u + b_d) %>%
  .$pred

model_4_rmse <- RMSE (test_set$Global_Sales, predicted_sales)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Genre + Platform + Publisher + Developer Effects Model",  
                                     RMSE = model_4_rmse ))
rmse_results %>% knitr::kable()


# Regularisation choosing penalty term for all effects
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_set$Global_Sales)
  b_g <- train_set %>%
    group_by(Genre) %>%
    summarize(b_g = sum(Global_Sales - mu)/(n()+l))
  b_p <- train_set %>% 
    left_join(b_g, by='Genre') %>%
    group_by(Platform) %>%
    summarize(b_p = sum(Global_Sales - b_g - mu)/(n()+l))
  b_u <- train_set %>% 
    left_join(b_g, by='Genre') %>%
    left_join(b_p, by='Platform') %>%
    group_by(Publisher) %>%
    summarize(b_u = sum(Global_Sales - b_g - b_p - mu)/(n()+l))
  b_d <- train_set %>% 
    left_join(b_g, by='Genre') %>%
    left_join(b_p, by='Platform') %>%
    left_join(b_u, by='Publisher') %>%
    group_by(Developer) %>%
    summarize(b_d = sum(Global_Sales - b_g - b_p - b_u - mu)/(n()+l))
  predicted_sales <- 
    test_set %>% 
    left_join(b_g, by='Genre') %>%
    left_join(b_p, by='Platform') %>%
    left_join(b_u, by='Publisher') %>% 
    left_join(b_d, by='Developer') %>% 
    mutate(pred = mu + b_g + b_p + b_u + b_d) %>%
    .$pred
  return(RMSE(test_set$Global_Sales, predicted_sales))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Genre + Platform + Publisher + Developer Effect Model",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()

#No significant improvement from adding effects or regularisation indicates it won't be possible to use these predictors to accurately estimate global sales

# Testing using categorical predictor model such as Random Forest
train_rf <- randomForest(Global_Sales_range ~ Genre + Platform + Publisher, data = train_set)
preds_rf <- predict(train_rf,test_set)
confusionMatrix(preds_rf,test_set$Global_Sales_range)$overall["Accuracy"]

# Accuracy very low. Model using only Genre & Platform slightly higher
train_rf <- randomForest(Global_Sales_range ~ Genre + Platform, data = train_set)
preds_rf <- predict(train_rf,test_set)
confusionMatrix(preds_rf,test_set$Global_Sales_range)$overall["Accuracy"]

#Filter out NA Critic & User Scores
gamesales_critic <- gamesales %>%
  filter(Critic_Count != 'NA') %>%
  filter(User_Count != 'NA')
head(gamesales_critic)

# Convert user scores to numeric and also convert all scores to 5 point scale
gamesales_critic <- gamesales_critic %>%
  mutate(Critic_Score = Critic_Score / 20) %>%
  mutate(User_Score = as.numeric(User_Score)/2) 
head(gamesales_critic)

#Distribution of critic scores
gamesales_critic %>%
  ggplot(aes(Critic_Score)) + geom_histogram(binwidth = 0.5) + ggtitle("Critic Score Distribution")

#Distribution of user scores
gamesales_critic %>%
  ggplot(aes(User_Score)) + geom_histogram(binwidth = 0.5) + ggtitle("User Score Distribution")

#Investigate relationship between critic & user scores and global sales
ggplot(gamesales_critic, aes(x = Critic_Score, y = Global_Sales_log)) + geom_point() + geom_smooth(method = "lm") + ggtitle("Global Sales (log) vs Critic Score")
ggplot(gamesales_critic, aes(x = User_Score, y = Global_Sales_log)) + geom_point() + geom_smooth(method = "lm") + ggtitle("Global Sales (log) vs User Score")

ggplot(gamesales_critic, aes(x = Critic_Count, y = Global_Sales_log)) + geom_point() + geom_smooth(method = "lm") + ggtitle("Global Sales (log) vs Critic Count")
ggplot(gamesales_critic, aes(x = User_Count, y = Global_Sales_log)) + geom_point() + geom_smooth(method = "lm") + ggtitle("Global Sales (log) vs User Count")


#create train & test sets for critic filtered data
set.seed(1,sample.kind="Rounding")
test_index <- createDataPartition(y = gamesales_critic$Global_Sales, times = 1, p = 0.2, list = FALSE) 
train_critic <- gamesales_critic[-test_index,]
test_critic <- gamesales_critic[test_index,]

test_critic <- test_critic %>% 
  semi_join(train_critic, by = "Genre") %>% 
  semi_join(train_critic, by = "Platform") %>%
  semi_join(train_critic, by = "Publisher") %>%
  semi_join(train_critic, by = "Developer")

# guess as average of global sales
m <- mean(train_critic$Global_Sales_log)
naive_sl <- mean((m-test_critic$Global_Sales_log)^2)

SL_results <- data_frame(method = "Just the average", SL = naive_sl)

# fit linear regression model on global_sales + critics scores/counts
fit <- lm(Global_Sales_log ~ Critic_Score, data = train_critic)
fit$coefficients
y_hat <- predict(fit,test_critic)
SL1 <- mean((y_hat - test_critic$Global_Sales_log)^2)

SL_results <- bind_rows(SL_results,
                          data_frame(method="Critic Scores",
                                     SL = SL1))

fit <- lm(Global_Sales_log ~ Critic_Count, data = train_critic)
fit$coefficients
y_hat <- predict(fit,test_critic)
SL2 <- mean((y_hat - test_critic$Global_Sales_log)^2)

SL_results <- bind_rows(SL_results,
                        data_frame(method="Critic Count",
                                   SL = SL2))


# fit linear regression model on global_sales + user scores/counts
fit <- lm(Global_Sales_log ~ User_Score, data = train_critic)
fit$coefficients
y_hat <- predict(fit,test_critic)
SL3 <- mean((y_hat - test_critic$Global_Sales_log)^2)

SL_results <- bind_rows(SL_results,
                        data_frame(method="User Score",
                                   SL = SL3))

fit <- lm(Global_Sales_log ~ User_Count, data = train_critic)
fit$coefficients
y_hat <- predict(fit,test_critic)
SL4 <- mean((y_hat - test_critic$Global_Sales_log)^2)

SL_results <- bind_rows(SL_results,
                        data_frame(method="User Count",
                                   SL = SL4))

SL_results %>% knitr::kable()

#Multi-predictor algorithms
fit <- lm(Global_Sales_log ~ Critic_Score + User_Score, data = train_critic)
fit$coefficients
y_hat <- predict(fit,test_critic)
SL5 <- mean((y_hat - test_critic$Global_Sales_log)^2)

SL_results <- bind_rows(SL_results,
                        data_frame(method="Critic + User Score",
                                   SL = SL5))

fit <- lm(Global_Sales_log ~ Critic_Score + Critic_Count, data = train_critic)
fit$coefficients
y_hat <- predict(fit,test_critic)
SL6 <- mean((y_hat - test_critic$Global_Sales_log)^2)

SL_results <- bind_rows(SL_results,
                        data_frame(method="Critic Score + Count",
                                   SL = SL6))

fit <- lm(Global_Sales_log ~ Critic_Score + Critic_Count + User_Score + User_Count, data = train_critic)
fit$coefficients
y_hat <- predict(fit,test_critic)
SL7 <- mean((y_hat - test_critic$Global_Sales_log)^2)

SL_results <- bind_rows(SL_results,
                        data_frame(method="Critic Score + User Score + Critic Count + User Count",
                                   SL = SL7))

SL_results %>% knitr::kable()


# Predict user reception from other variables
# Model assuming same user score for all games
mu_hat <- mean(train_critic$User_Score)
mu_hat

naive_rmse <- RMSE(test_critic$User_Score, mu_hat)

rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

# Model based on on genre effect
mu <- mean(train_critic$User_Score) 
genre_avgs <- train_critic %>% 
  group_by(Genre) %>% 
  summarize(b_g = mean(User_Score - mu))

predicted_sales <- mu + test_critic %>% 
  left_join(genre_avgs, by='Genre') %>%
  .$b_g

model_1_rmse <- RMSE (test_critic$User_Score,predicted_sales)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Genre Effect Model",
                                     RMSE = model_1_rmse))

# Model based on genre and platform effect
platform_avgs <- train_critic %>% 
  left_join(genre_avgs, by='Genre') %>%
  group_by(Platform) %>%
  summarize(b_p = mean(User_Score - mu - b_g))

predicted_sales <- test_critic %>% 
  left_join(genre_avgs, by='Genre') %>%
  left_join(platform_avgs, by='Platform') %>%
  mutate(pred = mu + b_g + b_p) %>%
  .$pred

model_2_rmse <- RMSE (test_critic$User_Score, predicted_sales)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Genre + Platform Effects Model",  
                                     RMSE = model_2_rmse ))

# Model based on genre, platform & publisher effect
publisher_avgs <- train_critic %>% 
  left_join(genre_avgs, by='Genre') %>%
  left_join(platform_avgs, by='Platform') %>%
  group_by(Publisher) %>%
  summarize(b_u = mean(User_Score - mu - b_g - b_p))

predicted_sales <- test_critic %>% 
  left_join(genre_avgs, by='Genre') %>%
  left_join(platform_avgs, by='Platform') %>%
  left_join(publisher_avgs, by='Publisher') %>%
  mutate(pred = mu + b_g + b_p + b_u) %>%
  .$pred

model_3_rmse <- RMSE (test_critic$User_Score, predicted_sales)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Genre + Platform + Publisher Effects Model",  
                                     RMSE = model_3_rmse ))

# Model based on genre, platform, publisher & developer effect
developer_avgs <- train_critic %>% 
  left_join(genre_avgs, by='Genre') %>%
  left_join(platform_avgs, by='Platform') %>%
  left_join(publisher_avgs, by='Publisher') %>%
  group_by(Developer) %>%
  summarize(b_d = mean(User_Score - mu - b_g - b_p - b_u))

predicted_sales <- test_critic %>% 
  left_join(genre_avgs, by='Genre') %>%
  left_join(platform_avgs, by='Platform') %>%
  left_join(publisher_avgs, by='Publisher') %>%
  left_join(developer_avgs, by='Developer') %>%
  mutate(pred = mu + b_g + b_p + b_u + b_d) %>%
  .$pred

model_4_rmse <- RMSE (test_critic$User_Score, predicted_sales)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Genre + Platform + Publisher + Developer Effects Model",  
                                     RMSE = model_4_rmse ))

# Regularisation choosing penalty term for all effects
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_critic$User_Score)
  b_g <- train_critic %>%
    group_by(Genre) %>%
    summarize(b_g = sum(User_Score - mu)/(n()+l))
  b_p <- train_critic %>% 
    left_join(b_g, by='Genre') %>%
    group_by(Platform) %>%
    summarize(b_p = sum(User_Score - b_g - mu)/(n()+l))
  b_u <- train_critic %>% 
    left_join(b_g, by='Genre') %>%
    left_join(b_p, by='Platform') %>%
    group_by(Publisher) %>%
    summarize(b_u = sum(User_Score - b_g - b_p - mu)/(n()+l))
  b_d <- train_critic %>% 
    left_join(b_g, by='Genre') %>%
    left_join(b_p, by='Platform') %>%
    left_join(b_u, by='Publisher') %>%
    group_by(Developer) %>%
    summarize(b_d = sum(User_Score - b_g - b_p - b_u - mu)/(n()+l))
  predicted_sales <- 
    test_critic %>% 
    left_join(b_g, by='Genre') %>%
    left_join(b_p, by='Platform') %>%
    left_join(b_u, by='Publisher') %>% 
    left_join(b_d, by='Developer') %>% 
    mutate(pred = mu + b_g + b_p + b_u + b_d) %>%
    .$pred
  return(RMSE(test_critic$User_Score, predicted_sales))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Genre + Platform + Publisher + Developer Effect Model",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()

# Predict critical reception from other variables
# Model assuming same critic score for all games
mu_hat <- mean(train_critic$Critic_Score)
mu_hat

naive_rmse <- RMSE(test_critic$Critic_Score, mu_hat)

rmse_results <- data_frame(method = "Just the average", RMSE = naive_rmse)

# Model based on on genre effect
mu <- mean(train_critic$Critic_Score) 
genre_avgs <- train_critic %>% 
  group_by(Genre) %>% 
  summarize(b_g = mean(Critic_Score - mu))

predicted_sales <- mu + test_critic %>% 
  left_join(genre_avgs, by='Genre') %>%
  .$b_g

model_1_rmse <- RMSE (test_critic$Critic_Score,predicted_sales)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Genre Effect Model",
                                     RMSE = model_1_rmse))


# Model based on genre and platform effect
platform_avgs <- train_critic %>% 
  left_join(genre_avgs, by='Genre') %>%
  group_by(Platform) %>%
  summarize(b_p = mean(Critic_Score - mu - b_g))

predicted_sales <- test_critic %>% 
  left_join(genre_avgs, by='Genre') %>%
  left_join(platform_avgs, by='Platform') %>%
  mutate(pred = mu + b_g + b_p) %>%
  .$pred

model_2_rmse <- RMSE (test_critic$Critic_Score, predicted_sales)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Genre + Platform Effects Model",  
                                     RMSE = model_2_rmse ))

# Model based on genre, platform & publisher effect
publisher_avgs <- train_critic %>% 
  left_join(genre_avgs, by='Genre') %>%
  left_join(platform_avgs, by='Platform') %>%
  group_by(Publisher) %>%
  summarize(b_u = mean(Critic_Score - mu - b_g - b_p))

predicted_sales <- test_critic %>% 
  left_join(genre_avgs, by='Genre') %>%
  left_join(platform_avgs, by='Platform') %>%
  left_join(publisher_avgs, by='Publisher') %>%
  mutate(pred = mu + b_g + b_p + b_u) %>%
  .$pred

model_3_rmse <- RMSE (test_critic$Critic_Score, predicted_sales)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Genre + Platform + Publisher Effects Model",  
                                     RMSE = model_3_rmse ))

# Model based on genre, platform, publisher & developer effect
developer_avgs <- train_critic %>% 
  left_join(genre_avgs, by='Genre') %>%
  left_join(platform_avgs, by='Platform') %>%
  left_join(publisher_avgs, by='Publisher') %>%
  group_by(Developer) %>%
  summarize(b_d = mean(Critic_Score - mu - b_g - b_p - b_u))

predicted_sales <- test_critic %>% 
  left_join(genre_avgs, by='Genre') %>%
  left_join(platform_avgs, by='Platform') %>%
  left_join(publisher_avgs, by='Publisher') %>%
  left_join(developer_avgs, by='Developer') %>%
  mutate(pred = mu + b_g + b_p + b_u + b_d) %>%
  .$pred

model_4_rmse <- RMSE (test_critic$Critic_Score, predicted_sales)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Genre + Platform + Publisher + Developer Effects Model",  
                                     RMSE = model_4_rmse ))

# Regularisation choosing penalty term for all effects
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_critic$Critic_Score)
  b_g <- train_critic %>%
    group_by(Genre) %>%
    summarize(b_g = sum(Critic_Score - mu)/(n()+l))
  b_p <- train_critic %>% 
    left_join(b_g, by='Genre') %>%
    group_by(Platform) %>%
    summarize(b_p = sum(Critic_Score - b_g - mu)/(n()+l))
  b_u <- train_critic %>% 
    left_join(b_g, by='Genre') %>%
    left_join(b_p, by='Platform') %>%
    group_by(Publisher) %>%
    summarize(b_u = sum(Critic_Score - b_g - b_p - mu)/(n()+l))
  b_d <- train_critic %>% 
    left_join(b_g, by='Genre') %>%
    left_join(b_p, by='Platform') %>%
    left_join(b_u, by='Publisher') %>%
    group_by(Developer) %>%
    summarize(b_d = sum(Critic_Score - b_g - b_p - b_u - mu)/(n()+l))
  predicted_sales <- 
    test_critic %>% 
    left_join(b_g, by='Genre') %>%
    left_join(b_p, by='Platform') %>%
    left_join(b_u, by='Publisher') %>% 
    left_join(b_d, by='Developer') %>% 
    mutate(pred = mu + b_g + b_p + b_u + b_d) %>%
    .$pred
  return(RMSE(test_critic$Critic_Score, predicted_sales))
})

qplot(lambdas, rmses)  

lambda <- lambdas[which.min(rmses)]
lambda

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized Genre + Platform + Publisher + Developer Effect Model",  
                                     RMSE = min(rmses)))
rmse_results %>% knitr::kable()

# Final Model
l = 1.25
mu <- mean(train_critic$Critic_Score)
b_g <- train_critic %>%
  group_by(Genre) %>%
  summarize(b_g = sum(Critic_Score - mu)/(n()+l))
b_p <- train_critic %>% 
  left_join(b_g, by='Genre') %>%
  group_by(Platform) %>%
  summarize(b_p = sum(Critic_Score - b_g - mu)/(n()+l))
b_u <- train_critic %>% 
  left_join(b_g, by='Genre') %>%
  left_join(b_p, by='Platform') %>%
  group_by(Publisher) %>%
  summarize(b_u = sum(Critic_Score - b_g - b_p - mu)/(n()+l))
b_d <- train_critic %>% 
  left_join(b_g, by='Genre') %>%
  left_join(b_p, by='Platform') %>%
  left_join(b_u, by='Publisher') %>%
  group_by(Developer) %>%
  summarize(b_d = sum(Critic_Score - b_g - b_p - b_u - mu)/(n()+l))
predicted_sales <- 
  test_critic %>% 
  left_join(b_g, by='Genre') %>%
  left_join(b_p, by='Platform') %>%
  left_join(b_u, by='Publisher') %>% 
  left_join(b_d, by='Developer') %>% 
  mutate(pred = mu + b_g + b_p + b_u + b_d) %>%
  .$pred

final_RMSE <- RMSE(test_critic$Critic_Score, predicted_sales)
final_RMSE

