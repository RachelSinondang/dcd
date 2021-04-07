library(shiny)
library(shinydashboard)
library(shinyWidgets)

# Data Wrangling
library(tidyverse) 

# Text analysis
library(textclean)
library(tidytext)
library(tm)

library(caret)
library(e1071)
library(rpart)
library(RTextTools)
library(tm)
library(DMwR)
library(plotly)
library(ggthemes)
library(ggwordcloud)
library(wordcloud)
library(rsample)


yelp_shop <- read.csv("sentiments_by_shop.csv")

yelp_shop <- yelp_shop[order(yelp_shop$rating, decreasing = T),]

yelp_sent <- read.csv("ratings_and_sentiments.csv")

yelp_sent <- yelp_sent %>% 
  filter(food_sent != "#VALUE!" ,
         coffee_sent != "#VALUE!",
         vibe_sent != "#VALUE!" ,
         parking_sent != "#VALUE!") %>%
  select(-c(rating, cat_rating, bool_HIGH)) %>%
  drop_na()

yelp_sent$overall_sent <- if_else(condition = yelp_sent$overall_sent < 0,true = "negative",
                                  false = if_else(condition = yelp_sent$overall_sent > 1,
                                                  true = "positive",false = "neutral"))

yelp_sent$vibe_sent <-if_else(condition = yelp_sent$vibe_sent < 0,true = "negative",
                              false = if_else(condition = yelp_sent$vibe_sent > 1,
                                              true = "positive",false = "neutral"))

yelp_sent$tea_sent <-if_else(condition = yelp_sent$tea_sent < 0,true = "negative",
                             false = if_else(condition = yelp_sent$tea_sent > 1,
                                             true = "positive",false = "neutral"))

yelp_sent$service_sent <-if_else(condition = yelp_sent$service_sent < 0,true = "negative",
                                 false = if_else(condition = yelp_sent$service_sent > 1,
                                                 true = "positive",false = "neutral"))

yelp_sent$price_sent <-if_else(condition = yelp_sent$price_sent < 0,true = "negative",
                               false = if_else(condition = yelp_sent$price_sent > 1,
                                               true = "positive",false = "neutral"))

yelp_sent$service_sent <-if_else(condition = yelp_sent$service_sent < 0,true = "negative",
                                 false = if_else(condition = yelp_sent$service_sent > 1,
                                                 true = "positive",false = "neutral"))

yelp_sent$seating_sent <-if_else(condition = yelp_sent$seating_sent < 0,true = "negative",
                                 false = if_else(condition = yelp_sent$seating_sent > 1,
                                                 true = "positive",false = "neutral"))

yelp_sent$parking_sent <-if_else(condition = yelp_sent$parking_sent < 0,true = "negative",
                                 false = if_else(condition = yelp_sent$parking_sent > 1,
                                                 true = "positive",false = "neutral"))

yelp_sent$location_sent <-if_else(condition = yelp_sent$location_sent < 0,true = "negative",
                                  false = if_else(condition = yelp_sent$location_sent > 1,
                                                  true = "positive",false = "neutral"))

yelp_sent$alcohol_sent <-if_else(condition = yelp_sent$alcohol_sent < 0,true = "negative",
                                 false = if_else(condition = yelp_sent$alcohol_sent > 1,
                                                 true = "positive",false = "neutral"))

yelp_sent$coffee_sent <-if_else(condition = yelp_sent$coffee_sent < 0,true = "negative",
                                false = if_else(condition = yelp_sent$coffee_sent > 1,
                                                true = "positive",false = "neutral"))

yelp_sent$food_sent <-if_else(condition = yelp_sent$food_sent < 0,true = "negative",
                              false = if_else(condition = yelp_sent$food_sent > 1,
                                              true = "positive",false = "neutral"))

yelp_sent$hours_sent <-if_else(condition = yelp_sent$hours_sent < 0,true = "negative",
                               false = if_else(condition = yelp_sent$hours_sent > 1,
                                               true = "positive",false = "neutral"))

yelp_sent$internet_sent <-if_else(condition = yelp_sent$internet_sent < 0,true = "negative",
                                  false = if_else(condition = yelp_sent$internet_sent > 1,
                                                  true = "positive",false = "neutral"))

yelp_sent$local_sent <-if_else(condition = yelp_sent$local_sent < 0,true = "negative",
                               false = if_else(condition = yelp_sent$local_sent > 1,
                                               true = "positive",false = "neutral"))

yelp_clean <- yelp_sent %>%
  mutate(coffee_shop_name = as.factor(coffee_shop_name),
         num_rating = as.factor(num_rating),
         overall_sent = as.factor(overall_sent),
         vibe_sent = as.factor(vibe_sent),
         tea_sent = as.factor(tea_sent),
         service_sent = as.factor(service_sent),
         seating_sent = as.factor(seating_sent),
         price_sent = as.factor(price_sent),
         parking_sent = as.factor(parking_sent),
         location_sent = as.factor(location_sent),
         alcohol_sent = as.factor(alcohol_sent),
         coffee_sent = as.factor(coffee_sent),
         food_sent = as.factor(food_sent),
         hours_sent = as.factor(hours_sent),
         internet_sent = as.factor(internet_sent),
         local_sent = as.factor(local_sent))

clean <- read.csv("yelp_clean.csv")

clean2 <- clean %>%
  mutate(coffee_shop_name = as.factor(coffee_shop_name),
         num_rating = as.factor(num_rating),
         overall_sent = as.factor(overall_sent),
         vibe_sent = as.factor(vibe_sent),
         tea_sent = as.factor(tea_sent),
         service_sent = as.factor(service_sent),
         seating_sent = as.factor(seating_sent),
         price_sent = as.factor(price_sent),
         parking_sent = as.factor(parking_sent),
         location_sent = as.factor(location_sent),
         alcohol_sent = as.factor(alcohol_sent),
         coffee_sent = as.factor(coffee_sent),
         food_sent = as.factor(food_sent),
         hours_sent = as.factor(hours_sent),
         internet_sent = as.factor(internet_sent),
         local_sent = as.factor(local_sent))

clean_3 <- clean2 %>%
  select(-num_rating) %>%
  pivot_longer(!c(review_text,	coffee_shop_name), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = as.factor(parameter))

epoch.clean <- epoch_train %>%
  mutate(review_text = review_text %>%
           str_to_lower() %>% # transform menjadi huruf kecil
           replace_url()  %>% 
           replace_html() %>% 
           replace_contraction() %>%
           replace_word_elongation() %>% 
           replace_internet_slang() %>% 
           replace_emoji(.) %>% 
           replace_emoticon(.) %>% 
           str_remove_all(pattern = "[[:digit:]]") %>% # remove number
           str_remove_all(pattern = "[[:punct:]]") %>% 
           str_remove_all(pattern = "%") %>%
           str_remove_all(pattern = "\\$") %>% # remove dollar sign
           str_remove_all(pattern = "\\|") %>%
           str_remove_all(pattern = "\\=") %>%
           str_remove_all('[\\&]+') %>% 
           str_remove_all('[\\"]+') %>% 
           str_remove_all(pattern = "\\+") %>%
           str_remove_all(pattern = "\\>") %>%
           str_remove_all(pattern = "\\<") %>%
           str_squish()
  )

epoch.corpus <- VCorpus(VectorSource(epoch.clean$review_text))

epoch.corpus <- tm_map(epoch.corpus, content_transformer(textstem::lemmatize_words))

epoch.siap <- bind_cols(epoch.corpus %>% sapply(as.character) %>%
                          as.data.frame(stringsAsFactors = FALSE), epoch_train[,4:17]) %>%
  `colnames<-`(c("review", "overall_sent", "vibe_sent", "tea_sent", "service_sent","seating_sent", "price_sent", "parking_sent", "location_sent", "alcohol_sent", "coffee_sent", "food_sent", "hours_sent", "internet_sent", "local_sent"))


# model
md_alcohol <- readRDS("md_alcohol.RDS")
md_coffee <- readRDS("md_coffee.RDS")
md_food <- readRDS("md_food.RDS")
md_hours <- readRDS("md_hours.RDS")
md_int <- readRDS("md_int.RDS")
md_loc <- readRDS("md_loc.RDS")
md_local <- readRDS("md_local.RDS")
md_overall<- readRDS("md_overall.RDS")
md_parking <- readRDS("md_parking.RDS")
md_price<- readRDS("md_price.RDS")
md_seat <- readRDS("md_seat.RDS")
md_tea <- readRDS("md_tea.RDS")
md_vibe <- readRDS("md_vibe.RDS")