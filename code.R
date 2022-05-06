# Author:Yingchi Zhu
#Data Description:These data are from kaggle describing Taylor Swift's albums released 
#in recent years. I used danceability', energy, speechiness, acousticness to explore which factor is related to the popularity of the song.
# load library
library('tidyverse')
library(ggplot2)

# set the path of the folder


# read the data
data <- read_csv('spotify_taylorswift.csv')

# select the variables we need
cols_remain<-c('name', 'popularity', 'danceability', 'energy', 'speechiness', 'acousticness')
data <- data[ ,colnames(data) %in% cols_remain]

# inspect data
glimpse(data)


# ------------------ scatter plot ------------------- #
#
# scatterplot of danceability by popularity
data %>%
  ggplot(aes(x = danceability,
             y = popularity,
             color = popularity)) +
  geom_point(size =1) + 
  geom_smooth(method = 'lm')+
  scale_color_gradient('title', low = "#8FBC94", high = "#548687")+
  labs(title = "Danceability VS Popularity")

# scatterplot of energy by popularity
data %>%
  ggplot(aes(x = energy,
             y = popularity,
             color = popularity)) +
  geom_point(size = 1) + 
  geom_smooth(method = 'lm')+
  scale_color_gradient('title', low = "#CC9966", high = "#666666")+
  labs(title = "Energy VS Popularity")
  

# scatterplot of speechiness by popularity
data %>%
  ggplot(aes(x = speechiness,
             y = popularity,
             color = popularity)) +
  geom_point(size = 1) + 
  geom_smooth(method = 'lm',color = "black")+
  labs(title = "Speechiness VS Popularity")

# scatterplot of acousticness by popularity
data %>%
  ggplot(aes(x = acousticness,
             y = popularity,
             color = popularity)) +
  geom_point(size = 1) + 
  geom_smooth(method = 'lm',color = "black")+
  scale_color_gradient('title', low = "#99CCCC", high = "#FFCC99")+
  labs(title = "Acousticness VS Popularity")


# ------------------ bar plot ------------------- #
#
# preparation for barplot
# group the song's popularity
data$class[data$popularity >= 70] <- '[70, inf)'
data$class[data$popularity < 70] <- '[60, 70)'
data$class[data$popularity < 60] <- '[50, 60)'
data$class[data$popularity < 50] <- '[40, 50)'
data$class[data$popularity < 40] <- '[0, 40)'
data_count_pop <- data %>% count(class)

# barplot of song's popularity
data_count_pop %>%
  ggplot(aes(x = class,
             y = n,
             fill = class)) + 
  geom_col(position = "dodge") + 
  labs(x = 'popularity', y = 'count of songs')+
  scale_fill_brewer(palette = "Accent")+
  labs(title = "Popularity by level")


# ------------------ line plot ------------------- #
#
# sort the data by popularity
data <- data[order(-data$popularity),]

# get the top 10 songs
data <- head(data, 10)

data$name <- factor(data$name, levels=c("Blank Space", "Shake It Off", "Lover","Delicate",
                                        "You Need To Calm Down", "Look What You Made Me Do",
                                        "Cruel Summer", "ME! (feat. Brendon Urie of Panic! At The Disco)",
                                        "Getaway Car", "Paper Rings"), ordered=TRUE)

data %>%
  ggplot(aes(x = name,
             y = popularity,
             group = 1,
             label = popularity)) + 
  geom_line() + 
  geom_label() +
  theme(axis.text.x = element_text(angle = 35,vjust = 0.5,hjust = 0.5))

