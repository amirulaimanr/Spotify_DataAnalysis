#library packages that required

library(tidyverse)
library(dplyr)
library(ggplot2)
library(plotly)
library(corrplot)
library(factoextra)
library(plyr)
library(knitr)
library(RColorBrewer)
library(funModeling)
library(janitor)


#  reading the csv function (importing data)

Hot_Hits_Malaysia_2022 <- read.csv("C:/Users/ai/RStudio/Hot_Hits_Malaysia_2022/Hot_Hits_Malaysia_2022.csv")

# print dataset using the head and the glimpse function

head(Hot_Hits_Malaysia_2022)

glimpse(Hot_Hits_Malaysia_2022)

# cleaning data & removing missing value

colSums(is.na(Hot_Hits_Malaysia_2022))

Hot_Hits_Malaysia_2022 <- na.omit(Hot_Hits_Malaysia_2022)

# removing duplicate data

Hot_Hits_Malaysia_2022 <- Hot_Hits_Malaysia_2022[!duplicated(Hot_Hits_Malaysia_2022$track_id),]

#applying function summary()

print(summary(Hot_Hits_Malaysia_2022))

# apply linear regression lm() by checking using hist() & plot() function

hist(Hot_Hits_Malaysia_2022$acousticness)

plot(acoustticness ~ speechiness, data = Hot_Hits_Malaysia_2022)

#assign linear regression output as output variable

output <- lm(acousticness ~ speechiness, data = Hot_Hits_Malaysia_2022)

summary(output)


# visualize the results of linear regression

speechiness.graph<-ggplot(Hot_Hits_Malaysia_2022, aes(x=speechiness, y=acousticness))+
  geom_point()
speechiness.graph

speechiness.graph <- speechiness.graph + geom_smooth(method="lm", col="black")

speechiness.graph

# using str() function on dataframe

str(Hot_Hits_Malaysia_2022)

# select column and count using tabyl and xtabs function

tabyl(Hot_Hits_Malaysia_2022, songs.name, track.explicit)

xtabs(~ songs.name + track.explicit , data = Hot_Hits_Malaysia_2022)

# apply function subset() on dataframe

subset.dataframe <- subset(Hot_Hits_Malaysia_2022, select = album.name)

print(subset.dataframe)

# perform sorting to rearrange data frame ascending and descending

sort <- Hot_Hits_Malaysia_2022[order(Hot_Hits_Malaysia_2022$energy),]

head(sort)

sortdescending <- Hot_Hits_Malaysia_2022[order(-Hot_Hits_Malaysia_2022$danceability),]

head(sortdescending)

# visualize box and whisker

boxplot(energy~playlist_genre, data = Hot_Hits_Malaysia_2022, 
        main = "Variation:- Energy and Genre",
        xlab = "Energy",
        ylab = "Genre",
        col = "green",
        border = "blue",
        horizontal = TRUE,
        notch = FALSE)

boxplot(danceability~playlist_genre, data = Hot_Hits_Malaysia_2022,
          main = "Variation:- Danceability and Genre",
          xlab = "Danceability",
          ylab = "Genre",
          col = "green",
          border = "blue",
          horizontal = TRUE,
          notch = FALSE)

# scatterplot matrix visualization

pairs(~loudness+speechiness+acousticness+liveness,data=Hot_Hits_Malaysia_2022,
      main="Scatterplot Matrix")

# popularity tracks correlation graph

corr_spotify <- select(Hot_Hits_Malaysia_2022, danceability, energy, loudness, speechiness, acousticness, instrumentalness, liveness, valence, tempo)
corrplot(cor(corr_spotify), type="lower")

# playlist genre pie visualization

songs_clean_pie_data <- Hot_Hits_Malaysia_2022 %>% 
  group_by(playlist_genre) %>% 
  summarise(Total_number_of_tracks = length(playlist_genre))

ggplot(songs_clean_pie_data, aes(x="", y=Total_number_of_tracks, fill=playlist_genre)) + 
  geom_bar(width = 1, stat = "identity") + 
  coord_polar("y", start=0) + 
  geom_text(aes(label = paste(round(Total_number_of_tracks / sum(Total_number_of_tracks) * 100, 1), "%")),
            position = position_stack(vjust = 0.5))


