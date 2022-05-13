setwd('Downloads')
data1 = read.csv('popularity_dataset.csv')
data2 = read.csv('tracks_features.csv')
data3 = read.csv('song_info.csv')

library(dplyr)
library(readr)
library(stringr)
#install.packages('corrplot')
library(corrplot)
#install.packages('lattice')
library(lattice)
#install.packages('ggpubr')
library(ggpubr)

#combine data1 and data3
data3<-data3[-c(1)]
popularity_data<-cbind(data1,data3)

#NOTICE: the features has some minor differences for the same songs, resulting duplicated rows
#remove duplicated rows, identifying the songs' uniqueness with song_name and song_popularity)
popularity_data %>% distinct(song_name,song_popularity, .keep_all = TRUE)->popularity_data
                             
#prepare data2 for inner merge
#since data2 artist name has brackets, we use song_name and song_duration for merging
names(data2)[names(data2) == 'name']<-'song_name'
names(data2)[names(data2) == 'duration_ms']<-'song_duration_ms'

data2$id = NULL
data2$album = NULL
data2$album_id = NULL
data2$track_number = NULL
data2$disc_number = NULL
data2$year = NULL
data2$release_date = NULL
data2$artists = NULL
data2 = unique(data2)

#inner merge 2 datasets
#use song_name and song duration to identify the same song in 2 datasets
data_merge <- inner_join(popularity_data, data2, by = c("song_name","song_duration_ms"))
nrow(data_merge)

#for the features contained by both datasets, keep the ones in data2 since it's more recent(updated 1 yr ago)
#remove cols of the same features
data_merge_sub <- select(data_merge, -contains(".x"))
data_merge_sub$audio_mode = NULL
data_merge_sub$audio_valence = NULL

#clean col name
names(data_merge_sub) <- gsub( ".y",  "", names(data_merge_sub), fixed = TRUE)

#make sure no duplicates with same name and same popularity score
data_merge_sub %>% distinct(song_name,song_popularity, .keep_all = TRUE)->data_merge_sub
nrow(data_merge_sub)


#----------------cleaning--------------------
#change e to digits
options("scipen"=100, "digits"=4)
#change explicit to dummy
data_merge_sub$explicit_dummy <- ifelse(data_merge_sub$explicit == "True", 1, 0)
#change duration from ms to sec
data_merge_sub$song_duration_sec <- data_merge_sub$song_duration_ms/1000
#remove"[]" in character
data_merge_sub$artist_ids <- gsub("\\[|\\]", "", data_merge_sub$artist_ids)
data_merge_sub$danceability <- data_merge_sub$danceability * 100
data_merge_sub$energy <- data_merge_sub$energy * 100
data_merge_sub$speechiness <- data_merge_sub$speechiness * 100
data_merge_sub$liveness <- data_merge_sub$liveness * 100
data_merge_sub$valence <- data_merge_sub$valence * 100

#reorder columns
cols <- colnames(data_merge_sub)
cols
new_cols<-c(cols[1],cols[4:7],cols[length(cols)-1], cols[2], cols[length(cols)],cols[9:length(cols)-2],cols[3],cols[8])
new_cols
new<- new_cols[-9:-10]
new

#name it spotify
spotify <- data_merge_sub[new]
glimpse(spotify)
summary(spotify)
#drop unnecessary columns
drop<- c('artist_ids','song_duration_ms','explicit')
spotify<-spotify[,!(names(spotify) %in% drop)]

write.csv(spotify, '5205_Group8_Spotify_cleaned.csv',row.names = F)


#----------------visualization--------------------
cols1 = sapply(spotify, is.character)
data <- spotify[,!cols1]
#full_correlation_matrix

m <- cor(data,use = 'everything')
corrplot(m,method='circle')

#upper correlation plot
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(m)
upper_tri

library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


#categorical variable analysis
data1 = data

attach(data1)
par(mfrow = c(2,2))

data1$key = factor(data1$key)
k <- ggplot(data=data, aes(x=key))+
  geom_bar(fill = "orange",color='black')+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))

data1$time_signature = factor(data1$time_signature)
ts <- ggplot(data=data, aes(x=time_signature))+
  geom_bar(fill = "orange",color='black')+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))

data1$mode = factor(data1$mode)
m <- ggplot(data=data, aes(x=mode))+
  geom_bar(fill = "orange",color='black',width=0.5)+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))

ggarrange(k, ts, m, 
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)

#numerical variable analysis
attach(data1)
par(mfrow = c(3,4))
plot(density(data1$song_popularity,col='teal'),main='popularity')
rug(data1$song_popularity, col='red')

plot(density(data1$song_duration_sec),main='duration')
rug(data1$song_duration_ms, col='red')

plot(density(data1$danceability),main='danceability')
rug(data1$danceability, col='red')

plot(density(data1$energy),main='energy')
rug(data1$energy, col='red')

#plot(density(data1$key),main='key')
#rug(data1$key, col='red')

plot(density(data1$loudness),main='loudness')
rug(data1$loudness, col='red')

plot(density(data1$speechiness),main='speechiness')
rug(data1$speechiness, col='red')

#plot(density(data1$time_signature),main='time_signature')
#rug(data1$time_signature, col='red')

plot(density(data1$valence),main='valence')
rug(data1$valence, col='red')

plot(density(data1$tempo),main='tempo')
rug(data1$tempo, col='red')

#plot(density(data1$mode),main='mode')
#rug(data1$mode, col='red')

plot(density(data1$liveness),main='liveness')
rug(data1$liveness, col='red')

plot(density(data1$instrumentalness),main='instrumentalness')
rug(data1$instrumentalness, col='red')

plot(density(data1$acousticness),main='acousticness')
rug(data1$acousticness, col='red')


write.csv(spotify, 'spotify_merged_data.csv',row.names = F)