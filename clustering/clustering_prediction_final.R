#Read the raw data
getwd()
setwd('/Users/icy_rkx23/Downloads')
data = read.csv('SpotifyFeatures.csv')

#Initiate few libraries for further analysis
library(dplyr)
library(readr)
library(stringr)
library(corrplot)
library(lattice)
library(ggpubr)
install.packages('fastDummies')
library(fastDummies)
#Summarize the raw data
summary(data)

#NOTICE: the features has some minor differences for the same songs, resulting duplicated rows
#remove duplicated rows, identifying the songs' uniqueness with song_name and song_popularity)
data %>% distinct(genre,artist_name,track_name,popularity, .keep_all = TRUE)-> data

#----------------cleaning--------------------
#cleaning column names 
names(data) <- gsub( ".y",  "", names(data), fixed = TRUE)
#change e to digits
options("scipen"=100, "digits"=4)
#change duration from ms to sec
data$duration_ms <- data$duration_ms/1000
#remove"[]" in character
data$track_name <- gsub("\\[|\\]", "", data$track_name)
data$artist_name <- gsub("\\[|\\]", "", data$artist_name)
data$track_id <- gsub("\\[|\\]", "", data$track_id)


#Assign final dataset to Spotify
spotify <- data
cols1 = sapply(spotify, is.character)
data1 <- spotify[,!cols1]
str(data1)

###-----------------------------------------------------------#####
#visualization
#Generate Correlation plots 
corr_plot <- spotify[,c("duration_ms","popularity",'danceability','energy',"loudness",'speechiness','acousticness','instrumentalness',"liveness",'valence','tempo')]
M<-cor(corr_plot)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(M, method="color", col=col(200),  
         type="upper", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE 
)


#Observations 
#1. High positive correlation between energy & loudness 
#2. High negative correlation between energy & acousticness
#3. High negative correlation between loudness & acousticness
#3. Decent correlation between danceability and valence 
#4. Decent correlation between popularity and acousticness


#numerical variable analysis
cols1 = sapply(spotify, is.character)
data1 <- spotify[,!cols1]

attach(data1)
par(mfrow = c(3,4))
plot(density(data1$popularity,col='teal'),main='popularity')
rug(data1$popularity, col='red')

plot(density(data1$duration_ms),main='duration')
rug(data1$duration_ms, col='red')

plot(density(data1$danceability),main='danceability')
rug(data1$danceability, col='red')

plot(density(data1$energy),main='energy')
rug(data1$energy, col='red')

plot(density(data1$loudness),main='loudness')
rug(data1$loudness, col='red')

plot(density(data1$speechiness),main='speechiness')
rug(data1$speechiness, col='red')

plot(density(data1$valence),main='valence')
rug(data1$valence, col='red')

plot(density(data1$tempo),main='tempo')
rug(data1$tempo, col='red')

plot(density(data1$liveness),main='liveness')
rug(data1$liveness, col='red')

plot(density(data1$instrumentalness),main='instrumentalness')
rug(data1$instrumentalness, col='red')

plot(density(data1$acousticness),main='acousticness')
rug(data1$acousticness, col='red')


###-----------------------------------------------------------#####

# Clustering Analysis 
#Selecting the features required for cluster analysis
data_cluster = data1[,c('danceability','energy','speechiness','acousticness','instrumentalness','valence')]

#Observations
#1. Dropped time signature as most songs have a value 4
#2. Dropped loudness as its affect is captured by Energy (see correlation code below)
#3. Dropped key from analysis as it is pitch class notation (too technical)
#4. Dropped mode from analysis as it indicates the type of scale (too technical) 
#5. Dropped Liveness from analysis as it just indicates whether the song was performed live
#6. Dropped Tempo as it is beats per minute and might not add value to analysis

#Check for missing values in the dataset - No missing values 
nrow(na.omit(data_cluster))

#Scale the dataset to standardize the variables 
data_cluster = as.data.frame(scale(data_cluster))

#Perform K-Means Clustering
set.seed(617)
km = kmeans(x=data_cluster,centers=4,iter.max = 10000,nstart=25)

k_segments = km$cluster
table(k_segments)

#Elbow plot to determine the ideal number of clusters
#WSS Plot
within_ss = sapply(1:10,FUN = function(x){
  set.seed(617)
  kmeans(x=data_cluster,centers = x,iter.max = 1000,nstart=25)$tot.withinss})

ggplot(data=data.frame(cluster=1:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

#Ratio Plot
ratio_ss = sapply(1:10,FUN = function(x) {
  set.seed(617)
  km = kmeans(x=data_cluster, centers=x, iter.max=1000, nstart=25)
  km$betweenss/km$totss} )

ggplot(data=data.frame(cluster = 1:10,ratio_ss), aes(x=cluster,y=ratio_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

#Either 2,3,4 cluster solution seem good - Best solution with 4 clusters as there are 6 different parameters. 

#Visualize the clusters using psych package
library(psych)
temp = data.frame(cluster=factor(k_segments),
                  factor1= fa(data_cluster, nfactors=2, rotate='varimax')$scores[,1],
                  factor2= fa(data_cluster, nfactors=2, rotate='varimax')$scores[,2])

ggplot(temp,aes(x=factor1,y=factor2,col=cluster))+
  geom_point()

#Profiling the clusters
#Add k_segments data to the original data
data2 = cbind(data1,k_segments)
str(data2)
summary

#Visualize the dominant parameters across clusters
library(tidyr)
cluster_analysis = data2 %>%
  select(c('acousticness', 'danceability', 'instrumentalness', 
           'energy', 'speechiness','valence'),k_segments)%>%
  group_by(k_segments)%>%
  summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
  gather(key=var,value=value,c('acousticness', 'danceability', 'instrumentalness', 
                               'energy', 'speechiness','valence'))%>%
  
ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
  geom_col(position = 'dodge')+scale_fill_brewer(palette="Pastel2")+
  coord_flip()
cluster_analysis




###-----------------------------------------------------------#####


#Predict the popularity of songs basis the clusters 

#str(df)
data_reg = data1[,c('danceability','energy','speechiness','acousticness','instrumentalness','valence','popularity')]
#Check for missing values in the dataset - No missing values
data_reg <- data1 %>% drop_na(popularity)
colnames(data_reg)

#Scale the dataset to standardize the variables 
norm_data <- data_reg %>% 
  mutate(danceability_norm= danceability/max(danceability,na.rm=TRUE))%>%
  mutate(energy_norm= energy/max(energy,na.rm=TRUE))%>%
  mutate(speechiness_norm= speechiness/max(speechiness,na.rm=TRUE))%>%
  mutate(acousticness_norm= acousticness/max(acousticness,na.rm=TRUE))%>%
  mutate(instrumental_norm= instrumentalness/max(instrumentalness,na.rm=TRUE))%>%
  mutate(valence_norm= valence/max(valence,na.rm=TRUE))%>%
  mutate(loudness_norm= loudness/max(loudness,na.rm=TRUE))%>%
  mutate(liveness_norm= liveness/max(liveness,na.rm=TRUE))%>%
  mutate(tempo_norm= tempo/max(tempo,na.rm=TRUE))%>%
  mutate(duration_ms_norm= duration_ms/max(duration_ms,na.rm=TRUE))
colnames(norm_data)
library(caret)
set.seed(1706)

#selecting normalised variables
select_data <- norm_data %>% select(danceability_norm,energy_norm,speechiness_norm,acousticness_norm,instrumental_norm,valence_norm,loudness_norm,liveness_norm,tempo_norm,duration_ms_norm,popularity)
colnames(select_data)
#removing outliers
outliers <- function(x) {
  
  Q1 <- quantile(x, probs=.25)
  Q3 <- quantile(x, probs=.75)
  iqr = Q3-Q1
  
  upper_limit = Q3 + (iqr*1.5)
  lower_limit = Q1 - (iqr*1.5)
  
  x > upper_limit | x < lower_limit
}

remove_outliers <- function(df, cols = names(df)) {
  for (col in cols) {
    df <- df[!outliers(df[[col]]),]
  }
  df
}

df1 <- remove_outliers(select_data, c('danceability_norm','energy_norm','speechiness_norm','acousticness_norm','instrumental_norm','valence_norm','loudness_norm','liveness_norm','tempo_norm','duration_ms_norm','popularity'))
#df1<- select_data
colnames(df1)
#split data
split = createDataPartition(df1$popularity,p=0.7,list=F,groups = 100)
train = df1[split,]
test = df1[-split,]

colnames(train)
#linear regression to predict popularity
linear = lm(popularity~.,train)
summary(linear)

#calculating sum of squared errors
sselinear = sum(linear$residuals^2)
sselinear

pred = predict(linear, newdata = test)
sse2 = sum((pred-test$popularity)^2)
sse2

#calculating rmse
install.packages('Metrics')
library(Metrics)
rmse_before = rmse(pred,test$popularity)
rmse_before
##predicting popularity for each cluster after clustering

#clustering and checking number of clusters
km1 = kmeans(x=train,centers=2,iter.max=10000,nstart=100)
k1_segments = km1$cluster
table(k1_segments)

#Elbow plot to determine the ideal number of clusters
within_ss = sapply(1:10,FUN = function(x){
  set.seed(617)
  kmeans(x=train_transf,centers = x,iter.max = 10000,nstart=25)$tot.withinss})

ggplot(data=data.frame(cluster=1:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))
###(clusters = 4)


kmf = kmeans(x=train,centers=4,iter.max=10000,nstart=100)
kf_segments = kmf$cluster


install.packages('flexclust')
library(flexclust)
km_kcca = as.kcca(kmf,train) # flexclust uses objects of the classes kcca
clusterTrain = predict(km_kcca)
clusterTest = predict(km_kcca,newdata=test)
table(clusterTrain)
table(clusterTest)

#dividing the train and test sets with respect to the clusters
train1 = subset(train,clusterTrain==1)
train2 = subset(train,clusterTrain==2)
train3 = subset(train,clusterTrain==3)
train4 = subset(train,clusterTrain==4)

test1 = subset(test,clusterTest==1)
test2 = subset(test,clusterTest==2)
test3 = subset(test,clusterTest==3)
test4 = subset(test,clusterTest==4)

#linear regression on each cluster
lm1 = lm(popularity~.,train1)
lm2 = lm(popularity~.,train2)
lm3 = lm(popularity~.,train3)
lm4 = lm(popularity~.,train4)

#prediction on each cluster
pred1 = predict(lm1,newdata=test1)
write.csv(pred1, "cluster_1.csv", row.names=T)
cluster_1 = read.csv('cluster_1.csv')
mean(cluster_1$x)

pred2 = predict(lm2,newdata=test2)
write.csv(pred2, "cluster_2.csv", row.names=T)
cluster_2 = read.csv('cluster_2.csv')
mean(cluster_2$x)

pred3 = predict(lm3,newdata=test3)
write.csv(pred3, "cluster_3.csv", row.names=T)
cluster_3 = read.csv('cluster_3.csv')
mean(cluster_3$x)

pred4 = predict(lm4,newdata=test4)
write.csv(pred2, "cluster_4.csv", row.names=T)
cluster_4 = read.csv('cluster_4.csv')
mean(cluster_4$x)




#error for each cluster
sse1 = sum((test1$popularity-pred1)^2)
sse1

sse2 = sum((test2$popularity-pred2)^2)
sse2

sse3 = sum((test3$popularity-pred3)^2)
sse3

sse4 = sum((test4$popularity-pred4)^2)
sse4

#overall prediction for all the clusters
overall_pred = c(pred1,pred2,pred3,pred4)
overall_pop = c(test1$popularity,test2$popularity,test3$popularity,test4$popularity)
sse_overall = sum((overall_pred-overall_pop)^2)
sse_overall

rmse_after = rmse(overall_pred,overall_pop)
rmse_after


##result analysis

##sse is pretty high and also the adjusted r square, multiple r square are very low. This indicated model is 
## not a good fit

##rmse claculated before clustering was 12.9 and when overall rmse is calculated after clustering reduced to 4.25

##Average popularity for cluster1: 54.2, cluster 2: 26.52, cluster 3: 67.01, cluster 4: 26.52

##we observe that the cluster 3 has high popularity among all the other clusters. Cluster 3 is high in acousticeness which means majority of the people mostly listen to slow music.

## also, ot is observed that cluster 2 and 4 have similar popularity. But the loophole here is that, cluster 2 is high 
## in dabcebaility and energy, cluster 4 is high in acousticeness and instrumentalness.

## This is leading to a contradiction as both of these clusters have low popularity. This might be due to the 
## irregularity in the data. We aim to collect data on a larger scope which includes all genre songs to derive
## accurate insights.

