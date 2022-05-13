# Project : Predicting popularity of Spotify songs

### [Project Description](doc/project2_desc.md)

<img src="figs/spotify.png" width="300" height="300">

Spotify releases over 137M new tracks every year to meet the high demands (Ingham,
2021). Undoubtedly, some songs are beloved much more than others, going viral globally with
billions of plays– leading to questions, 'Is there any pattern we can identify from popular songs?
Can Spotify's vast user data potentially help pinpoint U.S. music listeners' preferences for
music?'
A leading audio streaming platform with millions of tracks and episodes, Spotify's
objective is to make it 'easy for its users to find the right music or podcast for every moment – on
your phone, computer, tablet and more.' Whether you are working out, driving, partying, or
studying, the streaming service will allow you to find the audio that is appropriate for you.
Meanwhile, Spotify's mission is 'to unlock the potential of human creativity—by allowing a
million creative artists to live off their art and billions of fans the opportunity to enjoy and be
inspired by it.

The study will focus on the relationship between a song's characteristics and popularity.
The final goal is to find out how different technical attributes in a song, such as its 'danceability,'
'energy,' 'tempo,' 'instrumentalness,' etc., play a role in determining its 'popularity'; furthermore,
to find out if there is any recognizable pattern that makes a song popular.

## Project Title: SPOTIFY Cluster Analysis and Popularity Prediction

+ Team members
	+ Ren, Icy
	+ Chen, Yiting
	+ Nallamaddi, Shriya
	+ Vuppula, Sushmitha
	+ Yu, Joohyun

+ **Data Collection**: The raw dataset named SpotifyFeatures is retrieved from Kaggle websites, containing
228159 observations and 18 variables that the author retrieved three years ago from Spotify API.
The dataset includes song information such as a genre, artist name, track name, track id, and
song popularity score. Additionally, it contains 13 song audio features: duration, danceability,
energy, loudness, speechiness, acounsticness, instrumentalness, key, liveness, mode, tempo, time
signature, and valence, which are useful for predicting the songs' popularity scores offered by
Spotify

+ **Project summary**: The first technique we employed was clustering analysis. Based upon our focus on the
prediction power of songs' audio features in relation to the songs' popularity score and the results
of exploratory data analysis, we dropped: time_signature(as most songs have a value of 4),
loudness(strongly correlated with energy), liveness(a dummy variable indicating whether the
song is live performance), tempo(beats per minute which is ambiguous to decide the speed of the
song), key(a pitch class notation) and mode(only used during music composition).
Following, we cherry-picked six numeric variables for cluster analysis: danceability,
energy, speechiness, acousticness, instrumentalness, and valence. These variables are numeric
and contain no missing values, so we standardized our variables to ensure uniformity in scale to
prepare for the clustering

The second analytical technique we employed was the linear regression model for
prediction. After categorizing the four clusters of songs and interpreting the profile of each
cluster, we employed the simple linear regression model to predict the popularity score for the
songs in each cluster. In this case, the dependent variable is the popularity score. The
independent variables are all the ten audio features variables, including the six variables used in
cluster analysis and variables of loudness, liveness, tempo, and duration. We first scaled the
dataset to standardize each variable to prepare for the linear regression. Then, to limit the
influence of outliers, we removed the observations beyond the range between the 1st quartile and
3rd quartile based upon the popularity variable and the six audio features variables

+ **Results**: 
+ Cluster 1 : Melodious & Lyrical Tracks - High speechiness, High acousticness and High energy
+ Cluster 2 : Party Tracks - High Danceability, High Valence, High Energy
+ Cluster 3 : Time to relax - Medium Danceability, Medium Acousticness, Medium Energy
+ Cluster 4 : Instrumental Tracks - High Instrumentalness, High Acousticness, Low Speechiness

+ **Contribution statement**: ([default](doc/a_note_on_contributions.md)) All team members contributed equally in all stages of this project. All team members approve our work presented in this GitHub repository including this contributions statement.
