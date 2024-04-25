### Read in dataset###

#Make sure your working directory is the project dir
Top500 <- read.csv("RollingStoneTop500.csv") 

### Most Common Genres ###

#Grabs first listed genre of each album
genres <- sub(',.*', '', Top500$Genre) 

#Create dataframe of 10 most common genres
topGenres <- as.data.frame(tail(sort(table(genres)), 10))
reorder(topGenres$genres, topGenres$Freq, decreasing = TRUE)

#Create plot
par(mar=c(5.1,8,4.1,2.1))
genrePlot <- barplot(topGenres$Freq,
                      xlim = c(0,45),
                      main = "Top Genres",
                      names.arg = topGenres$genres,
                      col = "lightblue",
                      cex.names = .8,
                      las = 1,
                      horiz = TRUE)
text(
     x = genrePlot,
     #y = topGenres$genres,
     labels = topGenres$Freq,
     pos = 4,
     offset = 00,
     cex = .9)

title(xlab="frequency", mgp=c(2,1,0),cex.lab=1) 


### Most Common Descriptors ### 

#Grabs every listed descriptor and removes leading whitespace
descriptors <- sub(' ', '', unlist( strsplit( Top500$Descriptors, "," ) ) )
topDescriptors <- as.data.frame(tail(sort(table(descriptors)), 10))

reorder(topDescriptors$descriptors, topDescriptors$Freq, decreasing = TRUE)


descPlot <- barplot(topDescriptors$Freq,
                      xlim = c(0,300),
                      main = "Top Descriptors",
                      names.arg = topDescriptors$descriptors,
                      col = "pink",
                      cex.names = 1,
                      las = 1,
                      horiz = TRUE)

text(
  x= descPlot,
  labels = topDescriptors$Freq,
  pos = 4,
  offset = .1,
  cex = .9)

title(xlab="frequency", mgp=c(2,1,0),cex.lab=1) 

### Most Common Record Labels ###

topLabels <- as.data.frame(tail(sort(table(Top500$Label)), 10))

par(mar=c(5.1,8,4.1,2.1))
labelPlot <- barplot(topLabels$Freq,
                      xlim = c(0, 50),
                      main = "Top Labels",
                      names.arg = topLabels$Var1,
                      col = "orange",
                      cex.names = 1,
                      las = 1,
                      horiz = TRUE)
text(
      x= descPlot,
      labels = topLabels$Freq,
      pos = 4,
      offset = .1,
      cex = .9)

title(xlab="frequency", mgp=c(2,1,0),cex.lab=1) 


### Average Rating & Position for Albums ###


### Year Distribution ###
years <- Top500$Year
max(years)
min(years)

par(mar=c(5.1,4.8,4.1,2.1))
hist(years,
          breaks = 30,
          xlim = c(1940, 2030),
          ylim = c(0, 45),
          col = "lavender",
          main="Release Year Distribution",
          xlab="Year",
          mgp=c(2.2,1,0),cex.lab=1) 
     

### Rating Distribution ###
par(mar=c(5.1,4.8,4.1,2.1))
hist(ratings,
     breaks = 20,
     xlim = c(2.5,5),
     col = "darkgreen",
     main="Rating Distribution",
     xlab="Ratings",
     mgp=c(2.2,1,0),cex.lab=1) 

### Rating Boxplot with Outliers ###

par(mar=c(5.1,4.1,4.1,2.1))
boxplot(ratings, horizontal = TRUE)
title(main="Boxplot of Ratings", xlab = "Rating", mgp=c(2.5,1,0),cex.lab=1) 
        