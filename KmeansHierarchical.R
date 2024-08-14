sddata = read.csv("seeds_dataset.csv")
sdreal = read.csv("seeds_real.csv")

#K-MEANS CLUSTERING

#k=1
fitK1 <- kmeans(sddata, 1)
#Find the mean value of each variable for each cluster
aggregate(sddata, by=list(fitK1$cluster), FUN=mean)
#Assign each cluster to it's separate group
Kgroups1 = fitK1$cluster
#Scatter plot of the Kgroups
plot(sddata, col=Kgroups1, main = paste("K-means clustering k = 1"))
source("WK_R.r")
wk1 = WK_R(Kgroups1, sdreal$Real)

#k=2
fitK2 <- kmeans(sddata, 2)
aggregate(sddata, by=list(fitK2$cluster), FUN=mean)
Kgroups2 = fitK2$cluster
plot(sddata, col=Kgroups2, main = paste("K-means clustering k = 2"))
source("WK_R.r")
wk2 = WK_R(Kgroups2, sdreal$Real)

#k=3
fitK3 <- kmeans(sddata, 3)
aggregate(sddata, by=list(fitK3$cluster), FUN=mean)
Kgroups3 = fitK3$cluster
plot(sddata, col=Kgroups3, main = paste("K-means clustering k = 3"))
source("WK_R.r")
wk3 = WK_R(Kgroups3, sdreal$Real)

#HIERARCHICAL CLUSTERING

d <- dist(sddata, method = "euclidean")

#Single measure
fitH1 <- hclust(d, method="single")
plot(fitH1, main = paste("Hierarchical clustering SINGLE measure")) # display dendogram
Hgroups1 <- cutree(fitH1, k=3) # cut tree into 3 clusters
rect.hclust(fitH1, k=3, border="red")
plot(sddata, col=Hgroups1, main = paste("Hierarchical clustering SINGLE measure"))
wkH1 = WK_R(Hgroups1, sdreal$Real)

#Complete measure
fitH2 <- hclust(d, method="complete")
plot(fitH2, main = paste("Hierarchical clustering COMPLETE measure")) # display dendogram
Hgroups2 <- cutree(fitH2, k=3) # cut tree into 3 clusters
rect.hclust(fitH2, k=3, border="red")
plot(sddata, col=Hgroups2, main = paste("Hierarchical clustering COMPLETE measure"))
wkH2 = WK_R(Hgroups2, sdreal$Real)

#Average measure
fitH3 <- hclust(d, method="average")
plot(fitH3, main = paste("Hierarchical clustering AVERAGE measure")) # display dendogram
Hgroups3 <- cutree(fitH3, k=3) # cut tree into 3 clusters
rect.hclust(fitH3, k=3, border="red")
plot(sddata, col=Hgroups3, main = paste("Hierarchical clustering AVERAGE measure"))
wkH3 = WK_R(Hgroups3, sdreal$Real)

x = c(wk3, wkH1, wkH2, wkH3)
plot(x)
