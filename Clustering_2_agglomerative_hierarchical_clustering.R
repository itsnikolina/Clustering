library('cluster')

############
  #K-MEANS ALGORITHM - FOOD RETAILER DATASET
############

  #Read data
  ds<-read.table("fr_data_bin.txt",dec=".",sep=",",header=TRUE,colClasses=c("numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
  attach(ds)
  #Select the number of clusters based on:
  
  #a) The sum of squared error (SSE)
kmax <- 10
res <- rep(0,kmax)
for(i in 1:kmax){
  res[i] <- kmeans(ds,i,nstart=30)$tot.withinss
}
plot(1:kmax,res,type="b",pch=19,frame=FALSE,xlab="Number of clusters K",ylab="Total within-clusters sum of squares")
abline(v=3,lty=2)
abline(v=5,lty=2)


#b) The percentage of variance explained
kmax <- 10
res <- rep(0,kmax)
for(i in 1:kmax){
  kmmtemp <- kmeans(ds,i,nstart=30)
  res[i] <- kmmtemp$betweenss/kmmtemp$totss
}
plot(1:kmax,res,type="b",pch=19,frame=FALSE,xlab="Number of clusters K",ylab="Total variance explained")


#c) The Silhouette coefficient
kmax <- 10
res <- rep(0,kmax)
for(i in 2:kmax){
  kmmtemp <- kmeans(ds,i,nstart=30)
  resv <- silhouette(kmmtemp$cluster,dist(ds))
  res[i] <- mean(resv[,3])
}
plot(1:kmax,res,type="b",pch=19,frame=FALSE,xlab="Number of clusters K",ylab="Silhouette coeficient")	   
abline(v=2,lty=3)


#Build the model. To account for different initial seed selection we ask to try 30 different random starting assignments (nstart=30) and select the one with the lowest SSE.
kmm <- kmeans(ds,3,iter.max=100,nstart=30)

kmm$cluster
kmm$centers

#Combine dataset and clusters. Write the new dataset to file
dsnew <- cbind(ds,kmm$cluster)
write.table(dsnew,file="YOUR PATH HERE/ws_results_Xclusters_kmeans.txt",row.names=FALSE)

############
  #AGGLOMERATIVE HIERARCHICAL CLUSTERING - FOOD RETAILER DATASET
#############
  #Read data
  ds<-read.table("YOUR PATH HERE/fr_data_bin.txt",dec=".",sep=",",header=TRUE,colClasses=c("numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
attach(ds)
#Build the model
distds <- dist(ds,method="euclidean")
hc <- hclust(distds,method="ward.D")
#Visualization of hclust
plot(hc,labels=FALSE,hang=-1)
#Add rectangle around 5 groups
rect.hclust(hc,k=5,border=2:4)
#Cut into 5 groups
hc.cut <- cutree(hc,k=5)
dsnew <- cbind(ds,hc.cut)
write.table(dsnew,file="YOUR PATH HERE/Xclusters_hierarc.txt",row.names=FALSE)


************
  DBSCAN ALGORITHM - FOOD RETAILER DATASET
************
  library("fpc")
#Read data
ds<-read.table("YOUR PATH HERE/fr_data_bin.txt",dec=".",sep=",",header=TRUE,colClasses=c("numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
attach(ds)
#Build the model
kNNdistplot(ds,k=5)
dbs <- dbscan(ds,eps=10000,MinPts=5)
dbs$cluster