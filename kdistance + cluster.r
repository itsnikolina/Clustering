#from categorical to factor

table1...Copy$Fall <- as.factor(table1...Copy$Fall)
uk_retailer$Description <- as.factor(uk_retailer$Description)
uk_retailer$ProdCode <- as.factor(uk_retailer$ProdCode)
uk_retailer$InvoiceDate <- as.factor(uk_retailer$InvoiceDate)
uk_retailer$Time <- gsub("[:]", "" , uk_retailer$Time, perl=TRUE)
uk_retailer$Time<-as.factor(uk_retailer$Time)
uk_retailer$CustomerID <- as.factor(uk_retailer$CustomerID)
uk_retailer$Country <- as.factor(uk_retailer$Country)
uk_retailer$Region <- as.factor(uk_retailer$Region)
uk_retailer$Season <- as.factor(uk_retailer$Season)
uk_retailer$DayOfWeek <- as.factor(uk_retailer$DayOfWeek)
uk_retailer$MornAftEvn <- as.factor(uk_retailer$MornAftEvn)
uk_retailer$Month <- as.factor(uk_retailer$Month)
uk_retailer$Quarter <- as.factor(uk_retailer$Quarter)
View(uk_retailer)
#Assessing the Optimal Number of Clusters with the Elbow Method
#IMPORTANT!!! RUNNING LINES 22-23 REQUIRES TIME!!!! YOU CAN SKIP THIS PART
#(LINES 21-29), THE RESULTING GRAPH IS THE ELBOW METHOD
library(clustMixType)
uk_retailer<- as.data.frame(uk_retailer)
wss <- sapply(1:20, 
              function(k){kproto(uk_retailer, k)$tot.withinss})
wss
plot(1:20, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")





# from the picture it is not really clear the optimal number of k/clusters
#and we cannot compute the silhouette function because it requires a 
#dissimilarity matrix (kproto does not calculate dissimilarity)
#press esc to turn off the return function!!!!
#library(cluster)
#clusGap(uk_retailer, kproto, K.max=2, B=500)
#Error: cannot allocate vector of size 585.3 Gb...
#library(NbClust)
#z<-daisy(uk_retailer, metric = c("gower"), stand = FALSE, type = list())
#res<-NbClust(uk_retailer, diss=z, distance = NULL, min.nc=2, max.nc=15, 
#method = "centroid", index = "ratkowsky") 
#Error: cannot allocate vector of size 585.3 Gb


#Plotting our dataset takes forever....so I stopped the script
#plot(uk_retailer)


####



#kproto

library(clustMixType)
table1...Copy<- as.data.frame(table1...Copy)
#Investigation of variances to specify lambda for k prototypes clustering.
#Lambda:Parameter > 0 to trade off between Euclidean distance of numeric 
#variables and simple matching coefficient between categorical variables. 
#Also a vector of variable specific factors is possible where the order 
#must correspond to the order of the variables in the data. In this case 
#all variables' distances will be multiplied by their corresponding lambda 
#value.

a<-lambdaest(table1...Copy, num.method = 1, fac.method = 1, outtype = "numeric")
#Compute k prototypes clustering for mixed type data.
kdata<-kproto(table1...Copy, 3, lambda = a, iter.max = 100, nstart = 1,
              keep.data = TRUE)
kdata
#Predict k prototypes cluster memberships and distances for new data
predicted.clusters <- predict(kdata, table1...Copy)
predicted.clusters
#Visualization of k prototypes clustering result for cluster interpretation.
clprofiles(kdata, table1...Copy)
return(clprofiles(kdata,table1...Copy))
#IMPORTANT!!!!RUN line 72 for 16 times, each time a new graph will appear 
#for each variable
#Details:For numerical variables boxplots and for factor variables 
#barplots of each cluster are generated.I think that the y axis stands either
#for lambda (for categorical avriables) or for the value of the numerical
#variable (e.g. price for unit price.)


#press esc to turn off the return function!!!!

summary(kdata)

#some graphs
library(ggp)
library(ggplot2)
library(gridExtra)
#combine clustering result into the original data
cluster <- as.factor(kdata$cluster)
data_cluster <- cbind(uk_retailer,cluster)


#customer profile
#run two lines at a time!!!
ggplot(data_cluster,aes(x=cluster,fill=uk_retailer$Country))+geom_bar(position = 'fill')
return(ggplot(data_cluster,aes(x=cluster,fill=uk_retailer$Country))+geom_bar(position = 'fill'))
ggplot(data_cluster,aes(x=cluster,fill=uk_retailer$Region))+geom_bar(position = 'fill')
return(ggplot(data_cluster,aes(x=cluster,fill=uk_retailer$Region))+geom_bar(position = 'fill'))
ggplot(data_cluster,aes(x=cluster,fill=uk_retailer$Month))+geom_bar(position = 'fill')
return(ggplot(data_cluster,aes(x=cluster,fill=uk_retailer$Month))+geom_bar(position = 'fill'))
ggplot(data_cluster,aes(x=cluster,fill=uk_retailer$MornAftEvn))+geom_bar(position = 'fill')
return(ggplot(data_cluster,aes(x=cluster,fill=uk_retailer$MornAftEvn))+geom_bar(position = 'fill'))
ggplot(data_cluster,aes(x=cluster,fill=uk_retailer$DayOfWeek))+geom_bar(position = 'fill')
return(ggplot(data_cluster,aes(x=cluster,fill=uk_retailer$DayOfWeek))+geom_bar(position = 'fill'))
ggplot(data_cluster,aes(x=cluster,fill=uk_retailer$Season))+geom_bar(position = 'fill')
return(ggplot(data_cluster,aes(x=cluster,fill=uk_retailer$Season))+geom_bar(position = 'fill'))
ggplot(data_cluster,aes(x=cluster,fill=uk_retailer$Quarter))+geom_bar(position = 'fill')
return(ggplot(data_cluster,aes(x=cluster,fill=uk_retailer$Quarter))+geom_bar(position = 'fill'))




#kmedoids, DBSCAN and OPTICS?







#############################################################################
###############FROM HERE ONWARDS NO NEDD TO RUN!!!!!###########################
###############Below there are some other things###########################
############# I tried to do but they do not work##############################
##############################################################################


library(cluster)
library(fpc)
plotcluster(uk_retailer, kdata$cluster)
library(psych)
cluster.plot(kdata, cluster = NULL, cut = 0, labels=NULL,
             title = "Cluster plot",pch=18,pos,show.points=TRUE,choose=NULL)
library(ggplot2)
ggplot(data_cluster,aes(x = uk_retailer_Revenue, y=uk_retailer$Quantity ), data = data_cluster) + geom_point(aes(color = cluster))


library(ggplot2)
library(gridExtra)
#combine clustering result into the original data
cluster <- as.factor(kdata$cluster)
customer_data_cluster <- cbind(uk_retailer,cluster)


#customer profile
ggplot(customer_data_cluster,aes(x=cluster,fill=uk_retailer$Country))+geom_bar(position = 'fill')
return(ggplot(customer_data_cluster,aes(x=cluster,fill=uk_retailer$Country))+geom_bar(position = 'fill'))


pairs(uk_retailer)



#we cannot use hierarchical cluster with kproto because hclust requires a dissimilarity matrix compute through daisy function
#clusters_d <- hclust(kdata, method="ward.D2")
#cluster <- cutree(clusters_d, k = 2) # k= number of clusters 
#cluster
#Mydata_cluster <- cbind(uk_retailer_grid, cluster, uk_retailer_grid$freq)
#Mydata_cluster_full <- Mydata_cluster[rep(row.names(Mydata_cluster), Mydata_cluster$freq), 1:(dim(Mydata_cluster)[2]-1)]
#Mydata_cluster_full



#####FAMD - Factor Analysis of Mixed Data in R
library(FactoMineR)
res.famd <- FAMD(uk_retailer, graph = FALSE)
library(factoextra)
eig.val <- get_eigenvalue(res.famd)
head(eig.val)
fviz_screeplot(res.famd)
var <- get_famd_var(res.famd)
var
# Coordinates of variables
head(var$coord)
# Cos2: quality of representation on the factore map
head(var$cos2)
# Contributions to the  dimensions
head(var$contrib)


# Plot of variables
fviz_famd_var(res.famd, repel = TRUE)
# Contribution to the first dimension
fviz_contrib(res.famd, "var", axes = 1)
# Contribution to the second dimension
fviz_contrib(res.famd, "var", axes = 2)
#quantitative variables
quanti.var <- get_famd_var(res.famd, "quanti.var")
quanti.var 
fviz_famd_var(res.famd, "quanti.var", repel = TRUE,
              col.var = "black")
fviz_famd_var(res.famd, "quanti.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)
# Color by cos2 values: quality on the factor map
fviz_famd_var(res.famd, "quanti.var", col.var = "cos2",
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
              repel = TRUE)
#qualitative variables
quali.var <- get_famd_var(res.famd, "quali.var")
quali.var 
fviz_famd_var(res.famd, "quali.var", col.var = "contrib", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
ind <- get_famd_ind(res.famd)
ind
fviz_famd_ind(res.famd, col.ind = "cos2", 
              gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
              repel = TRUE)
fviz_mfa_ind(res.famd, 
             habillage = "Label", # color by groups 
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, ellipse.type = "confidence", 
             repel = TRUE # Avoid text overlapping
) 
fviz_ellipses(res.famd, c("Label", "Soil"), repel = TRUE)
fviz_ellipses(res.famd, 1:2, geom = "point")

###PCAmix
library(PCAmixdata)
class.var<-c(1:16)
names <- c("InvoiceNo",	"ProdCode",	"Description",	"Quantity",	"InvoiceDate",	"Time",	"UnitPrice",	"CustomerID",	"Country",	"Revenue",	"Region",	"Season",	"DayOfWeek",	"MornAftEvn",	"Month",	"Quarter")
res<-MFAmix(uk_retailer,groups=class.var, name.groups=names, rename.level=TRUE, graph=FALSE)



####kmedoids
install.packages("fpc")
library(fpc)
medoids<-uk_retailer
medoids$Description<-NULL
medoids$Region<-NULL
medoids$Season<-NULL
medoids$Country<-NULL
medoids$DayOfWeek<-NULL
medoids$MornAftEvn<-NULL
medoids$Month<-NULL
medoids$ProdCode<-NULL
medoids$InvoiceDate<-NULL
pamk.result<-pamk(medoids)
pamk.result$nc
MATRIX<-as.data.frame(lapply(uk_retailer, as.numeric))
medoids<-str(uk_retailer)
options(max.print = 10000000)
clusters<-hclust(dist(uk_retailer[, 11:12]))



library(dbscan)
