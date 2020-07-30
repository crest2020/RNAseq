library(factoextra)
library(cluster)
library(magrittr)
data("USArrests")
my_data=USArrests%>%na.omit()%>%scale()

head(my_data,n=3)
res.dist=get_dist(USArrests,stand = T,method = "pearson")
fviz_dist(res.dist,gradient = list(low="blue",mid="white",high="red"))
#K-medoids clustering or PAM (Partitioning Around Medoids
fviz_nbclust(my_data,kmeans,method = "gap_stat")
set.seed(123)
km.res=kmeans(my_data,2,nstart = 25)
fviz_cluster(km.res,data = my_data,ellipse.type = "convex",palette="jco",ggtheme = theme_minimal())

pam.res=pam(my_data,2)
fviz_cluster(pam.res)
# Compute hierarchical clustering
res.hc=USArrests%>%scale()%>%dist(method = "euclidean")%>%hclust(method = "ward.D2")
res.hc
fviz_dend(res.hc,k=4,cex = 0.5,k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),color_labels_by_k = T,rect = T)

gradient.color=list(low="steelblue",high="white")
iris[,-5]%>%scale()%>%get_clust_tendency(n=50,gradient = gradient.color)
#Hopkins statistic: If the value of Hopkins statistic is close to 1 (far above 0.5), then we can conclude that the dataset is significantly clusterable.


#Determining the optimal number of clusters
install.packages("NbClust")

options(BioC_mirror="https://mirrors.tuna.tsinghua.edu.cn/bioconductor/")
options("repos" = c(CRAN="http://mirrors.cloud.tencent.com/CRAN/"))
options(download.file.method = 'libcurl')
options(url.method='libcurl')
library("NbClust")
res.nbclust=USArrests%>%scale()%>%NbClust(distance = "euclidean",min.nc = 2,max.nc = 10,method="complete",index = "all")
fviz_nbclust(res.nbclust,ggtheme=theme_minimal())

res.hc=iris[,-5]%>%scale()%>%eclust("hclust",k=2,graph = F)
fviz_dend(res.hc,palette = "jco",rect = T,show_labels = F)

fviz_silhouette(res.hc)
sil=res.hc$silinfo$widths[,1:3]
neg_sil_index=which(sil[,"sil_width"]<0)
sil[neg_sil_index,,drop=F]
res.hc
res.hc$cluster
dim(iris)
