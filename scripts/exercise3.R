# Case Study Exercise 3: Volleyball Player Clustering
# Author: Indudhara Swamy Vivekananda (4106839)

seed <- 366
set.seed(seed)
suppressPackageStartupMessages({library(dplyr);library(tidyr);library(ggplot2);library(readr);library(cluster);library(dbscan)})
dir.create("outputs/exercise3",recursive=TRUE,showWarnings=FALSE)

players <- read.csv("data/playerStats.csv",check.names=FALSE,fileEncoding="UTF-8") %>% mutate(Height=parse_number(Height))
# Rate variables limit the influence of unequal playing time; age and height retain morphology.
cluster_vars <- c("Age","Height","Sets Per Match","Receives Per Match","Serves Per Match",
                  "Blocks Per Match","Digs Per Match","Attacks Per Match")
X <- scale(players[,cluster_vars])

# Choose k using average silhouette width over a prespecified, interpretable range.
ks <- 2:8
sil <- sapply(ks,function(k){set.seed(seed); fit<-kmeans(X,centers=k,nstart=100,iter.max=100); mean(silhouette(fit$cluster,dist(X))[,3])})
silhouette_results <- tibble(k=ks,average_silhouette=sil)
write.csv(silhouette_results,"outputs/exercise3/silhouette_by_k.csv",row.names=FALSE)
best_k <- ks[which.max(sil)]
set.seed(seed); km <- kmeans(X,centers=best_k,nstart=200,iter.max=100)

# PCA is used only for a faithful two-dimensional display of the standardised clustering space.
pca <- prcomp(X,center=FALSE,scale.=FALSE)
scores <- as.data.frame(pca$x[,1:2]) %>% mutate(Cluster=factor(km$cluster),Position=players$Position,Team=players$Team)
variance <- 100*pca$sdev^2/sum(pca$sdev^2)
p_km <- ggplot(scores,aes(PC1,PC2,colour=Cluster,shape=Position)) + geom_point(alpha=.75,size=2) +
  stat_ellipse(aes(PC1,PC2,colour=Cluster,group=Cluster),inherit.aes=FALSE,linewidth=.55,level=.80) +
  labs(title=sprintf("K-means solution (k = %d)",best_k),x=sprintf("PC1 (%.1f%%)",variance[1]),y=sprintf("PC2 (%.1f%%)",variance[2])) +
  theme_minimal(base_size=10)
ggsave("outputs/exercise3/kmeans_pca.pdf",p_km,width=7.1,height=5.2)

# DBSCAN: minPts=2p; choose eps at the largest curvature in the sorted kNN-distance curve.
minPts <- 2*ncol(X)
knn <- sort(kNNdist(X,k=minPts-1))
curvature <- diff(diff(knn))
search <- seq(floor(.50*length(curvature)),floor(.95*length(curvature)))
elbow_index <- search[which.max(curvature[search])] + 1
eps <- unname(knn[elbow_index])
db <- dbscan(X,eps=eps,minPts=minPts)
scores$DBSCAN <- factor(db$cluster)
p_db <- ggplot(scores,aes(PC1,PC2,colour=DBSCAN,shape=Position)) + geom_point(alpha=.78,size=2) +
  labs(title=sprintf("DBSCAN (eps = %.2f, minPts = %d; 0 = noise)",eps,minPts),
       x=sprintf("PC1 (%.1f%%)",variance[1]),y=sprintf("PC2 (%.1f%%)",variance[2])) + theme_minimal(base_size=10)
ggsave("outputs/exercise3/dbscan_pca.pdf",p_db,width=7.1,height=5.2)

# Cluster profiles and alignment with excluded labels.
assigned <- players %>% mutate(KMeans=factor(km$cluster),DBSCAN=factor(db$cluster))
profiles <- assigned %>% group_by(KMeans) %>% summarise(across(all_of(cluster_vars),mean),n=n(),.groups="drop")
write.csv(profiles,"outputs/exercise3/kmeans_profiles.csv",row.names=FALSE)
pos_tab <- prop.table(table(assigned$KMeans,assigned$Position),1)
team_tab <- prop.table(table(assigned$KMeans,assigned$Team),1)
write.csv(as.data.frame.matrix(pos_tab),"outputs/exercise3/kmeans_position_proportions.csv")
write.csv(as.data.frame.matrix(team_tab),"outputs/exercise3/kmeans_country_proportions.csv")
write.csv(assigned %>% select(`Player Name`,Team,Position,KMeans,DBSCAN),"outputs/exercise3/player_clusters.csv",row.names=FALSE)

p_align <- ggplot(assigned,aes(Position,fill=KMeans)) + geom_bar(position="fill") + coord_flip() +
  scale_y_continuous(labels=scales::percent) + labs(title="K-means clusters by playing position",x=NULL,y="Within-position proportion",fill="Cluster") +
  theme_minimal(base_size=10)
ggsave("outputs/exercise3/clusters_by_position.pdf",p_align,width=6.4,height=4.4)

# Adjusted Rand index compares the two partitions, treating DBSCAN noise as its own label.
adjusted_rand <- function(a,b){tab<-table(a,b); n<-sum(tab); choose2<-function(z) z*(z-1)/2;
 s1<-sum(choose2(tab)); sa<-sum(choose2(rowSums(tab))); sb<-sum(choose2(colSums(tab))); total<-choose2(n);
 (s1-sa*sb/total)/(0.5*(sa+sb)-sa*sb/total)}
ari <- adjusted_rand(km$cluster,db$cluster)
cramers_v <- function(tab) {
  chi <- suppressWarnings(chisq.test(tab, correct=FALSE)$statistic)
  sqrt(as.numeric(chi)/(sum(tab)*min(nrow(tab)-1,ncol(tab)-1)))
}
position_v <- cramers_v(table(assigned$KMeans,assigned$Position))
country_v <- cramers_v(table(assigned$KMeans,assigned$Team))
writeLines(c(sprintf("best_k=%d",best_k),sprintf("best_silhouette=%.4f",max(sil)),
 sprintf("pca_first_two_variance=%.4f",sum(variance[1:2])/100),sprintf("dbscan_eps=%.5f",eps),
 sprintf("dbscan_minPts=%d",minPts),sprintf("dbscan_clusters=%d",max(db$cluster)),
 sprintf("dbscan_noise=%d",sum(db$cluster==0)),sprintf("adjusted_rand=%.4f",ari),
 sprintf("cramers_v_position=%.4f",position_v),sprintf("cramers_v_country=%.4f",country_v)),"outputs/exercise3/key_results.txt")


