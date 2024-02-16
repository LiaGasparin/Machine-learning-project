library('factoextra')
library('cluster')
library('FactoMineR')
library('dendextend')
library("fBasics")
library("heatmaply")

# Lecture du fichier dans un data frame
chemin_fichier <-"DATA/voitures.unknown"
setwd("C:/Users/Lia/OneDrive - Université Paris 1 Panthéon-Sorbonne/Bureau/MASTER 1/SONDAGES ET AD/PROJET_CRISTEA_GASPARIN_CHRISTIEN_HURTADO")
cars <- read.table(chemin_fichier, header = TRUE, row.names = 1)

# Visualisation  et Statistiques
head(cars)
dim(cars)
str(cars)
summary(cars)

# Analyse valeurs manquantes
sum(is.na(cars))

# Analyse d'une seule variable à la fois
boxplot(cars)
boxplot(cars$CYL)
boxplot(cars$LON)
boxplot(cars$VITESSE)
boxplot(cars$POIDS)
boxplot(cars$CO2)
hist(cars$POIDS)
hist(cars$CYL)

#Analyse bivariée et multivariée 
pairs(cars)
heatmaply_cor(cor(cars),xlab = "Variables", ylab = "Variables",k_col = 2
              ,k_row = 2,dendrogram=FALSE, cellnote = cor(cars))

#Exo2

#PCA
cars.pca <- PCA(cars, graph=F)

#Les valeurs propres et % d'inertie expliquée 
cars.pca$eig 
fviz_eig(cars.pca, addlabels = TRUE, linecolor="darkred",main = "Scree Plot - Analyse en Composantes Principales")

#Le cercle des corrélations
plot(cars.pca, choix = "var", cex = 0.8, col.var = "red4", title = "Variables - PCA")


#Représentation des individus, ici voitures, sur le premier plan factoriel
plot(cars.pca, choix="ind", cex=0.8,col.ind="midnightblue", title="Individus-PCA")
fviz_pca_ind(cars.pca,
             col.ind = "cos2", 
             gradient.cols = c("#FFD700", "#008000", "#0000FF"),
             repel = TRUE)

#Etude des cos^2
cars.pca$ind$cos2[,1:2]
cars.pca$var$cos2[,1:2]

#Etude des contributions
cars.pca$ind$contrib[,1:2]
cars.pca$var$contrib[,1:2]

#Biplot
fviz_pca_biplot(cars.pca, repel = TRUE,
                col.var = "orange3", 
                col.ind = "royalblue")  

#Exo3 partieI

fviz_nbclust(cars, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2,color="darkred",lwd=1)+
  labs(subtitle = "Elbow method")

cars2<-scale(cars)

#K-means 3 - clusters
set.seed(5)
cars.kmeans<-kmeans(cars2,centers=3,nstart=10)
cars.kmeans

#Visualisation
fviz_cluster(cars.kmeans,data=cars, palette = c("#6A0572", "#00AFBB", "#E7B800"))+ 
  geom_hline(yintercept = 0, linetype = "solid", color = "black") + geom_vline(xintercept = 0, linetype = "solid", color = "black")


#Moyenne des groupes
aggregate(cars, by=list(cluster=cars.kmeans$cluster), mean)

#K-means 2 clusters
set.seed(10)
cars.kmeans2<-kmeans(cars2,centers=2,nstart=10)
cars.kmeans2
fviz_cluster(cars.kmeans2,data=cars)+
  geom_hline(yintercept = 0, linetype = "solid", color = "black") + geom_vline(xintercept = 0, linetype = "solid", color = "black")
aggregate(cars, by=list(cluster=cars.kmeans2$cluster), mean)

#K-means 4 clusters
set.seed(116)
cars.kmeans4<-kmeans(cars2,centers=4,nstart=10)
cars.kmeans4
fviz_cluster(cars.kmeans4,data=cars)+
  geom_hline(yintercept = 0, linetype = "solid", color = "black") + geom_vline(xintercept = 0, linetype = "solid", color = "black")
aggregate(cars, by=list(cluster=cars.kmeans4$cluster), mean)



#Exo3 partieII

cars.ward<-agnes(cars2,method="ward")
pltree(cars.ward, cex = 0.6, main = "Dendrogram",ylab='Distance',xlab='Véhicules') 
rect.hclust(cars.ward, k = 3, border = 2:3)
cars.groups<-cutree(cars.ward,k=3)
table(cars.groups)
cbind(cars,cars.groups)
aggregate(cars, by=list(cluster=cars.groups), mean)

