## EXERCICE 4 CODE :

# Importation des librairies
library("FactoMineR") #Package de ACP et ACM 
library("factoextra") #Clustering
library("fBasics")
library("heatmaply")

# Chargement des données
file.choose()
chiens <-read.table("DATA/chiens.unknown")

# Statistiques Descriptives et visualization des données
View(chiens)
basicStats(chiens)
str(chiens)
dim(chiens)
summary(chiens)

sum(is.na(chiens)) # Évaluation Valeur Manquantes 

boxplot

heatmaply_cor(cor(chiens),xlab = "Variables", ylab = "Variables",k_col = 2
              ,k_row = 2,dendrogram=FALSE, cellnote = cor(chiens))

chiens2 <- scale(chiens) # Normalisation des données

chisq.test(chiens)
chisq.test(chiens2) # Test non faisable 

?CA
 
sol.mca <-CA(chiens, col.sup =7, graph = TRUE)
sol.mca$eig
# Le premier plan factoriel explique autour de 70% de la variance. Lorsque nous
# tenons en compte les 3 premiers plans, nous avons 87% de la variance. 

sol.mca$row$cos2
# Ceci nous laisse voir la qualité de répresentation des races selon l'axe. En
# général lorsque c'est inférieur à 0.5 nous disons que cela s'est fait écraser

sol.mca$col$cos2
# Ici nous pouvons tirer comme conclusion que notre première axe désigne le
# niveau d'affection et l'axe 2 explique l’aggrésivité.

# Nous voyons que d'après ceci, la affection serait en rélation à la taille. 
# Quant à l'axe 2, nous voyons que la velocité et l'aggresivité ceci peut être dû
# au fait que c'est des méthodes de défense ou sécurité. 

# Il paraîtrait qu'il existerait une rélation forte entre fonctionnalité et taille et 
# plus forte encore par rapport le poids. 

sol.mca$row$contrib
sol.mca$col$contrib

sol.mca$col.sup
sol.mca$call

sol2.mca <-CA(chiens, col.sup =7, graph = TRUE, axes = c(2,3))
# Ce graphique permet d'analyser une dimension supplémentaire.
# Graphiquement, il semblerait que la dimension 3 est expliquée par l'intelligence 
# et l'aggresivité ce qui semble logique car l'agressivité peut être une réponse de
# spontanéité

sol.mca$col$cos2
sol.mca$row$cos2
sol.mca$row$contrib