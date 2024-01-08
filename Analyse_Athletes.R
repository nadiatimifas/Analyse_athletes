install.packages("units")
install.packages("FactoMineR")
install.packages("factoextra")
install.packages("corrplot")
install.packages("ggsignif")
install.packages("cowplot")
library(FactoMineR)
library(units)
library(dplyr)
library("factoextra")
library("corrplot")
library("ggsignif")
library("cowplot")



#extraction des données  
data(decathlon)

#afficher les noms des colonnes
colnames(decathlon)
glimpse(decathlon)

#calculer la matrice de corrélation des variables initiales
cor(decathlon[,1:12])

#Réaliser l'ACP sur tous les individus avec les 12 premiÃ¨res variables.
res.pca=PCA(decathlon[,1:12], scale.unit=TRUE, ncp=2, graph=TRUE)
round(res.pca$eig,2)
inertie=res.pca$eig[,2]
inertie
inertie_tot=sum(res.pca$eig[1:2,][,2])
inertie_tot#Les deux premières axes traduisent 54.1578 % de lâ€™inertie totale.
barplot(inertie,ylab="pourcentage d'inertie (%)",names.arg=round(inertie,2))
title("valeurs propres (inerties ou variances de chaque composante) en %")
scree.plot=fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))#scree plot
scree.plot
var=get_pca_var(res.pca)
var$coord
var$cos2
var.plot=fviz_pca_var(res.pca, col.var = "cos2",gradient.cols = c("green", "blue", "red"),repel = TRUE)
var.plot

corrplot(var$cos2)#visualiser les corrÃ©lations
ind=get_pca_ind(res.pca)
ind
ind$coord # ou head(ind$coord)
ind$cos2
ind$contrib
ind.plot=fviz_pca_ind (res.pca, col.ind = "cos2",gradient.cols = c("red", "blue", "green"),repel =
                         TRUE)
ind.plot

# Visualiser les variables avec cos2> = 0.6
fviz_pca_var (res.pca, select.var = list(cos2 = 0.6))
fviz_pca_var (res.pca, select.var = list(cos2 = 5))
name= list (name = c ("", "", ""))

X=res.pca$ind$coord
X 

Res.kmeans=kmeans(X[,1:2],centers=2) 
Plotk=fviz_cluster(Res.kmeans,X[,1:2])
Plotk
library (ggpubr)
ggexport (plotlist = list(scree.plot, ind.plot, var.plot,Plotk),nrow=2,ncol=2,filename =
            "PCA.pdf")

