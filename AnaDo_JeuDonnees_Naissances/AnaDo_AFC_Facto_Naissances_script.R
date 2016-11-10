# Données du nombre de naissances en 2003 en fonction de l'âge du père
et de l'âge de la mère
Naissance <- read.table("AnaDo_JeuDonnees_Naissances.csv", header=TRUE,sep=";", 
      row.names=1, check.names=FALSE, fileEncoding="latin1")
summary(Naissance)

# Chargement de FactoMineR
library(FactoMineR)

# L'AFC
res <- CA(Naissance, row.sup=7:8, col.sup=9:10)
summary(res)          ## fonction summary.CA

plot(res, cex=0.7, title="Représentation simultanée")

dimdesc(res)

# Graphes avec différentes sélections de lignes et/ou colonnes
plot(res, invisible=c("row.sup","col.sup"), cex=0.7, shadow=TRUE, title="Eléments actifs")
plot(res, shadow=TRUE, cex=0.7, selectRow="cos2 0.7", selectCol="cos2 0.7")
plot(res, shadow=TRUE, cex=0.7, selectRow="cos2 4", selectCol="cos2 3")
plot(res, shadow=TRUE, cex=0.7, selectRow="contrib 4", selectCol="contrib 3")
plot(res, shadow=TRUE, cex=0.7, selectRow=c("Mère - 20 ans"))

# Graphes sur les dimensions 1 et 3
plot(res, shadow=TRUE, cex=0.7, axes=c(1,3), title="Représentation sur le plan 1-3")

# Classification sur les colonnes
res.hcpc <- HCPC(res,cluster.CA="columns")
