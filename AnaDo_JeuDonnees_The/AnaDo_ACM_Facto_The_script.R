# Importation du jeu de donn�es depuis le site
the <- read.table("http://factominer.free.fr/livre/the.csv",
    header=TRUE, sep=";", fileEncoding="latin1")
summary(the)

# Chargement de FactoMineR
library(FactoMineR)

# L'ACM
res <- MCA(the, quanti.sup=22, quali.sup=c(19:21,23:36))
summary(res, ncp=3, nbelements=Inf)    ## fonction summary.MCA

# Description des dimensions
dimdesc(res)

# Graphes en mettant certains libell�s et pas d'autres
plot(res, label=c("var","quali.sup"), cex=0.7)

# Graphes en rendant invisible certains �l�ments
plot(res, invisible=c("var","quali.sup"), cex=0.7)
plot(res, invisible=c("ind","quali.sup"),autoLab="y",cex=0.7,title="Modalit�s actives")
plot(res, invisible=c("ind","var"),autoLab="y",cex=0.7,title="Modalit�s suppl�mentaires")

# S�lection des modalites
plot(res, invisible="ind",autoLab="y",cex=0.7,selectMod="cos2 10")
plot(res, invisible="ind",autoLab="y",cex=0.7,selectMod="contrib 20")

# S�lection des individus
plot(res, invisible=c("var","quali.sup"),autoLab="y",cex=0.7,select="cos2 20")

# S�lection des individus et des modalit�s
plot(res, autoLab="y",cex=0.7, select="cos2 20", selectMod="cos2 10")

# Graphes des variables
plot(res, choix="var",xlim=c(0,0.6),ylim=c(0,0.6))
plot(res, choix="var",xlim=c(0,0.6),ylim=c(0,0.6),invisible=c("quali.sup","quanti.sup"))

# Graphes sur les axes 3 et 4
plot(res,invisible=c("var","quali.sup"),cex=0.7,select="contrib 20",axes=3:4)
plot(res, invisible="ind",autoLab="y",cex=0.7,selectMod="cos2 20",axes=3:4)

# Ellipses de confiance autour de modalit�s
plotellipses(res,keepvar=c(14:17))

