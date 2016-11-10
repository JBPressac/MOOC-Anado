# Chargement de FactoMineR
library(FactoMineR)

# Importation du jeu de données
don <- read.table("http://factominer.free.fr/livre/femme_travail.csv",
    header=TRUE, row.names=1, sep=";", fileEncoding="latin1")
don <- don[,1:3]  ## seules les premières colonnes du jeu de données sont utilisées ici
summary(don)
# Test du Chi2 : Significativité de la liaison (écart entre les données observées et le modèle d'indépendance)
# L'AFC ne dit rien de la significativité de la liaison car elle travaille sur les probabilités
# qui ne dépendent pas des objectifs.
res.test.chi2 <- chisq.test(don)

# L'AFC : les sortie et le graphe par défaut
res.ca <- CA(don)
summary(res.ca)

# Le graphe des valeurs propres
res.ca$eig
barplot(res.ca$eig[,1], main="Valeurs propres",names.arg=1:nrow(res.ca$eig))

# Les graphiques des lignes et des colonnes séparés
plot(res.ca, invisible="col")
plot(res.ca, invisible="row")
