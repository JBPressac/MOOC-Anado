# Chargement de FactoMineR
library(FactoMineR)

getwd()

# Importation du jeu de données :
# A partir des données d'Eurostat, on a construit un tableau de données croisant 
# en lignes les 29 pays de l'UE et en colonnes 16 branches d'emplois. 
# Dans une case de ce tableau, on retrouve le nombre d'emplois 
# pour un pays et une branche d'emplois.
don <- read.table("AnaDo_JeuDonnees_UEemploi.csv",
                  header=TRUE, row.names=1, sep=";", fileEncoding="latin1")

summary(don)

# Test du Chi2 : Significativité de la liaison (écart entre les données observées et le modèle d'indépendance)
# L'AFC ne dit rien de la significativité de la liaison car elle travaille sur les probabilités
# qui ne dépendent pas des objectifs.
res.test.chi2 <- chisq.test(don)

# Il faut commencer par faire un test du chi2. Si on accepte l'hypothèse d'indépendance, on ne fait pas d'AFC.
# Car les points projetés seront tous proches ou confondus avec le centre de gravité, confondu avec le centre du graphe.
# Le test du chi2 permet d'accepter ou de rejetter l'indépendance entre les deux variables
# En général on accepte l'hypothèse d'indépendance lorsque p-value est supérieure à 5 % (0,05).
# Si on rejette l'hypothèse d'indépendance (p-value < 0,05), on peut dire que la liaison entre les 
# deux variables est significative sans que l'on puisse définir l'intensité de la liaison.
# http://mehdikhaneboubi.free.fr/stat/co/khi_deux_r.html
# 
# On rejette l'hypothèse d'indépendance, on peut donc faire une AFC
res.test.chi2 # p-value < 2.2e-16 < 0.05

# L'AFC : les sortie et le graphe par défaut
res.ca <- CA(don)
# Question : EN AFC on a deux nuages de points, celui des lignes et celui des colonnes.
# Dans ce cas, quelles inerties permet d'afficher la commande suivante : Celle des deux nuages ?
summary(res.ca)

# Le graphe des valeurs propres (i.e. variance)
res.ca$eig
# Si on n'a bien compris, ce graphe doit servir à définir le nombre d'axes (ou "dimension" dans FactoMineR) à retenir.
# Par contre, ne faudrait-il pas dessiner le coude de Cattell ?
# Si le nombre d'axes à retenir est supérieur à deux, que fait-on ?
barplot(res.ca$eig[,1], main="Valeurs propres",names.arg=1:nrow(res.ca$eig))

# Les graphiques des lignes et des colonnes séparés
plot(res.ca, invisible="col")
plot(res.ca, invisible="row")

# Quel est le pays qui contribue le plus à la construction de l'axe 2 ?
# On affiche la liste des contributions des lignes (pays) pour chaque dimension
# Pour connaitre le nom des objets disponibles dans res.ca, on execute tout simplement res.ca
# ce qui permet de savoir que res.ca$row$contrib affiche les contributions par pays.
# Une fois que l'on a la réponse à cette question, qu'en fait-on ? A quoi ça sert de le savoir ?
# QU'est ce que signifie "contribuer" ? Cela a-t-il des conséquences sur la projection du nuage sur le plan ?
# Le MOOC donne pour réponse à cette question l'Italie alors que sa contribution n'est pas la plus élevée 
# d'après res.ca$row$contrib...
res.ca$row$contrib
