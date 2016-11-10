# Chargement des différents Packages
# questionr (Fonctions utiles à l'usage de R en SHS. Détails sur www.rdocumentation.org)
# Rcmdr (Interface graphique)
# FactoMineR (Dédié à l'analyse multidimensionnelle de données)
# RcmdrPlugin.FactoMineR (Plugin de FactoMineR pour l'interface graphique Rcmdr)
library (questionr)
# library (Rcmdr)
library (FactoMineR)
# library (RcmdrPlugin.FactoMineR)
library(explor)


# Voir plus tard quant à des graphes interactifs avec Factoshiny ou Explor
library (Factoshiny)
library (explor)


#Afficher la localisation du répertoire de travail sous la forme d'un chemin absolu
getwd()
# Pour moi : "/Users/ubo/Documents/Recherche/Statistique - AFC sous R avec JB/AnaDo_JeuDonnees_UEemploi"


# Importation du jeu de données :
# À partir des données d'Eurostat, on a construit un tableau de données croisant en lignes les 29 pays de
# l'UE et en colonnes 16 branches d'emplois. Dans une case de ce tableau, on retrouve le nombre d'emplois
# pour un pays et une branche d'emplois.
don <- read.table("AnaDo_JeuDonnees_UEemploi.csv", header=TRUE, row.names=1, sep=";", fileEncoding="latin1")


# Obtenir quelques statistiques de base concernant les 2 variables qualitatives du tableau de contingence
summary(don)


# Test du khi2
# Affiche la valeur du khi2 et son niveau de significativité
# Le test du khi2 mesure la significativité d'une relation mais pas son intensité
chisq.test(don) -> khi2don

# Explication du test du khi2
# Test permettant de déterminer la probabilité que les deux variables d'un tableau de contingence sont
# indépendantes, c'est-à-dire qu'il n'existe pas de relation entre les modalités en ligne et les modalités
# en colonne (les unes ne conditionnent pas les autres, et réciproquement). Dit autrement, cela veut dire que
# le fait d'appartenir à une modalité de la première variable n'a pas d'influence sur la modalité
# d'appartenance de la deuxième variable.
# Dans ce test, l'hypothèse nulle (H0) suppose qu'il y a indépendance entre les 2 variables.
# Si on accepte l'hypothèse d'indépendance (H0), on ne fait pas d'AFC. Car les points projetés seront tous
# proches ou confondus avec le centre de gravité, confondu avec le centre du graphe.
# Si on rejette l'hypothèse d'indépendance (p-value < 0,05), l'hypothèse alternative (H1) suppose que la
# liaison entre les deux variables est significative sans que l'on puisse définir l'intensité de la liaison.

# Dans le cas de notre jeu de données
# La valeur de l'indicateur de khi2 est "34087000". Le nombre de degré de liberté est "420". La probabilité
# d'observer une valeur du khi2 de 34087000 pour 420 degré de liberté est infinitésimale : p-value < 2.2e-16.
# Dit autrement, cela veur dire que la probabilité d'obtenir le tableau croisé sous l'hypothèse
# d'indépendance (H0) des 2 variables est d'environ 2,2x10^-16 (0,0000000000000022).
# Remarque : 10^-16 = 0.0000000000000001 (16 zéros avant le 1)
# 2,2x10^-16 = 0,0000000000000022 (15 zéros avant le premier 2)
# Source : http://www.ac-grenoble.fr/disciplines/spc/file/accompa/conversions/co/module_conversions_1.html
# On rejette donc l'hypothèse d'indépendance entre les 2 variables. En général on accepte l'hypothèse
# d'indépendance lorsque p-value est supérieure à 5 % (0,05).

# Test du khi2 - Aides à l'interprétation
# Afficher le tableau de contingence d'origine
# Le test du khi2 est symétrique. Les lignes et les colonnes du tableau croisé sont interchangeables. Le
# résultat du test sera exactement le même. Il n'y a pas de "sens de lecture" du tableau.
khi2don$observed
# Afficher le tableau d'indépendance (tableau des effectifs théoriques)
# On calcule le tableau des pourcentages théoriques, en multipliant pour chaque case la proportion observée
# dans la population des deux modalités correspondantes. Puis, le tableau des effectifs théoriques se calcule
# en multipliant le tableau des pourcentages théoriques par l'effectif total.
# Voir détails : Julien Barnier (2016), "Tout ce que vous n'avez jamais voulu savoir sur le Khi2 sans jamais
# avoir eu envie de le demander", (https://alea.fr.eu.org/pages/khi2)
khi2don$expected
# Afficher le tableau des résidus (sens des écarts à l'indépendance)
# Un résidu positif signifie que les effectifs dans la case sont supérieurs à ceux attendus sous l'hypothèse
# d'indépendance. Et l'inverse pour un résidu négatif.
khi2don$residuals

# Test de Cramer
# Le V de Cramer permet de déterminer l'intensité de la relation entre les 2 variables (entre 0 et 1)
# En complément du khi2 (significativité de la relation)
cramer.v(don)


# L'AFC : les sorties et le graphe par défaut
res.ca <- CA(don)

# Pour affichage interactif avec le package explor de Julien Barnier
explor(res.ca)

# Question : EN AFC on a deux nuages de points, celui des lignes et celui des colonnes.
# Dans ce cas, quelles inerties permet d'afficher la commande suivante : Celle des 2 nuages ?

# Afficher les résultats de l'AFC
# Le résultat du test du khi2 (uniquement sur les lignes et les colonnes actives) avec la p-value
# Un tableau avec les valeurs propres, les pourcentages d'inertie associés à chaque dimension
# Un tableau avec les résultats sur les lignes actives avec leur coordonnée (dim.n) sur chaque dimension,
# leur contribution à la construction (ctr) de chaque dimension et leur qualité de réprésentation (cos2)
# sur chaque dimension
# Un tableau avec les résultats sur les colonnes actives (dim.n, ctr, cos2)
# Un tableau (optionnel) avec les résultats sur les éléments supplémentaires en ligne avec la coordonnée
# (dim.n) et la qualité de représentation (cos2)
# Un tableau (optionnel) avec les résultats sur les éléments supplémentaires en colonne avec la coordonnée
# (dim.n) et la qualité de représentation (cos2)
summary(res.ca, nbelements = Inf)
# Les tableaux avec les résultats n'affichent, par défaut, que les 10 réponses les plus significatives
# L'argument "nbelements=Inf" permet de retirer cette limite.

# Décrire les dimensions factorielles
dimdesc(res.ca)
# Un tableau présentant les lignes en fonctions de leurs coordonnées croissantes pour la dimension 1
# Un tableau présentant les colonnes en fonctions de leurs coordonnées croissantes pour la dimension 1
# Ainsi de suite pour les dimensions 2 et 3

# Le graphe des valeurs propres (i.e. variance)
res.ca$eig
# Si on n'a bien compris, ce graphe doit servir à définir le nombre d'axes (ou "dimension" dans FactoMineR)
# à retenir. Par contre, ne faudrait-il pas dessiner le coude de Cattell ?
# Si le nombre d'axes à retenir est supérieur à deux, que fait-on ?
barplot(res.ca$eig[,1], main="Valeurs propres",names.arg=1:nrow(res.ca$eig))

# Les graphiques des lignes et des colonnes séparés
plot(res.ca, invisible="col")
plot(res.ca, invisible="row")

# Quel est le pays qui contribue le plus à la construction de l'axe 2 ?
# On affiche la liste des contributions des lignes (pays) pour chaque dimension
# Pour connaitre le nom des objets disponibles dans res.ca, on execute tout simplement res.ca
# ce qui permet de savoir que res.ca$row$contrib affiche les contributions par pays.
# Une fois que l'on a la réponse à la question, qu'en fait-on ? A quoi ça sert de le savoir ?
# QU'est ce que signifie "contribuer" ? Cela a-t-il des conséquences sur la projection du
# nuage sur le plan ? Le MOOC donne pour réponse à cette question l'Italie alors que sa
# contribution n'est pas la plus élevée d'après res.ca$row$contrib...
res.ca$row$contrib
