# AMDJAHED Inès - KIHAL Amina 
# Conspiracy theories beliefs
# Analyse de données avancée


# Intro -------------------------------------------------------------------

# 0- Importation des librairies

library(tidyverse)
library(skimr)
library(FactoMineR) 
library(factoextra) 
library(explor)
library(skimr)
library(dagitty)


# a- Importation du jeu de données

library(readr)
conspiracy <- read_csv("conspiracy.csv")


# b- Premier coup d'œil au jeu de données

skim(conspiracy)

# Observations générales : 


# - Variables Q1 à Q15 : Les écart-types varient entre environ 1,38 et 1,51. Cela indique une dispersion modérée des réponses autour de la moyenne pour ces variables. Les réponses semblent être réparties de manière relativement homogène dans cette plage.

# - Variables E1 à E15 : Ces variables ont des écart-types beaucoup plus élevés, allant de l'ordre de milliers à des millions. Cela signifie qu'il y a une énorme variabilité dans les réponses pour ces variables. Il peut y avoir des valeurs extrêmement élevées ou basses qui contribuent à cette grande dispersion.

# - Variables introelapse, testelapse et surveyelapse : Les écart-types de ces variables sont également élevés, indiquant une grande variabilité dans les temps de réponse en millisecondes. Cela peut être dû à des différences significatives dans la rapidité avec laquelle les participants ont répondu à ces questions.

# - Il y a certaines variables considérées en tant que variables quantitatives mais qui sont en réalité des variables qualitatives, il faudra donc effectuer les changements.

# - De même, il faudrait renommer certaines réponses des colonnes pour faciliter l'interprétation et la compréhension des analyses. 

# NB : Le terme théorie du complot sera écrit sous forme d'acronyme "TC"

# c- Centrage de l'analyse 


questions_conspiracy <- conspiracy %>% 
  select(1:15)

pca_questions <- PCA(questions_conspiracy)


fviz_pca_biplot(pca_questions)

# On remarque des individus très atypiques notamment le 2154 et aussi des individus très corrélés aux questions concernant les TC sur les alliens : 756, 257, 1529. 

# En analysant l'analyse en composantes principales, on remarque que certaines questions sont fortement corrélées entre elles, notamment les questions Q3 Q8 Q13 relatives aux questions sur les TC sur les extraterrestres. 
# De ce fait, nous avons choisi de nous concentrer sur les TC inhérentes aux alliens afin de pouvoir y rechercher des pistes intéressantes. 
# Plus particulièrement, nous souhaitons savoir si les personnes qui croient aux théories du complot concernant les alliens ont un type de personnalité précis.
# Nous souhaitons également savoir quelles sont les caractéristiques socio démographiques qui peuvent caractériser les individus qui croient en ces théories du complot. 


# NB : Le terme théorie du complot sera écrit sous forme d'acronyme "TC"


# Nos hypothèses : 

# - 1) Un certain type de personnalité caractérise les individus croyant aux TC sur les alliens 
# H0 (hypothèse nulle) : Il n'existe aucune différence significative dans les traits de personnalité entre les individus croyant aux théories du complot sur les extraterrestres et ceux qui ne croient pas.
# H1 (hypothèse alternative) : Les individus croyant aux théories du complot sur les extraterrestres présentent des caractéristiques de personnalité distinctes par rapport à ceux qui ne croient pas. 


# - 2) Il y a un ou des facteurs socio démographiques qui sont spécifiques aux individus qui croient aux TC sur les alliens
# H0 : Aucun facteur socio-démographique particulier ne différencie les individus qui croient aux théories du complot sur les extraterrestres de ceux qui ne croient pas.
# H1 : Il existe au moins un facteur socio-démographique spécifique associé aux individus croyant aux théories du complot sur les extraterrestres.


# -3) Des clusters peuvent être determinés pour les individus croyant aux TC sur alliens. 
# H0 : Il n'y a pas de formation de clusters distincts parmi les individus croyant aux théories du complot sur les extraterrestres.
# H1 : Les individus croyant aux théories du complot sur les extraterrestres peuvent être regroupés en clusters distincts en fonction de certaines caractéristiques ou croyances communes. 

# I - Analyse primaire et nettoyage des données  ----------------------------------------------


#  1. Filtrage du jeu de données ----------------------------------------------


# a) Sélection des questions traitant des TC sur les alliens

questions_alien <- conspiracy %>% 
  select(3, 8, 13)

pca_aliens <- PCA(questions_alien)


# b) Ajout d'une colonne score allien qui pondère les 3 questions 

conspiracy <- conspiracy %>% 
  mutate(score_alien = round(rowSums(questions_alien) / 15 * 5, 2)) # On fait une moyenne sur 5 pour standardiser à la même échelle des réponses des questions 


# 2. Nettoyage des données relatives aux TC sur les alliens   ---------------------------------------------------


# a) Convertir les variables socio démographiques en facteur 

# - Liste des noms de colonnes à convertir en facteur et renommage des niveaux

# Éducation
conspiracy$education <- factor(conspiracy$education, 
                               levels = c(1, 2, 3, 4), 
                               labels = c("Less than high school", "High school", "University degree", "Graduate degree"))

# Urban
conspiracy$urban <- factor(conspiracy$urban, 
                           levels = c(1, 2, 3), 
                           labels = c("Rural (country side)", "Suburban", "Urban (town, city)"))

# Gender
conspiracy$gender <- factor(conspiracy$gender, 
                            levels = c(1, 2, 3), 
                            labels = c("Male", "Female", "Other"))

# Hand
conspiracy$hand <- factor(conspiracy$hand, 
                          levels = c(1, 2, 3), 
                          labels = c("Right", "Left", "Both"))

# Religion
conspiracy$is_religious <- ifelse(conspiracy$religion %in% 1:11, TRUE, FALSE)

# Voted
conspiracy$voted <- factor(conspiracy$voted, 
                           levels = c(1, 2),
                           labels = c("Yes", "No"))

# Married
conspiracy$married <- factor(conspiracy$married, 
                             levels = c(1, 2, 3), 
                             labels = c("Never married", "Currently married", "Previously married"))



# b) Nettoyage des données TIPI

# Renommer les colonnes TIPI avec les bons traits de personnalité

conspiracy <- conspiracy %>%
  rename(
    "Extraverted, enthusiastic" = TIPI1,
    "Critical, quarrelsome" = TIPI2,
    "Dependable, self-disciplined" = TIPI3,
    "Anxious, easily upset" = TIPI4,
    "Open to new experiences, complex" = TIPI5,
    "Reserved, quiet" = TIPI6,
    "Sympathetic, warm" = TIPI7,
    "Disorganized, careless" = TIPI8,
    "Calm, emotionally stable" = TIPI9,
    "Conventional, uncreative" = TIPI10
  )
  


# b) Analyse des NA pour ces variables

colSums(is.na(conspiracy[, c("Extraverted, enthusiastic", "Critical, quarrelsome", "Dependable, self-disciplined", "Anxious, easily upset", "Open to new experiences, complex", "Reserved, quiet", "Sympathetic, warm", "Disorganized, careless", "Calm, emotionally stable", "Conventional, uncreative")]))


# Pas de NA dans les questions relatives aux TIPI


# Résumé des données avec comptage des valeurs manquantes

summary(conspiracy[c("education", "urban", "gender", "hand", "is_religious", "voted", "married")])


# Supprimer les NA des colonnes socio-démographiques 

# Utilisation de na.omit() pour supprimer les NA dans les colonnes spécifiques

conspiracy <- conspiracy[!is.na(conspiracy$education) & !is.na(conspiracy$urban) & 
                                 !is.na(conspiracy$gender) & !is.na(conspiracy$hand) & 
                                 !is.na(conspiracy$is_religious), ]


# pas beaucoup de perte de données donc c'est pas dérangeant : - de 80 observations perdues

# Compter le nombre de NA dans les colonnes introelapse, testelapse et surveyelapse

colSums(is.na(conspiracy[, c("introelapse", "testelapse", "surveyelapse")]))

# Pas de NA ! 

# En résumé, il y a plus de NA dans les colonnes socio démographiquess et il n'yen a pas de base pour les colonnes introelpase, testelapse, surveyelapse et les colonnes sur les TIPI. 




# II - Questions d'Analyse ----------------------------------------------

# A - Personnalités & score alien ---------------------------------------------


tipi_allien <- conspiracy %>% 
  select(34:43, "score_alien")

pca_tipi <- PCA(tipi_allien, quanti.sup = 11) 

# Interprétation : score_allien corrélé avec les personnes qui se considérent désorganisés et careless
# A l'inverse, les gens qui se considérent reservés et silencieux ont une corrélation négative avec le score allien

# Pour en savoir un peu plus, faisons des clusters ! 

tipi_allien_HCPC <- HCPC(pca_tipi, nb.clust = -1)

tipi_allien_HCPC$desc.var 

# Interprétation des clusters 

# - Groupe 1 :
  
# - Anxieux, facilement contrarié
# - Désorganisé, négligent
# - Réservé, calme
# - Critique, querelleur
# - Conventionnel, peu créatif

# Ce groupe peut être interprété comme le groupe des "névrosés".
# Les personnes appartenant à ce groupe sont plus susceptibles d'être anxieuses, stressées et inquiètes. 
# Elles sont également plus susceptibles d'être désorganisées et négligentes. En outre, elles peuvent être plus réservées et introverties, et avoir une vision plus critique et négative d'elles-mêmes et des autres.

# - Groupe 2 : 

# - Calme, émotionnellement stable
# - Fiable, autodiscipliné
# - Réservé, calme
# - Conventionnel, peu créatif

# Ce groupe peut être interprété comme le groupe "consciencieux". 
# Les personnes appartenant à ce groupe sont plus susceptibles d'être calmes et émotionnellement stables. 
# Elles sont également plus susceptibles d'être fiables et autodisciplinées. En outre, elles peuvent être plus réservées et introverties, et avoir des opinions plus conventionnelles et traditionnelles.

# - Groupe 3 : 

# - Extraverti, enthousiaste
# - Ouvert aux nouvelles expériences, complexe
# - Sympathique, chaleureux

# - Ce groupe peut être interprété comme le groupe "ouvert". 
# Les personnes appartenant à ce groupe sont plus susceptibles d'être extraverties et enthousiastes. 
# Elles sont également plus susceptibles d'être ouvertes à de nouvelles expériences et idées. En outre, elles peuvent être plus sympathiques et chaleureuses envers les autres.


# Il y a donc trois types de profils qui peuvent caractériser les personnes qui croient aux TC sur les aliens. 

tipi_allien_HCPC$desc.ind

# Groupe 1 :
  
# Les individus 1830, 256, 228, 145 et 180 ont de fortes probabilités d'appartenir au groupe 1.
# Ces individus sont relativement proches du centroïde de l'amas, avec des distances respectives de 6,033749, 5,959359, 5,713002, 5,713002 et 5,713002.


# Groupe 2 :
  
# Les individus 1410, 795, 1733, 784 et 2209 ont de fortes probabilités d'appartenir au groupe 2.
# Ces individus sont également relativement proches du centroïde de l'amas, avec des distances respectives de 5,444943, 5,062196, 5,014727, 5,013894 et 4,979558.

# Groupe 3 :
  
# Les individus 1655, 2193, 1750, 1588 et 934 ont de fortes probabilités d'appartenir à l'amas 3.
# Ces individus sont les plus proches du centroïde de l'amas, avec des distances respectives de 5,272111, 4,740104, 4,703568, 4,691390 et 4,684828.

# Ajout de colonne 
conspiracy2 <- tipi_allien_HCPC$data.clust # on peut ajouter au jeu de donnÃ©es original une nouvelle colonne qui indique les clusters
conspiracy <- cbind(conspiracy, conspiracy2$clust)


# B- Facteurs socio démographiques & score alien ----------------------------------------------------


# B1 - Score alien & Education ----------------------------------------------------


critere_education <- conspiracy %>% 
  select("score_alien", 60)

mca_education <- MCA(critere_education, quanti.sup = 1) 

fviz_mca_biplot(mca_education)


# High school et score_alien sont corrélés positivement 
# Au contraire, graduate degree et score_alien sont corrélés négativement

critere_education_HCPC <- HCPC(mca_education, nb.clust = 3)    # nombre de clusters fixés à 3

critere_education_HCPC$desc.var


# Interprétation :

# - Analyse MCA (Multiple Correspondence Analysis) :

#L'analyse MCA montre des relations significatives entre le niveau d'éducation et la croyance aux théories du complot sur les aliens.
#Les individus ayant un diplôme universitaire ont tendance à former un groupe distinct (cluster 1), avec une croyance plus faible envers les théories du complot sur les aliens. Ils ont également un score_alien plus bas.
#Les individus avec un niveau d'éducation inférieur (école secondaire, moins que le secondaire) ont tendance à former un autre groupe (cluster 2) avec une croyance plus élevée dans les théories du complot sur les aliens et des scores_alien plus élevés.
#Un troisième groupe (cluster 3) est formé principalement par les individus ayant un diplôme d'études supérieures, avec une croyance plus faible envers les théories du complot sur les aliens et des scores_alien plus bas.


# - Analyse HCPC (Hierarchical Clustering on Principal Components) :
  
# L'analyse HCPC confirme ces groupes en montrant que le niveau d'éducation est significativement lié à la formation de clusters. Les tests du chi carré indiquent des p-values très faibles, suggérant une forte association entre l'éducation et la formation de clusters.
# Les clusters sont définis par le niveau d'éducation, avec des individus ayant un diplôme universitaire formant un groupe distinct, tandis que ceux avec un niveau d'éducation plus faible se regroupent différemment.
# De plus, l'analyse révèle que le score_alien est également significativement lié à la formation de clusters. Les individus dans le cluster 2 ont des scores_alien plus élevés, tandis que ceux dans le cluster 1 ont des scores_alien plus bas et ceux dans le cluster 3 ont des scores_alien intermédiaires.



# B2 - Score alien & Religion ----------------------------------------------------

# Test t pour comparer les moyennes des scores de croyance en la TC sur les aliens entre les personnes religieuses et les personnes non religieuses

t.test(score_alien ~ is_religious, data = conspiracy)


# Le test t de Welch a montré qu'il existe une différence significative entre les moyennes des scores de croyance en la théorie du complot sur les aliens entre les personnes religieuses et les personnes non religieuses (p = 0,003357). La moyenne des scores de croyance en la théorie du complot sur les aliens est plus élevée chez les personnes religieuses que chez les personnes non religieuses.
# L'intervalle de confiance à 95% indique que la différence entre les moyennes des deux groupes est comprise entre -0,7846478 et -0,1562328. Cela signifie que nous pouvons être confiants à 95% que la vraie différence entre les moyennes des deux groupes est comprise dans cet intervalle.
# En conclusion, les résultats du test t de Welch suggèrent que les personnes religieuses sont plus susceptibles de croire aux théories du complot sur les aliens que les personnes non religieuses.


# B3 - Score alien & Urban ----------------------------------------------------

critere_urban <- conspiracy %>% 
  select(score_alien, 61)

mca_urban <- MCA(critere_urban, quanti.sup = 1) # mêmes variables en supplémentaire

fviz_mca_biplot(mca_urban)

#Sur ce graphique on voit que notre Score Alien  des personnes est corrélé avec les personnes qui habite en zone rural ou en ville. 

# A l'inverse, les personnes qui habite dans les banlieues n’ont pas une corrélation significative avec le Score Alien. Ce qui signifie que les personnes qui croient aux théories sur les Aliens ont plus "tendance" à habiter en zone rural ou en ville. 

hcpc_urban <- HCPC(mca_urban, nb.clust = 2)

hcpc_urban$desc.var

# La p-value est de 0, ce qui suggère une forte dépendance entre les clusters créés par l'analyse HCPC et la variable "urban" (zone de résidence). En d'autres termes, le choix du lieu de résidence est fortement lié à la structure des clusters.

# Cluster 1 : Dans ce cluster, deux catégories de la variable "urban" sont fortement représentées : "Suburban" (45.79%) et "Urban (town, city)" (34.73%). Les valeurs de "Cla/Mod" et "Mod/Cla" proches de 100% indiquent que ces catégories sont caractéristiques de ce cluster. Le test de chi-carré (v-test) montre que la catégorie "Suburban" est fortement associée à ce cluster, avec un v-test de 25.77, et la catégorie "Urban (town, city)" est également associée avec un v-test de 21.28. En revanche, la catégorie "Rural (country side)" n'est pas du tout représentée dans ce cluster.
# Cluster 2 : Dans ce cluster, la catégorie "Rural (country side)" est la seule catégorie représentée (100%). Les valeurs de "Cla/Mod" et "Mod/Cla" proches de 100% indiquent que cette catégorie est caractéristique de ce cluster. Le test de chi-carré (v-test) montre que la catégorie "Rural (country side)" est fortement associée à ce cluster, avec un v-test positif (infini), tandis que les catégories "Urban (town, city)" et "Suburban" ne sont pas du tout représentées dans ce cluster.
# En résumé, l'analyse HCPC a permis de créer deux clusters fortement liés à la variable "urban" (zone de résidence). Le premier cluster est associé aux zones "Suburban" et "Urban (town, city)", tandis que le deuxième cluster est associé à la zone "Rural (country side)". Les résultats mettent en évidence des différences significatives dans la répartition des individus en fonction de leur zone de résidence, et montrent comment la variable "urban" contribue à la formation de ces clusters.


hcpc_urban$desc.ind



# Les résultats suggèrent que les clusters créés sont associés de manière significative aux catégories de résidence (rural, suburbain, urbain) représentées par la variable "urban". Cela signifie que le choix du milieu de résidence peut influencer l'appartenance à un cluster particulier.




# B4 - Score alien & Genre ----------------------------------------------------

critere_gender <- conspiracy %>% 
  select(score_alien, 62)

mca_gender <- MCA(critere_gender, quanti.sup = 1) # mêmes variables en supplémentaire

fviz_mca_biplot(mca_gender)


# Femme est corrélé positivement avec le score alien

# Sur ce graphique on voit que notre Score Alien est corrélé avec les personnes qui s’identifie comme «Femme» en termes de genre . 
# A l'inverse, les personnes qui s’identifie comme «Homme» n’ont pas une corrélation claire avec le Score Alien. 
#Ce qui signifie que les personnes qui croient aux théories sur les Aliens ont plus "tendance" à s’identifie comme «Femme» en termes de genre 


# B5 - Score alien & Hand -----------------------------------------------------


critere_hand  <- conspiracy %>% 
  select(score_alien, 65)

mca_hand <- MCA(critere_hand, quanti.sup = 1) # mêmes variables en supplémentaire


fviz_mca_biplot(mca_hand)

# Sur ce graphique on voit que notre Score Alien est corrélé avec les personnes qui sont ambidextres et un peu avec ceux qui sont gaucher.
#A l'inverse, les personnes qui sont droitier n’ont pas une corrélation claire avec le Score Alien. 
#Ce qui signifie que les personnes qui croient aux théories sur les Aliens ont plus "tendance" à être ambidextres ou gaucher.




# Conclusion BIS ----------------------------------------------------------


critere_mult2 <- conspiracy %>% 
  select("score_alien", 60, 61,62,65,75)

mca_mult2 <- MCA(critere_mult2, quanti.sup = 1) 

fviz_mca_var(mca_mult2)

#Cluster
# dendrogramme


critere_mult2_HCPC <- HCPC(mca_mult2, nb.clust = 3)    # nombre de clusters fixé à 3

critere_mult2_HCPC$desc.var



fviz_cluster(critere_mult2_HCPC,
              repel = TRUE,
              show.clust.cent = TRUE, # Show cluster centers
              palette = "jco",
              ggtheme = theme_minimal(),
              main = "Clusterisation base sur l'analyse factorielle (PCA)"
             )



# III - Conclusion ----------------------------------------------

#Pour résumer, les personnes qui ont tendance à croire aux théorie de complot sur les Aliens  s’identifier comme “Femme”. Ils ont tendance à aimer la ruralité ou  la ville. 
#(Les résultats nous montrent des différences significatives dans la répartition des gens en fonction de leur lieu de vie, et comment ce choix de quartier peut influencer l’appartenance à un cluster particulier)
#L’éducation joue bien un rôle puisque si la personne s’est arrêté au lycée, elle a plus tendance à croire aux Aliens. 
#(Les Clusters a suggère que plus on se plonge dans l’éducation, moins on a tendance à y croire). 
#Ils ont aussi tendance à se considéré comme religieux. 
#Au niveau de leur type de personnalité, ceux qui se considèrent comme “désorganisé” et “careless” ont plus tendance à y croire tandis que le type  “réservé” et “silencieux” non.
#Les amateurs de théories alien ont plus tendance à être ambidextres ou gauchers. 



# Modèles -----------------------------------------------------------------

#(Genre : et maintenant... on aimerait bien pouvoir identifier automatiquement nos believers à partir des traits démographiques et psychologiques qu'on a vu être significatifs...)


critere_mult_clust <- conspiracy %>% 
  select("score_alien", 60, 61, 62,65, 66, 75)

library(dagitty)


# Convert the dataframe to numeric
critere_mult_clust <- as.data.frame(sapply(critere_mult_clust, as.numeric))

critere_mult_clust <- critere_mult_clust %>%
  mutate(across(everything(), as.numeric))


# on peut copier-coller le "Model code" du site web de dagitty

d1 <- dagitty('
dag {
bb="0,0,1,1"
clust [exposure,pos="0.508,0.795"]
education [pos="0.233,0.524"]
gender [pos="0.818,0.722"]
hand [pos="0.285,0.676"]
religion [pos="0.698,0.750"]
score_alien [outcome,pos="0.535,0.324"]
urban [pos="0.348,0.757"]
clust -> score_alien
education <-> score_alien
gender -> education
gender -> score_alien
hand -> score_alien
religion -> score_alien
score_alien -> urban
}
')

plot(d1)

impliedConditionalIndependencies(d1) # indÃ©pendances impliquÃ©es par le graphe

critere_mult_clust <- as.numeric(critere_mult_clust) # toutes les colonnes doivent Ãªtre quanti


critere_mult_clust <- critere_mult_clust %>%
  mutate_at(c("conspiracy2$clust", "education", "gender", "urban", "is_religious", "hand"), as.numeric)



test_results <- dagitty::localTests(d1, data = critere_mult_clust)

localTests(d1, critere_mult_clust) # test automatique de toutes les indépendances impliquées vis-à-vis des données

library(rpart)
library(rpart.plot)


conspiracy_train <- conspiracy %>% slice_sample(prop = 0.8) # données train
conspiracy_test <- conspiracy %>% anti_join(conspiracy_train) # données test




arbre_1 <- rpart(score_alien ~ education,
                 data = conspiracy_train)
rpart.plot(arbre_1) # visualisation


arbre_2 <- rpart(score_alien ~ is_religious,
                 data = conspiracy_train)
rpart.plot(arbre_2) # visualisation


classe_prevue <- predict(arbre_1, 
                         conspiracy_train, 
                         type = "class") # la classe la plus probable


library(randomForest)

conspiracy_colonne <- conspiracy %>% 
  select("score_alien", 60, 61, 62,65, 74, 64, 34,35,36,37,38,39,40,41,42,43)



conspiracy_train2 <- conspiracy_colonne %>% slice_sample(prop = 0.8) # données train
conspiracy_test2 <- conspiracy_colonne %>% anti_join(conspiracy_train2) # données test


foret_1 <- randomForest(score_alien ~ ., 
                        conspiracy_train2)


foret_1 # matrice de confusion avec incluse une estimation des erreurs possibles sur des données de test

varImpPlot(foret_1) # les prédicteurs les plus importants



foret_1 <- randomForest(score_alien ~ ., 
                        conspiracy_train2)


foret_1 # matrice de confusion avec incluse une estimation des erreurs possibles sur des données de test

varImpPlot(foret_1) # les prédicteurs les plus importants




