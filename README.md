# Prédiction des profils croyants aux théories du complot

Analyse statistique avancée d'un dataset OpenPsychometrics sur les croyances aux théories du complot. Ce projet explore qui croit aux théories sur les extraterrestres et pourquoi, via ACP, ACM, clustering et Random Forest.

## Contexte

Projet réalisé dans le cadre du cours Analyse de données avancée, M2 Information-Communication, Université Paris Nanterre.  

Source des données : OpenPsychometrics - dataset "Conspiracy Beliefs Scale" (données collectées librement sur le web, format CSV).
Vous pouvez consulter les données sur [Kaggle](https://www.kaggle.com/datasets/lucasgreenwell/generic-conspiracist-beliefs-scale-responses), ou via le fichier csv présent dans /data.

## Dataset

- 2 154+ répondants analysés  
- 15 questions sur les croyances aux théories du complot  
- 10 traits de personnalité TIPI  
- 3 clusters identifiés  

## Questions de recherche

- Les individus croyant aux théories sur les extraterrestres présentent-ils des traits de personnalité TIPI distincts ?  
- Des facteurs socio-démographiques caractérisent-ils les croyants aux théories alien ?  
- Peut-on regrouper les croyants en clusters distincts selon leurs caractéristiques ?

## Pipeline d'analyse

1. Import CSV  
2. ACP exploratoire  
3. Construction du "score alien"  
4. ACM sur les variables catégorielles  
5. HCPC clustering  
6. Random Forest  

## Méthodes statistiques

- **ACP (Analyse en Composantes Principales)** : exploration des 15 questions, identification d’un groupe cohérent sur les extraterrestres (Q3, Q8, Q13)  
- **ACM (Analyse des Correspondances Multiples)** : corrélation du score alien avec les variables catégorielles (genre, éducation, religion, lieu de vie, latéralité)  
- **HCPC (Clustering hiérarchique)** : 3 clusters identifiés, fortement structurés par la variable "urban"  
- **Random Forest et arbres de décision** : modélisation prédictive du score alien (split 80/20 train/test)  
- **DAG via dagitty** : hypothèses de dépendances causales entre variables

## Résultats clés

- Le score alien révèle une population distincte de croyants  
- Les femmes et les personnes vivant en milieu rural ont un score alien plus élevé  
- Le niveau d'éducation "lycée" est associé à un score alien plus élevé  
- Les personnes religieuses ont un score alien plus élevé  
- TIPI "Disorganized, careless" corrélé positivement, "Reserved, quiet" négativement  
- Les gauchers et ambidextres ont tendance à scorer plus haut  

## Limites

- Résultats **associatifs**, non causaux  
- Construction du score alien simplifiée (moyenne des 3 questions)  
- DAG basé sur des hypothèses théoriques, non validé statistiquement  
- Dataset biaisé par les répondants volontaires et la répartition géographique

## Fichiers inclus

- `data/` : dataset CSV  
- `scripts/` : scripts R d’analyse  
- `rmarkdown/` : RMarkdown report (`conspiracy_report.Rmd`)  
- `output/figures/` : graphiques générés  
- `output/html/` : rendu HTML du rapport  
- `assets/images/` : images utilisées  
- `assets/music/` : musique de fond pour le rapport  
- `rsconnect/` : fichiers de déploiement pour RPubs  

## Licence

Projet réalisé à des fins académiques, utilisation libre dans un cadre pédagogique.
