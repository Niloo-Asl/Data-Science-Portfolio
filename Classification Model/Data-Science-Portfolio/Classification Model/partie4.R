############################################### Importer le fichier de data ############################################
library(readr)
df <- read_delim(" data_examen_final.csv", ";", escape_double = FALSE, trim_ws = TRUE)

############################################### Nettoyage de donnees ############################################
#remove the extra characters in column name by changing the column names
colnames(df)= gsub('"','',colnames(df))

#remove extra characters in age column
df$age=gsub('"','',df$age)
df$job=gsub('"','',df$job)
df$marital=gsub('"','', df$marital)
df$education=gsub('"','',df$education)
df$default=gsub('"','', df$default)
df$housing=gsub('"','', df$housing)
df$loan=gsub('"','', df$loan)
df$contact=gsub('"','', df$contact)
df$month=gsub('"','', df$month)
df$poutcome=gsub('"','', df$poutcome)
df$y=gsub('"','', df$y)

# est-ce que chaque client est repete une seule fois ou plus ?
A=df[(duplicated(df)) , ]
# la reponse est non 
rm(A)

############################################### Repondre aux questions ############################################
# 1)	Pouvez-vous déterminer l’âge moyen ainsi que la médiane concernant la balance du compte de notre clientèle ? 
summary(df)
# on se rend compte que la variable age est character donc on doit le convertir en numerique
df$age=as.numeric(df$age)
# calcul de moyenne et median de l'age
age_mean= mean(df$age)
age_madian=median(df$age)

# 2)	Dans la base de données se trouve la variable y représentant si la personne à souscrit au dépôt direct.
# Pouvez-vous changer le nom de cette variable en ‘deposit’ ? 
a=grep("^y", colnames(df))
names(df)[a]="deposit"

# 3) Veuillez créer la table de données data_bank_deposit qui ne possèdera que les clients qui ont une balance strictement supérieure à 0.
data_bank_deposit=subset(df, balance > 0)

# 4) un arbre de classification sur la table de données data_bank_deposit en prenant toutes les variables explicatives. 
# Cet arbre de classification sera un arbre à base de la mesure de Gini, possédant une profondeur maximale de 5 ainsi qu’un
# nombre minimal de 50 observations dans les feuilles terminales. 
# Veuillez effecteur un tel arbre de classification et afficher le graphique de l’arbre en question. 
library(rpart)
rpart_bank_deposit=rpart(deposit~., data_bank_deposit, parms = list(split='gini'), control=list(maxdepth=5, minsplit=50) , method ='class')
# affichier en graphique la presentation de modele
par(mar=c(0,2,2,0))
plot(rpart_bank_deposit)
text(rpart_bank_deposit)

library(rattle)
fancyRpartPlot(rpart_bank_deposit, caption = NULL)

# 5)	Pouvez interpréter la règle menant à la première feuille terminale 
rpart_bank_deposit

# 6)	citer les 3 variables explicatives ayant la plus grande importance dans la construction de l’arbre.
rpart_bank_deposit$variable.importance[order(rpart_bank_deposit$variable.importance, decreasing = TRUE)]

# 7) Veuillez déterminer le taux naturel d’erreur que nous possédons dans notre table de données data_bank_deposit. 
#Prediction sur toute notre table de donnees
# yes % = TNE (taux naturel erreur)
table(data_bank_deposit$deposit)/nrow(data_bank_deposit)

# 8)	Veuillez construire trois modèles, boosting, bagging, et forêt aléatoire tous de 50 arbres et ayant une 
# profondeur maximale de 4. 
# Pour ce qui est de la forêt aléatoire, veuillez choisir un tirage aléatoire de variables de 5. 

library(adabag)
# pour bagging, boosting et foret aleatoire les colonnes de character doivent etre des facteurs
data_bank_deposit$deposit=factor(data_bank_deposit$deposit)
data_bank_deposit$job=factor(data_bank_deposit$job)
data_bank_deposit$marital=factor(data_bank_deposit$marital)
data_bank_deposit$education=factor(data_bank_deposit$education)
data_bank_deposit$housing=factor(data_bank_deposit$housing)
data_bank_deposit$loan=factor(data_bank_deposit$loan)
data_bank_deposit$contact=factor(data_bank_deposit$contact)
data_bank_deposit$month=factor(data_bank_deposit$month)
data_bank_deposit$poutcome=factor(data_bank_deposit$poutcome)
data_bank_deposit$default=factor(data_bank_deposit$default)
summary(data_bank_deposit)

# et type de table doit etre data.frame
data_bank_deposit=data.frame(data_bank_deposit)
class(data_bank_deposit)

############################################## Bagging ##########################################################
bagging_bank_deposit=bagging(deposit~.,data_bank_deposit,mfinal=50, control=rpart.control(maxdepth=4))

# le nombre total ou le modele a bien classifie
SUMM=sum(diag(table(bagging_bank_deposit$class, data_bank_deposit$deposit)))
TBC=SUMM/nrow(data_bank_deposit)
# taux de mauvais classification de Bagging
TMC_bagging= 1-TBC

############################################## Boosting ##########################################################
boosting_bank_deposit=boosting(deposit~.,data_bank_deposit,mfinal=50, control=rpart.control(maxdepth=4))

# le nombre total ou le modele a bien classifie
SUMM=sum(diag(table(boosting_bank_deposit$class, data_bank_deposit$deposit)))
TBC=SUMM/nrow(data_bank_deposit)
# taux de mauvais classification de Bagging
TMC_boosting= 1-TBC

############################################## Foret aleatoire ##########################################################
library(randomForest)
FA_bank_deposit=randomForest(deposit~.,data_bank_deposit,ntree=50,mtry=5)

SUMM=sum(diag(table(FA_bank_deposit$predicted, data_bank_deposit$deposit)))
TBC=SUMM/nrow(data_bank_deposit)
TMC_FA=1-TBC

