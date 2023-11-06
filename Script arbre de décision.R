doc <- read.table("covtype.data", header = TRUE, sep=",")

a <- rep("Soil_Type_",40)
for (i in 1:40) {
  a[i]<-paste0(a[i],as.character(i))
} 
name <- c("Elevation",
          "Aspect",
          "Slope",
          "Horizontal_Distance_To_Hydrology",
          "Vertical_Distance_To_Hydrology",
          "Horizontal_Distance_To_Roadways",
          "Hillshade_9am",
          "Hillshade_Noon",
          "Hillshade_3pm",
          "Horizontal_Distance_To_Fire_Points",
          "Wilderness_Area_1",
          "Wilderness_Area_2",
          "Wilderness_Area_3",
          "Wilderness_Area_4",
          a,
          "Cover_Type")
colnames(doc)<- name

for (nb_var in 11:55){
  doc <- doc %>% 
    mutate(across(colnames(doc)[nb_var], as.factor))
}

library(tidyverse)
library(rpart)
library(partykit)
library(caret)
library(dplyr)
library(randomForest)
library(randomForestExplainer)
library(ggplot2)












###ARBRE DE DESICION AVEC APPRENTISSAGE ET TEST REPRESENTATIF: Resultat comparable avec Youna


##Fonction de partition de data
part_data <- function(data){
  # prend en entree le je de donnee à echantillonner
  # renvoie les indices des individus de l'echantillon d'entrainement et de l'echantillon de validation
  nb_indiv <- length(data[,1])
  itrain <- c()
  itest <- c(1:nb_indiv)
  # on force l'echantillon d'entrainement à avoir au moins 3 observations par type de sol
  for (i in 15:54) { #les colonnes 14 à 53 sont les colonnes "soil type"
    itrain <- c(itrain,sample(x = which(data[,i] == 1 ),size = 3)) 
  }
  
  # on force l'echantillon d'entrainement à avoir au moins 3 observations par type de sol
  for (i in 11:14) { #les colonnes" 14 à 53 sont les colonnes "soil type""Wilderness_area"
    itrain <- c(itrain,sample(x = setdiff(which(data[,i] == 1 ),itrain),size = 3)) #dans les données pas deja echantilonnee 
  }
  
  # on force l'echantillon d'entrainement à avoir au moins 1000 observations par type de recouvrement végétal
  for (i in 1:7) {
    itrain <- c(itrain,sample(x = setdiff(which(data$Cover_Type == i),itrain),size = 1000))
  }
  
  itest <- setdiff(itest,itrain)
  itest <- sample(itest,3560) 
  
  return(list(itrain,itest))
}



N=10

doc_shuffled <- doc[sample(nrow(doc)), ] #Mélange aléatoire
accuracy_scores <- numeric(N)

for (b in 1:N) { #550 répétitions du modèle 
  res=part_data(doc)
  itest=res[[2]]
  itrain=res[[1]]
  doc_test <-doc[itest,]
  doc_train <-doc[itrain,]

  X_doc_test <-doc_test[,1:54]
  X_doc_train <-doc_train[,1:54]
  
  Y_doc_test<-doc_test$Cover_Type
  Y_doc_train <-doc_train$Cover_Type
  
  
  tree_model <- rpart(Cover_Type~., data = doc_train, method="class",control = rpart.control(cp = 0.00005)) # seuil sur le raffinage, force à aller voi run peutplus dans la compléxité pour l'arbre
  
  #tree_model
  #plot(tree_model)
  #plotcp(tree_model)
  # Visualisation de l'arbre de décision
  #plot(tree_model, uniform = TRUE, branch = 0.5, margin = 0.2)
  #text(tree_model, all = FALSE, use.n = FALSE)
  # Évaluation de la précision
  y_pred <- predict(tree_model,doc_test, type='class')
  #y_pred
  accuracy_scores[b] <- sum(y_pred == Y_doc_test)/ length(Y_doc_test)
  #cat("Accuracy percentage:", accuracy, "\n")
  print(accuracy_scores)
}
#Matrice de confusion pour le dernier arbre
confusion1 = table(Y_doc_test,y_pred,dnn=list("Observed","Predicted"))
confusion1

mean_accuracy <- mean(accuracy_scores)
cat("Moyenne de l'exactitude sur", b, "itérations de Monte Carlo Cross-Validation:", mean_accuracy, "\n")





#AUTRE ENSEMBLE DE APPRENTISSAGE POUR LES ARBRES ON REPASSE EN 2/3 1/3 : résultats comparable avec Lucile
##Compléxité faible



N=10 #nombre d'itérations
doc_shuffled <- doc[sample(nrow(doc)), ] #Mélange aléatoire
accuracy_scores <- numeric(N)


for (b in 1:N) { 
  set.seed(b)
  doc_shuffled <- doc[sample(nrow(doc)), ] #Mélange aléatoire
  doc_apprentissage <- doc_shuffled[1:400000, ] #Apprentissage
  doc_test <- doc_shuffled[400001:581011, ]  #Test
  
  
  # Préparation des données pour l'apprentissage
  #Y <- doc_apprentissage$Target
  Y <- doc_apprentissage$Cover_Type
  X <- doc_apprentissage[,1:54]
  
  
  #Préparation des données pour le test
  #Y2 <- doc_test$Target
  Y2 <- doc_test$Cover_Type
  X2 <- doc_test[,1:54]
  
  tree_model <- rpart(Cover_Type~., data = doc_apprentissage, method="class",control = rpart.control(cp = 0.1))
  
  tree_model
  
  plotcp(tree_model)
   #Visualisation de l'arbre de décision
  plot(tree_model, uniform = TRUE, branch = 0.5, margin = 0.2)
  text(tree_model, all = FALSE, use.n = FALSE)
  # Évaluation de la précision
  y_pred <- predict(tree_model,doc_test, type='class')
  #y_pred
  accuracy_scores[b] <- sum(y_pred == Y2)/ length(Y2)
  #cat("Accuracy percentage:", accuracy, "\n")
  print(accuracy_scores)
  
}

confusion2 = table(Y2,y_pred,dnn=list("Observed","Predicted"))


mean_accuracy <- mean(accuracy_scores)
cat("Moyenne de l'exactitude sur", b, "itérations de Monte Carlo Cross-Validation:", mean_accuracy, "\n")




##Complexité forte

N=1 #nombre d'itérations
doc_shuffled <- doc[sample(nrow(doc)), ] #Mélange aléatoire
accuracy_scores <- numeric(N)


for (b in 1:N) { 
  set.seed(b)
  doc_shuffled <- doc[sample(nrow(doc)), ] #Mélange aléatoire
  doc_apprentissage <- doc_shuffled[1:400000, ] #Apprentissage
  doc_test <- doc_shuffled[400001:581011, ]  #Test
  
  
  # Préparation des données pour l'apprentissage
  #Y <- doc_apprentissage$Target
  Y <- doc_apprentissage$Cover_Type
  X <- doc_apprentissage[,1:54]
  
  
  #Préparation des données pour le test
  #Y2 <- doc_test$Target
  Y2 <- doc_test$Cover_Type
  X2 <- doc_test[,1:54]
  
  tree_model <- rpart(Cover_Type~., data = doc_apprentissage, method="class",control = rpart.control(cp = 0.00005)) # seuil sur le raffinage, force à aller voi run peutplus dans la compléxité pour l'arbre
  
  
  plot(tree_model)
  plotcp(tree_model)
  # Visualisation de l'arbre de décision
  #plot(tree_model, uniform = TRUE, branch = 0.5, margin = 0.2)
  #text(tree_model, all = FALSE, use.n = FALSE)
  # Évaluation de la précision
  y_pred <- predict(tree_model,doc_test, type='class')
  #y_pred
  accuracy_scores[b] <- sum(y_pred == Y2)/ length(Y2)
  #cat("Accuracy percentage:", accuracy, "\n")
  print(accuracy_scores)
  
}
confusion3 = table(Y2,y_pred,dnn=list("Observed","Predicted"))

mean_accuracy <- mean(accuracy_scores)
cat("Moyenne de l'exactitude sur", b, "itérations de Monte Carlo Cross-Validation:", mean_accuracy, "\n")






###FORET  ALEATOIRE
N=1 #nombre d'itérations
doc_shuffled <- doc[sample(nrow(doc)), ] #Mélange aléatoire
accuracy_scores <- numeric(N)

for (b in 1:N) { #550 répétitions du modèle 
  set.seed(b)
  doc_shuffled <- doc[sample(nrow(doc)), ] #Mélange aléatoire
  doc_apprentissage <- doc_shuffled[1:400000, ] #Apprentissage
  doc_test <- doc_shuffled[400001:581011, ]  #Test
  
  
  # Préparation des données pour l'apprentissage
  #Y <- doc_apprentissage$Target
  Y <- doc_apprentissage$Cover_Type
  X <- doc_apprentissage[,1:54]
  
  # library(caret)
  #Préparation des données pour le test
  #Y2 <- doc_test$Target
  Y2 <- doc_test$Cover_Type
  X2 <- doc_test[,1:54]
  
  
  rf <-randomForest(Cover_Type~.,data=doc_apprentissage,ntree=20,mtry=20,control=rpart.control(maxdepth=5, minsplit=15))

  #tree_model
  #plot(tree_model)
  #plotcp(tree_model)
  # Visualisation de l'arbre de décision
  #plot(tree_model, uniform = TRUE, branch = 0.5, margin = 0.2)
  #text(tree_model, all = FALSE, use.n = FALSE)
  # Évaluation de la précision
  y_pred <- predict(rf,doc_test, type='class')
  #y_pred  n 
  accuracy_scores[b] <- sum(y_pred == Y2)/ length(Y2)
  #cat("Accuracy percentage:", accuracy, "\n")
  print(accuracy_scores)
  
}

confusion4 = table(Y2,y_pred,dnn=list("Observed","Predicted"))

mean_accuracy <- mean(accuracy_scores)
cat("Moyenne de l'exactitude sur", b, "itérations de Monte Carlo Cross-Validation:", mean_accuracy, "\n")

##Trouver quelle variable est la plus importante
plot(rf$importance)
rf$importance
print("The most importante variables for prediction are")
which(rf$importance>1)

#Pour regarder OOB
print(rf)
plot(rf$err.rate[, 1], type = "l", xlab = "nombre d'arbres", ylab = "erreur OOB")











###ANCIEN CODE YOUNA


doc_shuffled <- doc[sample(nrow(doc)), ] #Mélange aléatoire
accuracy_scores <- numeric(550)


for (b in 1:550) { #550 répétitions du modèle 
  set.seed(b)
  itrain <- c()
  for (i in 1:7) {
    itrain <- c(itrain,sample(x = which(doc$Cover_Type == i),size = 1000))
  }
  for (i in unique(doc$Soil_type)) {
    itrain <- c(itrain,sample(x = which(doc$Soil_type == i ),size = 3)) #forcer pour avoir au moins 3 données de chaque
    #au total on a 7120 données d'entrainement
  }
  itest <- setdiff(1:581012,itrain)
  itest <- sample(itest,2380)
  
  doc_test <-doc[itest,]
  doc_train <-doc[itrain,]
  print(doc_train)
  
  X_doc_test <-doc_test[,1:54]
  X_doc_train <-doc_train[,1:54]
  
  Y_doc_test<-doc_test$Cover_Type
  Y_doc_train <-doc_train$Cover_Type
  
  
  tree_model <- rpart(Cover_Type~., data = doc_train, method="class",control = rpart.control(cp = 0.00005)) # seuil sur le raffinage, force à aller voi run peutplus dans la compléxité pour l'arbre
  
  #tree_model
  #plot(tree_model)
  #plotcp(tree_model)
  # Visualisation de l'arbre de décision
  #plot(tree_model, uniform = TRUE, branch = 0.5, margin = 0.2)
  #text(tree_model, all = FALSE, use.n = FALSE)
  # Évaluation de la précision
  y_pred <- predict(tree_model,doc_test, type='class')
  #y_pred
  accuracy_scores[b] <- sum(y_pred == Y_doc_train)/ length(Y_doc_train)
  #cat("Accuracy percentage:", accuracy, "\n")
  print(accuracy_scores)
}
#Matrice de confusion pour le dernier arbre
confusion1 = table(Y_doc_train,y_pred,dnn=list("Observed","Predicted"))


mean_accuracy <- mean(accuracy_scores)
cat("Moyenne de l'exactitude sur", b, "itérations de Monte Carlo Cross-Validation:", mean_accuracy, "\n")

