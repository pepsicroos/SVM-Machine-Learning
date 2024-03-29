library(e1071)
library(scales)
library(dplyr)
library(viridis)

#caso estudio SVM
###### Preparacion data set, separacion de dataset en 2 grupos #########
setwd("C:/rafa/Clasificacion int datos/archive (5)")
getwd()
datatoAnalizedata=read.csv("penguins_size Limpieza.csv")
datatoAnalize=datatoAnalizedata[c("culmen_length_mm","culmen_depth_mm","species")]

datatoAnalize$species=factor(datatoAnalize$species) ##Convertir columna a etiqueta
datatoAnalize
set.seed(1234)
indexSet<-sample(2,nrow(datatoAnalize),replace = T, prob = c(0.7,0.3))
train<-datatoAnalize[indexSet==1,]
test<- datatoAnalize[indexSet==2,]


train
test

##### #plot modelo SVM ######


svmfit = svm(species ~ ., data = train, kernel = "linear", cost = 10, scale = FALSE, type = "C")
print(svmfit)
plot(svmfit, train)

#######implementacion de modelo para prediccion y obtencion de matriz de confusion#####
test_pred <- predict(svmfit, newdata = test)
test_pred
library('caret')

library(ggplot2)

confusionMatrix(table(test_pred, test$species))


library(gmodels)
confusion_matrix <- CrossTable(test$species, test_pred, prop.chisq = FALSE, 
                               prop.t = FALSE, prop.r = FALSE)
    
    
    
    

    
    
    
    
    
    
    
    
    
