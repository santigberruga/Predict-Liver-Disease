#################################
##                             ##
##    Preparación datos TFM    ##
##                             ##
##  Santiago González Berruga  ##
##                             ##
#################################


################################################################################################

################
#   Cargamos   #
#  los datos   #
################

# Cargamos los datos
ILPD <- read.csv("data/ILPD.csv", header=FALSE)


################################################################################################

################
#   Explorar   #
#      y       #
#   Preparar   #
#     los      #
#    datos     #
################


# Nombramos correctamente las columnas del conjunto de datos
variables <- c("Age","Gender","TB","DB","Alkphos","Sgpt","Sgot","TP","ALB","AG","Class")
colnames(ILPD) <-  variables

# Dimensiones
dim(ILPD)

# Valores NA:
table(is.na(ILPD))

# Primeros 6 registros
# head(ILPD)
knitr::kable(head(ILPD), align = "l",
             caption= "Primeros 6 registros del conjunto de datos ILPD.")


################
#    Tipos     #
#      de      #
#   Variables  #
################

# Estructura del conjunto de datos:
str(ILPD)

# Variable class es un factor con dos niveles:
ILPD$Class <- factor(ILPD$Class, levels = c(1,2), labels = c("LD","H"))

# Variable class es un factor con dos niveles:
ILPD$Gender <- factor(ILPD$Gender, levels = c("Female","Male"))

# Estructura del conjunto de datos:
str(ILPD)




#################
#    Número     #
#      de       #
# Observaciones #
#       y       #
#    Valores    #
#    Ausentes   #
#################

# Cantiadad de muestras de cada clase
knitr::kable(t(table(ILPD$Class)), align = "c",
             caption= "Cantidad de muestras de cada clase en el conjunto de datos ILPD.")

# Porcentaje de muestras de cada clase
knitr::kable(t(round(prop.table(table(ILPD$Class)),2)), align = "c",
             caption= "Poncentaje de muestras de cada clase en el conjunto de datos ILPD.")

# Cantiadad de muestras da cada género
knitr::kable(t(table(ILPD$Gender)), align = "c",
             caption= "Cantidad de muestras de cada género en el conjunto de datos ILPD.")


## Vemos que variables presentan valores NA
# apply(ILPD, 2, function(x){sum(is.na(x))})
knitr::kable(t(apply(ILPD, 2, function(x){sum(is.na(x))})), align = "l",
             caption= "Distribución de los valores NA en conjunto de datos ILPD.")


# Guardamos el conjunto de datos como objeto de R
save(ILPD, file = "data/ILPD.RData")



#################
#################
# Distribución  #
#    variable   #
#   respuesta   #
#################
#################

## Gráfico de barras:
library(ggplot2)

ggplot(data = ILPD, aes(x = Class, y = ..count.., fill = Class)) +
  geom_bar() +
  geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white") +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  labs(title = "Enfermedad hepática") +
  theme_bw() +
  theme(legend.position = "right")


## Porcentaje de aciertos si se predice para todas las observaciones que padecen LD.
n_observaciones <- nrow(ILPD)
predicciones <- rep(x = "LD",  n_observaciones)
mean(predicciones == ILPD$Class) * 100 # porcentaje mínimo que hay que intentar superar con los modelos predictivos



#################
#################
# Distribución  #
#   variables   #
#   continuas   #
#################
#################

# Eliminamos los registros con NAs 
data4 <- ILPD[complete.cases(ILPD),]

# Guardamos el nuevo conjunto de datos como csv:
write.csv(data4, file = "data/data4.csv")

# Guardamos el conjunto de datos como objeto de R
save(data4, file = "data/data4.RData")

# Primeros 6 registros
knitr::kable(head(data4), align = "l",
             caption= "Primeros 6 registros del conjunto data4.")

# Cantidad de muestras de cada clase
knitr::kable(t(table(data4$Class)), align = "c",
             caption= "Cantidad de muestras de cada clase en el conjunto de datos data4.")

# Proporción de muestras de cada clase
knitr::kable(t(round(prop.table(table(data4$Class)),2)), align = "c",
             caption= "Poncentaje de muestras de cada clase en el conjunto de datos data4.")

# Cantiadad de muestras da cada género
knitr::kable(t(table(data4$Gender)), align = "c",
             caption= "Cantidad de muestras de cada género en el conjunto de datos data4.")


# Resumen estadístico variables numéricas
stats <- data.frame()
num_index <- as.vector(which(sapply(data4, is.numeric)==TRUE))
for (i in num_index) {
  stat <- summary(data4[[i]])
  stats <- rbind(stats,stat)
}
colnames(stats) <- names(stat)
rownames(stats) <- colnames(data4[num_index])

knitr::kable(stats, digits = 2, align = "l",
             caption= "Estadísticas descriptivas de las variables numéricas")



# Boxplots
boxplot(ILPD[num_index], col="lightblue")


# Graficos de dispersión entre variables:
pairs(ILPD[num_index], col=ILPD$Class, pch=20, oma=c(3,3,3,12))
par(xpd = TRUE)
legend("topright", pch=16, c("LD","H"), col=1:2, cex=0.8)



# Histogramas 
par(mfrow=c(3,3))

for (i in num_index) {
  hist(ILPD[[i]], 
       xlab = colnames(ILPD[i]),
       ylab = "Frecuencia",
       main = colnames(ILPD[i]))
}

par(mfrow=c(1,1))







# Gráfico de densidad y boxplot en función de la clase de variables contínuas
library(ggplot2)
library(ggpubr)



#################
#   Variable    #
#      Age      #
#################

## Variable Age
p1 <- ggplot(data = ILPD, aes(x = Age, fill = Class)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_rug(aes(color = Class), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  coord_flip() +
  theme_bw()
p2 <- ggplot(data = ILPD, aes(x = Class, y = Age, color = Class)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "top")
final_plot <- annotate_figure(final_plot, top = text_grob("Age", size = 15))
final_plot

## Resumen estadístico de la variable age por clase
library(dplyr)

stats <- ILPD %>% group_by(Class) %>%
  summarise(media = mean(Age),
            mediana = median(Age),
            min = min(Age),
            max = max(Age))

knitr::kable(stats, digits = 2, align = "l",
             caption= "Estadísticas descriptivas de la variable Age por clase.")





#################
#   Variable    #
#      TB       #
#################

## Variable log(TB)
p1 <- ggplot(data = ILPD, aes(x = log(TB), fill = Class)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_rug(aes(color = Class), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  coord_flip() +
  theme_bw()
p2 <- ggplot(data = ILPD, aes(x = Class, y = log(TB), color = Class)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "top")
final_plot <- annotate_figure(final_plot, top = text_grob("log(TB)", size = 15))
final_plot


## Resumen estadístico de la variable TB por clase
library(dplyr)

stats <- ILPD %>% group_by(Class) %>%
  summarise(media = mean(TB),
            mediana = median(TB),
            min = min(TB),
            max = max(TB))

knitr::kable(stats, digits = 2, align = "l",
             caption= "Estadísticas descriptivas de la variable TB por clase.")



#################
#   Variable    #
#      DB       #
#################

## Variable log(DB)
p1 <- ggplot(data = ILPD, aes(x = log(DB), fill = Class)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_rug(aes(color = Class), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  coord_flip() +
  theme_bw()
p2 <- ggplot(data = ILPD, aes(x = Class, y = log(DB), color = Class)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "top")
final_plot <- annotate_figure(final_plot, top = text_grob("log(DB)", size = 15))
final_plot

## Resumen estadístico de la variable DB por clase
library(dplyr)

stats <- ILPD %>% group_by(Class) %>%
  summarise(media = mean(DB),
            mediana = median(DB),
            min = min(DB),
            max = max(DB))

knitr::kable(stats, digits = 2, align = "l",
             caption= "Estadísticas descriptivas de la variable DB por clase.")



#################
#   Variable    #
#    Alkphos    #
#################

## Variable log(Alkphos)
p1 <- ggplot(data = ILPD, aes(x = log(Alkphos), fill = Class)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_rug(aes(color = Class), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  coord_flip() +
  theme_bw()
p2 <- ggplot(data = ILPD, aes(x = Class, y = log(Alkphos), color = Class)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "top")
final_plot <- annotate_figure(final_plot, top = text_grob("log(Alkphos)", size = 15))
final_plot


## Resumen estadístico de la variable Alkphos por clase
library(dplyr)

stats <- ILPD %>% group_by(Class) %>%
  summarise(media = mean(Alkphos),
            mediana = median(Alkphos),
            min = min(Alkphos),
            max = max(Alkphos))

knitr::kable(stats, digits = 2, align = "l",
             caption= "Estadísticas descriptivas de la variable Alkphos por clase.")



#################
#   Variable    #
#     Sgpt      #
#################

## Variable log(Sgpt)
p1 <- ggplot(data = ILPD, aes(x = log(Sgpt), fill = Class)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_rug(aes(color = Class), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  coord_flip() +
  theme_bw()
p2 <- ggplot(data = ILPD, aes(x = Class, y = log(Sgpt), color = Class)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "top")
final_plot <- annotate_figure(final_plot, top = text_grob("log(Sgpt)", size = 15))
final_plot


## Resumen estadístico de la variable Sgpt por clase
library(dplyr)

stats <- ILPD %>% group_by(Class) %>%
  summarise(media = mean(Sgpt),
            mediana = median(Sgpt),
            min = min(Sgpt),
            max = max(Sgpt))

knitr::kable(stats, digits = 2, align = "l",
             caption= "Estadísticas descriptivas de la variable Sgpt por clase.")




#################
#   Variable    #
#     Sgot      #
#################


## Variable log(Sgot)
p1 <- ggplot(data = ILPD, aes(x = log(Sgot), fill = Class)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_rug(aes(color = Class), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  coord_flip() +
  theme_bw()
p2 <- ggplot(data = ILPD, aes(x = Class, y = log(Sgot), color = Class)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "top")
final_plot <- annotate_figure(final_plot, top = text_grob("log(Sgot)", size = 15))
final_plot


## Resumen estadístico de la variable Sgot por clase
library(dplyr)

stats <- ILPD %>% group_by(Class) %>%
  summarise(media = mean(Sgot),
            mediana = median(Sgot),
            min = min(Sgot),
            max = max(Sgot))

knitr::kable(stats, digits = 2, align = "l",
             caption= "Estadísticas descriptivas de la variable Sgot por clase.")




#################
#   Variable    #
#      TP       #
#################


## Variable TP
p1 <- ggplot(data = ILPD, aes(x = TP, fill = Class)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_rug(aes(color = Class), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  coord_flip() +
  theme_bw()
p2 <- ggplot(data = ILPD, aes(x = Class, y = TP, color = Class)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "top")
final_plot <- annotate_figure(final_plot, top = text_grob("TP", size = 15))
final_plot


## Resumen estadístico de la variable TP por clase
library(dplyr)

stats <- ILPD %>% group_by(Class) %>%
  summarise(media = mean(TP),
            mediana = median(TP),
            min = min(TP),
            max = max(TP))

knitr::kable(stats, digits = 2, align = "l",
             caption= "Estadísticas descriptivas de la variable TP por clase.")



#################
#   Variable    #
#      ALB      #
#################

## Variable ALB
p1 <- ggplot(data = ILPD, aes(x = ALB, fill = Class)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_rug(aes(color = Class), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  coord_flip() +
  theme_bw()
p2 <- ggplot(data = ILPD, aes(x = Class, y = ALB, color = Class)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "top")
final_plot <- annotate_figure(final_plot, top = text_grob("ALB", size = 15))
final_plot


## Resumen estadístico de la variable ALB por clase
library(dplyr)

stats <- ILPD %>% group_by(Class) %>%
  summarise(media = mean(ALB),
            mediana = median(ALB),
            min = min(ALB),
            max = max(ALB))

knitr::kable(stats, digits = 2, align = "l",
             caption= "Estadísticas descriptivas de la variable ALB por clase.")


#################
#   Variable    #
#      AG       #
#################

## Variable AG
p1 <- ggplot(data = ILPD, aes(x = AG, fill = Class)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_rug(aes(color = Class), alpha = 0.5) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  coord_flip() +
  theme_bw()
p2 <- ggplot(data = ILPD, aes(x = Class, y = AG, color = Class)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(alpha = 0.3, width = 0.15) +
  scale_color_manual(values = c("gray50", "orangered2")) +
  theme_bw()
final_plot <- ggarrange(p1, p2, legend = "top")
final_plot <- annotate_figure(final_plot, top = text_grob("AG", size = 15))
final_plot


## Resumen estadístico de la variable AG por clase
library(dplyr)

stats <- data4 %>% group_by(Class) %>%
  summarise(media = mean(AG),
            mediana = median(AG),
            min = min(AG),
            max = max(AG))

knitr::kable(stats, digits = 2, align = "l",
             caption= "Estadísticas descriptivas de la variable AG por clase.")





# Resumen estadístico variables numéricas por clase
stats <- data.frame()
num_index <- as.vector(which(sapply(data4, is.numeric)==TRUE))
for (i in num_index) {
  stat <- tapply(data4[[i]], data4$Class, summary)
  stats <- rbind(stats, unlist(stat[1]), unlist(stat[2]))
}
colnames(stats) <- names(stat[[1]])
a <- rep(colnames(data4[num_index]), times = 1, each = 2)
b <- rep(c("LH", "H"), 9)
rownames(stats) <- paste(a,b)

knitr::kable(stats, digits = 2, align = "l",
             caption= "Estadísticas descriptivas de las variables numéricas por clase.")





#################
#################
# Distribución  #
#   variables   #
#  cualitativas #
#################
#################


#################
#   Variable    #
#    Gender     #
#################

# Gráfico de barras variable Gender por clase
ggplot(data = ILPD, aes(x = Gender, y = ..count.., fill = Class)) +
  geom_bar() +
  labs(title = "Gender") +
  scale_fill_manual(values = c("gray50", "orangered2")) +
  geom_text(aes(label = ..count..), stat = "count", vjust = 2, colour = "white") +
  theme_bw() +
  theme(legend.position = "bottom")


# Proporción de muestras de cada clase por género
tabla.gender <- prop.table(table(ILPD$Gender, ILPD$Class), margin = 1) %>% round(digits = 2)

knitr::kable(tabla.gender, align = "c",
             caption= "Poncentaje de muestras de cada clase por género en el conjunto de datos ILPD.")




#################
#################
#  Importancia  #
#    de las     #
#   variables   #
#################
#################


#################
#  Correlación  #
#     entre     #
#   variables   #   
#   continuas   #
#################


# Matriz de correlación (correlación de Pearson)
library(ggcorrplot)
ggcorrplot(cor(data4[num_index]), method="square",
           type="lower", lab=TRUE) + 
  ggtitle("Correlograma del conjunto ILPD") + 
  theme_minimal()

cor_pmat(data4[num_index]) # p-valor Pearson

# Gráfico de disperión y matriz de correlación 
library(GGally)
ggpairs(data4[num_index])

# Test de Pearson 
cor.test(data4$DB, data4$TB)

cor.test(data4$Sgpt, data4$Sgot)

cor.test(data4$ALB, data4$TP)

cor.test(data4$AG, data4$ALB)

# En base a estos resultados excluiremos las variables DB, Sgot y ALB en los modelos.



#################
#   Varianza    #
#   variables   #   
#   continuas   #
#################


# Varianza de las variables numéricas
round(apply(data4[num_index], 2, var),2)

library(caret)
nearZeroVar(data4[num_index], saveMetrics = TRUE)




#################
#  Importancia  #
#    de las     #
#   variables   #
#      rf       #
#################


## Importancia de las variables con Random Forest:
library(randomForest)
library(tibble)

modelo_randforest <- randomForest(formula = Class ~ . ,
                                  data = data4,
                                  mtry = 5,
                                  importance = TRUE, 
                                  ntree = 1000) 

# The nclass + 1st column is the mean descrease in accuracy over 
# all classes. The last column is the mean decrease in Gini index.
importancia <- as.data.frame(modelo_randforest$importance)
importancia <- rownames_to_column(importancia,var = "variable")

library(ggpubr)

p1 <- ggplot(data = importancia, aes(x = reorder(variable, MeanDecreaseAccuracy),
                                     y = MeanDecreaseAccuracy,
                                     fill = MeanDecreaseAccuracy)) +
  labs(x = "variable", title = "Reducción de Accuracy") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

p2 <- ggplot(data = importancia, aes(x = reorder(variable, MeanDecreaseGini),
                                     y = MeanDecreaseGini,
                                     fill = MeanDecreaseGini)) +
  labs(x = "variable", title = "Reducción de pureza (Gini)") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
ggarrange(p1, p2)




######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################
######################################################################################



#################
#   División    #
#    train      #
#    test       #
#################

# Excluimos los registros con NAs
ILPD_less <- ILPD[complete.cases(ILPD), ]

# Partición de los datos train/test
library(caret)
set.seed(123)
## Muestras que pertenecen al conjunto train
in_train <- createDataPartition(ILPD_less$Class,
                                p = 0.7, list = FALSE)

## Creamos los conjuntos train y test:
data_train <- ILPD_less[in_train,]
data_test <- ILPD_less[-in_train,]

## Variable objetivo por conjunto de datos
train_labels <- ILPD_less[in_train,length(ILPD_less)]
test_labels <- ILPD_less[-in_train,length(ILPD_less)]


# Confirmamos que los subconjuntos son representativos del
# conjunto completo de datos
prob_train <- prop.table(table(train_labels))
prob_test <- prop.table(table(test_labels))

prob <- as.data.frame(rbind(prob_train,prob_test))
row.names(prob) <- c("% Train", "% Test")

knitr::kable(prob, align = "c", digits = 2,
             caption = "Porcentaje de cada tipo de localización en los conjuntos train y test.")




#################
#  Preprocesar  #
#     datos     #
#################

data_train_prep <- data_train
data_test_prep  <- data_test


#################
#  Varianza 0   #
#   eliminar    #
#################

## Se crea un objeto recipe() con la variable respuesta y los predictores. 
library(recipes)
objeto_recipe <- recipe(formula = Class ~ .,
                        data =  data_train_prep)
objeto_recipe

# Eliminamos variables con varianza 0 o próxima
# objeto_recipe <- objeto_recipe %>% step_nzv(all_numeric_predictors())
objeto_recipe <- objeto_recipe %>% step_nzv(all_predictors())
objeto_recipe


#################
#  Normalizar   #
#     datos     #
#################


## Normalización de los datos
objeto_recipe <- objeto_recipe %>% step_range(all_numeric_predictors())
objeto_recipe



#################
#   Binarizar   #
#     datos     #
#################


# Binarizamos las variables cualitativas
objeto_recipe <- objeto_recipe %>% step_dummy(all_nominal(), - all_outcomes())
objeto_recipe



#################
#   Aplicamos   #
#    cambios    #
#################

## Entrenar el objeto recipe
trained_recipe <- prep(objeto_recipe, training = data_train_prep)
trained_recipe


## Se aplican las transformaciones al conjunto de entrenamiento y de test
data_train_prep <- bake(trained_recipe, new_data = data_train_prep)
data_test_prep  <- bake(trained_recipe, new_data = data_test_prep)




#################
#   Selección   #
#  predictores  #
#################


#################
#  Eliminación  #
#   recursiva   #
#       RF      #
#################

# Eliminación recursiva mediante Random Forest y Bootstraping

# Tamaño de los conjuntos de predictores analizados
subsets <- c(1:10)

# Número de resamples para el proceso de bootstrapping
repeticiones <- 30

# Se crea una semilla para cada repetición de validación. 
set.seed(123)
seeds <- vector(mode = "list", length = repeticiones + 1)
for (i in 1:repeticiones) {
  seeds[[i]] <- sample.int(1000, length(subsets))
}
seeds[[repeticiones + 1]] <- sample.int(1000, 1)

# Se crea un control de entrenamiento:
ctrl_rfe <- rfeControl(functions = rfFuncs, method = "boot", number = repeticiones,
                       returnResamp = "all", allowParallel = TRUE, verbose = FALSE,
                       seeds = seeds)

# Se ejecuta la eliminación recursiva de predictores
set.seed(342)
rf_rfe <- rfe(Class ~ ., data = data_train_prep,
              sizes = subsets,
              metric = "Accuracy",
              # El accuracy es la proporción de clasificaciones correctas
              rfeControl = ctrl_rfe,
              ntree = 500)

# Se muestra una tabla resumen con los resultados
rf_rfe

# El objeto rf_rfe almacena en optVariables las variables del mejor modelo.
rf_rfe$optVariables


# Evolución del accuracy estimado en función del número de predictores incluido en el modelo.
ggplot(data = rf_rfe$results, aes(x = Variables, y = Accuracy)) +
  geom_line() +
  scale_x_continuous(breaks  = unique(rf_rfe$results$Variables)) +
  geom_point() +
  geom_errorbar(aes(ymin = Accuracy - AccuracySD, ymax = Accuracy + AccuracySD),
                width = 0.2) +
  geom_point(data = rf_rfe$results %>% slice(which.max(Accuracy)),
             color = "red") +
  theme_bw()


# Guardamos en una variable los predictores filtrados para emplearlos posteriormente en el ajuste de los modelos.
predictores_filtrados_rf <- as.formula(paste("Class", paste(rf_rfe$optVariables, collapse=" + "), sep=" ~ "))



#################
#  Eliminación  #
#   recursiva   #
#  A. Genetico  #
#################

# Control de entrenamiento
ga_ctrl <- gafsControl(functions = rfGA,
                       method = "repeatedcv",
                       number = 5,
                       allowParallel = TRUE,
                       genParallel = TRUE,
                       verbose = FALSE)

# Selección de predictores
set.seed(10)
rf_ga <- gafs(x = data_train_prep %>% select(-Class),
              y = data_train_prep$Class,
              iters = 10,
              popSize = 50,
              gafsControl = ga_ctrl,
              ntree = 100)

# Se muestra una tabla resumen con los resultados
rf_ga

# El objeto rf_ga almacena en optVariables las variables del mejor modelo.
rf_ga$optVariables


# Guardamos en una variable los predictores filtrados para emplearlos posteriormente en el ajuste de los modelos.
predictores_filtrados_ag <- as.formula(paste("Class", paste(rf_ga$optVariables, collapse=" + "), sep=" ~ "))




#################
#   Método de   #
#   Filtrado    #
#################


# FILTRADO DE PREDICTORES MEDIANTE ANOVA, RANDOM FOREST Y CV-REPETIDA

# Se crea una semilla para cada partición y cada repetición: el vector debe
# tener B+1 semillas donde B = particiones * repeticiones.
particiones = 10
repeticiones = 5
set.seed(123)
seeds <- sample.int(1000, particiones * repeticiones + 1)

# Control del filtrado
ctrl_filtrado <- sbfControl(functions = rfSBF, method = "repeatedcv",
                            number = particiones, repeats = repeticiones,
                            seeds = seeds, verbose = FALSE,
                            saveDetails = TRUE, allowParallel = TRUE)
set.seed(234)
rf_sbf <- sbf(Class ~ ., data = data_train_prep,
              sbfControl = ctrl_filtrado,
              # argumentos para el modelo de evaluación
              ntree = 500)

# Se muestra una tabla resumen con los resultados
rf_sbf

# El objeto rf_sbf almacena en optVariables las variables del mejor modelo.
rf_sbf$optVariables


# Guardamos en una variable los predictores filtrados para emplearlos posteriormente en el ajuste de los modelos.
predictores_filtrados_rfSBF <- as.formula(paste("Class", paste(rf_sbf$optVariables, collapse=" + "), sep=" ~ "))





################################################################################################
################################################################################################
################################################################################################



#################
#   Creación    #
#    modelos    #
#  Predictivos  #
#################


# Ajuste de diferentes modelos:(en los siguientes apartados)
library(caret)



################################################################################################

################
#              #
#     KNN      #
#              #
################

# se recurre a validación cruzada repetida como método de validación.
# Número de particiones y repeticiones
particiones  <- 10
repeticiones <- 5

hiperparametros <- data.frame(k = c(5,10,15,20))

set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros)) 
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)


# DEFINICIÓN DEL ENTRENAMIENTO
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
set.seed(342)
modelo_knn <- train(Class ~ ., data = data_train_prep,
                    method = "knn",
                    tuneGrid = hiperparametros,
                    metric = "Accuracy",
                    trControl = control_train)

modelo_knn


# REPRESENTACIÓN GRÁFICA
library(ggplot2)
ggplot(modelo_knn, highlight = TRUE) +
  scale_x_continuous(breaks = hiperparametros$k) +
  labs(title = "Evolución del accuracy del modelo KNN", x = "K") +
  theme_bw()


# Predicciones
predicciones_knn <- predict(modelo_knn, newdata = data_test_prep,
                            type = "raw")
predicciones_knn


# Evaluación
conf_mat_knn <- confusionMatrix(data = predicciones_knn, reference = data_test_prep$Class,
                                positive = "LD")
conf_mat_knn


stats_class_knn <- data.frame(model         = "KNN",
                              accuracy      = conf_mat_knn$overall[[1]],
                              FN            = conf_mat_knn$table[2,1],
                              FP            = conf_mat_knn$table[1,2],
                              error.rate    = 1-conf_mat_knn$overall[[1]],
                              kappa         = conf_mat_knn$overall[[2]],
                              sensibilidad  = conf_mat_knn$byClass[[1]], 
                              especificidad = conf_mat_knn$byClass[[2]], 
                              precision     = conf_mat_knn$byClass[[5]], 
                              recall        = conf_mat_knn$byClass[[6]], 
                              f.measure     = conf_mat_knn$byClass[[7]])
stats_class_knn



################################################################################################

################
#              #
#      NB      #
#              #
################

# https://uc-r.github.io/naive_bayes#caret

# se recurre a validación cruzada repetida como método de validación.
# Número de particiones y repeticiones
particiones  <- 10
repeticiones <- 5

hiperparametros <- expand.grid(usekernel = c(TRUE, FALSE),
                               fL = c(0:5),
                               adjust = c(1:5))


set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros)) 
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)


# DEFINICIÓN DEL ENTRENAMIENTO
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
set.seed(342)
modelo_nb <- train(Class ~ ., data = data_train_prep,
                   method = "nb",
                   tuneGrid = hiperparametros,
                   metric = "Accuracy",
                   trControl = control_train)
modelo_nb


# REPRESENTACIÓN GRÁFICA
ggplot(modelo_nb, highlight = TRUE) +
  labs(title = "Evolución del accuracy del modelo NB") +
  theme_bw()


# Predicciones
predicciones_nb <- predict(modelo_nb, newdata = data_test_prep,
                           type = "raw")
predicciones_nb


# Evaluación
conf_mat_nb <- confusionMatrix(data = predicciones_nb, reference = data_test_prep$Class,
                               positive = "LD")
conf_mat_nb


stats_class_nb <- data.frame(model         = "NB",
                             accuracy      = conf_mat_nb$overall[[1]],
                             FN            = conf_mat_nb$table[2,1],
                             FP            = conf_mat_nb$table[1,2],
                             error.rate    = 1-conf_mat_nb$overall[[1]],
                             kappa         = conf_mat_nb$overall[[2]],
                             sensibilidad  = conf_mat_nb$byClass[[1]], 
                             especificidad = conf_mat_nb$byClass[[2]], 
                             precision     = conf_mat_nb$byClass[[5]], 
                             recall        = conf_mat_nb$byClass[[6]], 
                             f.measure     = conf_mat_nb$byClass[[7]])
stats_class_nb



################################################################################################

################
#              #
#     ANN      #
#              #
################


# se recurre a validación cruzada repetida como método de validación.
# Número de particiones y repeticiones
particiones  <- 10
repeticiones <- 5

hiperparametros <- expand.grid(size = c(10, 20, 40, 60, 80, 90, 100,120),
                               decay = c(0.0001))


set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros)) 
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)


# DEFINICIÓN DEL ENTRENAMIENTO
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
set.seed(342)
modelo_ann <- train(Class ~ ., data = data_train_prep,
                    method = "nnet",
                    tuneGrid = hiperparametros,
                    metric = "Accuracy",
                    trControl = control_train,
                    # Rango de inicialización de los pesos
                    rang = c(-0.7, 0.7),
                    # Número máximo de pesos
                    # se aumenta para poder incluir más meuronas
                    MaxNWts = 2000,
                    # Para que no se muestre cada iteración por pantalla
                    trace = FALSE)
modelo_ann


# REPRESENTACIÓN GRÁFICA
ggplot(modelo_ann, highlight = TRUE) +
  labs(title = "Evolución del accuracy del modelo ANN") +
  theme_bw()


# Predicciones
predicciones_ann <- predict(modelo_ann, newdata = data_test_prep,
                            type = "raw")
predicciones_ann


# Evaluación
conf_mat_ann <- confusionMatrix(data = predicciones_ann, reference = data_test_prep$Class,
                                positive = "LD")
conf_mat_ann


stats_class_ann <- data.frame(model         = "ANN",
                              accuracy      = conf_mat_ann$overall[[1]],
                              FN            = conf_mat_ann$table[2,1],
                              FP            = conf_mat_ann$table[1,2],
                              error.rate    = 1-conf_mat_ann$overall[[1]],
                              kappa         = conf_mat_ann$overall[[2]],
                              sensibilidad  = conf_mat_ann$byClass[[1]], 
                              especificidad = conf_mat_ann$byClass[[2]], 
                              precision     = conf_mat_ann$byClass[[5]], 
                              recall        = conf_mat_ann$byClass[[6]], 
                              f.measure     = conf_mat_ann$byClass[[7]])
stats_class_ann



################################################################################################

################
#              #
#     MLP      #
#              #
################


# Se recurre a validación cruzada repetida como método de validación.
# Número de particiones y repeticiones
particiones  <- 10
repeticiones <- 5

# hiperparametros <- expand.grid(size = c(1:20),
#                                decay = c(0.0001, 0.1, 0.5))

hiperparametros <- expand.grid(size = c(1:5),
                               decay = c(0.0001))


set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros)) 
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)


# DEFINICIÓN DEL ENTRENAMIENTO
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
set.seed(342)
modelo_mlp <- train(Class ~ ., data = data_train_prep,
                    method = "mlpWeightDecay",
                    tuneGrid = hiperparametros,
                    metric = "Accuracy",
                    trControl = control_train,
                    learnFunc = "Std_Backpropagation")
modelo_mlp


# REPRESENTACIÓN GRÁFICA
ggplot(modelo_mlp, highlight = TRUE) +
  labs(title = "Evolución del accuracy del modelo MLP") +
  theme_bw()


# Predicciones
predicciones_mlp <- predict(modelo_mlp, newdata = data_test_prep,
                            type = "raw")
predicciones_mlp


# Evaluación
conf_mat_mlp <- confusionMatrix(data = predicciones_mlp, reference = data_test_prep$Class,
                                positive = "LD")
conf_mat_mlp


stats_class_mlp <- data.frame(model         = "MLP",
                              accuracy      = conf_mat_mlp$overall[[1]],
                              FN            = conf_mat_mlp$table[2,1],
                              FP            = conf_mat_mlp$table[1,2],
                              error.rate    = 1-conf_mat_mlp$overall[[1]],
                              kappa         = conf_mat_mlp$overall[[2]],
                              sensibilidad  = conf_mat_mlp$byClass[[1]], 
                              especificidad = conf_mat_mlp$byClass[[2]], 
                              precision     = conf_mat_mlp$byClass[[5]], 
                              recall        = conf_mat_mlp$byClass[[6]], 
                              f.measure     = conf_mat_mlp$byClass[[7]])
stats_class_mlp



################################################################################################

################
#              #
#     SVM      #
#    lineal    #
#              #
################


# https://rpubs.com/uky994/593668

# https://learnrmanoharkapse.blogspot.com/2019/01/basic-of-r-session-21-support-vector.html


# se recurre a validación cruzada repetida como método de validación.
# Número de particiones y repeticiones
particiones  <- 10
repeticiones <- 5

hiperparametros <- data.frame(C = c(10,15,20,30,40,50))

set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros)) 
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)


# DEFINICIÓN DEL ENTRENAMIENTO
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)


# AJUSTE DEL MODELO
set.seed(342)
modelo_svmlineal <- train(Class ~ ., data = data_train_prep,
                          method = "svmLinear",
                          tuneGrid = hiperparametros,
                          metric = "Accuracy",
                          trControl = control_train)

modelo_svmlineal


# REPRESENTACIÓN GRÁFICA
ggplot(modelo_svmlineal, highlight = TRUE) +
  labs(title = "Evolución del accuracy del modelo SVMlineal") +
  theme_bw()


# Predicciones
predicciones_svmlineal <- predict(modelo_svmlineal, newdata = data_test_prep,
                                  type = "raw")
predicciones_svmlineal


# Evaluación
conf_mat_svmlineal <- confusionMatrix(data = predicciones_svmlineal, reference = data_test_prep$Class,
                                      positive = "LD")
conf_mat_svmlineal


stats_class_svmlineal <- data.frame(model         = "SVMlineal",
                                    accuracy      = conf_mat_svmlineal$overall[[1]],
                                    FN            = conf_mat_svmlineal$table[2,1],
                                    FP            = conf_mat_svmlineal$table[1,2],
                                    error.rate    = 1-conf_mat_svmlineal$overall[[1]],
                                    kappa         = conf_mat_svmlineal$overall[[2]],
                                    sensibilidad  = conf_mat_svmlineal$byClass[[1]], 
                                    especificidad = conf_mat_svmlineal$byClass[[2]], 
                                    precision     = conf_mat_svmlineal$byClass[[5]], 
                                    recall        = conf_mat_svmlineal$byClass[[6]], 
                                    f.measure     = conf_mat_svmlineal$byClass[[7]])
stats_class_svmlineal





################################################################################################

################
#              #
#     SVM      #
#     Poly     #
#              #
################

# se recurre a validación cruzada repetida como método de validación.
# Número de particiones y repeticiones
particiones  <- 10
repeticiones <- 5


hiperparametros <- expand.grid(degree = c(2),
                               scale = c(0.1,0.2,0.3,0.5),
                               C = c(10,15,20,30,40,50))


set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros)) 
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)


# DEFINICIÓN DEL ENTRENAMIENTO
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
set.seed(342)
modelo_svmPoly <- train(Class ~ ., data = data_train_prep,
                        method = "svmPoly",
                        tuneGrid = hiperparametros,
                        metric = "Accuracy",
                        trControl = control_train)

modelo_svmPoly



# REPRESENTACIÓN GRÁFICA
ggplot(modelo_svmPoly, highlight = TRUE) +
  labs(title = "Evolución del accuracy del modelo SVMpoly") +
  theme_bw()



# Predicciones
predicciones_svmPoly <- predict(modelo_svmPoly, newdata = data_test_prep,
                                type = "raw")
predicciones_svmPoly


# Evaluación
conf_mat_svmPoly <- confusionMatrix(data = predicciones_svmPoly, reference = data_test_prep$Class,
                                    positive = "LD")
conf_mat_svmPoly


stats_class_svmPoly <- data.frame(model         = "SVMpoly",
                                  accuracy      = conf_mat_svmPoly$overall[[1]],
                                  FN            = conf_mat_svmPoly$table[2,1],
                                  FP            = conf_mat_svmPoly$table[1,2],
                                  error.rate    = 1-conf_mat_svmPoly$overall[[1]],
                                  kappa         = conf_mat_svmPoly$overall[[2]],
                                  sensibilidad  = conf_mat_svmPoly$byClass[[1]], 
                                  especificidad = conf_mat_svmPoly$byClass[[2]], 
                                  precision     = conf_mat_svmPoly$byClass[[5]], 
                                  recall        = conf_mat_svmPoly$byClass[[6]], 
                                  f.measure     = conf_mat_svmPoly$byClass[[7]])
stats_class_svmPoly




################################################################################################

################
#              #
#     SVM      #
#    Radial    #
#              #
################

# se recurre a validación cruzada repetida como método de validación.
# Número de particiones y repeticiones
particiones  <- 10
repeticiones <- 5


hiperparametros <- expand.grid(sigma = c(0.1, 0.3, 0.5,1,3,5,10),
                               C = c(10,15,20,30,40,50))

set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros)) 
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)


# DEFINICIÓN DEL ENTRENAMIENTO
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
set.seed(342)
modelo_svmRadial <- train(Class ~ ., data = data_train_prep,
                          method = "svmRadial",
                          tuneGrid = hiperparametros,
                          metric = "Accuracy",
                          trControl = control_train)
modelo_svmRadial


# REPRESENTACIÓN GRÁFICA
ggplot(modelo_svmRadial, highlight = TRUE) +
  labs(title = "Evolución del accuracy del modelo SVMradial") +
  theme_bw()



# Predicciones
predicciones_svmRadial <- predict(modelo_svmRadial, newdata = data_test_prep,
                                  type = "raw")
predicciones_svmRadial


# Evaluación
conf_mat_svmRadial <- confusionMatrix(data = predicciones_svmRadial, reference = data_test_prep$Class,
                                      positive = "LD")
conf_mat_svmRadial


stats_class_svmRadial <- data.frame(model         = "SVMradial",
                                    accuracy      = conf_mat_svmRadial$overall[[1]],
                                    FN            = conf_mat_svmRadial$table[2,1],
                                    FP            = conf_mat_svmRadial$table[1,2],
                                    error.rate    = 1-conf_mat_svmRadial$overall[[1]],
                                    kappa         = conf_mat_svmRadial$overall[[2]],
                                    sensibilidad  = conf_mat_svmRadial$byClass[[1]], 
                                    especificidad = conf_mat_svmRadial$byClass[[2]], 
                                    precision     = conf_mat_svmRadial$byClass[[5]], 
                                    recall        = conf_mat_svmRadial$byClass[[6]], 
                                    f.measure     = conf_mat_svmRadial$byClass[[7]])
stats_class_svmRadial


################################################################################################

################
#              #
#      DT      #
#   C5.0Tree   #
#              #
################


# se recurre a validación cruzada repetida como método de validación.
# Número de particiones y repeticiones
particiones  <- 10
repeticiones <- 5

hiperparametros <- data.frame(parameter = "none")


set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros)) 
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)


# DEFINICIÓN DEL ENTRENAMIENTO
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
set.seed(342)
modelo_C5.0Tree <- train(Class ~ ., data = data_train_prep,
                         method = "C5.0Tree",
                         tuneGrid = hiperparametros,
                         metric = "Accuracy",
                         trControl = control_train)
modelo_C5.0Tree


summary(modelo_C5.0Tree$finalModel)


# Predicciones
predicciones_C5.0Tree <- predict(modelo_C5.0Tree, newdata = data_test_prep,
                                 type = "raw")
predicciones_C5.0Tree


# Evaluación
conf_mat_C5.0Tree <- confusionMatrix(data = predicciones_C5.0Tree, reference = data_test_prep$Class,
                                     positive = "LD")
conf_mat_C5.0Tree


stats_class_C5.0Tree <- data.frame(model         = "C5.0Tree",
                                   accuracy      = conf_mat_C5.0Tree$overall[[1]],
                                   FN            = conf_mat_C5.0Tree$table[2,1],
                                   FP            = conf_mat_C5.0Tree$table[1,2],
                                   error.rate    = 1-conf_mat_C5.0Tree$overall[[1]],
                                   kappa         = conf_mat_C5.0Tree$overall[[2]],
                                   sensibilidad  = conf_mat_C5.0Tree$byClass[[1]], 
                                   especificidad = conf_mat_C5.0Tree$byClass[[2]], 
                                   precision     = conf_mat_C5.0Tree$byClass[[5]], 
                                   recall        = conf_mat_C5.0Tree$byClass[[6]], 
                                   f.measure     = conf_mat_C5.0Tree$byClass[[7]])
stats_class_C5.0Tree



################################################################################################

################
#              #
#      DT      #
#     C5.0     #
#              #
################


# se recurre a validación cruzada repetida como método de validación.
# Número de particiones y repeticiones
particiones  <- 10
repeticiones <- 5


# hiperparametros <- expand.grid(trials = c(10:15),
#                                model = "tree",
#                                winnow = c(TRUE, FALSE))

hiperparametros <- expand.grid(trials = c(10:15),
                               model = "tree",
                               winnow = c(FALSE))

set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros)) 
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)


# DEFINICIÓN DEL ENTRENAMIENTO
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
set.seed(342)
modelo_C5.0 <- train(Class ~ ., data = data_train_prep,
                     method = "C5.0",
                     tuneGrid = hiperparametros,
                     metric = "Accuracy",
                     trControl = control_train)
modelo_C5.0


summary(modelo_C5.0$finalModel)


# REPRESENTACIÓN GRÁFICA
ggplot(modelo_C5.0, highlight = TRUE) +
  labs(title = "Evolución del accuracy del modelo C5.0") +
  theme_bw()


# Predicciones
predicciones_C5.0 <- predict(modelo_C5.0, newdata = data_test_prep,
                             type = "raw")
predicciones_C5.0


# Evaluación
conf_mat_C5.0 <- confusionMatrix(data = predicciones_C5.0, reference = data_test_prep$Class,
                                 positive = "LD")
conf_mat_C5.0


stats_class_C5.0 <- data.frame(model         = "C5.0",
                               accuracy      = conf_mat_C5.0$overall[[1]],
                               FN            = conf_mat_C5.0$table[2,1],
                               FP            = conf_mat_C5.0$table[1,2],
                               error.rate    = 1-conf_mat_C5.0$overall[[1]],
                               kappa         = conf_mat_C5.0$overall[[2]],
                               sensibilidad  = conf_mat_C5.0$byClass[[1]], 
                               especificidad = conf_mat_C5.0$byClass[[2]], 
                               precision     = conf_mat_C5.0$byClass[[5]], 
                               recall        = conf_mat_C5.0$byClass[[6]], 
                               f.measure     = conf_mat_C5.0$byClass[[7]])
stats_class_C5.0




################################################################################################

################
#              #
#      DT      #
#     J48      #
#              #
################


# se recurre a validación cruzada repetida como método de validación.
# Número de particiones y repeticiones
particiones  <- 10
repeticiones <- 5

hiperparametros <- expand.grid(C = c(0.5),
                               M = c(2:10))

set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros))
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)


# DEFINICIÓN DEL ENTRENAMIENTO
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
library(RWeka)
set.seed(342)
modelo_J48 <- train(Class ~ ., data = data_train_prep,
                    method = "J48",
                    tuneGrid = hiperparametros,
                    metric = "Accuracy",
                    trControl = control_train)
modelo_J48


summary(modelo_J48$finalModel)


# REPRESENTACIÓN GRÁFICA
ggplot(modelo_J48, highlight = TRUE) +
  labs(title = "Evolución del accuracy del modelo J48") +
  theme_bw()


# Predicciones
predicciones_J48 <- predict(modelo_J48, newdata = data_test_prep,
                            type = "raw")
predicciones_J48


# Evaluación
conf_mat_J48 <- confusionMatrix(data = predicciones_J48, reference = data_test_prep$Class,
                                positive = "LD")
conf_mat_J48


stats_class_J48 <- data.frame(model         = "J48",
                              accuracy      = conf_mat_J48$overall[[1]],
                              FN            = conf_mat_J48$table[2,1],
                              FP            = conf_mat_J48$table[1,2],
                              error.rate    = 1-conf_mat_J48$overall[[1]],
                              kappa         = conf_mat_J48$overall[[2]],
                              sensibilidad  = conf_mat_J48$byClass[[1]], 
                              especificidad = conf_mat_J48$byClass[[2]], 
                              precision     = conf_mat_J48$byClass[[5]], 
                              recall        = conf_mat_J48$byClass[[6]], 
                              f.measure     = conf_mat_J48$byClass[[7]])
stats_class_J48






################################################################################################

################
#              #
#      RF      #
#              #
################


# library(doParallel)  
# no_cores <- detectCores()-2  
# registerDoParallel(cores=no_cores)

# se recurre a validación cruzada repetida como método de validación.
# Número de particiones y repeticiones
particiones  <- 10
repeticiones <- 5


hiperparametros <- expand.grid(mtry = c(1:10),
                               min.node.size = c(4, 5, 10),
                               splitrule = c("gini"))


set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros)) 
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)


# DEFINICIÓN DEL ENTRENAMIENTO
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO 
set.seed(342)
modelo_rf <- train(Class ~ ., data = data_train_prep,
                   method = "ranger",
                   tuneGrid = hiperparametros,
                   metric = "Accuracy",
                   trControl = control_train,
                   # Número de árboles ajustados
                   num.trees = 500)
modelo_rf

summary(modelo_rf$finalModel)

# REPRESENTACIÓN GRÁFICA
ggplot(modelo_rf, highlight = TRUE) +
  labs(title = "Evolución del accuracy del modelo RF") +
  theme_bw()


# Predicciones
predicciones_rf <- predict(modelo_rf, newdata = data_test_prep,
                           type = "raw")
predicciones_rf


# Evaluación
conf_mat_rf <- confusionMatrix(data = predicciones_rf, reference = data_test_prep$Class,
                               positive = "LD")
conf_mat_rf


stats_class_rf <- data.frame(model         = "RF",
                             accuracy      = conf_mat_rf$overall[[1]],
                             FN            = conf_mat_rf$table[2,1],
                             FP            = conf_mat_rf$table[1,2],
                             error.rate    = 1-conf_mat_rf$overall[[1]],
                             kappa         = conf_mat_rf$overall[[2]],
                             sensibilidad  = conf_mat_rf$byClass[[1]], 
                             especificidad = conf_mat_rf$byClass[[2]], 
                             precision     = conf_mat_rf$byClass[[5]], 
                             recall        = conf_mat_rf$byClass[[6]], 
                             f.measure     = conf_mat_rf$byClass[[7]])
stats_class_rf



################################################################################################

################
#              #
#      LR      #
#     glm      #
#              #
################

# se recurre a validación cruzada repetida como método de validación.
# Número de particiones y repeticiones
particiones  <- 10
repeticiones <- 5

hiperparametros <- data.frame(parameter = "none")


set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros)) 
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)


# DEFINICIÓN DEL ENTRENAMIENTO
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
set.seed(342)
modelo_lrglm <- train(Class ~ ., data = data_train_prep,
                      method = "glm",
                      tuneGrid = hiperparametros,
                      metric = "Accuracy",
                      trControl = control_train,
                      family = "binomial")
modelo_lrglm


summary(modelo_lrglm$finalModel)


# Predicciones
predicciones_lrglm <- predict(modelo_lrglm, newdata = data_test_prep,
                              type = "raw")
predicciones_lrglm


# Evaluación
conf_mat_lrglm <- confusionMatrix(data = predicciones_lrglm, reference = data_test_prep$Class,
                                  positive = "LD")
conf_mat_lrglm


stats_class_lrglm <- data.frame(model         = "LRglm",
                                accuracy      = conf_mat_lrglm$overall[[1]],
                                FN            = conf_mat_lrglm$table[2,1],
                                FP            = conf_mat_lrglm$table[1,2],
                                error.rate    = 1-conf_mat_lrglm$overall[[1]],
                                kappa         = conf_mat_lrglm$overall[[2]],
                                sensibilidad  = conf_mat_lrglm$byClass[[1]], 
                                especificidad = conf_mat_lrglm$byClass[[2]], 
                                precision     = conf_mat_lrglm$byClass[[5]], 
                                recall        = conf_mat_lrglm$byClass[[6]], 
                                f.measure     = conf_mat_lrglm$byClass[[7]])
stats_class_lrglm


################################################################################################

################
#              #
#      LR      #
#   multinom   #
#              #
################

# se recurre a validación cruzada repetida como método de validación.
# Número de particiones y repeticiones
particiones  <- 10
repeticiones <- 5

# hiperparametros <- data.frame(decay  = c(0.01,0.1,0.3,0.5))

# hiperparametros <- data.frame(decay  = seq(0,0.1,length.out=10))

hiperparametros <- data.frame(decay  = seq(0,0.5,length.out=20))


set.seed(123)
seeds <- vector(mode = "list", length = (particiones * repeticiones) + 1)
for (i in 1:(particiones * repeticiones)) {
  seeds[[i]] <- sample.int(1000, nrow(hiperparametros)) 
}
seeds[[(particiones * repeticiones) + 1]] <- sample.int(1000, 1)


# DEFINICIÓN DEL ENTRENAMIENTO
control_train <- trainControl(method = "repeatedcv", number = particiones,
                              repeats = repeticiones, seeds = seeds,
                              returnResamp = "final", verboseIter = FALSE,
                              allowParallel = TRUE)

# AJUSTE DEL MODELO
set.seed(342)
modelo_lrmulti <- train(Class ~ ., data = data_train_prep,
                        method = "multinom",
                        tuneGrid = hiperparametros,
                        metric = "Accuracy",
                        trControl = control_train,
                        trace = FALSE)
modelo_lrmulti


summary(modelo_lrmulti$finalModel)


# REPRESENTACIÓN GRÁFICA
ggplot(modelo_lrmulti, highlight = TRUE) +
  labs(title = "Evolución del accuracy del modelo LRmulti") +
  theme_bw()


# Predicciones
predicciones_lrmulti <- predict(modelo_lrmulti, newdata = data_test_prep,
                                type = "raw")
predicciones_lrmulti


# Evaluación
conf_mat_lrmulti <- confusionMatrix(data = predicciones_lrmulti, reference = data_test_prep$Class,
                                    positive = "LD")
conf_mat_lrmulti


stats_class_lrmulti <- data.frame(model         = "LRmulti",
                                  accuracy      = conf_mat_lrmulti$overall[[1]],
                                  FN            = conf_mat_lrmulti$table[2,1],
                                  FP            = conf_mat_lrmulti$table[1,2],
                                  error.rate    = 1-conf_mat_lrmulti$overall[[1]],
                                  kappa         = conf_mat_lrmulti$overall[[2]],
                                  sensibilidad  = conf_mat_lrmulti$byClass[[1]], 
                                  especificidad = conf_mat_lrmulti$byClass[[2]], 
                                  precision     = conf_mat_lrmulti$byClass[[5]], 
                                  recall        = conf_mat_lrmulti$byClass[[6]], 
                                  f.measure     = conf_mat_lrmulti$byClass[[7]])
stats_class_lrmulti



################################################################################################
################################################################################################
################################################################################################

# Tabla resumen del rendimiento de los diferentes modelos:
stats_models <- rbind(stats_class_knn, 
                      stats_class_nb, 
                      stats_class_ann,
                      stats_class_mlp,
                      stats_class_svmlineal,
                      stats_class_svmPoly,
                      stats_class_svmRadial,
                      stats_class_C5.0Tree,
                      stats_class_C5.0,
                      stats_class_J48,
                      stats_class_rf,
                      stats_class_lrglm,
                      stats_class_lrmulti)

stats_models

# Ordenamos la tabla por el accuracy
stats_models %>% arrange(desc(accuracy))

# Ordenamos la tabla por la precisión y lo guardamos
stats_models_prec <- stats_models %>% arrange(desc(precision))
stats_models_prec

write.csv(stats_models_prec, "rendimiento_modelos.csv")














