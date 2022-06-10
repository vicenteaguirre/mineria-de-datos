### PROYECTO 2 ###
# Vicente Aguirre, Francesco Andrade

#El objetivo principal de este proyecto es crear un programa computacional que permita a la aerolínea
#PANAM estimar si un vuelo tendrá más de 4 no show en su vuelo. Por ejemplo, si un vuelo de Santiago a
#Concepción tiene 3 no show entonces el vuelo se cataloga como 0, ya que el número de no show fue bajo.
#Caso contrario, si 4 o más personas no se presentaron, entonces el vuelo se cataloga como 1, ya que la
#aerolínea pudo haber realizado overbooking.

#-------------------------------------
#Importamos packages
pacman::p_load(dbscan, tidyverse, Rtsne, ggdendro,factoextra, fpc, cluster, usethis, dplyr,ggplot2,magrittr, discrim, naivebayes,lubridate,modeldata)
pacman::p_load(tidyverse, tidymodels, discrim, naivebayes, nycflights13)

#Definimos los seeds
set.seed(32)

#-------------------------------------
#Lectura de datos de entrenamiento para la base de datos.
filepath_trainData= "/Users/vaguirre/Desktop/UAI/2022/1er/MINERIA DE DATOS/Proyectos/Proyecto 2/trainData.csv"
originalData<- 
  read.csv(filepath_trainData)

trainData<- 
  originalData %>%
  mutate(
    #1 si los noshows son iguales o mayor a 4
    #0 si los noshows son menos de 4
    noshow = ifelse(noshow >= 4, "1", "0"),
    noshow = factor(noshow),
    date = lubridate::as_date(date)
  ) %>%
  dplyr::select(id, date, origin, destination, distance, noshow, denied_boarding, departure_time, capacity, bookings) %>%
  na.omit() %>% 
  mutate_if(is.character, as.factor) %>%
  sample_n(10000)

glimpse(trainData)
#summary(trainData)

#-------------------------------------
#Preparamos data de entrenamiento y test
data_split <- initial_split(trainData, strata = "noshow")
train <- training(data_split)
test <- testing(data_split)




#-------------------------------------
#Generamos la receta
receta <- 
  recipe(noshow ~ ., data = train_data) %>% 
  update_role(id, new_role = "ID") %>% 
  step_date(date, features = c("dow", "month")) %>%               
  step_holiday(date, 
               holidays = timeDate::listHolidays("US"), 
               keep_original_cols = FALSE) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors())
#-------------------------------------
#Creamos funcion para fitear modelos

fitea <- function(modelo){
  
  modelo_fit <- 
    workflow() %>% 
    add_model(modelo) %>% 
    add_recipe(receta) %>% 
    fit(data = train_data)
  
  model_pred <- 
    predict(modelo_fit, test_data, type = "prob") %>% 
    bind_cols(test_data) 
  
  model_pred
  
  return(model_pred %>% 
           roc_auc(truth = noshow, .pred_mayor_igual))
}
#-------------------------------------
#Generamos los dos modelos a analizar

#K-NN
modelo_knn <- 
  nearest_neighbor(neighbors = 5) %>% 
  set_engine("kknn") %>%
  set_mode("classification")

#NaiveBayes
modelo_naivebayes <-
  naive_Bayes() %>% 
  set_mode("classification") %>% 
  set_engine("klaR")

#-------------------------------------
#Fitear modelos
fitea(modelo_knn)
fitea(modelo_naivebayes)