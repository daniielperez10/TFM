library(caret)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(Synth)
library(xgboost)


df_combined <- read_csv("df_combined_complete.csv")
df_combined_clean <- df_combined %>% drop_na(air_quality)

colSums(is.na(df_combined_clean))

#DISTRIBUCIÓN VARIABLE DEPENDIENTE
ggplot(df_combined_clean, aes(x = air_quality)) +
  geom_density(fill = "blue", color = "black") +
  labs(title = "Densidad de la Calidad del Aire", x = "Calidad del Aire", y = "Densidad") +
  theme_minimal()

#CONSTRUIR EL CONTRAFACTUAL CON MODELOS DE MACHINE LEARNING

data_pre_policy <- df_combined_clean %>% filter(fecha < as.Date('2018-11-30'))
data_pre_policy <- data_pre_policy%>% filter(city_center == 1)

data_pre_policy <- data_pre_policy %>%
  mutate(year = as.numeric(year),
         month = as.numeric(month),
         day = as.numeric(dia),
         station_type = as.factor(station_type),
         station_code.x = as.factor(station_code.x))

data_pre_policy <- data_pre_policy |> 
  select(c(5:6, 8, 14, 16:18, 20:22, 25:27))

#CREAR TRAINING AND TESTING SET

set.seed(42)
index_20 <- createDataPartition(data_pre_policy$air_quality, p = 0.5, list = FALSE)  
data_20 <- data_pre_policy[index_20, ]

set.seed(42)
trainIndex_20 <- createDataPartition(data_20$air_quality, p = 0.6, list = FALSE)
train_data_20 <- data_20[trainIndex_20, ]
test_data_20 <- data_20[-trainIndex_20, ]

str(train_data_20)

train_control <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation. Si lo aumento me tarda muchísmo en cargar el modelo.

tune_grid <- expand.grid(
  nrounds = c(500, 1000),
  max_depth = c(6, 8,10),
  eta = c(0.01, 0.1),
  gamma = c(1, 2),
  colsample_bytree = c(0.6, 0.8),  # Debe estar entre 0 y 1
  min_child_weight = c(1, 2, 4),
  subsample = c(0.5, 0.8)
) #Con estos valores -> RMSE 8.2074403 Rsquared 0.8311039 MAE 6.1128442

tune_grid <- expand.grid(
  nrounds = c(500, 1000),
  max_depth = c(6, 8, 10, 12),
  eta = c(0.01, 0.05, 0.1, 0.2),
  gamma = c(0, 1, 2, 4),
  colsample_bytree = c(0.6, 0.8, 1.0),  # Debe estar entre 0 y 1
  min_child_weight = c(1, 2, 4, 6),
  subsample = c(0.5, 0.7, 0.8)
)  


set.seed(42)
xgb_tune <- train(
  air_quality ~ . ,
  data = train_data_20, 
  method = "xgbTree",
  trControl = train_control,
  tuneGrid = tune_grid,
  verbose = TRUE
)

set.seed(42)
predictions <- predict(xgb_tune, newdata = test_data_20)

# Evaluar el modelo
results <- postResample(predictions, test_data_20$air_quality)
print(results)


data_pre_policy$predicted_air_quality <- predict(xgb_tune, newdata = data_pre_policy)

str(data_pre_policy)

data_pre_policy <- data_pre_policy %>%
  mutate(
    year = as.factor(year),
    month = as.factor(month)
  )


#write.csv(data_pre_policy, "data_pre_policy_predictions.csv", row.names = FALSE) #guardo el dataset con las predicciones

#DATAFRAME CON LAS PREDICCIONES PRE TREATMENT
data_pre_policy <- read.csv("data_pre_policy_predictions.csv")

#GRAFICO PREDICTED VS ACTUAL VALUES EN EL PRE-TREATMENT


# Crear etiquetas de fecha
monthly_means <- data_pre_policy %>%
  mutate(
    year_month_label = paste(year, month, sep = "-")
  )

monthly_means <- monthly_means %>%
  group_by(year_month_label) %>%
  summarise(
    air_quality_mean = mean(air_quality, na.rm = TRUE),
    predicted_air_quality_mean = mean(predicted_air_quality, na.rm = TRUE)
  )

# Graficar las series temporales con etiquetas personalizadas
ggplot(monthly_means, aes(x = year_month_label)) +
  geom_line(aes(y = air_quality_mean, color = "Air Quality", group = 1), linetype = "dashed") +
  geom_line(aes(y = predicted_air_quality_mean, color = "Predicted Air Quality", group = 1)) +
  labs(
    x = "Year-Month",
    y = "Quality Index",
    title = "Monthly Mean Air Quality and Predicted Air Quality"
  ) +
  scale_color_manual(values = c("Air Quality" = "blue", "Predicted Air Quality" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(labels = function(x) substr(x, 1, 7))


#NO NECESARIO EJECUTAR ESTAS LINEAS
data_post_policy <- df_combined_clean %>% filter(fecha >= as.Date('2018-11-30'))
data_post_policy <- data_post_policy%>% filter(city_center == 1)

data_post_policy <- data_post_policy %>%
  mutate(year = as.numeric(year),
         month = as.numeric(month),
         day = as.numeric(dia),
         station_type = as.factor(station_type),
         station_code.x = as.factor(station_code.x))

data_post_policy <- data_post_policy |> 
  select(c(5:6, 7:9, 14, 16:18, 20:22, 25:27))

set.seed(42)
data_post_policy$predicted_air_quality <- predict(xgb_tune, newdata = data_post_policy)

#write.csv(data_post_policy, file = "predictions_xgboost_POSTPOLICY_CITYCENTER_CV", row.names = FALSE) #guardar el dataframe con las predicciones generadas por xgboost

data_post_policy<- read_csv("predictions_xgboost_POSTPOLICY_CITYCENTER_CV") #DATAFRAME CON LAS PREDICCIONES POST POLICY, ES DECIR, EL CONTRAFACTUAL


Suposición 4 (estabilidad de la función contrafactual):
  
  Propósito: Asegurar que la predicción de lo que habría ocurrido sin el tratamiento es consistente y estable a lo largo del tiempo.
Analogía con Pre-tendencias: En DiD, las tendencias paralelas implican que cualquier diferencia entre los grupos después del tratamiento se debe al tratamiento. Similarmente, la estabilidad de la función contrafactual implica que cualquier diferencia observada se puede atribuir al tratamiento y no a cambios en la función de predicción.





#NO NECESARIO EJECUTAR ESTAS LINEAS 
cv_residuos <- residuals(xgb_tune, type = "raw")

tratamiento_date <- as.Date("2018-11-30")


tratamiento_fecha <- as.Date("2018-11-01")  # Establece la fecha de inicio del tratamiento (por ejemplo, el 1 de noviembre de 2018)

# Paso 2: Crear una fecha con día 01 en train_data_20
train_data_20$fecha <- as.Date(paste(train_data_20$year, train_data_20$month, "01", sep = "-"))

# Paso 3: Calcular los meses antes del tratamiento
train_data_20$weeks_Prior_to_Treatment <- as.numeric(difftime(tratamiento_fecha, train_data_20$fecha, units = "weeks")) 

train_data_20$cv_residuos <- cv_residuos

#write.csv(train_data_20, "train_data_20.csv", row.names = FALSE) #guardo el training set con los residuos para evaluar supuesto 4

train_data_20<- read_csv("train_data_20.csv") #DATASET CON LOS RESIDUOS DE LA CV

#DEMOSTRACIÓN ESTABILIDAD DE LA FUNCIÓN PREDICTIVA

train_data_20$weeks_Prior_to_Treatment <- ceiling(train_data_20$weeks_Prior_to_Treatment)

regresion <- lm(cv_residuos ~ weeks_Prior_to_Treatment , data = train_data_20)
summary(regresion)

set.seed(42)
predicciones <- predict(regresion, newdata = train_data_20, interval = "confidence")


weekly_residuals <- train_data_20 %>%
  group_by(weeks_Prior_to_Treatment) %>%
  summarise(cv_residuos_mean = mean(cv_residuos, na.rm = TRUE),
            cv_residuos_sd = sd(cv_residuos, na.rm = TRUE),
            n = n()) %>%
  mutate(se = cv_residuos_sd / sqrt(n),
         lower_ci = cv_residuos_mean - 1.96 * se,
         upper_ci = cv_residuos_mean + 1.96 * se)

weekly_residuals <- train_data_20 %>%
  mutate(weeks_group = cut(weeks_Prior_to_Treatment, 
                           breaks = seq(0, max(weeks_Prior_to_Treatment) + 5, by = 5),
                           right = FALSE, labels = FALSE)) %>%
  group_by(weeks_group) %>%
  summarise(cv_residuos_mean = mean(cv_residuos, na.rm = TRUE),
            cv_residuos_sd = sd(cv_residuos, na.rm = TRUE),
            n = n()) %>%
  mutate(se = cv_residuos_sd / sqrt(n),
         lower_ci = cv_residuos_mean - 1.96 * se,
         upper_ci = cv_residuos_mean + 1.96 * se)


weekly_residuals <- train_data_20 %>%
  group_by(weeks_Prior_to_Treatment) %>%
  summarise(cv_residuos_mean = mean(cv_residuos, na.rm = TRUE),
            cv_residuos_sd = sd(cv_residuos, na.rm = TRUE),
            n = n()) %>%
  mutate(se = cv_residuos_sd / sqrt(n),
         lower_ci = cv_residuos_mean - 1.96 * se,
         upper_ci = cv_residuos_mean + 1.96 * se)

weekly_residuals <- weekly_residuals %>%
  filter(row_number() != 11)

# Crear el gráfico
ggplot(weekly_residuals, aes(x = weeks_Prior_to_Treatment, y = cv_residuos_mean)) +
  geom_point(color = "blue") +  # Puntos de datos reales
  geom_line(color = "blue") +  # Línea de predicción
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci), fill = "blue", alpha = 0.2) +  # Intervalo de confianza
  labs(title = "Evaluación de Efectos Anticipados y la Estabilidad de la Función Contrafactual",
       x = "Semanas Previas al Tratamiento",
       y = "Residuos Validados Cruzados") +
  theme_minimal() +
  annotate("text", x = Inf, y = Inf, label = sprintf("F-stat = %.3f\nP-value = %.3f",
                                                     summary(regresion)$fstatistic[1],
                                                     pf(summary(regresion)$fstatistic[1], 
                                                        summary(regresion)$fstatistic[2], 
                                                        summary(regresion)$fstatistic[3], 
                                                        lower.tail = FALSE)),
           hjust = 1.1, vjust = 1.5, size = 5, color = "red") +
  coord_cartesian(ylim = c(-0.15, 0.15)) 



####### #### GRUPO CONTROL SINTÉTICO (NO FUNCIONA)

#EJEMPLO CON DATOS DE PRUEBA DE LA LIBRERIA QUE SÍ FUNCIONA


data(synth.data)

str(synth.data)
dataprep.out<-
  dataprep(
    foo = synth.data,
    predictors = c("X1", "X2", "X3"),
    predictors.op = "mean",
    dependent = "Y",
    unit.variable = "unit.num",
    time.variable = "year",
    special.predictors = list(
      list("Y", 1991, "mean"),
      list("Y", 1985, "mean"),
      list("Y", 1980, "mean")
    ),
    treatment.identifier = 7,
    controls.identifier = c(29, 2, 13, 17, 32, 38),
    time.predictors.prior = c(1984:1989),
    time.optimize.ssr = c(1984:1990),
    unit.names.variable = "name",
    time.plot = 1984:1996
  )

synth.out <- synth(dataprep.out)


#INTENTO CON MIS DATOS
df_combined_clean <- df_combined_clean %>%
  mutate(date_numeric = as.numeric(fecha - min(fecha)))

df_combined_clean <- df_combined_clean %>%
  mutate(punto_muestreo_num = as.integer(factor(station_name)))

df_combined_clean$punto_muestreo_num <- as.numeric(df_combined_clean$punto_muestreo_num)

str(df_combined_clean$punto_muestreo_num)

treatment_units <- c(7:48)  # Números de las estaciones tratadas
control_units <- c(1:6)  # Números de las estaciones de control
time_predictors_prior <- 0:100  # Ajustar según el periodo anterior al tratamiento
time_optimize_ssr <- 0:100  # Ajustar según el periodo anterior al tratamiento
time_plot <- 0:max(df_combined_clean$date_numeric)

dataprep.out <- dataprep(
  foo = df_combined_clean,
  predictors = c("altitud", "tmed", "tmax", "tmin", "dir", "velmedia"),
  predictors.op = "mean",
  dependent = "air_quality",
  unit.variable = "punto_muestreo_num",
  time.variable = "year",
  treatment.identifier = treatment_units,
  controls.identifier = control_units,
  time.predictors.prior = time_predictors_prior,
  time.optimize.ssr = time_optimize_ssr,
  unit.names.variable = "station_name",
  time.plot = time_plot
)



