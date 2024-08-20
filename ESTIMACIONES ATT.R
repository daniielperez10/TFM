
data_post_policy<- read_csv("predictions_xgboost_POSTPOLICY_CITYCENTER_CV")

#FILTRAR HASTA MAYO 2021 PARA NO SOLAPARSE CON MADRID 360 (OTRA POLÍTICA)
data_post_policy<- data_post_policy %>%
  filter(fecha < as.Date("2021-05-01"))

data_post_policy <- data_post_policy %>%
  group_by(year, month, station_name) %>%
  summarise(
    mean_altitud = mean(altitud, na.rm = TRUE),
    mean_tmed = mean(tmed, na.rm = TRUE),
    mean_prec = mean(prec, na.rm = TRUE),
    mean_dir = mean(dir, na.rm = TRUE),
    mean_velmedia = mean(velmedia, na.rm = TRUE),
    mean_racha = mean(racha, na.rm = TRUE),
    mean_predicted_air_quality = mean(predicted_air_quality, na.rm = TRUE),
    mean_air_quality = mean(air_quality, na.rm = TRUE),
    latitude = first(latitude),  # Conservar la latitud original
    longitude = first(longitude) # Conservar la longitud original
  )

unique_values <- length(unique(data_post_policy$station_name))
print(paste('Número de valores únicos en station_name:', unique_values))

reference_station <- data_post_policy %>%
  filter(station_name == "Plaza del Carmen") %>%
  select(longitude, latitude) %>%
  distinct()

reference_longitude <- reference_station$longitude
reference_latitude <- reference_station$latitude

calculate_distance <- function(lon1, lat1, lon2, lat2) {
  distHaversine(c(lon1, lat1), c(lon2, lat2)) / 1000
}

# Aplicar la función para calcular la distancia desde la estación de referencia
data_post_policy <- data_post_policy %>%
  mutate(distance = mapply(calculate_distance, longitude, latitude, MoreArgs = list(reference_longitude, reference_latitude)))

data_post_policy <- data_post_policy %>%
  mutate(
    zone = cut(
      distance,
      breaks = c( -Inf, 2.5, 8, Inf ),
      labels = c("Zone 0", "Zone 1", "Zone 2"),
      right = FALSE
    )
  )




reference_station <- data_post_policy %>%
  filter(station_name == "Plaza del Carmen") %>%
  select(longitude, latitude) %>%
  distinct()

# Extraer la longitud y latitud de referencia
reference_longitude <- reference_station$longitude
reference_latitude <- reference_station$latitude

# Añadir las columnas de latitud y longitud de referencia al dataframe original
data_post_policy <- data_post_policy %>%
  mutate(
    reference_longitude = reference_longitude,
    reference_latitude = reference_latitude
  )

# Función para calcular la distancia
calculate_distance <- function(lon1, lat1, lon2, lat2) {
  distHaversine(c(lon1, lat1), c(lon2, lat2)) / 1000  # Convertir metros a kilómetros
}

# Aplicar la función para calcular la distancia desde la estación de referencia
data_post_policy <- data_post_policy %>%
  rowwise() %>%
  mutate(distance = calculate_distance(longitude, latitude, reference_longitude, reference_latitude)) %>%
  ungroup()

# Añadir zonas basadas en la distancia
data_post_policy <- data_post_policy %>%
  mutate(
    zone = cut(
      distance,
      breaks = c(-Inf, 2.5, 8, Inf),
      labels = c("Zone 0", "Zone 1", "Zone 2"),
      right = FALSE
    )
  )

#ATT CLÁSICO 


df_combined <- read_csv("df_combined_complete.csv")


df_combined <- df_combined %>% drop_na(air_quality)

df_combined<- df_combined %>%
  filter(as.Date(fecha) <= as.Date("2021-06-30"))

df_combined <- df_combined %>%
  mutate(zone = case_when(
    station_name %in% c("Escuelas Aguirre", "Castellana", "Plaza Castilla", 
                        "Ramón y Cajal", "Cuatro Caminos", "Plaza de España", 
                        "Barrio del Pilar", "Plaza del Carmen", "Méndez Álvaro", 
                        "Parque del Retiro") ~ "Zone 1",
    station_name %in% c("Plaza Elíptica", "Farolillo", "Villaverde", "Moratalaz", "Vallecas", "Ensanche de Vallecas") ~ "Zone 2",
    station_name %in% c("Arturo Soria", "Sanchinarro", "Urb. Embajada", 
                        "Barajas Pueblo", "Tres Olivos", "Juan Carlos I") ~ "Zone 3",
    station_name %in% c("El Pardo", "Casa de Campo") ~ "Zone 4",
    #station_name %in% c("Plaza Elíptica", "Farolillo", "Villaverde", "Moratalaz", "Vallecas", "Ensanche de Vallecas") ~ "Zone 5",
    station_name %in% c("Valdemoro", "Arganda del Rey", "Collado Villalba", "Algete", "Colmenar Viejo", "Alcala de Henares") ~ "Control",
    TRUE ~ "Excluded"
  ))
#GRAPH PARALLEL TRENDS

monthly_avg <- df_combined %>%
  group_by(year, month, zone) %>%
  summarise(mean_air_quality = mean(air_quality, na.rm = TRUE)) %>%
  ungroup()

# Convertir year y month en un solo campo de fecha para el gráfico
monthly_avg <- monthly_avg %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%m-%d"))

ggplot(monthly_avg, aes(x = date, y = mean_air_quality, color = zone)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2018-11-01"), linetype = "dashed", color = "red", size = 1) +
  labs(title = "Serie Temporal de la Contaminación Media Mensual por Año y Tratamiento",
       x = "Fecha",
       y = "Contaminación Media (air_quality)",
       color = "zone") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

annual_avg <- df_combined %>%
  group_by(year, zone) %>%
  summarise(mean_air_quality = mean(air_quality, na.rm = TRUE)) %>%
  ungroup()

# Convertir year en un campo de fecha para el gráfico
annual_avg <- annual_avg %>%
  mutate(date = as.Date(paste(year, "01", "01", sep = "-"), format = "%Y-%m-%d"))

# Graficar la contaminación media anual
ggplot(annual_avg, aes(x = date, y = mean_air_quality, color = zone)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2018-01-01"), linetype = "dashed", color = "red", size = 1) +
  labs(title = "Serie Temporal de la Contaminación Media Anual por Año y Tratamiento",
       x = "Fecha",
       y = "Contaminación Media (air_quality)",
       color = "zone") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(leaflet)

# Definir una paleta de colores con nombres específicos
color_pal <- colorFactor(
  palette = c("red", "grey", "lightblue", "orange", "purple", "yellow"),
  domain = c("Control", "Excluded", "Zone 1", "Zone 2", "Zone 3", "Zone 4")
)

# Crear el mapa con leaflet
map <- leaflet(df_combined) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  setView(lng = median(df_combined$longitude), lat = median(df_combined$latitude), zoom = 12) %>%
  # Añadir círculos con color basado en la variable 'zone'
  addCircles(
    lng = ~longitude, 
    lat = ~latitude,
    color = ~color_pal(zone),
    fillOpacity = 0.7,
    radius = 5
  ) %>%
  addLabelOnlyMarkers(
    lng = median(df_combined$longitude) - 0.3, 
    lat = median(df_combined$latitude) + 0.1,
    labelOptions = labelOptions(textsize = "20px", noHide = TRUE, textOnly = TRUE)
  ) %>%
  addLegend(
    position = "topright",
    pal = color_pal,
    values = c("Zone 1", "Zone 2", "Zone 3", "Zone 4", "Excluded", "Control"),
    title = "Zone",
    opacity = 1
  )

# Mostrar el mapa
map



df_combined <- df_combined %>%
  filter(zone != "Excluded")

monthly_avg <- df_combined %>%
  group_by(year, month, zone) %>%
  summarise(mean_air_quality = mean(air_quality, na.rm = TRUE)) %>%
  ungroup()

# Convertir year y month en un solo campo de fecha para el gráfico
monthly_avg <- monthly_avg %>%
  mutate(date = as.Date(paste(year, month, "01", sep = "-"), format = "%Y-%m-%d"))

ggplot(monthly_avg, aes(x = date, y = mean_air_quality, color = zone)) +
  geom_line() +
  geom_vline(xintercept = as.Date("2018-11-01"), linetype = "dashed", color = "red", size = 1) +
  labs(title = "Serie Temporal de la Contaminación Media Mensual por Año y Tratamiento",
       x = "Fecha",
       y = "Contaminación Media (air_quality)",
       color = "zone") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

df_combined <- df_combined %>%
  filter(!(fecha >= as.Date("2020-02-01") & fecha <= as.Date("2020-07-31"))) #omitted covid period

df_combined <- df_combined %>% filter(as.Date(fecha) <= as.Date("2021-06-30")) 

model0 <- feols(air_quality ~ city_center + post_treat_mad_central + city_center*post_treat_mad_central + prec + dir + velmedia + racha +year + zone,  data = df_combined, cluster = ~zone)
summary(model0)

df_combined<- df_combined %>%
  mutate(year = as.factor(year), zone = as.factor(zone), post = as.factor(post_treat_mad_central))


df_combined <- df_combined %>%
  mutate(
    post = as.numeric(as.character(post)),
    year = as.numeric(as.character(year))
  )

# Crear variables de tratamiento para cada zona
df_combined <- df_combined %>%
  mutate(
    treatment_zone1 = ifelse(zone == "Zone 1", 1, 0),
    treatment_zone2 = ifelse(zone == "Zone 2", 1, 0),
    treatment_zone3 = ifelse(zone == "Zone 3", 1, 0),
    treatment_zone4 = ifelse(zone == "Zone 4", 1, 0))
    #treatment_zone5 = ifelse(zone == "Zone 5", 1, 0)
  

# Crear variables de interacción post-tratamiento para cada zona
df_combined <- df_combined %>%
  mutate(
    POST_treat_1 = post * treatment_zone1,
    POST_treat_2 = post * treatment_zone2,
    POST_treat_3 = post * treatment_zone3,
    POST_treat_4 = post * treatment_zone4,
    #POST_treat_5 = post * treatment_zone5
  )


model1 <- feols(air_quality ~ post + treatment_zone1 + treatment_zone2 + treatment_zone3 + treatment_zone4 + POST_treat_1  + POST_treat_2 + POST_treat_3 + POST_treat_4 + prec + dir + velmedia + racha + year,  data = df_combined, cluster = ~zone)
summary(model1)

#ATT CLÁSICO

# Leer datos de post-política
data_post_policy <- read_csv("predictions_xgboost_POSTPOLICY_CITYCENTER_CV") %>%
  filter(fecha < as.Date("2021-05-01")) %>%
  filter(!(fecha >= as.Date("2020-02-01") & fecha <= as.Date("2020-07-31")))

# Ajustar el nombre de las zonas
data_post_policy <- data_post_policy %>%
  mutate(zone = case_when(
    station_name %in% c("Escuelas Aguirre", "Castellana", "Plaza Castilla", 
                        "Ramón y Cajal", "Cuatro Caminos", "Plaza de España", 
                        "Barrio del Pilar", "Plaza del Carmen", "Méndez Álvaro", 
                        "Parque del Retiro") ~ "Zone 1",
    station_name %in% c("Plaza Elíptica", "Farolillo", "Villaverde", "Moratalaz", "Vallecas", "Ensanche de Vallecas") ~ "Zona 2",
    station_name %in% c("Arturo Soria", "Sanchinarro", "Urb. Embajada", 
                        "Barajas Pueblo", "Tres Olivos", "Juan Carlos I") ~ "Zone 3",
    station_name %in% c("El Pardo", "Casa de Campo") ~ "Zone 4",
    TRUE ~ NA_character_
  ))

# Calcular la diferencia entre la calidad del aire observada y la predicha
data_with_difference <- data_post_policy %>%
  mutate(
    difference_air_quality_predicted = air_quality - predicted_air_quality
  )

# Calcular la media general de la diferencia
mean_difference_general <- data_with_difference %>%
  summarise(
    mean_difference = mean(difference_air_quality_predicted, na.rm = TRUE)
  )

# Ajustar la diferencia restando la media general
data_with_adjusted_difference <- data_with_difference %>%
  mutate(
    adjusted_difference = difference_air_quality_predicted - mean_difference_general$mean_difference
  )

# Calcular el sumatorio de las diferencias ajustadas
sum_adjusted_difference_general <- data_with_adjusted_difference %>%
  summarise(
    sum_adjusted_difference = sum(adjusted_difference, na.rm = TRUE)
  )

# Leer datos de entrenamiento
train_data_20 <- read_csv("train_data_20.csv") %>%
  mutate(zone = case_when(
    station_name %in% c("Escuelas Aguirre", "Castellana", "Plaza Castilla", 
                        "Ramón y Cajal", "Cuatro Caminos", "Plaza de España", 
                        "Barrio del Pilar", "Plaza del Carmen", "Méndez Álvaro", 
                        "Parque del Retiro") ~ "Zone 1",
    station_name %in% c("Plaza Elíptica", "Farolillo", "Villaverde", "Moratalaz", "Vallecas", "Ensanche de Vallecas") ~ "Zona 2",
    station_name %in% c("Arturo Soria", "Sanchinarro", "Urb. Embajada", 
                        "Barajas Pueblo", "Tres Olivos", "Juan Carlos I") ~ "Zone 3",
    station_name %in% c("El Pardo", "Casa de Campo") ~ "Zone 4",
    TRUE ~ NA_character_
  ))

# Calcular el sumatorio de residuos de validación cruzada
sumatorio_cv_residuos_general <- train_data_20 %>%
  summarise(
    sumatorio_cv_residuos = sum(cv_residuos, na.rm = TRUE)
  )

# Calcular la varianza y el error estándar
standar_error_cv_general <- sum_adjusted_difference_general %>%
  mutate(
    variance_cv = (sum_adjusted_difference + sumatorio_cv_residuos_general$sumatorio_cv_residuos)^2,
    se_cv = sqrt(variance_cv)
  )

# Mostrar los resultados
print(standar_error_cv_general)


print(mean_difference_general)




#ATT CONDITIONAL ML
########################################################################################

data_post_policy<- read_csv("predictions_xgboost_POSTPOLICY_CITYCENTER_CV")

data_post_policy<- data_post_policy %>%
  filter(fecha < as.Date("2021-05-01"))

data_post_policy <- data_post_policy %>%
  filter(!(fecha >= as.Date("2020-02-01") & fecha <= as.Date("2020-07-31")))

data_post_policy <- data_post_policy %>%
  mutate(zone = case_when(
    station_name %in% c("Escuelas Aguirre", "Castellana", "Plaza Castilla", 
                        "Ramón y Cajal", "Cuatro Caminos", "Plaza de España", 
                        "Barrio del Pilar", "Plaza del Carmen", "Méndez Álvaro", 
                        "Parque del Retiro") ~ "Zone 1",
    station_name %in% c("Plaza Elíptica", "Farolillo", "Villaverde", "Moratalaz", "Vallecas", "Ensanche de Vallecas") ~ "Zona 2",
    station_name %in% c("Arturo Soria", "Sanchinarro", "Urb. Embajada", 
                        "Barajas Pueblo", "Tres Olivos", "Juan Carlos I") ~ "Zone 3",
    station_name %in% c("El Pardo", "Casa de Campo") ~ "Zone 4",
    #station_name %in% c("Plaza Elíptica", "Farolillo", "Villaverde", "Moratalaz", "Vallecas", "Ensanche de Vallecas") ~ "Zone 5",
    TRUE ~ NA_character_
  ))


data_with_difference <- data_post_policy %>%
  mutate(
    difference_air_quality_predicted = air_quality - predicted_air_quality
  )

mean_difference_by_zone <- data_with_difference %>%
  group_by(zone) %>%
  summarise(
    mean_difference = mean(difference_air_quality_predicted, na.rm = TRUE)
  )


train_data_20<- read_csv("train_data_20.csv")

train_data_20 <- train_data_20 %>%
  mutate(zone = case_when(
    station_name %in% c("Escuelas Aguirre", "Castellana", "Plaza Castilla", 
                        "Ramón y Cajal", "Cuatro Caminos", "Plaza de España", 
                        "Barrio del Pilar", "Plaza del Carmen", "Méndez Álvaro", 
                        "Parque del Retiro") ~ "Zone 1",
    station_name %in% c("Plaza Elíptica", "Farolillo", "Villaverde", "Moratalaz", "Vallecas", "Ensanche de Vallecas") ~ "Zona 2",
    station_name %in% c("Arturo Soria", "Sanchinarro", "Urb. Embajada", 
                        "Barajas Pueblo", "Tres Olivos", "Juan Carlos I") ~ "Zone 3",
    station_name %in% c("El Pardo", "Casa de Campo") ~ "Zone 4",
    #station_name %in% c("Plaza Elíptica", "Farolillo", "Villaverde", "Moratalaz", "Vallecas", "Ensanche de Vallecas") ~ "Zone 5",
    TRUE ~ NA_character_
  ))


data_with_difference <- data_post_policy %>%
  mutate(
    difference_air_quality_predicted = air_quality - predicted_air_quality
  )

mean_difference_by_zone <- data_with_difference %>%
  group_by(zone) %>%
  summarise(
    mean_difference = mean(difference_air_quality_predicted, na.rm = TRUE)
  )



# Paso 3: Restar la media general de cada valor en la columna de diferencia para cada categoría de zone
data_with_adjusted_difference <- data_with_difference %>%
  left_join(mean_difference_by_zone, by = "zone") %>%
  mutate(
    adjusted_difference = difference_air_quality_predicted - mean_difference
  )

# Paso 4: Calcular el sumatorio de las diferencias ajustadas para cada categoría de zone
sum_adjusted_difference_by_zone <- data_with_adjusted_difference %>%
  group_by(zone) %>%
  summarise(
    sum_adjusted_difference = sum(adjusted_difference, na.rm = TRUE)
  )

# Paso 5: Calcular el sumatorio de cv_residuos (suponiendo que tienes los residuos por zone)
# Nota: Aquí necesitas tener el dataframe `train_data_20` con la columna `cv_residuos` y la columna `zone`
sumatorio_cv_residuos_by_zone <- train_data_20 %>%
  group_by(zone) %>%
  summarise(
    sumatorio_cv_residuos = sum(cv_residuos, na.rm = TRUE)
  )

standar_error_cv_by_zone <- sum_adjusted_difference_by_zone %>%
  left_join(sumatorio_cv_residuos_by_zone, by = "zone") %>%
  mutate(
    variance_cv = (sum_adjusted_difference + sumatorio_cv_residuos)^2
  )

# Paso 2: Calcular el error estándar a partir de la varianza
standar_error_cv_by_zone <- standar_error_cv_by_zone %>%
  mutate(
    se_cv = sqrt(variance_cv)
  )

# Mostrar los resultados
print(standar_error_cv_by_zone)
print(mean_difference_by_zone)

observations_per_zone <- data_post_policy %>%
  count(zone)

# Mostrar los resultados
print(observations_per_zone)


###########################################################
#MENSUALES

data_post_policy<- read_csv("predictions_xgboost_POSTPOLICY_CITYCENTER_CV")

data_post_policy<- data_post_policy %>%
  filter(fecha < as.Date("2021-05-01"))

data_post_policy <- data_post_policy %>%
  filter(!(fecha >= as.Date("2020-02-01") & fecha <= as.Date("2020-07-31")))

data_post_policy <- data_post_policy %>%
  mutate(zone = case_when(
    station_name %in% c("Escuelas Aguirre", "Castellana", "Plaza Castilla", 
                        "Ramón y Cajal", "Cuatro Caminos", "Plaza de España", 
                        "Barrio del Pilar", "Plaza del Carmen", "Méndez Álvaro", 
                        "Parque del Retiro") ~ "Zone 1",
    station_name %in% c("Plaza Elíptica", "Farolillo", "Villaverde", "Moratalaz", "Vallecas", "Ensanche de Vallecas") ~ "Zona 2",
    station_name %in% c("Arturo Soria", "Sanchinarro", "Urb. Embajada", 
                        "Barajas Pueblo", "Tres Olivos", "Juan Carlos I") ~ "Zone 3",
    station_name %in% c("El Pardo", "Casa de Campo") ~ "Zone 4",
    #station_name %in% c("Plaza Elíptica", "Farolillo", "Villaverde", "Moratalaz", "Vallecas", "Ensanche de Vallecas") ~ "Zone 5",
    TRUE ~ NA_character_
  ))

# Agrupar por zona, año y mes, y calcular medias mensuales
data_post_policy_monthly <- data_post_policy %>%
  group_by(year, month, zone) %>%
  summarise(
    mean_air_quality = mean(air_quality, na.rm = TRUE),
    mean_predicted_air_quality = mean(predicted_air_quality, na.rm = TRUE),
    .groups = 'drop'
  )

# Calcular la diferencia mensual
data_with_difference_monthly <- data_post_policy_monthly %>%
  mutate(
    difference_air_quality_predicted = mean_air_quality - mean_predicted_air_quality
  )

mean_difference_by_zone_monthly <- data_with_difference_monthly %>%
  group_by(zone) %>%
  summarise(
    mean_difference = mean(difference_air_quality_predicted, na.rm = TRUE),
    .groups = 'drop'
  )

train_data_20<- read_csv("train_data_20.csv")

train_data_20 <- train_data_20 %>%
  mutate(zone = case_when(
    station_name %in% c("Escuelas Aguirre", "Castellana", "Plaza Castilla", 
                        "Ramón y Cajal", "Cuatro Caminos", "Plaza de España", 
                        "Barrio del Pilar", "Plaza del Carmen", "Méndez Álvaro", 
                        "Parque del Retiro") ~ "Zone 1",
    station_name %in% c("Plaza Elíptica", "Farolillo", "Villaverde", "Moratalaz", "Vallecas", "Ensanche de Vallecas") ~ "Zona 2",
    station_name %in% c("Arturo Soria", "Sanchinarro", "Urb. Embajada", 
                        "Barajas Pueblo", "Tres Olivos", "Juan Carlos I") ~ "Zone 3",
    station_name %in% c("El Pardo", "Casa de Campo") ~ "Zone 4",
    #station_name %in% c("Plaza Elíptica", "Farolillo", "Villaverde", "Moratalaz", "Vallecas", "Ensanche de Vallecas") ~ "Zone 5",
    TRUE ~ NA_character_
  ))

data_with_adjusted_difference_monthly <- data_with_difference_monthly %>%
  left_join(mean_difference_by_zone_monthly, by = "zone") %>%
  mutate(
    adjusted_difference = difference_air_quality_predicted - mean_difference
  )

# Calcular el sumatorio de las diferencias ajustadas para cada zona
sum_adjusted_difference_by_zone_monthly <- data_with_adjusted_difference_monthly %>%
  group_by(zone) %>%
  summarise(
    sum_adjusted_difference = sum(adjusted_difference, na.rm = TRUE),
    .groups = 'drop'
  )

train_data_20_monthly <- train_data_20 %>%
  group_by(year, month, zone) %>%
  summarise(
    sumatorio_cv_residuos = sum(cv_residuos, na.rm = TRUE),
    .groups = 'drop'
  )

# Combinar con los datos ajustados
standar_error_cv_by_zone_monthly <- sum_adjusted_difference_by_zone_monthly %>%
  left_join(train_data_20_monthly, by = "zone") %>%
  mutate(
    variance_cv = (sum_adjusted_difference + sumatorio_cv_residuos)^2
  )

# Calcular el error estándar
standar_error_cv_by_zone_monthly <- standar_error_cv_by_zone_monthly %>%
  mutate(
    se_cv = sqrt(variance_cv)
  )


# Mostrar los resultados
print(standar_error_cv_by_zone_monthly)



