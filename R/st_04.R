## ------------------------------------------------------------------------------------------------------------------
# Instalar los paquetes si es necesario.
# install.packages(c(
#   "sf", "spdep", "spatialreg", "tidyverse",
#   "readxl", "Matrix"
# ))

library(tidyverse)
library(readxl)
library(sf)
library(spdep)
library(spatialreg)
library(Matrix)


# Nombre del archivo Excel.
archivo <- "../datos/base_borregos_espaciotemporal_Hidalgo.xlsx"

# Consultar las hojas disponibles.
excel_sheets(archivo)

# Importar la tabla longitudinal.
datos <- read_excel(archivo, sheet = "datos_long")

datos_b2 <- datos |>
  mutate(
    animal_id = as.factor(animal_id),
    granja_id = as.factor(granja_id),
    sexo = as.factor(sexo),
    raza = as.factor(raza),
    tratamiento = as.factor(tratamiento),
    sistema_produccion = as.factor(sistema_produccion),
    salud_score_0_4 = factor(
      salud_score_0_4,
      levels = 0:4,
      ordered = TRUE
    ),
    salud_alta = as.factor(salud_alta),
    periodo = cut(
      tiempo,
      breaks = c(-Inf, 20, 40, 60, 80, Inf),
      labels = c(
        "Semanas 1-20",
        "Semanas 21-40",
        "Semanas 41-60",
        "Semanas 61-80",
        "Semanas 81-100"
      )
    )
  )



# Comprobar que los objetos de los bloques anteriores están disponibles.
dim(datos_b2)
glimpse(datos_b2)


## ------------------------------------------------------------------------------------------------------------------
granjas_b6 <- datos_b2 |>
  select(
    granja_id,
    longitud,
    latitud,
    zona,
    tamano_granja,
    sistema_produccion
  ) |>
  distinct() |>
  mutate(
    granja_id = as.factor(granja_id)
  )

# Número de filas y granjas únicas.
granjas_b6 |>
  summarise(
    n_filas = n(),
    n_granjas = n_distinct(granja_id)
  )


## ------------------------------------------------------------------------------------------------------------------
coordenadas_por_granja <- datos_b2 |>
  group_by(granja_id) |>
  summarise(
    n_longitudes = n_distinct(longitud, na.rm = TRUE),
    n_latitudes = n_distinct(latitud, na.rm = TRUE),
    .groups = "drop"
  )

coordenadas_por_granja |>
  filter(
    n_longitudes != 1 |
    n_latitudes != 1
  )


## ------------------------------------------------------------------------------------------------------------------
granjas_b6 |>
  summarise(
    n_longitud_ausente = sum(is.na(longitud)),
    n_latitud_ausente = sum(is.na(latitud)),
    longitud_minima = min(longitud, na.rm = TRUE),
    longitud_maxima = max(longitud, na.rm = TRUE),
    latitud_minima = min(latitud, na.rm = TRUE),
    latitud_maxima = max(latitud, na.rm = TRUE)
  )


## ------------------------------------------------------------------------------------------------------------------
granjas_b6 <- granjas_b6 |>
  drop_na(longitud, latitud) |>
  arrange(granja_id)


## ------------------------------------------------------------------------------------------------------------------
granjas_sf_b6 <- st_as_sf(
  granjas_b6,
  coords = c("longitud", "latitud"),
  crs = 4326,
  remove = FALSE
)

granjas_sf_b6


## ------------------------------------------------------------------------------------------------------------------
granjas_utm_b6 <- st_transform(
  granjas_sf_b6,
  crs = 32614
)

st_crs(granjas_utm_b6)


## ------------------------------------------------------------------------------------------------------------------
distancias_m <- st_distance(granjas_utm_b6)

# Convertir las unidades espaciales a una matriz numérica en kilómetros.
distancias_km <- units::drop_units(distancias_m) / 1000

# Mostrar una parte de la matriz.
round(distancias_km[1:min(6, nrow(distancias_km)),
                    1:min(6, ncol(distancias_km))], 2)


## ------------------------------------------------------------------------------------------------------------------
distancia_vecino_mas_cercano <- apply(
  distancias_km,
  1,
  function(x) min(x[x > 0], na.rm = TRUE)
)

resumen_distancias <- tibble(
  granja_id = granjas_utm_b6$granja_id,
  distancia_minima_km = distancia_vecino_mas_cercano
)

summary(resumen_distancias$distancia_minima_km)


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  resumen_distancias,
  aes(x = distancia_minima_km)
) +
  geom_histogram(
    bins = 15,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Distancia al vecino más cercano",
    x = "Distancia mínima (km)",
    y = "Número de granjas"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
# Extraer las coordenadas proyectadas.
coords_utm <- st_coordinates(granjas_utm_b6)

# Definir cuatro vecinos más cercanos.
k_vecinos <- min(4, nrow(granjas_utm_b6) - 1)

vecinos_knn <- knearneigh(
  coords_utm,
  k = k_vecinos
)

lista_vecinos_dirigida <- knn2nb(vecinos_knn)

lista_vecinos_dirigida


## ------------------------------------------------------------------------------------------------------------------
lista_vecinos <- make.sym.nb(
  lista_vecinos_dirigida
)

# Número de vecinos de cada granja.
numero_vecinos <- card(lista_vecinos)

summary(numero_vecinos)


## ------------------------------------------------------------------------------------------------------------------
table(numero_vecinos)

which(numero_vecinos == 0)


## ------------------------------------------------------------------------------------------------------------------
coordenadas_duplicadas <- granjas_b6 |>
  count(longitud, latitud, name = "n_granjas") |>
  filter(n_granjas > 1)

coordenadas_duplicadas


## ------------------------------------------------------------------------------------------------------------------
plot(
  st_geometry(granjas_utm_b6),
  pch = 19,
  col = "steelblue4",
  main = "Red de vecindad entre granjas"
)

plot(
  lista_vecinos,
  coords_utm,
  add = TRUE,
  col = "grey45",
  lwd = 1
)


## ------------------------------------------------------------------------------------------------------------------
segmentos_vecindad <- map_dfr(
  seq_along(lista_vecinos),
  function(i) {
    tibble(
      origen = i,
      destino = lista_vecinos[[i]],
      x = coords_utm[i, 1],
      y = coords_utm[i, 2],
      xend = coords_utm[lista_vecinos[[i]], 1],
      yend = coords_utm[lista_vecinos[[i]], 2]
    )
  }
) |>
  filter(origen < destino)

ggplot() +
  geom_segment(
    data = segmentos_vecindad,
    aes(x = x, y = y, xend = xend, yend = yend),
    color = "grey65",
    linewidth = 0.5
  ) +
  geom_sf(
    data = granjas_utm_b6,
    aes(color = zona),
    size = 2.5
  ) +
  labs(
    title = "Granjas y conexiones de vecindad",
    color = "Zona"
  ) +
  coord_sf() +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
B <- nb2mat(
  lista_vecinos,
  style = "B",
  zero.policy = TRUE
)

rownames(B) <- as.character(granjas_utm_b6$granja_id)
colnames(B) <- as.character(granjas_utm_b6$granja_id)

# Mostrar una parte de la matriz binaria.
B[1:min(8, nrow(B)), 1:min(8, ncol(B))]


## ------------------------------------------------------------------------------------------------------------------
# Comprobar la diagonal y la simetría.
all(diag(B) == 0)
all(B == t(B))


## ------------------------------------------------------------------------------------------------------------------
W <- nb2mat(
  lista_vecinos,
  style = "W",
  zero.policy = TRUE
)

rownames(W) <- as.character(granjas_utm_b6$granja_id)
colnames(W) <- as.character(granjas_utm_b6$granja_id)

# Mostrar una parte de la matriz estandarizada.
round(
  W[1:min(8, nrow(W)), 1:min(8, ncol(W))],
  3
)


## ------------------------------------------------------------------------------------------------------------------
sumas_filas_W <- rowSums(W)

summary(sumas_filas_W)


## ------------------------------------------------------------------------------------------------------------------
all.equal(W, t(W))


## ------------------------------------------------------------------------------------------------------------------
listw_W <- nb2listw(
  lista_vecinos,
  style = "W",
  zero.policy = TRUE
)

listw_W


## ------------------------------------------------------------------------------------------------------------------
identical(
  as.character(granjas_utm_b6$granja_id),
  rownames(W)
)


## ------------------------------------------------------------------------------------------------------------------
peso_medio_granja_b6 <- datos_b2 |>
  group_by(granja_id) |>
  summarise(
    peso_medio_kg = mean(peso_kg, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    granja_id = as.factor(granja_id)
  )

peso_espacial_b6 <- granjas_utm_b6 |>
  left_join(
    peso_medio_granja_b6,
    by = "granja_id"
  )

# Confirmar que no falta el resultado en ninguna granja.
sum(is.na(peso_espacial_b6$peso_medio_kg))


## ------------------------------------------------------------------------------------------------------------------
peso_espacial_b6 <- peso_espacial_b6 |>
  mutate(
    rezago_peso = lag.listw(
      listw_W,
      peso_medio_kg,
      zero.policy = TRUE
    )
  )

st_drop_geometry(peso_espacial_b6) |>
  select(
    granja_id,
    peso_medio_kg,
    rezago_peso
  ) |>
  head(10)


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  st_drop_geometry(peso_espacial_b6),
  aes(x = rezago_peso, y = peso_medio_kg)
) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(
    method = "lm",
    formula = y ~ x,
    se = TRUE,
    color = "steelblue4"
  ) +
  labs(
    title = "Peso de la granja y promedio de sus vecinas",
    x = "Rezago espacial del peso (kg)",
    y = "Peso medio de la granja (kg)"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
panel_granja_semana <- datos_b2 |>
  group_by(granja_id, tiempo) |>
  summarise(
    peso_medio_kg = mean(peso_kg, na.rm = TRUE),
    consumo_medio_kg_dia = mean(
      consumo_kg_dia,
      na.rm = TRUE
    ),
    edad_media_meses = mean(
      edad_inicio_meses,
      na.rm = TRUE
    ),
    temperatura_media_C = mean(
      temperatura_C,
      na.rm = TRUE
    ),
    humedad_media_pct = mean(
      humedad_pct,
      na.rm = TRUE
    ),
    n_animales = n_distinct(animal_id),
    n_mediciones = sum(!is.na(peso_kg)),
    .groups = "drop"
  ) |>
  mutate(
    granja_id = as.factor(granja_id)
  )

head(panel_granja_semana)


## ------------------------------------------------------------------------------------------------------------------
panel_granja_semana <- panel_granja_semana |>
  mutate(
    across(
      where(is.numeric),
      ~ ifelse(is.nan(.x), NA_real_, .x)
    )
  )


## ------------------------------------------------------------------------------------------------------------------
resumen_panel <- panel_granja_semana |>
  summarise(
    n_filas = n(),
    n_granjas = n_distinct(granja_id),
    n_semanas = n_distinct(tiempo),
    combinaciones_posibles =
      n_distinct(granja_id) * n_distinct(tiempo),
    proporcion_observada =
      n_filas / combinaciones_posibles
  )

resumen_panel


## ------------------------------------------------------------------------------------------------------------------
mediciones_panel <- panel_granja_semana |>
  count(granja_id, name = "n_semanas")

summary(mediciones_panel$n_semanas)


## ------------------------------------------------------------------------------------------------------------------
semana_sar <- panel_granja_semana |>
  filter(
    !is.na(peso_medio_kg),
    !is.na(consumo_medio_kg_dia),
    !is.na(edad_media_meses)
  ) |>
  count(tiempo, name = "n_granjas") |>
  arrange(desc(n_granjas), tiempo) |>
  slice_head(n = 1) |>
  pull(tiempo)

semana_sar


## ------------------------------------------------------------------------------------------------------------------
datos_semana_sar <- granjas_utm_b6 |>
  select(granja_id) |>
  left_join(
    panel_granja_semana |>
      filter(tiempo == semana_sar),
    by = "granja_id"
  )

st_drop_geometry(datos_semana_sar) |>
  select(
    granja_id,
    tiempo,
    peso_medio_kg,
    consumo_medio_kg_dia,
    edad_media_meses,
    n_animales
  ) |>
  head(10)


## ------------------------------------------------------------------------------------------------------------------
variables_sar <- c(
  "peso_medio_kg",
  "consumo_medio_kg_dia",
  "edad_media_meses",
  "temperatura_media_C"
)

completos_sar <- complete.cases(
  st_drop_geometry(datos_semana_sar)[, variables_sar]
)

table(completos_sar)


## ------------------------------------------------------------------------------------------------------------------
datos_semana_completos <- datos_semana_sar[
  completos_sar,
]

lista_vecinos_semana <- subset(
  lista_vecinos,
  subset = completos_sar
)

listw_semana <- nb2listw(
  lista_vecinos_semana,
  style = "W",
  zero.policy = TRUE
)

# Comprobar el número de observaciones y unidades espaciales.
nrow(datos_semana_completos)
length(listw_semana$neighbours)


## ------------------------------------------------------------------------------------------------------------------
card(lista_vecinos_semana)
table(card(lista_vecinos_semana) == 0)


## ------------------------------------------------------------------------------------------------------------------
ggplot(datos_semana_completos) +
  geom_sf(
    aes(color = peso_medio_kg),
    size = 3
  ) +
  scale_color_viridis_c(
    option = "C",
    name = "Peso medio\n(kg)"
  ) +
  labs(
    title = paste(
      "Peso medio por granja en la semana",
      semana_sar
    )
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
modelo_semana_lm <- lm(
  peso_medio_kg ~
    consumo_medio_kg_dia +
    edad_media_meses +
    temperatura_media_C,
  data = st_drop_geometry(datos_semana_completos)
)

summary(modelo_semana_lm)


## ------------------------------------------------------------------------------------------------------------------
residuos_lm_semana <- residuals(modelo_semana_lm)

moran_residuos <- moran.test(
  residuos_lm_semana,
  listw = listw_semana,
  zero.policy = TRUE
)

moran_residuos


## ------------------------------------------------------------------------------------------------------------------
moran.plot(
  residuos_lm_semana,
  listw = listw_semana,
  zero.policy = TRUE,
  labels = as.character(
    datos_semana_completos$granja_id
  ),
  xlab = "Residuo estandarizado",
  ylab = "Rezago espacial del residuo",
  main = "Diagrama de Moran de los residuos"
)


## ------------------------------------------------------------------------------------------------------------------
modelo_sar_semana <- lagsarlm(
  peso_medio_kg ~
    consumo_medio_kg_dia +
    edad_media_meses +
    temperatura_media_C,
  data = st_drop_geometry(datos_semana_completos),
  listw = listw_semana,
  zero.policy = TRUE,
  method = "eigen"
)

summary(modelo_sar_semana)


## ------------------------------------------------------------------------------------------------------------------
coeficientes_sar <- broom::tidy(
  modelo_sar_semana,
  conf.int = TRUE
)

coeficientes_sar


## ------------------------------------------------------------------------------------------------------------------
resumen_sar <- broom::glance(
  modelo_sar_semana
)

resumen_sar


## ------------------------------------------------------------------------------------------------------------------
AIC(modelo_semana_lm, modelo_sar_semana)

logLik(modelo_sar_semana)


## ------------------------------------------------------------------------------------------------------------------
impactos_sar <- impacts(
  modelo_sar_semana,
  listw = listw_semana,
  R = 500
)

summary(
  impactos_sar,
  zstats = TRUE,
  short = TRUE
)


## ------------------------------------------------------------------------------------------------------------------
residuos_sar <- residuals(modelo_sar_semana)

moran.test(
  residuos_sar,
  listw = listw_semana,
  zero.policy = TRUE
)


## ------------------------------------------------------------------------------------------------------------------
diagnostico_sar <- tibble(
  ajustado = fitted(modelo_sar_semana),
  residuo = residuos_sar
)

ggplot(
  diagnostico_sar,
  aes(x = ajustado, y = residuo)
) +
  geom_point(size = 2, alpha = 0.8) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "firebrick"
  ) +
  geom_smooth(
    method = "loess",
    formula = y ~ x,
    se = FALSE,
    color = "steelblue4"
  ) +
  labs(
    title = "Residuos del modelo SAR",
    x = "Peso medio ajustado (kg)",
    y = "Residuo (kg)"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
datos_semana_completos <- datos_semana_completos |>
  mutate(
    peso_ajustado_sar = as.numeric(
      fitted(modelo_sar_semana)
    ),
    residuo_sar = as.numeric(residuos_sar)
  )

ggplot(
  st_drop_geometry(datos_semana_completos),
  aes(x = peso_ajustado_sar, y = peso_medio_kg)
) +
  geom_point(size = 2, alpha = 0.8) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    color = "firebrick"
  ) +
  labs(
    title = "Peso observado y ajustado por el modelo SAR",
    x = "Peso medio ajustado (kg)",
    y = "Peso medio observado (kg)"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
k_vecinos_6 <- min(6, nrow(granjas_utm_b6) - 1)

vecinos_6 <- knearneigh(
  coords_utm,
  k = k_vecinos_6
) |>
  knn2nb() |>
  make.sym.nb()

vecinos_6_semana <- subset(
  vecinos_6,
  subset = completos_sar
)

listw_6_semana <- nb2listw(
  vecinos_6_semana,
  style = "W",
  zero.policy = TRUE
)

modelo_sar_6 <- lagsarlm(
  peso_medio_kg ~
    consumo_medio_kg_dia +
    edad_media_meses +
    temperatura_media_C,
  data = st_drop_geometry(datos_semana_completos),
  listw = listw_6_semana,
  zero.policy = TRUE,
  method = "eigen"
)

c(
  rho_4_vecinos = modelo_sar_semana$rho,
  rho_6_vecinos = modelo_sar_6$rho
)


## ------------------------------------------------------------------------------------------------------------------
panel_ordenado <- panel_granja_semana |>
  arrange(granja_id, tiempo) |>
  group_by(granja_id) |>
  mutate(
    peso_anterior_kg = lag(peso_medio_kg),
    cambio_peso_kg = peso_medio_kg -
      peso_anterior_kg
  ) |>
  ungroup()

panel_ordenado |>
  select(
    granja_id,
    tiempo,
    peso_medio_kg,
    peso_anterior_kg,
    cambio_peso_kg
  ) |>
  head(12)


## ------------------------------------------------------------------------------------------------------------------
panel_ordenado <- panel_ordenado |>
  group_by(granja_id) |>
  mutate(
    tiempo_anterior = lag(tiempo),
    diferencia_semanas = tiempo - tiempo_anterior,
    peso_t_1 = if_else(
      diferencia_semanas == 1,
      peso_anterior_kg,
      NA_real_
    )
  ) |>
  ungroup()

count(panel_ordenado, diferencia_semanas)


## ------------------------------------------------------------------------------------------------------------------
granjas_orden_W <- as.character(
  granjas_utm_b6$granja_id
)

calcular_rezago_semana <- function(tabla_semana) {
  tabla_completa <- tibble(
    granja_id = factor(
      granjas_orden_W,
      levels = levels(granjas_utm_b6$granja_id)
    )
  ) |>
    left_join(
      tabla_semana,
      by = "granja_id"
    )

  if (anyNA(tabla_completa$peso_medio_kg)) {
    tabla_completa$rezago_espacial_peso <- NA_real_
  } else {
    tabla_completa$rezago_espacial_peso <- lag.listw(
      listw_W,
      tabla_completa$peso_medio_kg,
      zero.policy = TRUE
    )
  }

  tabla_completa
}

panel_con_rezago_espacial <- panel_granja_semana |>
  group_split(tiempo) |>
  map_dfr(calcular_rezago_semana)

head(panel_con_rezago_espacial)


## ------------------------------------------------------------------------------------------------------------------
semanas_mostrar <- sort(
  unique(panel_granja_semana$tiempo)
)

semanas_mostrar <- semanas_mostrar[
  unique(round(seq(
    1,
    length(semanas_mostrar),
    length.out = min(6, length(semanas_mostrar))
  )))
]

mapas_panel <- granjas_utm_b6 |>
  select(granja_id) |>
  crossing(
    tiempo = semanas_mostrar
  ) |>
  left_join(
    panel_granja_semana,
    by = c("granja_id", "tiempo")
  )

# Convertir mapas_panel en un objeto sf
mapas_panel_sf <- st_as_sf(
  mapas_panel,
  sf_column_name = "geometry",
  crs = 32614
)

# Comprobar la estructura
class(mapas_panel_sf)
st_geometry(mapas_panel_sf)

# Crear el mapa espacio-temporal
ggplot(mapas_panel_sf) +
  geom_sf(
    aes(color = peso_medio_kg),
    size = 2.3
  ) +
  facet_wrap(~ tiempo) +
  scale_color_viridis_c(
    option = "C",
    na.value = "grey80",
    name = "Peso medio\n(kg)"
  ) +
  labs(
    title = "Evolución espacio-temporal del peso medio",
    subtitle = "Selección de semanas del seguimiento"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
#| eval: false

# # install.packages("splm")
# library(splm)
# 
# panel_splm <- panel_granja_semana |>
#   drop_na(
#     peso_medio_kg,
#     consumo_medio_kg_dia,
#     edad_media_meses,
#     temperatura_media_C
#   ) |>
#   arrange(granja_id, tiempo)
# 
# modelo_panel_sar <- spml(
#   peso_medio_kg ~
#     consumo_medio_kg_dia +
#     edad_media_meses +
#     temperatura_media_C,
#   data = panel_splm,
#   index = c("granja_id", "tiempo"),
#   listw = listw_W,
#   model = "within",
#   effect = "twoways",
#   lag = TRUE,
#   spatial.error = "none"
# )
# 
# summary(modelo_panel_sar)

