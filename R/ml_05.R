## ------------------------------------------------------------------------------------------------------------------
# install.packages(c(
#   "tidyverse", "cluster", "factoextra",
#   "tidymodels", "sf", "patchwork"
# ))

library(tidyverse)
library(cluster)
library(factoextra)
library(tidymodels)
library(sf)
library(patchwork)


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

set.seed(2026)

animales_muestra <- datos |>
  distinct(animal_id) |>
  slice_sample(prop = 0.2)

datos_b2 <- datos |>
  semi_join(animales_muestra, by = "animal_id")


datos_b2 <- datos_b2 |>
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
perfiles_granja_ml <- datos_b2 |>
  group_by(
    granja_id,
    zona,
    sistema_produccion,
    tamano_granja,
    longitud,
    latitud
  ) |>
  summarise(
    peso_medio = mean(peso_kg, na.rm = TRUE),
    ganancia_media = mean(ganancia_semanal_kg, na.rm = TRUE),
    consumo_medio = mean(consumo_kg_dia, na.rm = TRUE),
    agua_media = mean(consumo_agua_l_dia, na.rm = TRUE),
    actividad_media = mean(actividad_h_dia, na.rm = TRUE),
    temperatura_media = mean(temperatura_C, na.rm = TRUE),
    humedad_media = mean(humedad_pct, na.rm = TRUE),
    precipitacion_media = mean(precipitacion_mm, na.rm = TRUE),
    salud_media = mean(
      as.numeric(as.character(salud_score_0_4)),
      na.rm = TRUE
    ),
    visitas_medias = mean(n_visitas_veterinarias, na.rm = TRUE),
    lana_media = mean(produccion_lana_kg, na.rm = TRUE),
    n_animales = n_distinct(animal_id),
    .groups = "drop"
  ) |>
  mutate(
    across(
      where(is.numeric),
      ~ ifelse(is.nan(.x), NA_real_, .x)
    )
  ) |>
  drop_na()

glimpse(perfiles_granja_ml)


## ------------------------------------------------------------------------------------------------------------------
variables_cluster <- perfiles_granja_ml |>
  select(
    peso_medio,
    ganancia_media,
    consumo_medio,
    agua_media,
    actividad_media,
    temperatura_media,
    humedad_media,
    precipitacion_media,
    salud_media,
    visitas_medias,
    lana_media
  )

receta_cluster <- recipe(~ ., data = variables_cluster) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())

matriz_cluster <- receta_cluster |>
  prep() |>
  bake(new_data = NULL) |>
  as.matrix()

summary(matriz_cluster[, 1:min(5, ncol(matriz_cluster))])


## ------------------------------------------------------------------------------------------------------------------
fviz_nbclust(
  matriz_cluster,
  kmeans,
  method = "wss",
  k.max = 8
) +
  labs(
    title = "Método del codo"
  )


## ------------------------------------------------------------------------------------------------------------------
fviz_nbclust(
  matriz_cluster,
  kmeans,
  method = "silhouette",
  k.max = 8
) +
  labs(
    title = "Silueta media según el número de grupos"
  )


## ------------------------------------------------------------------------------------------------------------------
set.seed(2026)

k_seleccionado <- 3

modelo_kmeans <- kmeans(
  matriz_cluster,
  centers = k_seleccionado,
  nstart = 100,
  iter.max = 100
)

modelo_kmeans


## ------------------------------------------------------------------------------------------------------------------
perfiles_granja_cluster <- perfiles_granja_ml |>
  mutate(
    cluster_kmeans = factor(modelo_kmeans$cluster)
  )

count(perfiles_granja_cluster, cluster_kmeans)


## ------------------------------------------------------------------------------------------------------------------
resumen_clusters <- perfiles_granja_cluster |>
  group_by(cluster_kmeans) |>
  summarise(
    n_granjas = n(),
    peso_medio = mean(peso_medio),
    consumo_medio = mean(consumo_medio),
    temperatura_media = mean(temperatura_media),
    humedad_media = mean(humedad_media),
    salud_media = mean(salud_media),
    visitas_medias = mean(visitas_medias),
    lana_media = mean(lana_media),
    .groups = "drop"
  )

resumen_clusters


## ------------------------------------------------------------------------------------------------------------------
perfiles_granja_cluster |>
  count(cluster_kmeans, sistema_produccion) |>
  group_by(cluster_kmeans) |>
  mutate(
    porcentaje = 100 * n / sum(n)
  ) |>
  ggplot(
    aes(
      x = cluster_kmeans,
      y = porcentaje,
      fill = sistema_produccion
    )
  ) +
  geom_col() +
  labs(
    title = "Sistema productivo dentro de cada conglomerado",
    x = "Conglomerado",
    y = "Porcentaje de granjas",
    fill = "Sistema productivo"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
pca_granjas <- prcomp(
  matriz_cluster,
  center = FALSE,
  scale. = FALSE
)

summary(pca_granjas)


## ------------------------------------------------------------------------------------------------------------------
varianza_pca <- tibble(
  componente = seq_along(pca_granjas$sdev),
  proporcion = pca_granjas$sdev^2 /
    sum(pca_granjas$sdev^2),
  acumulada = cumsum(proporcion)
)

varianza_pca


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  varianza_pca,
  aes(x = componente, y = proporcion)
) +
  geom_col(fill = "steelblue4") +
  geom_line(aes(y = acumulada), color = "firebrick") +
  geom_point(aes(y = acumulada), color = "firebrick") +
  labs(
    title = "Varianza explicada por los componentes",
    x = "Componente principal",
    y = "Proporción de varianza"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
coordenadas_pca <- as_tibble(
  pca_granjas$x[, 1:2]
) |>
  bind_cols(
    perfiles_granja_cluster |>
      select(
        granja_id,
        zona,
        sistema_produccion,
        cluster_kmeans
      )
  )

ggplot(
  coordenadas_pca,
  aes(
    x = PC1,
    y = PC2,
    color = cluster_kmeans,
    shape = sistema_produccion
  )
) +
  geom_point(size = 3, alpha = 0.9) +
  labs(
    title = "Granjas en los dos primeros componentes",
    color = "Conglomerado",
    shape = "Sistema productivo"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
cargas_pca <- as_tibble(
  pca_granjas$rotation[, 1:2],
  rownames = "variable"
)

cargas_pca |>
  arrange(desc(abs(PC1)))


## ------------------------------------------------------------------------------------------------------------------
distancias_granjas <- dist(
  matriz_cluster,
  method = "euclidean"
)

modelo_jerarquico <- hclust(
  distancias_granjas,
  method = "ward.D2"
)

plot(
  modelo_jerarquico,
  labels = perfiles_granja_ml$granja_id,
  main = "Dendrograma de las granjas",
  xlab = "Granja",
  sub = "Método de Ward"
)

rect.hclust(
  modelo_jerarquico,
  k = k_seleccionado,
  border = 2:4
)


## ------------------------------------------------------------------------------------------------------------------
perfiles_granja_cluster <- perfiles_granja_cluster |>
  mutate(
    cluster_jerarquico = factor(
      cutree(
        modelo_jerarquico,
        k = k_seleccionado
      )
    )
  )

 table(
  Kmeans = perfiles_granja_cluster$cluster_kmeans,
  Jerarquico = perfiles_granja_cluster$cluster_jerarquico
)


## ------------------------------------------------------------------------------------------------------------------
siluetas_kmeans <- silhouette(
  modelo_kmeans$cluster,
  distancias_granjas
)

summary(siluetas_kmeans)

fviz_silhouette(siluetas_kmeans) +
  labs(
    title = "Silueta de los conglomerados K-means"
  )


## ------------------------------------------------------------------------------------------------------------------
# Instalar los paquetes si todavía no están instalados.
# install.packages(c("sf", "geodata", "terra", "dplyr", "ggplot2"))

library(sf)
library(geodata)
library(terra)

# Límites nacionales de México.
mexico <- geodata::gadm(
  country = "MEX",
  level = 0,
  path = "mapas"
)

# Límites de los estados mexicanos.
estados <- geodata::gadm(
  country = "MEX",
  level = 1,
  path = "mapas"
)

mexico_sf <- st_as_sf(mexico)
estados_sf <- st_as_sf(estados)

estados_region_sf <- estados_sf %>%
  filter(
    NAME_1 %in% c(
      "Hidalgo",
      "México",
      "Tlaxcala",
      "Veracruz"
    )
  )

# Comprobar qué estados se han seleccionado.
sort(unique(estados_region_sf$NAME_1))

# Comprobar cuántos polígonos contiene el objeto.
nrow(estados_region_sf)


## ------------------------------------------------------------------------------------------------------------------
granjas_cluster_sf <- perfiles_granja_cluster |>
  st_as_sf(
    coords = c("longitud", "latitud"),
    crs = 4326,
    remove = FALSE
  )

delta=0.4
ggplot() +
  geom_sf(
    data = estados_region_sf,
    aes(fill = NAME_1),
    color = "grey45",
    linewidth = 0.4,
    alpha = 0.60
  ) +
  geom_sf(
    data = granjas_cluster_sf,
    aes(color = cluster_kmeans),
    size = 3
  ) +
  scale_fill_brewer(
    palette = "Pastel1",
    name = "Estado"
  ) +
  labs(
    title = "Distribución espacial de los perfiles de granja",
    color = "Conglomerado",
    x = "Longitud",
    y = "Latitud"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )+coord_sf(
    xlim = range(granjas_cluster_sf$longitud, na.rm = TRUE) +
      c(-delta, delta),
    ylim = range(granjas_cluster_sf$latitud, na.rm = TRUE) +
      c(-delta, delta),
    expand = FALSE
  ) 

