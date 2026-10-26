## ------------------------------------------------------------------------------------------------------------------
# Instalar los paquetes si es necesario.
# install.packages(c("readxl", "readr", "dplyr", "ggplot2",
#                    "corrplot", "naniar"))

library(readxl)
library(tidyverse)
library(corrplot)
library(naniar)

# Consultar la carpeta de trabajo.
getwd()

# Nombre del archivo Excel.
archivo <- "../datos/base_borregos_espaciotemporal_Hidalgo.xlsx"

# Consultar las hojas disponibles.
excel_sheets(archivo)

# Importar la tabla longitudinal.
datos <- read_excel(archivo, sheet = "datos_long")
# Examinar el tamaño de la tabla.
dim(datos)

# Mostrar las primeras filas.
head(datos)

# Consultar los nombres de las variables.
names(datos)

# Examinar los tipos de datos.
str(datos)


## ------------------------------------------------------------------------------------------------------------------
# Importar un CSV separado por comas.
datos_csv <- read_csv("../datos/mastitis.csv",show_col_types = FALSE)

# Si el archivo utiliza punto y coma:
# datos_csv <- read_delim(
#   "datos_long.csv",
#   delim = ";",
#   show_col_types = FALSE
# )


## ------------------------------------------------------------------------------------------------------------------
# Revisar los tipos de las variables.
glimpse(datos)

# Examinar algunas columnas.
class(datos$animal_id)
class(datos$fecha)
class(datos$peso_kg)


## ------------------------------------------------------------------------------------------------------------------
datos |> 
  summarise(
    n_filas = n(),
    n_animales = n_distinct(animal_id),
    n_granjas = n_distinct(granja_id),
    n_semanas = n_distinct(tiempo),
    n_fechas = n_distinct(fecha)
  )


## ------------------------------------------------------------------------------------------------------------------
peso_semanal <- datos |> 
  group_by(tiempo) |> 
  summarise(
    peso_medio_kg = mean(peso_kg, na.rm = TRUE),
    n_mediciones = sum(!is.na(peso_kg)),
    .groups = "drop"
  )

ggplot(peso_semanal,
       aes(x = tiempo, y = peso_medio_kg)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  labs(
    title = "Evolución temporal del peso medio",
    x = "Semana de seguimiento",
    y = "Peso medio (kg)"
  )


## ------------------------------------------------------------------------------------------------------------------
datos |> 
  summarise(
    n_animales = n_distinct(animal_id),
    n_granjas = n_distinct(granja_id),
    n_razas = n_distinct(raza),
    n_sistemas = n_distinct(sistema_produccion)
  )

count(datos, raza)
count(datos, sistema_produccion)
count(datos, tratamiento)


## ------------------------------------------------------------------------------------------------------------------
mediciones_por_animal <- datos |> 
  count(animal_id, name = "n_mediciones")

summary(mediciones_por_animal$n_mediciones)

ggplot(mediciones_por_animal,
       aes(x = n_mediciones)) +
  geom_bar() +
  labs(
    title = "Número de mediciones por animal",
    x = "Número de mediciones",
    y = "Número de animales"
  )


## ------------------------------------------------------------------------------------------------------------------
animales_ejemplo <- datos  |> 
  distinct(animal_id) |>
  slice_head(n = 8)

datos_ejemplo <- datos |>
  semi_join(animales_ejemplo, by = "animal_id")

ggplot(datos_ejemplo,
       aes(x = tiempo, y = peso_kg,
           group = animal_id, color = animal_id)) +
  geom_line(na.rm = TRUE) +
  labs(
    title = "Trayectorias individuales del peso",
    x = "Semana de seguimiento",
    y = "Peso (kg)",
    color = "Animal"
  ) 


## ------------------------------------------------------------------------------------------------------------------
peso_granja <- datos |>
  group_by(granja_id) |>
  summarise(
    peso_medio_kg = mean(peso_kg, na.rm = TRUE),
    n_animales = n_distinct(animal_id),
    n_mediciones = sum(!is.na(peso_kg)),
    .groups = "drop"
  )

ggplot(peso_granja,
       aes(x = reorder(granja_id, peso_medio_kg),
           y = peso_medio_kg)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Peso medio por granja",
    x = "Granja",
    y = "Peso medio (kg)"
  )


## ------------------------------------------------------------------------------------------------------------------
variables_cor <- datos |>
  select(
    peso_kg,
    ganancia_semanal_kg,
    consumo_kg_dia,
    consumo_agua_l_dia,
    actividad_h_dia,
    temperatura_C,
    humedad_pct,
    precipitacion_mm
  )

matriz_cor <- cor(
  variables_cor,
  use = "pairwise.complete.obs",
  method = "pearson"
)

round(matriz_cor, 2)

corrplot(
  matriz_cor,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  number.cex = 0.65
)


## ------------------------------------------------------------------------------------------------------------------
resumen_ausentes <- tibble(
  variable = names(datos),
  n_ausentes = sapply(datos, function(x) sum(is.na(x))),
  porcentaje_ausente = sapply(
    datos,
    function(x) mean(is.na(x)) * 100
  )
) |>
  arrange(desc(porcentaje_ausente))

resumen_ausentes


## ------------------------------------------------------------------------------------------------------------------
set.seed(2026)

datos_na <- datos

variables_faltantes <- c(
  "peso_kg",
  "consumo_kg_dia",
  "consumo_agua_l_dia",
  "temperatura_C",
  "humedad_pct"
)

for (variable in variables_faltantes) {
  n <- nrow(datos_na)
  filas_na <- sample(
    seq_len(n),
    size = floor(0.05 * n),
    replace = FALSE
  )

  datos_na[[variable]][filas_na] <- NA
}


## ------------------------------------------------------------------------------------------------------------------
resumen_na <- tibble(
  variable = names(datos_na),
  n_ausentes = sapply(datos_na, function(x) sum(is.na(x))),
  porcentaje_ausente = sapply(
    datos_na,
    function(x) mean(is.na(x)) * 100
  )
) |>
  arrange(desc(porcentaje_ausente))

resumen_na


## ------------------------------------------------------------------------------------------------------------------
ggplot(resumen_na,
       aes(x = reorder(variable, porcentaje_ausente),
           y = porcentaje_ausente)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Porcentaje de valores ausentes por variable",
    x = "Variable",
    y = "Valores ausentes (%)"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
vis_miss(
  datos_na |>
    select(
      peso_kg,
      consumo_kg_dia,
      consumo_agua_l_dia,
      temperatura_C,
      humedad_pct
    )
)


## ------------------------------------------------------------------------------------------------------------------
set.seed(2026)

muestra_na <- datos_na |>
  slice_sample(n = min(200, nrow(datos_na))) |>
  select(
    animal_id,
    granja_id,
    tiempo,
    peso_kg,
    consumo_kg_dia,
    consumo_agua_l_dia,
    temperatura_C,
    humedad_pct
  )

vis_miss(muestra_na)


## ------------------------------------------------------------------------------------------------------------------
duplicados <- datos |>
  count(animal_id, tiempo, name = "n") |>
  filter(n > 1)

duplicados


## ------------------------------------------------------------------------------------------------------------------
filas_duplicadas <- datos |>
  semi_join(
    duplicados,
    by = c("animal_id", "tiempo")
  ) |>
  arrange(animal_id, tiempo)

filas_duplicadas


## ------------------------------------------------------------------------------------------------------------------
coordenadas_granja <- datos |>
  group_by(granja_id) |>
  summarise(
    n_latitudes = n_distinct(latitud, na.rm = TRUE),
    n_longitudes = n_distinct(longitud, na.rm = TRUE),
    .groups = "drop"
  )

coordenadas_granja |>
  filter(
    n_latitudes > 1 |
    n_longitudes > 1
  )


## ------------------------------------------------------------------------------------------------------------------
# Instalar los paquetes si todavía no están instalados.
# install.packages(c("sf", "geodata", "terra", "dplyr", "ggplot2"))

library(sf)
library(geodata)
library(terra)


## ------------------------------------------------------------------------------------------------------------------
granjas <- read_excel(archivo, sheet = "granjas")

head(granjas[, c("granja_id", "longitud", "latitud")])

summary(granjas[, c("longitud", "latitud")])

sum(is.na(granjas$longitud))
sum(is.na(granjas$latitud))


## ------------------------------------------------------------------------------------------------------------------
granjas_sf <- st_as_sf(
  granjas,
  coords = c("longitud", "latitud"),
  crs = 4326,
  remove = FALSE
)

class(granjas_sf)

head(granjas_sf)


## ------------------------------------------------------------------------------------------------------------------
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


## ------------------------------------------------------------------------------------------------------------------
mexico_sf <- st_as_sf(mexico)
estados_sf <- st_as_sf(estados)

# Consultar los nombres de los estados disponibles.
sort(unique(estados_sf$NAME_1))


## ------------------------------------------------------------------------------------------------------------------
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
ggplot() +
  geom_sf(
    data = mexico_sf,
    fill = "grey95",
    color = "grey55",
    linewidth = 0.3
  ) +
  geom_sf(
    data = estados_region_sf,
    aes(fill = NAME_1),
    color = "steelblue4",
    linewidth = 0.6
  ) +
  geom_sf(
    data = granjas_sf,
    aes(color = tamano_granja),
    size = 2.5,
    alpha = 0.9
  ) +
  scale_fill_brewer(
    palette = "Pastel1",
    name = "Estado"
  ) +
  scale_color_brewer(
    palette = "Dark2",
    name = "Tamaño de granja"
  ) +
  labs(
    title = "Granjas ovinas simuladas en el centro-oriente de México",
    subtitle = "Hidalgo, Estado de México, Tlaxcala y Veracruz",
    x = "Longitud",
    y = "Latitud",
    caption = "Datos simulados; no representan granjas reales."
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
delta <- 0.4


## ------------------------------------------------------------------------------------------------------------------
ggplot() +
  geom_sf(
    data = estados_region_sf,
    aes(fill = NAME_1),
    color = "grey35",
    linewidth = 0.5
  ) +
  geom_sf(
    data = granjas_sf,
    aes(
      color = zona,
      shape = sistema_produccion
    ),
    size = 3,
    alpha = 0.9
  ) +
  scale_fill_brewer(
    palette = "Pastel1",
    name = "Estado"
  ) +
  labs(
    title = "Distribución espacial de las granjas",
    subtitle = "Cuatro estados: Hidalgo, Estado de México, Tlaxcala y Veracruz",
    color = "Zona",
    shape = "Sistema productivo",
    x = "Longitud",
    y = "Latitud"
  ) +
  coord_sf(
    xlim = range(granjas$longitud, na.rm = TRUE) +
      c(-delta, delta),
    ylim = range(granjas$latitud, na.rm = TRUE) +
      c(-delta, delta),
    expand = FALSE
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
# Unir los cuatro polígonos en una geometría regional.
region_union <- st_union(estados_region_sf)

# Comprobar si cada granja está dentro de la región seleccionada.
dentro_region <- lengths(
  st_intersects(granjas_sf, region_union)
) > 0

# Resumen de la comprobación.
table(dentro_region)

# Mostrar las granjas que quedan fuera de los cuatro estados.
granjas_fuera <- granjas_sf[!dentro_region, ]

granjas_fuera[, c("granja_id", "longitud", "latitud")]

