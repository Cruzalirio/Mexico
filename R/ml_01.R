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
# Instalar los paquetes si es necesario.
# install.packages(c("tidyverse", "tidymodels", "rsample", "sf"))

library(tidyverse)
library(tidymodels)
library(sf)

# Comprobar los datos disponibles.
dim(datos_b2)
glimpse(datos_b2)


## ------------------------------------------------------------------------------------------------------------------
# Ejemplo de variables para regresión.
datos_regresion_ml <- datos_b2 |>
  select(
    animal_id, granja_id, tiempo, estado = zona,
    peso_kg, consumo_kg_dia, edad_inicio_meses,
    sexo, raza, temperatura_C, humedad_pct
  ) |>
  drop_na()

# La respuesta es continua.
summary(datos_regresion_ml$peso_kg)


## ------------------------------------------------------------------------------------------------------------------
# Ejemplo de respuesta para clasificación.
datos_clasificacion_ml <- datos_b2 |>
  mutate(
    salud_alta_ml = factor(
      if_else(
        as.numeric(as.character(salud_score_0_4)) >= 3,
        "Sí",
        "No"
      ),
      levels = c("No", "Sí")
    )
  ) |>
  select(
    animal_id, granja_id, tiempo,
    salud_alta_ml, consumo_kg_dia,
    edad_inicio_meses, sexo, raza,
    temperatura_C, humedad_pct
  ) |>
  drop_na()

count(datos_clasificacion_ml, salud_alta_ml)


## ------------------------------------------------------------------------------------------------------------------
# Una fila por granja para evitar que las granjas con más registros
# dominen el análisis no supervisado.
perfiles_granja <- datos_b2 |>
  group_by(granja_id, zona, sistema_produccion) |>
  summarise(
    peso_medio = mean(peso_kg, na.rm = TRUE),
    consumo_medio = mean(consumo_kg_dia, na.rm = TRUE),
    agua_media = mean(consumo_agua_l_dia, na.rm = TRUE),
    temperatura_media = mean(temperatura_C, na.rm = TRUE),
    humedad_media = mean(humedad_pct, na.rm = TRUE),
    visitas_medias = mean(n_visitas_veterinarias, na.rm = TRUE),
    .groups = "drop"
  )

glimpse(perfiles_granja)


## ------------------------------------------------------------------------------------------------------------------
resumen_unidades <- datos_b2 |>
  summarise(
    mediciones = n(),
    animales = n_distinct(animal_id),
    granjas = n_distinct(granja_id),
    semanas = n_distinct(tiempo)
  )

resumen_unidades


## ------------------------------------------------------------------------------------------------------------------
set.seed(2026)

particion_animales <- group_initial_split(
  datos_regresion_ml,
  group = animal_id,
  prop = 0.80
)

entrenamiento <- training(particion_animales)
prueba <- testing(particion_animales)

entrenamiento |>
  summarise(
    filas = n(),
    animales = n_distinct(animal_id),
    granjas = n_distinct(granja_id)
  )

prueba |>
  summarise(
    filas = n(),
    animales = n_distinct(animal_id),
    granjas = n_distinct(granja_id)
  )


## ------------------------------------------------------------------------------------------------------------------
# Verificar que ningún animal aparezca en ambos conjuntos.
intersect(
  unique(entrenamiento$animal_id),
  unique(prueba$animal_id)
)


## ------------------------------------------------------------------------------------------------------------------
# Variables disponibles para una predicción didáctica del peso.
predictores_candidatos <- entrenamiento |>
  select(
    consumo_kg_dia,
    edad_inicio_meses,
    sexo,
    raza,
    tiempo,
    temperatura_C,
    humedad_pct
  )

glimpse(predictores_candidatos)


## ------------------------------------------------------------------------------------------------------------------
receta_intro <- recipe(
  peso_kg ~
    consumo_kg_dia + edad_inicio_meses +
    sexo + raza + tiempo +
    temperatura_C + humedad_pct,
  data = entrenamiento
) |>
  step_unknown(all_nominal_predictors()) |>
  step_novel(all_nominal_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())

receta_intro


## ------------------------------------------------------------------------------------------------------------------
# Estimar las transformaciones solamente con entrenamiento.
receta_preparada <- prep(receta_intro)

# Aplicarlas a entrenamiento y prueba.
entrenamiento_procesado <- bake(
  receta_preparada,
  new_data = NULL
)

prueba_procesada <- bake(
  receta_preparada,
  new_data = prueba
)

glimpse(entrenamiento_procesado)


## ------------------------------------------------------------------------------------------------------------------
set.seed(2026)

folds_animales <- group_vfold_cv(
  entrenamiento,
  group = animal_id,
  v = 5
)

folds_animales


## ------------------------------------------------------------------------------------------------------------------
preguntas_ml <- tribble(
  ~pregunta, ~respuesta, ~unidad, ~tipo,
  "Predecir peso semanal", "peso_kg", "medición", "Regresión",
  "Predecir salud alta", "salud_alta_ml", "medición", "Clasificación",
  "Predecir producción de lana", "produccion_lana_kg", "animal", "Regresión",
  "Predecir muchas visitas", "visitas_altas", "animal", "Clasificación",
  "Agrupar granjas productivas", "Sin respuesta", "granja", "Clustering"
)

preguntas_ml


## ------------------------------------------------------------------------------------------------------------------
media_entrenamiento <- mean(
  entrenamiento$peso_kg,
  na.rm = TRUE
)

prediccion_base <- prueba |>
  transmute(
    peso_kg,
    .pred = media_entrenamiento
  )

metric_set(rmse, mae, rsq)(
  prediccion_base,
  truth = peso_kg,
  estimate = .pred
)

