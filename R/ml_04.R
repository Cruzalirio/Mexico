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
# install.packages(c("tidyverse", "tidymodels", "nnet", "vip"))

library(tidyverse)
library(tidymodels)
library(nnet)
library(vip)

tidymodels_prefer()


## ------------------------------------------------------------------------------------------------------------------
datos_nn <- datos_b2 |>
  select(
    animal_id, granja_id, tiempo,
    peso_kg, consumo_kg_dia,
    consumo_agua_l_dia, edad_inicio_meses,
    sexo, raza, temperatura_C,
    humedad_pct, precipitacion_mm,
    actividad_h_dia, sistema_produccion, zona
  ) |>
  drop_na()

set.seed(2026)
split_nn <- group_initial_split(
  datos_nn,
  group = animal_id,
  prop = 0.80
)

train_nn <- training(split_nn)
test_nn <- testing(split_nn)

set.seed(2026)
folds_nn <- group_vfold_cv(
  train_nn,
  group = animal_id,
  v = 5
)


## ------------------------------------------------------------------------------------------------------------------
receta_nn <- recipe(
  peso_kg ~
    consumo_kg_dia + consumo_agua_l_dia +
    edad_inicio_meses + sexo + raza + tiempo +
    temperatura_C + humedad_pct + precipitacion_mm +
    actividad_h_dia + sistema_produccion + zona,
  data = train_nn
) |>
  step_unknown(all_nominal_predictors()) |>
  step_novel(all_nominal_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())

receta_nn


## ------------------------------------------------------------------------------------------------------------------
modelo_nn <- mlp(
  hidden_units = tune(),
  penalty = tune(),
  epochs = tune()
) |>
  set_engine(
    "nnet",
    trace = FALSE,
    MaxNWts = 20000
  ) |>
  set_mode("regression")

flujo_nn <- workflow() |>
  add_recipe(receta_nn) |>
  add_model(modelo_nn)


## ------------------------------------------------------------------------------------------------------------------
rejilla_nn <- grid_latin_hypercube(
  hidden_units(range = c(2L, 10L)),
  penalty(range = c(-4, -1)),
  epochs(range = c(20L, 100L)),
  size = 3
)

set.seed(2026)
resultados_nn <- tune_grid(
  flujo_nn,
  resamples = vfold_cv(train_nn, v = 2),
  grid = rejilla_nn,
  metrics = metric_set(rmse),
  control = control_grid(
    save_pred = FALSE,
    verbose = TRUE
  )
)

show_best(
  resultados_nn,
  metric = "rmse",
  n = 3
)


## ------------------------------------------------------------------------------------------------------------------
mejor_nn <- select_best(
  resultados_nn,
  metric = "rmse"
)

flujo_nn_final <- finalize_workflow(
  flujo_nn,
  mejor_nn
)

resultado_nn <- last_fit(
  flujo_nn_final,
  split = split_nn,
  metrics = metric_set(rmse, mae, rsq)
)

collect_metrics(resultado_nn)


## ------------------------------------------------------------------------------------------------------------------
predicciones_nn <- collect_predictions(
  resultado_nn
)

ggplot(
  predicciones_nn,
  aes(x = .pred, y = peso_kg)
) +
  geom_point(alpha = 0.20, size = 1) +
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = "dashed",
    color = "firebrick"
  ) +
  labs(
    title = "Peso observado y predicho por la red neuronal",
    x = "Peso predicho (kg)",
    y = "Peso observado (kg)"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
modelo_lineal_referencia <- linear_reg() |>
  set_engine("lm")

flujo_lineal_referencia <- workflow() |>
  add_recipe(receta_nn) |>
  add_model(modelo_lineal_referencia)

resultado_lineal_referencia <- last_fit(
  flujo_lineal_referencia,
  split = split_nn,
  metrics = metric_set(rmse, mae, rsq)
)

comparacion_nn <- bind_rows(
  collect_metrics(resultado_lineal_referencia) |>
    mutate(modelo = "Regresión lineal"),
  collect_metrics(resultado_nn) |>
    mutate(modelo = "Red neuronal")
)

comparacion_nn


## ------------------------------------------------------------------------------------------------------------------
predicciones_nn_detalle <- predicciones_nn |>
  left_join(
    test_nn |>
      mutate(.row = row_number()) |>
      select(.row, tiempo, granja_id, animal_id),
    by = ".row"
  ) |>
  mutate(
    error = peso_kg - .pred,
    error_absoluto = abs(error)
  )

predicciones_nn_detalle |>
  group_by(tiempo) |>
  summarise(
    MAE = mean(error_absoluto),
    .groups = "drop"
  ) |>
  ggplot(aes(x = tiempo, y = MAE)) +
  geom_line(color = "steelblue4") +
  geom_point(color = "steelblue4") +
  labs(
    title = "Error absoluto medio por semana",
    x = "Semana",
    y = "MAE (kg)"
  ) +
  theme_minimal()

