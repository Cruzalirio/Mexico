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
# install.packages(c(
#   "tidyverse", "tidymodels", "rpart", "rpart.plot",
#   "ranger", "xgboost", "vip"
# ))

library(tidyverse)
library(tidymodels)
library(rpart.plot)
library(ranger)
library(xgboost)
library(vip)

tidymodels_prefer()


## ------------------------------------------------------------------------------------------------------------------
datos_arboles <- datos_b2 |>
  mutate(
    salud_num = as.numeric(as.character(salud_score_0_4)),
    salud_alta_ml = factor(
      if_else(salud_num >= 3, "Sí", "No"),
      levels = c("No", "Sí")
    )
  ) |>
  select(
    animal_id, granja_id, tiempo,
    peso_kg, salud_alta_ml,
    consumo_kg_dia, consumo_agua_l_dia,
    edad_inicio_meses, sexo, raza,
    temperatura_C, humedad_pct,
    precipitacion_mm, actividad_h_dia,
    sistema_produccion, zona
  ) |>
  drop_na()

set.seed(2026)
split_arboles <- group_initial_split(
  datos_arboles,
  group = animal_id,
  prop = 0.80
)

train_arboles <- training(split_arboles)
test_arboles <- testing(split_arboles)

set.seed(2026)
folds_arboles <- group_vfold_cv(
  train_arboles,
  group = animal_id,
  v = 5
)


## ------------------------------------------------------------------------------------------------------------------
receta_arbol_peso <- recipe(
  peso_kg ~
    consumo_kg_dia + consumo_agua_l_dia +
    edad_inicio_meses + sexo + raza + tiempo +
    temperatura_C + humedad_pct + precipitacion_mm +
    actividad_h_dia + sistema_produccion + zona,
  data = train_arboles
) |>
  step_unknown(all_nominal_predictors()) |>
  step_novel(all_nominal_predictors())

modelo_arbol_reg <- decision_tree(
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()
) |>
  set_engine("rpart") |>
  set_mode("regression")

flujo_arbol_reg <- workflow() |>
  add_recipe(receta_arbol_peso) |>
  add_model(modelo_arbol_reg)


## ------------------------------------------------------------------------------------------------------------------
rejilla_arbol <- grid_random(
  cost_complexity(range = c(-5, -1)),
  tree_depth(range = c(2L, 12L)),
  min_n(range = c(5L, 50L)),
  size = 10
)

folds_arboles_rapidos <- vfold_cv(
  train_arboles,
  v = 3
)

set.seed(2026)
resultados_arbol_reg <- tune_grid(
  flujo_arbol_reg,
  resamples = folds_arboles_rapidos,
  grid = rejilla_arbol,
  metrics = metric_set(rmse),
  control = control_grid(verbose = TRUE)
)

show_best(resultados_arbol_reg, metric = "rmse", n = 5)


## ------------------------------------------------------------------------------------------------------------------
mejor_arbol <- select_best(
  resultados_arbol_reg,
  metric = "rmse"
)

arbol_final <- finalize_workflow(
  flujo_arbol_reg,
  mejor_arbol
) |>
  fit(data = train_arboles)

arbol_rpart <- extract_fit_engine(arbol_final)

rpart.plot(
  arbol_rpart,
  type = 2,
  extra = 101,
  fallen.leaves = TRUE
)


## ------------------------------------------------------------------------------------------------------------------
receta_forest_peso <- receta_arbol_peso |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors())

modelo_rf_reg <- rand_forest(
  mtry = tune(),
  min_n = tune(),
  trees = 100
) |>
  set_engine(
    "ranger",
    importance = "permutation"
  ) |>
  set_mode("regression")

flujo_rf_reg <- workflow() |>
  add_recipe(receta_forest_peso) |>
  add_model(modelo_rf_reg)

rejilla_rf <- grid_space_filling(
  finalize(mtry(), train_arboles),
  min_n(range = c(2L, 40L)),
  size = 20
)


## ------------------------------------------------------------------------------------------------------------------
rejilla_rf <- grid_random(
  mtry(range = c(2L, 12L)),
  min_n(range = c(5L, 50L)),
  size = 10
)

set.seed(2026)
resultados_rf <- tune_grid(
  flujo_rf_reg,
  resamples = folds_arboles_rapidos,
  grid = rejilla_rf,
  metrics = metric_set(rmse),
  control = control_grid(verbose = TRUE)
)

show_best(resultados_rf, metric = "rmse", n = 5)


## ------------------------------------------------------------------------------------------------------------------
mejor_rf <- select_best(resultados_rf, metric = "rmse")

rf_final <- finalize_workflow(
  flujo_rf_reg,
  mejor_rf
)

resultado_rf_prueba <- last_fit(
  rf_final,
  split = split_arboles,
  metrics = metric_set(rmse, mae, rsq)
)

collect_metrics(resultado_rf_prueba)


## ------------------------------------------------------------------------------------------------------------------
ajuste_rf_completo <- fit(
  rf_final,
  data = train_arboles
)

motor_rf <- extract_fit_engine(ajuste_rf_completo)

vip(
  motor_rf,
  num_features = 10
) +
  labs(
    title = "Importancia de variables en Random Forest"
  )


## ------------------------------------------------------------------------------------------------------------------
receta_boost <- receta_forest_peso

modelo_boost <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  min_n = tune(),
  sample_size = tune(),
  mtry = tune()
) |>
  set_engine("xgboost") |>
  set_mode("regression")

flujo_boost <- workflow() |>
  add_recipe(receta_boost) |>
  add_model(modelo_boost)


## ------------------------------------------------------------------------------------------------------------------
parametros_boost <- parameters(
  trees(range = c(30L, 50L)),
  tree_depth(range = c(2L, 3L)),
  learn_rate(range = c(-2, -1)),
  loss_reduction(),
  min_n(range = c(5L, 15L)),
  sample_prop(range = c(0.8, 1.0)),
  finalize(mtry(), train_arboles)
)

set.seed(2026)
rejilla_boost <- grid_latin_hypercube(
  parametros_boost,
  size = 3
)

set.seed(2026)
resultados_boost <- tune_grid(
  flujo_boost,
  resamples = vfold_cv(train_arboles, v = 2),
  grid = rejilla_boost,
  metrics = metric_set(rmse),
  control = control_grid(verbose = TRUE)
)

show_best(resultados_boost, metric = "rmse", n = 3)


## ------------------------------------------------------------------------------------------------------------------
mejor_boost <- select_best(
  resultados_boost,
  metric = "rmse"
)

boost_final <- finalize_workflow(
  flujo_boost,
  mejor_boost
)

resultado_boost_prueba <- last_fit(
  boost_final,
  split = split_arboles,
  metrics = metric_set(rmse, mae, rsq)
)

metricas_arboles <- bind_rows(
  collect_metrics(resultado_rf_prueba) |>
    mutate(modelo = "Random Forest"),
  collect_metrics(resultado_boost_prueba) |>
    mutate(modelo = "Gradient Boosting")
)

metricas_arboles


## ------------------------------------------------------------------------------------------------------------------
receta_clas_arbol <- recipe(
  salud_alta_ml ~
    consumo_kg_dia + consumo_agua_l_dia +
    edad_inicio_meses + sexo + raza + tiempo +
    temperatura_C + humedad_pct + precipitacion_mm +
    actividad_h_dia + sistema_produccion + zona,
  data = train_arboles
) |>
  step_unknown(all_nominal_predictors()) |>
  step_novel(all_nominal_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors())

modelo_rf_clas <- rand_forest(
  mtry = tune(),
  min_n = tune(),
  trees = 100
) |>
  set_engine(
    "ranger",
    importance = "permutation",
    probability = TRUE
  ) |>
  set_mode("classification")

flujo_rf_clas <- workflow() |>
  add_recipe(receta_clas_arbol) |>
  add_model(modelo_rf_clas)


## ------------------------------------------------------------------------------------------------------------------
set.seed(2026)

rejilla_rf_clas <- grid_random(
  mtry(range = c(2L, 10L)),
  min_n(range = c(5L, 20L)),
  size = 3
)

resultados_rf_clas <- tune_grid(
  flujo_rf_clas,
  resamples = vfold_cv(train_arboles, v = 2),
  grid = rejilla_rf_clas,
  metrics = metric_set(roc_auc),
  control = control_grid(verbose = TRUE)
)

show_best(
  resultados_rf_clas,
  metric = "roc_auc",
  n = 3
)

