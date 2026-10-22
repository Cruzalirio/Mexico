## ------------------------------------------------------------------------------------------------------------------
library(readxl)
library(tidyverse)
library(corrplot)
library(naniar)
library(lmerTest)
library(lme4)
library(broom.mixed)

# Consultar la carpeta de trabajo.
getwd()

# Nombre del archivo Excel.
archivo <- "../datos/base_borregos_espaciotemporal_Hidalgo.xlsx"

# Consultar las hojas disponibles.
excel_sheets(archivo)

# Importar la tabla longitudinal.
datos <- read_excel(archivo, sheet = "datos_long")



## ------------------------------------------------------------------------------------------------------------------
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

# Comprobar las variables preparadas.
datos_b2 |>
  select(
    animal_id, granja_id, tiempo, periodo,
    sexo, raza, peso_kg, consumo_kg_dia,
    salud_score_0_4, salud_alta
  ) |>
  glimpse()


## ------------------------------------------------------------------------------------------------------------------
datos_lmm <- datos_b2 |>
  select(
    animal_id,
    granja_id,
    tiempo,
    peso_kg,
    consumo_kg_dia,
    edad_inicio_meses,
    sexo,
    raza
  ) |>
  drop_na(
    animal_id,
    granja_id,
    tiempo,
    peso_kg,
    consumo_kg_dia,
    edad_inicio_meses,
    sexo,
    raza
  ) |>
  mutate(
    animal_id = droplevels(as.factor(animal_id)),
    granja_id = droplevels(as.factor(granja_id)),
    sexo = droplevels(as.factor(sexo)),
    raza = droplevels(as.factor(raza)),
    tiempo_c = tiempo - mean(tiempo),
    consumo_c = consumo_kg_dia - mean(consumo_kg_dia),
    edad_inicio_c = edad_inicio_meses -
      mean(edad_inicio_meses)
  )

# Resumen de la estructura jerárquica.
datos_lmm |>
  summarise(
    n_mediciones = n(),
    n_animales = n_distinct(animal_id),
    n_granjas = n_distinct(granja_id),
    mediciones_por_animal = n() / n_distinct(animal_id),
    animales_por_granja = n_distinct(animal_id) /
      n_distinct(granja_id)
  )


## ------------------------------------------------------------------------------------------------------------------
animales_en_varias_granjas <- datos_lmm |>
  distinct(animal_id, granja_id) |>
  count(animal_id, name = "n_granjas") |>
  filter(n_granjas > 1)

animales_en_varias_granjas


## ------------------------------------------------------------------------------------------------------------------
animales_ejemplo_lmm <- datos_lmm |>
  count(animal_id, sort = TRUE) |>
  slice_head(n = 12) |>
  select(animal_id)

datos_trayectorias <- datos_lmm |>
  semi_join(
    animales_ejemplo_lmm,
    by = "animal_id"
  )

ggplot(
  datos_trayectorias,
  aes(
    x = tiempo,
    y = peso_kg,
    group = animal_id,
    color = animal_id
  )
) +
  geom_line(alpha = 0.80, linewidth = 0.7) +
  geom_point(alpha = 0.70, size = 1) +
  labs(
    title = "Trayectorias de peso de varios animales",
    x = "Semana de seguimiento",
    y = "Peso (kg)",
    color = "Animal"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )


## ------------------------------------------------------------------------------------------------------------------
peso_por_animal <- datos_lmm |>
  group_by(animal_id, granja_id) |>
  summarise(
    peso_medio = mean(peso_kg),
    peso_inicial = peso_kg[which.min(tiempo)],
    peso_final = peso_kg[which.max(tiempo)],
    n_mediciones = n(),
    .groups = "drop"
  )

summary(peso_por_animal$peso_medio)


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  peso_por_animal,
  aes(x = peso_medio)
) +
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Distribución del peso medio entre animales",
    x = "Peso medio del animal (kg)",
    y = "Número de animales"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
modelo_nulo_animal <- lmer(
  peso_kg ~ 1 + (1 | animal_id),
  data = datos_lmm,
  REML = TRUE
)

summary(modelo_nulo_animal)


## ------------------------------------------------------------------------------------------------------------------
varianzas_animal <- as.data.frame(
  VarCorr(modelo_nulo_animal)
)

varianzas_animal


## ------------------------------------------------------------------------------------------------------------------
performance::icc(modelo_nulo_animal)


## ------------------------------------------------------------------------------------------------------------------
var_animal <- as.numeric(
  VarCorr(modelo_nulo_animal)$animal_id[1]
)

var_residual <- sigma(modelo_nulo_animal)^2

ICC_animal <- var_animal /
  (var_animal + var_residual)

ICC_animal


## ------------------------------------------------------------------------------------------------------------------
modelo_nulo_dos_niveles <- lmer(
  peso_kg ~ 1 +
    (1 | granja_id) +
    (1 | animal_id),
  data = datos_lmm,
  REML = TRUE
)

summary(modelo_nulo_dos_niveles)


## ------------------------------------------------------------------------------------------------------------------
# Alternativa segura: crear un identificador global granja-animal.
datos_lmm <- datos_lmm |>
  mutate(
    animal_granja = interaction(
      granja_id,
      animal_id,
      drop = TRUE
    )
  )

modelo_nulo_anidado <- lmer(
  peso_kg ~ 1 +
    (1 | granja_id) +
    (1 | animal_granja),
  data = datos_lmm,
  REML = TRUE
)

summary(modelo_nulo_anidado)


## ------------------------------------------------------------------------------------------------------------------
# Modelo equivalente mediante notación de anidamiento.
modelo_nulo_notacion_anidada <- lmer(
  peso_kg ~ 1 +
    (1 | granja_id/animal_id),
  data = datos_lmm,
  REML = TRUE
)


## ------------------------------------------------------------------------------------------------------------------
componentes_varianza <- as.data.frame(
  VarCorr(modelo_nulo_anidado)
) |>
  select(grp, var1, vcov, sdcor)

componentes_varianza


## ------------------------------------------------------------------------------------------------------------------
performance::icc(
  modelo_nulo_anidado,
  by_group = TRUE
)


## ------------------------------------------------------------------------------------------------------------------
modelo_lmm <- lmer(
  peso_kg ~
    consumo_c +
    edad_inicio_c +
    sexo +
    raza +
    tiempo_c +
    (1 | granja_id) +
    (1 | animal_granja),
  data = datos_lmm,
  REML = TRUE
)

summary(modelo_lmm)


## ------------------------------------------------------------------------------------------------------------------
efectos_fijos_lmm <- tidy(
  modelo_lmm,
  effects = "fixed",
  conf.int = TRUE,
  conf.method = "Wald"
)

efectos_fijos_lmm


## ------------------------------------------------------------------------------------------------------------------
efectos_aleatorios_lmm <- tidy(
  modelo_lmm,
  effects = "ran_pars",
  conf.int = TRUE,
  conf.method = "Wald"
)

efectos_aleatorios_lmm


## ------------------------------------------------------------------------------------------------------------------
efectos_fijos_lmm |>
  filter(term != "(Intercept)") |>
  mutate(
    term = forcats::fct_reorder(term, estimate)
  ) |>
  ggplot(
    aes(x = estimate, y = term)
  ) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "grey45"
  ) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.20,
    color = "steelblue4"
  ) +
  geom_point(
    size = 2.5,
    color = "steelblue4"
  ) +
  labs(
    title = "Efectos fijos del modelo lineal mixto",
    subtitle = "Estimaciones e intervalos de confianza del 95 %",
    x = "Coeficiente estimado",
    y = "Variable"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
modelo_lm_equivalente <- lm(
  peso_kg ~
    consumo_c +
    edad_inicio_c +
    sexo +
    raza +
    tiempo_c,
  data = datos_lmm
)

comparacion_coeficientes <- bind_rows(
  tidy(
    modelo_lm_equivalente,
    conf.int = TRUE
  ) |>
    mutate(modelo = "Regresión convencional"),
  tidy(
    modelo_lmm,
    effects = "fixed",
    conf.int = TRUE,
    conf.method = "Wald"
  ) |>
    mutate(modelo = "Modelo mixto")
) |>
  select(
    modelo,
    term,
    estimate,
    std.error,
    conf.low,
    conf.high,
    p.value
  )

comparacion_coeficientes


## ------------------------------------------------------------------------------------------------------------------
performance::r2_nakagawa(modelo_lmm)


## ------------------------------------------------------------------------------------------------------------------
interceptos_aleatorios <- ranef(
  modelo_lmm,
  condVar = TRUE
)

head(interceptos_aleatorios$granja_id)
head(interceptos_aleatorios$animal_granja)


## ------------------------------------------------------------------------------------------------------------------
efectos_granja <- interceptos_aleatorios$granja_id |>
  rownames_to_column("granja_id") |>
  rename(desviacion_intercepto = `(Intercept)`) |>
  arrange(desviacion_intercepto) |>
  mutate(
    granja_id = factor(
      granja_id,
      levels = granja_id
    )
  )

ggplot(
  efectos_granja,
  aes(x = desviacion_intercepto, y = granja_id)
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "grey45"
  ) +
  geom_point(color = "steelblue4", size = 2) +
  labs(
    title = "Desviaciones aleatorias de las granjas",
    x = "Desviación respecto al intercepto general (kg)",
    y = "Granja"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
datos_lmm <- datos_lmm |>
  mutate(
    prediccion_poblacional = predict(
      modelo_lmm,
      re.form = NA
    ),
    prediccion_condicional = predict(
      modelo_lmm,
      re.form = NULL
    )
  )

head(
  datos_lmm |>
    select(
      animal_id,
      granja_id,
      tiempo,
      peso_kg,
      prediccion_poblacional,
      prediccion_condicional
    )
)


## ------------------------------------------------------------------------------------------------------------------
animal_prediccion <- datos_lmm |>
  count(animal_granja, sort = TRUE) |>
  slice_head(n = 1) |>
  pull(animal_granja)

datos_lmm |>
  filter(animal_granja == animal_prediccion) |>
  select(
    tiempo,
    peso_kg,
    prediccion_poblacional,
    prediccion_condicional
  ) |>
  pivot_longer(
    cols = c(
      peso_kg,
      prediccion_poblacional,
      prediccion_condicional
    ),
    names_to = "serie",
    values_to = "peso"
  ) |>
  ggplot(
    aes(
      x = tiempo,
      y = peso,
      color = serie,
      linetype = serie
    )
  ) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  labs(
    title = "Predicción poblacional y predicción individual",
    x = "Semana de seguimiento",
    y = "Peso (kg)",
    color = NULL,
    linetype = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )


## ------------------------------------------------------------------------------------------------------------------
diagnostico_lmm <- tibble(
  ajustado = fitted(modelo_lmm),
  residuo = resid(modelo_lmm),
  residuo_pearson = residuals(
    modelo_lmm,
    type = "pearson"
  )
)

ggplot(
  diagnostico_lmm,
  aes(x = ajustado, y = residuo)
) +
  geom_point(
    alpha = 0.15,
    size = 1,
    color = "grey35"
  ) +
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
    title = "Residuos del modelo lineal mixto",
    x = "Peso ajustado (kg)",
    y = "Residuo (kg)"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  diagnostico_lmm,
  aes(sample = residuo_pearson)
) +
  stat_qq(alpha = 0.25, size = 1) +
  stat_qq_line(
    color = "firebrick",
    linewidth = 0.9
  ) +
  labs(
    title = "Gráfico Q-Q de los residuos del modelo mixto",
    x = "Cuantiles teóricos",
    y = "Cuantiles observados"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
isSingular(
  modelo_lmm,
  tol = 1e-4
)

performance::check_singularity(modelo_lmm)


## ------------------------------------------------------------------------------------------------------------------
datos_glmm <- datos_b2 |>
  select(
    animal_id,
    granja_id,
    tiempo,
    salud_score_0_4,
    consumo_kg_dia,
    edad_inicio_meses,
    sexo,
    raza
  ) |>
  mutate(
    salud_num = as.numeric(
      as.character(salud_score_0_4)
    ),
    salud_alta_bin = case_when(
      is.na(salud_num) ~ NA_real_,
      salud_num >= 3 ~ 1,
      salud_num < 3 ~ 0
    )
  ) |>
  drop_na(
    animal_id,
    granja_id,
    tiempo,
    salud_alta_bin,
    consumo_kg_dia,
    edad_inicio_meses,
    sexo,
    raza
  ) |>
  mutate(
    animal_id = droplevels(as.factor(animal_id)),
    granja_id = droplevels(as.factor(granja_id)),
    sexo = droplevels(as.factor(sexo)),
    raza = droplevels(as.factor(raza)),
    animal_granja = interaction(
      granja_id,
      animal_id,
      drop = TRUE
    ),
    tiempo_c = tiempo - mean(tiempo),
    consumo_c = consumo_kg_dia - mean(consumo_kg_dia),
    edad_inicio_c = edad_inicio_meses -
      mean(edad_inicio_meses)
  )

# Comprobar la codificación.
count(datos_glmm, salud_score_0_4, salud_alta_bin)


## ------------------------------------------------------------------------------------------------------------------
resumen_salud_binaria <- datos_glmm |>
  count(salud_alta_bin, name = "n") |>
  mutate(
    porcentaje = 100 * n / sum(n)
  )

resumen_salud_binaria


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  resumen_salud_binaria,
  aes(
    x = factor(salud_alta_bin),
    y = porcentaje,
    fill = factor(salud_alta_bin)
  )
) +
  geom_col(width = 0.70) +
  geom_text(
    aes(label = paste0(round(porcentaje, 1), "%")),
    vjust = -0.3
  ) +
  labs(
    title = "Distribución de la respuesta binaria de salud",
    x = "Salud alta",
    y = "Porcentaje de mediciones",
    fill = "Salud alta"
  ) +
  scale_x_discrete(
    labels = c("0" = "No", "1" = "Sí")
  ) +
  guides(fill = "none") +
  expand_limits(
    y = max(resumen_salud_binaria$porcentaje) * 1.10
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
salud_alta_semana_glmm <- datos_glmm |>
  group_by(tiempo) |>
  summarise(
    n = n(),
    casos = sum(salud_alta_bin),
    proporcion = mean(salud_alta_bin),
    .groups = "drop"
  )

ggplot(
  salud_alta_semana_glmm,
  aes(x = tiempo, y = proporcion)
) +
  geom_line(color = "steelblue4", linewidth = 0.9) +
  geom_point(color = "steelblue4", size = 1.3) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  labs(
    title = "Proporción de salud alta por semana",
    x = "Semana de seguimiento",
    y = "Proporción con salud alta"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
modelo_logistico <- glm(
  salud_alta_bin ~
    consumo_c +
    edad_inicio_c +
    sexo +
    raza +
    tiempo_c,
  data = datos_glmm,
  family = binomial(link = "logit")
)

summary(modelo_logistico)


## ------------------------------------------------------------------------------------------------------------------
tidy(
  modelo_logistico,
  conf.int = TRUE
)


## ------------------------------------------------------------------------------------------------------------------
modelo_glmm <- glmer(
  salud_alta_bin ~
    consumo_c +
    edad_inicio_c +
    sexo +
    raza +
    tiempo_c +
    (1 | granja_id) +
    (1 | animal_granja),
  data = datos_glmm,
  family = binomial(link = "logit"),
  nAGQ = 0,
  control = glmerControl(
    optimizer = "bobyqa",
    optCtrl = list(maxfun = 20000)
  )
)


summary(modelo_glmm)


## ------------------------------------------------------------------------------------------------------------------
OR_glmm <- tidy(
  modelo_glmm,
  effects = "fixed",
  conf.int = TRUE,
  conf.method = "Wald",
  exponentiate = TRUE
)

OR_glmm


## ------------------------------------------------------------------------------------------------------------------
OR_glmm_interpretacion <- OR_glmm |>
  mutate(
    cambio_porcentual_odds = 100 * (estimate - 1)
  ) |>
  select(
    term,
    estimate,
    conf.low,
    conf.high,
    p.value,
    cambio_porcentual_odds
  )

OR_glmm_interpretacion


## ------------------------------------------------------------------------------------------------------------------
OR_glmm |>
  filter(term != "(Intercept)") |>
  mutate(
    term = forcats::fct_reorder(term, estimate)
  ) |>
  ggplot(
    aes(x = estimate, y = term)
  ) +
  geom_vline(
    xintercept = 1,
    linetype = "dashed",
    color = "grey45"
  ) +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high),
    height = 0.20,
    color = "steelblue4"
  ) +
  geom_point(
    size = 2.5,
    color = "steelblue4"
  ) +
  scale_x_log10() +
  labs(
    title = "Odds ratios del modelo logístico mixto",
    subtitle = "Estimaciones e intervalos de confianza del 95 %",
    x = "Odds ratio, escala logarítmica",
    y = "Variable"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
tidy(
  modelo_glmm,
  effects = "ran_pars",
  conf.int = TRUE,
  conf.method = "Wald"
)


## ------------------------------------------------------------------------------------------------------------------
VarCorr(modelo_glmm)


## ------------------------------------------------------------------------------------------------------------------
consumos_prediccion <- seq(
  quantile(datos_glmm$consumo_c, 0.05),
  quantile(datos_glmm$consumo_c, 0.95),
  length.out = 100
)

nuevos_datos_glmm <- expand_grid(
  consumo_c = consumos_prediccion,
  edad_inicio_c = 0,
  tiempo_c = 0,
  sexo = levels(datos_glmm$sexo)[1],
  raza = levels(datos_glmm$raza)[1]
) |>
  mutate(
    sexo = factor(
      sexo,
      levels = levels(datos_glmm$sexo)
    ),
    raza = factor(
      raza,
      levels = levels(datos_glmm$raza)
    )
  )

nuevos_datos_glmm <- nuevos_datos_glmm |>
  mutate(
    probabilidad = predict(
      modelo_glmm,
      newdata = nuevos_datos_glmm,
      type = "response",
      re.form = NA
    ),
    consumo_kg_dia = consumo_c +
      mean(datos_glmm$consumo_kg_dia)
  )

head(nuevos_datos_glmm)


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  nuevos_datos_glmm,
  aes(x = consumo_kg_dia, y = probabilidad)
) +
  geom_line(
    color = "steelblue4",
    linewidth = 1
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  labs(
    title = "Probabilidad predicha de salud alta",
    subtitle = "Predicción poblacional para las categorías de referencia",
    x = "Consumo de alimento (kg/día)",
    y = "Probabilidad predicha"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
ejemplo_odds <- tibble(
  probabilidad = c(0.10, 0.25, 0.50, 0.75, 0.90)
) |>
  mutate(
    odds = probabilidad / (1 - probabilidad)
  )

ejemplo_odds


## ------------------------------------------------------------------------------------------------------------------
comparacion_logistica <- bind_rows(
  tidy(
    modelo_logistico,
    conf.int = TRUE,
    exponentiate = TRUE
  ) |>
    mutate(modelo = "Logístico convencional"),
  tidy(
    modelo_glmm,
    effects = "fixed",
    conf.int = TRUE,
    conf.method = "Wald",
    exponentiate = TRUE
  ) |>
    mutate(modelo = "Logístico mixto")
) |>
  select(
    modelo,
    term,
    estimate,
    std.error,
    conf.low,
    conf.high,
    p.value
  )

comparacion_logistica


## ------------------------------------------------------------------------------------------------------------------
performance::r2_nakagawa(modelo_glmm)


## ------------------------------------------------------------------------------------------------------------------
modelo_glmm@optinfo$conv$lme4$messages

isSingular(
  modelo_glmm,
  tol = 1e-4
)

performance::check_singularity(modelo_glmm)


## ------------------------------------------------------------------------------------------------------------------
datos_glmm <- datos_glmm |>
  mutate(
    probabilidad_condicional = predict(
      modelo_glmm,
      type = "response",
      re.form = NULL
    ),
    probabilidad_poblacional = predict(
      modelo_glmm,
      type = "response",
      re.form = NA
    )
  )

ajuste_semana_glmm <- datos_glmm |>
  group_by(tiempo) |>
  summarise(
    proporcion_observada = mean(salud_alta_bin),
    prediccion_condicional = mean(probabilidad_condicional),
    prediccion_poblacional = mean(probabilidad_poblacional),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols = c(
      proporcion_observada,
      prediccion_condicional,
      prediccion_poblacional
    ),
    names_to = "serie",
    values_to = "proporcion"
  )

ggplot(
  ajuste_semana_glmm,
  aes(
    x = tiempo,
    y = proporcion,
    color = serie,
    linetype = serie
  )
) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 1)
  ) +
  labs(
    title = "Salud alta observada y predicha por semana",
    x = "Semana de seguimiento",
    y = "Proporción o probabilidad media",
    color = NULL,
    linetype = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )


## ------------------------------------------------------------------------------------------------------------------
tabla_final_glmm <- OR_glmm |>
  mutate(
    across(
      c(estimate, conf.low, conf.high, p.value),
      ~ round(.x, 3)
    )
  ) |>
  rename(
    odds_ratio = estimate,
    IC95_inferior = conf.low,
    IC95_superior = conf.high,
    valor_p = p.value
  )

tabla_final_glmm

