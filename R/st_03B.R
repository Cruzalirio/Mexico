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

