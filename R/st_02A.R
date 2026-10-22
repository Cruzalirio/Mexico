## ------------------------------------------------------------------------------------------------------------------
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
resumen_productivo <- datos_b2 |>
  summarise(
    n_peso = sum(!is.na(peso_kg)),
    peso_media = mean(peso_kg, na.rm = TRUE),
    peso_mediana = median(peso_kg, na.rm = TRUE),
    peso_de = sd(peso_kg, na.rm = TRUE),
    peso_q1 = quantile(peso_kg, 0.25, na.rm = TRUE),
    peso_q3 = quantile(peso_kg, 0.75, na.rm = TRUE),
    n_consumo = sum(!is.na(consumo_kg_dia)),
    consumo_media = mean(consumo_kg_dia, na.rm = TRUE),
    consumo_mediana = median(consumo_kg_dia, na.rm = TRUE),
    consumo_de = sd(consumo_kg_dia, na.rm = TRUE),
    consumo_q1 = quantile(consumo_kg_dia, 0.25, na.rm = TRUE),
    consumo_q3 = quantile(consumo_kg_dia, 0.75, na.rm = TRUE)
  )

resumen_productivo


## ------------------------------------------------------------------------------------------------------------------
ggplot(datos_b2, aes(x = peso_kg)) +
  geom_histogram(
    bins = 30,
    fill = "steelblue",
    color = "white",
    na.rm = TRUE
  ) +
  geom_vline(
    aes(xintercept = median(peso_kg, na.rm = TRUE)),
    color = "firebrick",
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  labs(
    title = "Distribución del peso",
    subtitle = "La línea discontinua representa la mediana",
    x = "Peso (kg)",
    y = "Número de mediciones"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
ggplot(datos_b2, aes(x = consumo_kg_dia)) +
  geom_histogram(
    bins = 30,
    fill = "darkseagreen4",
    color = "white",
    na.rm = TRUE
  ) +
  geom_vline(
    aes(xintercept = median(consumo_kg_dia, na.rm = TRUE)),
    color = "firebrick",
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  labs(
    title = "Distribución del consumo diario de alimento",
    subtitle = "La línea discontinua representa la mediana",
    x = "Consumo de alimento (kg/día)",
    y = "Número de mediciones"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
tabla_salud <- datos_b2 |>
  count(salud_score_0_4, name = "n") |>
  mutate(
    porcentaje = 100 * n / sum(n)
  )

tabla_salud


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  tabla_salud,
  aes(x = salud_score_0_4, y = porcentaje)
) +
  geom_col(fill = "darkorange3", width = 0.75) +
  geom_text(
    aes(label = paste0(round(porcentaje, 1), "%")),
    vjust = -0.3,
    size = 3.5
  ) +
  labs(
    title = "Distribución de la puntuación de salud",
    x = "Puntuación de salud",
    y = "Porcentaje de mediciones"
  ) +
  expand_limits(y = max(tabla_salud$porcentaje) * 1.12) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
resumen_sexo <- datos_b2 |>
  group_by(sexo) |>
  summarise(
    n_animales = n_distinct(animal_id),
    n_mediciones = sum(!is.na(peso_kg)),
    peso_medio = mean(peso_kg, na.rm = TRUE),
    peso_mediano = median(peso_kg, na.rm = TRUE),
    consumo_medio = mean(consumo_kg_dia, na.rm = TRUE),
    .groups = "drop"
  )

resumen_sexo


## ------------------------------------------------------------------------------------------------------------------
ggplot(datos_b2, aes(x = sexo, y = peso_kg, fill = sexo)) +
  geom_boxplot(
    alpha = 0.75,
    outlier.alpha = 0.25,
    na.rm = TRUE
  ) +
  labs(
    title = "Distribución del peso según el sexo",
    x = "Sexo",
    y = "Peso (kg)"
  ) +
  guides(fill = "none") +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
resumen_raza <- datos_b2 |>
  group_by(raza) |>
  summarise(
    n_animales = n_distinct(animal_id),
    n_mediciones = sum(!is.na(peso_kg)),
    peso_medio = mean(peso_kg, na.rm = TRUE),
    peso_mediano = median(peso_kg, na.rm = TRUE),
    consumo_medio = mean(consumo_kg_dia, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(peso_medio))

resumen_raza


## ------------------------------------------------------------------------------------------------------------------
ggplot(datos_b2, aes(x = raza, y = peso_kg, fill = raza)) +
  geom_boxplot(
    alpha = 0.75,
    outlier.alpha = 0.20,
    na.rm = TRUE
  ) +
  labs(
    title = "Distribución del peso según la raza",
    x = "Raza",
    y = "Peso (kg)"
  ) +
  guides(fill = "none") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1)
  )


## ------------------------------------------------------------------------------------------------------------------
resumen_semana <- datos_b2 |>
  group_by(tiempo) |>
  summarise(
    n = sum(!is.na(peso_kg)),
    peso_medio = mean(peso_kg, na.rm = TRUE),
    peso_mediano = median(peso_kg, na.rm = TRUE),
    peso_de = sd(peso_kg, na.rm = TRUE),
    peso_error_estandar = peso_de / sqrt(n),
    limite_inferior = peso_medio - 1.96 * peso_error_estandar,
    limite_superior = peso_medio + 1.96 * peso_error_estandar,
    consumo_medio = mean(consumo_kg_dia, na.rm = TRUE),
    .groups = "drop"
  )

head(resumen_semana, 10)


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  resumen_semana,
  aes(x = tiempo, y = peso_medio)
) +
  geom_ribbon(
    aes(
      ymin = limite_inferior,
      ymax = limite_superior
    ),
    fill = "steelblue",
    alpha = 0.20
  ) +
  geom_line(color = "steelblue4", linewidth = 0.9) +
  geom_point(color = "steelblue4", size = 1.2) +
  labs(
    title = "Evolución semanal del peso medio",
    subtitle = "La banda representa media ± 1,96 errores estándar",
    x = "Semana de seguimiento",
    y = "Peso medio (kg)"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
consumo_periodo <- datos_b2 |>
  group_by(periodo) |>
  summarise(
    consumo_medio = mean(consumo_kg_dia, na.rm = TRUE),
    consumo_de = sd(consumo_kg_dia, na.rm = TRUE),
    n = sum(!is.na(consumo_kg_dia)),
    .groups = "drop"
  )

consumo_periodo


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  consumo_periodo,
  aes(x = periodo, y = consumo_medio)
) +
  geom_col(fill = "darkseagreen4", width = 0.75) +
  geom_text(
    aes(label = round(consumo_medio, 2)),
    vjust = -0.3,
    size = 3.3
  ) +
  labs(
    title = "Consumo medio de alimento por periodo",
    x = "Periodo de seguimiento",
    y = "Consumo medio (kg/día)"
  ) +
  expand_limits(y = max(consumo_periodo$consumo_medio) * 1.10) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1)
  )


## ------------------------------------------------------------------------------------------------------------------
granjas_mas_observadas <- datos_b2 |>
  count(granja_id, sort = TRUE) |>
  slice_head(n = 10) |>
  pull(granja_id)

resumen_granja <- datos_b2 |>
  filter(granja_id %in% granjas_mas_observadas) |>
  group_by(granja_id) |>
  summarise(
    n_animales = n_distinct(animal_id),
    n_mediciones = sum(!is.na(peso_kg)),
    peso_medio = mean(peso_kg, na.rm = TRUE),
    peso_mediano = median(peso_kg, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(desc(peso_medio))

resumen_granja


## ------------------------------------------------------------------------------------------------------------------
datos_b2 |>
  filter(granja_id %in% granjas_mas_observadas) |>
  ggplot(
    aes(
      x = reorder(granja_id, peso_kg, FUN = median, na.rm = TRUE),
      y = peso_kg,
      fill = granja_id
    )
  ) +
  geom_boxplot(
    outlier.alpha = 0.15,
    na.rm = TRUE
  ) +
  coord_flip() +
  labs(
    title = "Distribución del peso en diez granjas",
    subtitle = "Granjas con mayor número de mediciones",
    x = "Granja",
    y = "Peso (kg)"
  ) +
  guides(fill = "none") +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
# Frecuencias absolutas.
tabla_sexo_salud <- table(
  datos_b2$sexo,
  datos_b2$salud_alta,
  useNA = "ifany"
)

tabla_sexo_salud

# Porcentajes dentro de cada sexo.
round(
  prop.table(tabla_sexo_salud, margin = 1) * 100,
  1
)


## ------------------------------------------------------------------------------------------------------------------
tabla_sexo_salud_larga <- datos_b2 |>
  filter(
    !is.na(sexo),
    !is.na(salud_alta)
  ) |>
  count(sexo, salud_alta, name = "n") |>
  group_by(sexo) |>
  mutate(
    porcentaje = 100 * n / sum(n)
  ) |>
  ungroup()

tabla_sexo_salud_larga


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  tabla_sexo_salud_larga,
  aes(x = sexo, y = porcentaje, fill = salud_alta)
) +
  geom_col(position = "dodge") +
  labs(
    title = "Salud alta según el sexo",
    x = "Sexo",
    y = "Porcentaje dentro de cada sexo",
    fill = "Salud alta"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
tabla_raza_salud <- table(
  datos_b2$raza,
  datos_b2$salud_score_0_4,
  useNA = "ifany"
)

tabla_raza_salud

# Porcentajes dentro de cada raza.
round(
  prop.table(tabla_raza_salud, margin = 1) * 100,
  1
)


## ------------------------------------------------------------------------------------------------------------------
tabla_raza_salud_larga <- datos_b2 |>
  filter(
    !is.na(raza),
    !is.na(salud_score_0_4)
  ) |>
  count(raza, salud_score_0_4, name = "n") |>
  group_by(raza) |>
  mutate(
    porcentaje = 100 * n / sum(n)
  ) |>
  ungroup()

# Barras apiladas al 100 %.
ggplot(
  tabla_raza_salud_larga,
  aes(x = raza, y = porcentaje, fill = salud_score_0_4)
) +
  geom_col() +
  labs(
    title = "Composición de la puntuación de salud por raza",
    x = "Raza",
    y = "Porcentaje dentro de cada raza",
    fill = "Puntuación de salud"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1)
  )


## ------------------------------------------------------------------------------------------------------------------
salud_periodo <- datos_b2 |>
  filter(
    !is.na(periodo),
    !is.na(salud_score_0_4)
  ) |>
  count(periodo, salud_score_0_4, name = "n") |>
  group_by(periodo) |>
  mutate(
    porcentaje = 100 * n / sum(n)
  ) |>
  ungroup()

salud_periodo


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  salud_periodo,
  aes(
    x = periodo,
    y = porcentaje,
    fill = salud_score_0_4
  )
) +
  geom_col() +
  labs(
    title = "Distribución de la salud a lo largo del seguimiento",
    x = "Periodo",
    y = "Porcentaje de mediciones",
    fill = "Puntuación de salud"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1)
  )


## ------------------------------------------------------------------------------------------------------------------
salud_alta_semana <- datos_b2 |>
  filter(!is.na(salud_alta)) |>
  count(tiempo, salud_alta, name = "n") |>
  group_by(tiempo) |>
  mutate(
    porcentaje = 100 * n / sum(n)
  ) |>
  ungroup()

salud_alta_semana


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  salud_alta_semana,
  aes(x = factor(tiempo), y = porcentaje, fill = salud_alta)
) +
  geom_col(width = 0.85) +
  labs(
    title = "Salud alta por semana de seguimiento",
    x = "Semana",
    y = "Porcentaje de mediciones",
    fill = "Salud alta"
  ) +
  scale_x_discrete(
    breaks = as.character(seq(
      min(datos_b2$tiempo, na.rm = TRUE),
      max(datos_b2$tiempo, na.rm = TRUE),
      by = 10
    ))
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  datos_b2,
  aes(x = consumo_kg_dia, y = peso_kg)
) +
  geom_point(
    alpha = 0.15,
    size = 1,
    na.rm = TRUE
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "firebrick",
    linewidth = 0.9,
    na.rm = TRUE
  ) +
  labs(
    title = "Relación entre consumo de alimento y peso",
    subtitle = "La línea representa una tendencia lineal descriptiva",
    x = "Consumo de alimento (kg/día)",
    y = "Peso (kg)"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  datos_b2,
  aes(
    x = consumo_kg_dia,
    y = peso_kg,
    color = sexo
  )
) +
  geom_point(
    alpha = 0.18,
    size = 1,
    na.rm = TRUE
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    linewidth = 0.8,
    na.rm = TRUE
  ) +
  labs(
    title = "Consumo y peso según el sexo",
    x = "Consumo de alimento (kg/día)",
    y = "Peso (kg)",
    color = "Sexo"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
variables_cor_b2 <- datos_b2 |>
  select(
    peso_kg,
    consumo_kg_dia,
    consumo_agua_l_dia,
    actividad_h_dia,
    temperatura_C,
    humedad_pct
  )

matriz_cor_b2 <- cor(
  variables_cor_b2,
  use = "pairwise.complete.obs",
  method = "pearson"
)

round(matriz_cor_b2, 2)


## ------------------------------------------------------------------------------------------------------------------
corrplot(
  matriz_cor_b2,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  number.cex = 0.70
)


## ------------------------------------------------------------------------------------------------------------------
animal_seleccionado <- datos_b2 |>
  filter(!is.na(peso_kg)) |>
  count(animal_id, sort = TRUE) |>
  slice_head(n = 1) |>
  pull(animal_id)

animal_seleccionado


## ------------------------------------------------------------------------------------------------------------------
datos_animal <- datos_b2 |>
  filter(animal_id == animal_seleccionado) |>
  arrange(tiempo)

 datos_animal |>
  select(
    animal_id, granja_id, tiempo,
    peso_kg, consumo_kg_dia,
    salud_score_0_4
  ) |>
  head(10)


## ------------------------------------------------------------------------------------------------------------------
ggplot(
  datos_animal,
  aes(x = tiempo, y = peso_kg)
) +
  geom_line(color = "steelblue4", linewidth = 0.9, na.rm = TRUE) +
  geom_point(color = "steelblue4", size = 2, na.rm = TRUE) +
  labs(
    title = paste(
      "Evolución del peso del animal",
      animal_seleccionado
    ),
    x = "Semana de seguimiento",
    y = "Peso (kg)"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
peso_promedio_semana <- datos_b2 |>
  group_by(tiempo) |>
  summarise(
    peso_promedio = mean(peso_kg, na.rm = TRUE),
    .groups = "drop"
  )

grafico_individual_promedio <- datos_animal |>
  select(tiempo, peso_individual = peso_kg) |>
  full_join(
    peso_promedio_semana,
    by = "tiempo"
  ) |>
  arrange(tiempo)

head(grafico_individual_promedio, 10)


## ------------------------------------------------------------------------------------------------------------------
grafico_individual_promedio |>
  pivot_longer(
    cols = c(peso_individual, peso_promedio),
    names_to = "serie",
    values_to = "peso_kg"
  ) |>
  mutate(
    serie = recode(
      serie,
      peso_individual = paste("Animal", animal_seleccionado),
      peso_promedio = "Promedio semanal"
    )
  ) |>
  ggplot(
    aes(
      x = tiempo,
      y = peso_kg,
      color = serie,
      linetype = serie
    )
  ) +
  geom_line(linewidth = 0.9, na.rm = TRUE) +
  labs(
    title = "Trayectoria individual y promedio semanal",
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
peso_semana_sexo <- datos_b2 |>
  group_by(tiempo, sexo) |>
  summarise(
    peso_medio = mean(peso_kg, na.rm = TRUE),
    n = sum(!is.na(peso_kg)),
    .groups = "drop"
  )

ggplot(
  peso_semana_sexo,
  aes(x = tiempo, y = peso_medio, color = sexo)
) +
  geom_line(linewidth = 0.9) +
  labs(
    title = "Peso medio semanal según el sexo",
    x = "Semana de seguimiento",
    y = "Peso medio (kg)",
    color = "Sexo"
  ) +
  theme_minimal()


## ------------------------------------------------------------------------------------------------------------------
peso_semana_raza <- datos_b2 |>
  group_by(tiempo, raza) |>
  summarise(
    peso_medio = mean(peso_kg, na.rm = TRUE),
    n = sum(!is.na(peso_kg)),
    .groups = "drop"
  )

ggplot(
  peso_semana_raza,
  aes(x = tiempo, y = peso_medio, color = raza)
) +
  geom_line(linewidth = 0.8) +
  labs(
    title = "Peso medio semanal según la raza",
    x = "Semana de seguimiento",
    y = "Peso medio (kg)",
    color = "Raza"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom"
  )

