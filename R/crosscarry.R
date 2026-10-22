## ------------------------------------------------------------------------------------------------------------------
# Una fila por unidad, período y tiempo
head(datos[, c(
  "id", "secuencia", "periodo",
  "tratamiento", "tiempo", "respuesta"
)])


## ------------------------------------------------------------------------------------------------------------------
datos <- datos |>
  dplyr::arrange(id, periodo, tiempo)


## ------------------------------------------------------------------------------------------------------------------
with(datos, table(secuencia, periodo, tratamiento))
with(datos, table(id, periodo))
summary(datos$tiempo)


## ------------------------------------------------------------------------------------------------------------------
# Objetos y nombres exactos dependen de la función ajustada
summary(modelo)
plot(modelo)
modelo$QIC


## ------------------------------------------------------------------------------------------------------------------
library(CrossCarry)
data("Arterial")

str(Arterial)
head(Arterial)


## ------------------------------------------------------------------------------------------------------------------
with(Arterial, table(Treatment, Period))
with(Arterial, table(Subject, Period))
summary(Arterial$Time)


## ------------------------------------------------------------------------------------------------------------------
carrydata <- createCarry(
  data = Arterial,
  treatment = "Treatment",
  period = "Period",
  id = "Subject",
  carrySimple = TRUE
)

datos <- carrydata$data
carry <- carrydata$carryover

head(datos)
head(carry)


## ------------------------------------------------------------------------------------------------------------------
modelo_kron <- CrossGEEKron(
  response = "Pressure",
  treatment = "Treatment",
  period = "Period",
  id = "Subject",
  time = "Time",
  carry = carry,
  data = datos,
  correlation = "AR-M",
  Mv = 1
)

summary(modelo_kron$model)
modelo_kron$QIC


## ------------------------------------------------------------------------------------------------------------------
comparacion <- data.frame(
  modelo = c(
    "Independencia", "AR(1)",
    "Kronecker", "Semiparamétrico"
  ),
  QIC = c(qic_ind, qic_ar1, qic_kron, qic_sp)
)

comparacion[order(comparacion$QIC), ]

