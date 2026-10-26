### Paquetes de R necesarios


library(tidyverse)
library(readxl)
library(sf)
library(spdep)
library(spatialreg)
library(tidymodels)
library(readxl)
library(corrplot)
library(naniar)
library(geodata)
library(terra)
library(broom)
library(lmerTest)
library(lme4)
library(broom.mixed)
library(glmnet)
library(rpart.plot)
library(ranger)
library(xgboost)
library(nnet)
library(factoextra)
library(patchwork)
library(knitr)

pak::pak("bgreenwell/vip@main") 
library(vip)

setwd("~/2026/Mexico/Mexico")


dir.create("R", showWarnings = FALSE)

archivos <- list.files(
  pattern = "\\.qmd$",
  full.names = TRUE, recursive = TRUE,
)

for (f in archivos) {
  salida <- file.path(
    "R",
    sub("\\.qmd$", ".R", basename(f))
  )
  
  purl(f, output = salida)
}

