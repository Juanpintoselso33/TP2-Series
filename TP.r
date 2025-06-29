# =============================================================================
# TP2: Elasticidades del Comercio Exterior Argentino (2004-2024)
# =============================================================================
# Modelos: M_t = α₀ + α₁*PIB_t + α₂*TCR_t + u_t (Importaciones)
#          X_t = β₀ + β₁*PIBG_t + β₂*TCR_t + v_t (Exportaciones)

# =============================================================================
# 1. CONFIGURACIÓN INICIAL Y CARGA DE LIBRERÍAS
# =============================================================================

# install.packages(c("readxl", "tidyverse", "tseries", "urca", "vars", "forecast", "strucchange", "lmtest", "car"))

library(readxl); library(tidyverse); library(tseries); library(urca)
library(vars); library(forecast); library(strucchange); library(lmtest); library(car)

# =============================================================================
# 2. CARGA Y EXPLORACIÓN DE DATOS
# =============================================================================

# Cargar datos - Buscar archivo Excel
file_name <- "Base TP2 SDT.xlsx"

# Opción 1: Path completo (cambia esta ruta por la tuya)
excel_file <- "C:/Users/trico/OneDrive/UBA/Series de tiempo/TP2/Base TP2 SDT.xlsx"

# Opción 2: Si no funciona, probar buscar automáticamente
if(!file.exists(excel_file)) {
  cat("Buscando archivo Excel...\n")
  cat("Directorio actual:", getwd(), "\n")
  
  # Buscar recursivamente en todos los directorios
  excel_files <- list.files(pattern = "Base.*TP2.*SDT.*xlsx", recursive = TRUE, full.names = TRUE)
  
  if(length(excel_files) > 0) {
    excel_file <- excel_files[1]
    cat("Archivo encontrado:", excel_file, "\n")
  } else {
    # Buscar cualquier archivo Excel como último recurso
    all_excel <- list.files(pattern = "*.xlsx", recursive = TRUE, full.names = TRUE)
    if(length(all_excel) > 0) {
      cat("Archivos Excel disponibles:\n")
      print(all_excel)
      excel_file <- all_excel[1]
      cat("Usando:", excel_file, "\n")
    } else {
      stop("No se encontró ningún archivo Excel. Asegurate de estar en el directorio correcto.")
    }
  }
}

# Cargar y limpiar datos principales de Argentina
datos_argentina <- read_excel(excel_file, sheet = "DATOS PARA TP2", skip = 1)

# Limpiar nombres de columnas
nombres_columnas <- c("PERIODO", "ITCRM", "PBI_ARG", "IMPORTACIONES", "DEMANDA", 
                     "EXPORTACIONES", "Brasil", "Canada", "Chile", "Estados_Unidos", 
                     "Mexico", "Uruguay", "China", "India", "Japon", "Reino_Unido", 
                     "Suiza", "Zona_Euro", "Vietnam", "Otros", "Total_Ponderadores")

# Ajustar nombres si hay diferencias en el número de columnas
if(ncol(datos_argentina) < length(nombres_columnas)) {
  nombres_columnas <- nombres_columnas[1:ncol(datos_argentina)]
} else if(ncol(datos_argentina) > length(nombres_columnas)) {
  nombres_extra <- paste0("Col_", (length(nombres_columnas)+1):ncol(datos_argentina))
  nombres_columnas <- c(nombres_columnas, nombres_extra)
}

names(datos_argentina) <- nombres_columnas

# Filtrar solo filas con datos
datos_argentina <- datos_argentina %>%
  filter(!is.na(PERIODO) & PERIODO != "")

# Cargar PIB de socios comerciales
pib_socios <- read_excel(excel_file, sheet = "PBI socios")

# Usar ponderadores fijos representativos del comercio exterior argentino
ponderadores_bcra <- data.frame(
  pais = c("Brasil", "China", "Estados Unidos", "Zona Euro", "México", 
           "Chile", "Canadá", "Uruguay", "Japón", "India", "Reino Unido", 
           "Suiza", "Vietnam"),
  peso = c(20.5, 18.2, 16.8, 12.5, 4.1, 3.8, 3.2, 2.1, 2.0, 1.8, 1.5, 1.2, 1.0)
)

cat("Ponderadores fijos del BCRA utilizados:\n")
print(ponderadores_bcra)

# Consolidar dataset final
dataset_final <- data.frame(
  PERIODO = datos_argentina$PERIODO,
  ITCRM = as.numeric(datos_argentina$ITCRM),
  PBI_ARG = as.numeric(datos_argentina$PBI_ARG), 
  IMPORTACIONES = as.numeric(datos_argentina$IMPORTACIONES),
  EXPORTACIONES = as.numeric(datos_argentina$EXPORTACIONES)
)

# Crear fechas trimestrales
dataset_final$Year <- as.numeric(paste0("20", substr(dataset_final$PERIODO, nchar(dataset_final$PERIODO)-1, nchar(dataset_final$PERIODO))))
dataset_final$Quarter <- ifelse(substr(dataset_final$PERIODO, 1, 1) == "I", 1,
                       ifelse(substr(dataset_final$PERIODO, 1, 2) == "II", 2,
                       ifelse(substr(dataset_final$PERIODO, 1, 3) == "III", 3,
                       ifelse(substr(dataset_final$PERIODO, 1, 2) == "IV", 4, NA))))
dataset_final$Date <- as.Date(paste0(dataset_final$Year, "-", (dataset_final$Quarter-1)*3 + 1, "-01"))

# Filtrar períodos comunes y calcular PIB ponderado
periodos_comunes <- intersect(dataset_final$PERIODO, pib_socios$PERIODO)
pib_socios_clean <- pib_socios[pib_socios$PERIODO %in% periodos_comunes, ]
dataset_temp <- dataset_final[dataset_final$PERIODO %in% periodos_comunes, ]

# Calcular PIB ponderado
pib_ponderado <- numeric(nrow(pib_socios_clean))

for(i in 1:nrow(pib_socios_clean)) {
  pib_periodo <- 0
  peso_total_usado <- 0
  
  for(j in 1:nrow(ponderadores_bcra)) {
    pais <- ponderadores_bcra$pais[j]
    peso <- ponderadores_bcra$peso[j]
    
    # Mapear nombre del país a la columna correcta
    nombre_columna <- case_when(
      pais == "Estados Unidos" ~ "Estados Unidos",
      pais == "Zona Euro" ~ "Zona Euro",
      pais == "México" ~ "México", 
      pais == "Canadá" ~ "Canadá",
      pais == "Japón" ~ "Japón",
      pais == "Reino Unido" ~ "Reino Unido",
      TRUE ~ pais
    )
    
    if(nombre_columna %in% names(pib_socios_clean)) {
      pib_pais <- as.numeric(pib_socios_clean[i, nombre_columna])
      
      if(!is.na(pib_pais) && pib_pais > 0) {
        pib_periodo <- pib_periodo + (peso * pib_pais / 100)
        peso_total_usado <- peso_total_usado + peso
      }
    }
  }
  
  if(peso_total_usado > 0) {
    pib_ponderado[i] <- pib_periodo * (100 / peso_total_usado)
  } else {
    pib_ponderado[i] <- NA
  }
}

# Dataset final consolidado
dataset_final <- data.frame(
  PERIODO = dataset_temp$PERIODO,
  Year = dataset_temp$Year,
  Quarter = dataset_temp$Quarter,
  Date = dataset_temp$Date,
  PIB_ARGENTINA = dataset_temp$PBI_ARG,
  IMPORTACIONES = dataset_temp$IMPORTACIONES,
  EXPORTACIONES = dataset_temp$EXPORTACIONES,
  TCR_MULTILATERAL = dataset_temp$ITCRM,
  PIB_SOCIOS_PONDERADO = pib_ponderado
)

# Filtrar filas completas
dataset_final <- dataset_final[complete.cases(dataset_final), ]

cat("Dataset final - Dimensiones:", dim(dataset_final), "\n")

# Convertir a logaritmos naturales
dataset_log <- dataset_final
dataset_log$log_PIB_ARG <- log(dataset_final$PIB_ARGENTINA)
dataset_log$log_IMPORTACIONES <- log(dataset_final$IMPORTACIONES)
dataset_log$log_EXPORTACIONES <- log(dataset_final$EXPORTACIONES)
dataset_log$log_TCR <- log(dataset_final$TCR_MULTILATERAL)
dataset_log$log_PIB_SOCIOS <- log(dataset_final$PIB_SOCIOS_PONDERADO)

# Variables en logaritmos creadas

# Función para graficar series temporales
plot_time_series <- function(data, var_name, title = "Serie Temporal") {
  plot(data$Date, data[[var_name]], 
       type = "l", 
       main = title,
       xlab = "Fecha", 
       ylab = var_name,
       col = "blue",
       lwd = 2)
  grid(col = "lightgray", lty = "dotted")
}

# Función para estadísticas descriptivas
descriptive_stats <- function(series, name) {
  cat("\n=== ESTADÍSTICAS DESCRIPTIVAS:", name, "===\n")
  cat("Media:", round(mean(series, na.rm = TRUE), 4), "\n")
  cat("Mediana:", round(median(series, na.rm = TRUE), 4), "\n")
  cat("Desviación estándar:", round(sd(series, na.rm = TRUE), 4), "\n")
  cat("Mínimo:", round(min(series, na.rm = TRUE), 4), "\n")
  cat("Máximo:", round(max(series, na.rm = TRUE), 4), "\n")
  cat("Observaciones:", length(series), "\n")
  cat("Valores faltantes:", sum(is.na(series)), "\n")
}

# Generar gráficos y estadísticas
par(mfrow = c(2, 3))
plot_time_series(dataset_log, "log_PIB_ARG", "PIB Argentina (log)")
plot_time_series(dataset_log, "log_IMPORTACIONES", "Importaciones (log)")
plot_time_series(dataset_log, "log_EXPORTACIONES", "Exportaciones (log)")
plot_time_series(dataset_log, "log_TCR", "Tipo de Cambio Real Multilateral (log)")
plot_time_series(dataset_log, "log_PIB_SOCIOS", "PIB Socios Ponderado (log)")
par(mfrow = c(1, 1))

# Estadísticas descriptivas
descriptive_stats(dataset_log$log_PIB_ARG, "log_PIB_ARG")
descriptive_stats(dataset_log$log_IMPORTACIONES, "log_IMPORTACIONES")
descriptive_stats(dataset_log$log_EXPORTACIONES, "log_EXPORTACIONES")
descriptive_stats(dataset_log$log_TCR, "log_TCR")
descriptive_stats(dataset_log$log_PIB_SOCIOS, "log_PIB_SOCIOS")

# =============================================================================
# 3. TESTS DE RAÍCES UNITARIAS
# =============================================================================

# Función para realizar tests de raíz unitaria
unit_root_tests <- function(series, name, max_lags = 8) {
  cat("\n=== TESTS DE RAÍZ UNITARIA:", name, "===\n")
  
  # Test ADF (Augmented Dickey-Fuller)
  cat("\n1. Test ADF:\n")
  
  # ADF con constante
  adf_const <- ur.df(series, type = "drift", lags = max_lags, selectlags = "AIC")
  cat("   ADF con constante - Estadístico:", adf_const@teststat[1], "\n")
  cat("   Valor crítico 1%:", adf_const@cval[1,1], "\n")
  cat("   Valor crítico 5%:", adf_const@cval[1,2], "\n")
  
  # ADF con constante y tendencia
  adf_trend <- ur.df(series, type = "trend", lags = max_lags, selectlags = "AIC")
  cat("   ADF con tendencia - Estadístico:", adf_trend@teststat[1], "\n")
  cat("   Valor crítico 1%:", adf_trend@cval[1,1], "\n")
  cat("   Valor crítico 5%:", adf_trend@cval[1,2], "\n")
  
  # Test KPSS
  cat("\n2. Test KPSS:\n")
  kpss_level <- ur.kpss(series, type = "mu")
  kpss_trend <- ur.kpss(series, type = "tau")
  cat("   KPSS nivel - Estadístico:", kpss_level@teststat, "\n")
  cat("   KPSS tendencia - Estadístico:", kpss_trend@teststat, "\n")
  
  # Test Phillips-Perron
  cat("\n3. Test Phillips-Perron:\n")
  pp_test <- ur.pp(series, type = "Z-tau", model = "trend")
  cat("   PP - Estadístico:", pp_test@teststat, "\n")
  cat("   Valor crítico 5%:", pp_test@cval[2], "\n")
  
  return(list(adf_const = adf_const, adf_trend = adf_trend, 
              kpss_level = kpss_level, kpss_trend = kpss_trend, pp = pp_test))
}

# =============================================================================
# 4. TESTS DE COINTEGRACIÓN
# =============================================================================

# Función para test de cointegración Engle-Granger
engle_granger_test <- function(y, x, model_name = "Modelo") {
  cat("\n=== TEST ENGLE-GRANGER:", model_name, "===\n")
  
  # Paso 1: Estimar relación de largo plazo por MCO
  if(ncol(x) == 1) {
    colnames(x) <- "X1"
    lm_model <- lm(y ~ x)
  } else {
    lm_model <- lm(y ~ ., data = as.data.frame(x))
  }
  
  cat("\nCoeficientes de la regresión de largo plazo:\n")
  print(summary(lm_model)$coefficients)
  
  # Paso 2: Extraer residuos
  residuals_lp <- residuals(lm_model)
  
  # Paso 3: Test ADF en los residuos
  cat("\nTest ADF en residuos (test de cointegración):\n")
  adf_residuals <- ur.df(residuals_lp, type = "none", lags = 8, selectlags = "AIC")
  
  cat("Estadístico ADF:", adf_residuals@teststat[1], "\n")
  
  # Valores críticos de Engle-Granger
  n_vars <- ncol(x) + 1
  if(n_vars == 2) {
    crit_1 <- -3.90
    crit_5 <- -3.34
    crit_10 <- -3.04
  } else if(n_vars == 3) {
    crit_1 <- -4.29
    crit_5 <- -3.74
    crit_10 <- -3.45
  } else {
    crit_1 <- -4.5
    crit_5 <- -4.0
    crit_10 <- -3.7
  }
  
  cat("Valor crítico 1%:", crit_1, "\n")
  cat("Valor crítico 5%:", crit_5, "\n")
  cat("Valor crítico 10%:", crit_10, "\n")
  
  # Interpretación
  if(adf_residuals@teststat[1] < crit_5) {
    cat("CONCLUSIÓN: HAY COINTEGRACIÓN (5% significancia)\n")
    cointegrated <- TRUE
  } else {
    cat("CONCLUSIÓN: NO HAY COINTEGRACIÓN (5% significancia)\n")
    cointegrated <- FALSE
  }
  
  return(list(
    lm_model = lm_model,
    residuals = residuals_lp,
    adf_test = adf_residuals,
    cointegrated = cointegrated
  ))
}

# Función para test Gregory-Hansen
gregory_hansen_test <- function(y, x, model_name = "Modelo") {
  cat("\n=== TEST GREGORY-HANSEN:", model_name, "===\n")
  
  n <- length(y)
  T <- n
  
  # Buscar el punto de quiebre óptimo
  tau_start <- floor(0.15 * T)
  tau_end <- floor(0.85 * T)
  
  min_stat <- Inf
  best_tau <- tau_start
  results <- list()
  
  cat("Buscando punto de quiebre óptimo...\n")
  
  for(tau in tau_start:tau_end) {
    # Crear variable dummy para el quiebre
    D <- c(rep(0, tau), rep(1, T - tau))
    
    # Modelo con quiebre en la constante
    if(ncol(x) == 1) {
      x_df <- data.frame(X1 = as.vector(x), D = D)
      model_c <- lm(y ~ X1 + D, data = x_df)
    } else {
      x_df <- data.frame(x, D = D)
      model_c <- lm(y ~ ., data = x_df)
    }
    
    # Test ADF en residuos
    residuals_c <- residuals(model_c)
    adf_c <- ur.df(residuals_c, type = "none", lags = 4, selectlags = "AIC")
    
    if(abs(adf_c@teststat[1]) > abs(min_stat)) {
      min_stat <- adf_c@teststat[1]
      best_tau <- tau
      results$best_model <- model_c
      results$best_residuals <- residuals_c
      results$best_adf <- adf_c
    }
  }
  
  cat("Punto de quiebre óptimo: observación", best_tau, "\n")
  cat("Estadístico ADF mínimo:", min_stat, "\n")
  
  # Valores críticos aproximados para Gregory-Hansen
  if(ncol(x) == 1) {
    crit_1_c <- -5.13
    crit_5_c <- -4.61
    crit_10_c <- -4.34
  } else {
    crit_1_c <- -5.5
    crit_5_c <- -5.0
    crit_10_c <- -4.7
  }
  
  cat("Valores críticos (modelo C):\n")
  cat("1%:", crit_1_c, "\n")
  cat("5%:", crit_5_c, "\n")
  cat("10%:", crit_10_c, "\n")
  
  # Interpretación
  if(min_stat < crit_5_c) {
    cat("CONCLUSIÓN: HAY COINTEGRACIÓN CON QUIEBRE ESTRUCTURAL (5% significancia)\n")
    cointegrated_gh <- TRUE
  } else {
    cat("CONCLUSIÓN: NO HAY COINTEGRACIÓN CON QUIEBRE ESTRUCTURAL (5% significancia)\n")
    cointegrated_gh <- FALSE
  }
  
  results$tau_break <- best_tau
  results$stat <- min_stat
  results$cointegrated <- cointegrated_gh
  
  return(results)
}

# =============================================================================
# 5. MODELOS DE CORRECCIÓN DEL ERROR (ECM)
# =============================================================================

# Función para ECM con corrección Wickens-Breusch
ecm_wickens_breusch <- function(y, x, residuals_lp, max_lags = 4, model_name = "ECM") {
  cat("\n=== MODELO ECM CON CORRECCIÓN WICKENS-BREUSCH:", model_name, "===\n")
  
  n <- length(y)
  
  # Crear diferencias
  dy <- diff(y)
  if(ncol(x) == 1) {
    dx <- diff(as.vector(x))
    dx_data <- data.frame(dx1 = dx)
  } else {
    dx <- apply(x, 2, diff)
    dx_data <- as.data.frame(dx)
    colnames(dx_data) <- paste0("dx", 1:ncol(dx))
  }
  
  # Término de corrección del error (rezagado)
  ect <- residuals_lp[-n]
  
  # Crear rezagos de las diferencias
  lags_data <- data.frame()
  for(lag in 1:max_lags) {
    if(lag < length(dy)) {
      dy_lag <- c(rep(NA, lag), dy[1:(length(dy)-lag)])
      if(ncol(dx_data) == 1) {
        dx_lag <- c(rep(NA, lag), dx_data$dx1[1:(nrow(dx_data)-lag)])
        lag_df <- data.frame(
          dy_lag = dy_lag,
          dx1_lag = dx_lag
        )
        names(lag_df) <- paste0(c("dy", "dx1"), "_lag", lag)
      } else {
        dx_lags <- apply(dx_data, 2, function(col) c(rep(NA, lag), col[1:(length(col)-lag)]))
        lag_df <- data.frame(dy_lag = dy_lag, dx_lags)
        names(lag_df) <- c(paste0("dy_lag", lag), paste0(names(dx_data), "_lag", lag))
      }
      
      if(nrow(lags_data) == 0) {
        lags_data <- lag_df
      } else {
        lags_data <- cbind(lags_data, lag_df)
      }
    }
  }
  
  # Combinar datos para ECM
  ecm_data <- data.frame(
    dy = dy,
    dx_data,
    ect = ect,
    lags_data
  )
  
  # Eliminar NAs
  ecm_data <- ecm_data[complete.cases(ecm_data), ]
  
  cat("Observaciones disponibles para ECM:", nrow(ecm_data), "\n")
  
  # Estimar ECM inicial
  ecm_formula <- paste("dy ~", paste(names(ecm_data)[-1], collapse = " + "))
  ecm_initial <- lm(as.formula(ecm_formula), data = ecm_data)
  
  cat("\nModelo ECM inicial (antes de eliminar no significativos):\n")
  print(summary(ecm_initial))
  
  # Eliminación iterativa de variables no significativas (excepto ECT)
  ecm_final <- ecm_initial
  repeat {
    summary_model <- summary(ecm_final)
    pvalues <- summary_model$coefficients[-1, 4]
    
    # No eliminar el término ECT
    pvalues_without_ect <- pvalues[!grepl("ect", names(pvalues))]
    
    if(length(pvalues_without_ect) == 0 || max(pvalues_without_ect) < 0.10) {
      break
    }
    
    # Eliminar la variable menos significativa
    max_pval_var <- names(pvalues_without_ect)[which.max(pvalues_without_ect)]
    
    # Crear nueva fórmula
    current_vars <- names(coef(ecm_final))[-1]
    new_vars <- current_vars[current_vars != max_pval_var]
    
    if(length(new_vars) == 0) break
    
    new_formula <- paste("dy ~", paste(new_vars, collapse = " + "))
    ecm_final <- lm(as.formula(new_formula), data = ecm_data)
  }
  
  cat("\nModelo ECM final (después de eliminar no significativos):\n")
  print(summary(ecm_final))
  
  # Diagnósticos
  cat("\nDiagnósticos del modelo:\n")
  
  # Test de autocorrelación
  bg_test <- bgtest(ecm_final, order = 4)
  cat("Test Breusch-Godfrey (autocorrelación): p-valor =", bg_test$p.value, "\n")
  
  # Test de heterocedasticidad
  bp_test <- bptest(ecm_final)
  cat("Test Breusch-Pagan (heterocedasticidad): p-valor =", bp_test$p.value, "\n")
  
  # Test de normalidad
  jb_test <- jarque.bera.test(residuals(ecm_final))
  cat("Test Jarque-Bera (normalidad): p-valor =", jb_test$p.value, "\n")
  
  return(list(
    initial_model = ecm_initial,
    final_model = ecm_final,
    data = ecm_data,
    diagnostics = list(bg = bg_test, bp = bp_test, jb = jb_test)
  ))
}

# Función para modelos en diferencias (sin cointegración)
difference_model <- function(y, x, max_lags = 4, model_name = "Modelo en Diferencias") {
  cat("\n=== MODELO EN DIFERENCIAS:", model_name, "===\n")
  
  # Crear diferencias
  dy <- diff(y)
  if(ncol(x) == 1) {
    dx <- diff(as.vector(x))
    dx_data <- data.frame(dx1 = dx)
  } else {
    dx <- apply(x, 2, diff)
    dx_data <- as.data.frame(dx)
    colnames(dx_data) <- paste0("dx", 1:ncol(dx))
  }
  
  # Crear rezagos de las diferencias
  lags_data <- data.frame()
  for(lag in 1:max_lags) {
    if(lag < length(dy)) {
      dy_lag <- c(rep(NA, lag), dy[1:(length(dy)-lag)])
      if(ncol(dx_data) == 1) {
        dx_lag <- c(rep(NA, lag), dx_data$dx1[1:(nrow(dx_data)-lag)])
        lag_df <- data.frame(
          dy_lag = dy_lag,
          dx1_lag = dx_lag
        )
        names(lag_df) <- paste0(c("dy", "dx1"), "_lag", lag)
      } else {
        dx_lags <- apply(dx_data, 2, function(col) c(rep(NA, lag), col[1:(length(col)-lag)]))
        lag_df <- data.frame(dy_lag = dy_lag, dx_lags)
        names(lag_df) <- c(paste0("dy_lag", lag), paste0(names(dx_data), "_lag", lag))
      }
      
      if(nrow(lags_data) == 0) {
        lags_data <- lag_df
      } else {
        lags_data <- cbind(lags_data, lag_df)
      }
    }
  }
  
  # Combinar datos
  diff_data <- data.frame(
    dy = dy,
    dx_data,
    lags_data
  )
  
  # Eliminar NAs
  diff_data <- diff_data[complete.cases(diff_data), ]
  
  cat("Observaciones disponibles:", nrow(diff_data), "\n")
  
  # Estimar modelo inicial
  diff_formula <- paste("dy ~", paste(names(diff_data)[-1], collapse = " + "))
  diff_initial <- lm(as.formula(diff_formula), data = diff_data)
  
  cat("\nModelo inicial (antes de eliminar no significativos):\n")
  print(summary(diff_initial))
  
  # Eliminación iterativa de variables no significativas
  diff_final <- diff_initial
  repeat {
    summary_model <- summary(diff_final)
    pvalues <- summary_model$coefficients[-1, 4]
    
    if(length(pvalues) == 0 || max(pvalues) < 0.10) {
      break
    }
    
    # Eliminar la variable menos significativa
    max_pval_var <- names(pvalues)[which.max(pvalues)]
    
    # Crear nueva fórmula
    current_vars <- names(coef(diff_final))[-1]
    new_vars <- current_vars[current_vars != max_pval_var]
    
    if(length(new_vars) == 0) break
    
    new_formula <- paste("dy ~", paste(new_vars, collapse = " + "))
    diff_final <- lm(as.formula(new_formula), data = diff_data)
  }
  
  cat("\nModelo final (después de eliminar no significativos):\n")
  print(summary(diff_final))
  
  # Diagnósticos
  cat("\nDiagnósticos del modelo:\n")
  
  # Test de autocorrelación
  bg_test <- bgtest(diff_final, order = 4)
  cat("Test Breusch-Godfrey (autocorrelación): p-valor =", bg_test$p.value, "\n")
  
  # Test de heterocedasticidad
  bp_test <- bptest(diff_final)
  cat("Test Breusch-Pagan (heterocedasticidad): p-valor =", bp_test$p.value, "\n")
  
  # Test de normalidad
  jb_test <- jarque.bera.test(residuals(diff_final))
  cat("Test Jarque-Bera (normalidad): p-valor =", jb_test$p.value, "\n")
  
  return(list(
    initial_model = diff_initial,
    final_model = diff_final,
    data = diff_data,
    diagnostics = list(bg = bg_test, bp = bp_test, jb = jb_test)
  ))
}

# =============================================================================
# 6. APLICACIÓN PRÁCTICA Y RESULTADOS
# =============================================================================

cat("=== PASO 1: TESTS DE RAÍCES UNITARIAS ===\n")

# Tests en las series en logaritmos
tests_pib_arg <- unit_root_tests(dataset_log$log_PIB_ARG, "log_PIB_ARG")
tests_import <- unit_root_tests(dataset_log$log_IMPORTACIONES, "log_IMPORTACIONES") 
tests_export <- unit_root_tests(dataset_log$log_EXPORTACIONES, "log_EXPORTACIONES")
tests_tcr <- unit_root_tests(dataset_log$log_TCR, "log_TCR")
tests_pib_socios <- unit_root_tests(dataset_log$log_PIB_SOCIOS, "log_PIB_SOCIOS")

cat("\n=== PASO 2: TESTS DE COINTEGRACIÓN ===\n")

# Modelo de Importaciones: log_M = α₀ + α₁*log_PIB_ARG + α₂*log_TCR + u
cat("\n>>> MODELO DE IMPORTACIONES <<<\n")
X_import <- cbind(dataset_log$log_PIB_ARG, dataset_log$log_TCR)
colnames(X_import) <- c("log_PIB_ARG", "log_TCR")

# Test Engle-Granger
eg_import <- engle_granger_test(dataset_log$log_IMPORTACIONES, X_import, "IMPORTACIONES")

# Test Gregory-Hansen
gh_import <- gregory_hansen_test(dataset_log$log_IMPORTACIONES, X_import, "IMPORTACIONES")

# Modelo de Exportaciones: log_X = β₀ + β₁*log_PIB_SOCIOS + β₂*log_TCR + v  
cat("\n>>> MODELO DE EXPORTACIONES <<<\n")
X_export <- cbind(dataset_log$log_PIB_SOCIOS, dataset_log$log_TCR)
colnames(X_export) <- c("log_PIB_SOCIOS", "log_TCR")

# Test Engle-Granger
eg_export <- engle_granger_test(dataset_log$log_EXPORTACIONES, X_export, "EXPORTACIONES")

# Test Gregory-Hansen  
gh_export <- gregory_hansen_test(dataset_log$log_EXPORTACIONES, X_export, "EXPORTACIONES")

cat("\n=== PASO 3: ESTIMACIÓN DE MODELOS ===\n")

# Para IMPORTACIONES
if(eg_import$cointegrated || gh_import$cointegrated) {
  cat("\n>>> ECM PARA IMPORTACIONES (HAY COINTEGRACIÓN) <<<\n")
  residuals_import <- if(gh_import$cointegrated) gh_import$best_residuals else eg_import$residuals
  ecm_import <- ecm_wickens_breusch(
    dataset_log$log_IMPORTACIONES, 
    X_import, 
    residuals_import, 
    model_name = "IMPORTACIONES"
  )
} else {
  cat("\n>>> MODELO EN DIFERENCIAS PARA IMPORTACIONES (NO HAY COINTEGRACIÓN) <<<\n")
  diff_import <- difference_model(
    dataset_log$log_IMPORTACIONES, 
    X_import, 
    model_name = "IMPORTACIONES"
  )
}

# Para EXPORTACIONES
if(eg_export$cointegrated || gh_export$cointegrated) {
  cat("\n>>> ECM PARA EXPORTACIONES (HAY COINTEGRACIÓN) <<<\n")
  residuals_export <- if(gh_export$cointegrated) gh_export$best_residuals else eg_export$residuals
  ecm_export <- ecm_wickens_breusch(
    dataset_log$log_EXPORTACIONES, 
    X_export, 
    residuals_export, 
    model_name = "EXPORTACIONES"
  )
} else {
  cat("\n>>> MODELO EN DIFERENCIAS PARA EXPORTACIONES (NO HAY COINTEGRACIÓN) <<<\n")
  diff_export <- difference_model(
    dataset_log$log_EXPORTACIONES, 
    X_export, 
    model_name = "EXPORTACIONES"
  )
}

# =============================================================================
# 7. FUNCIONES DE RESUMEN Y COMPARACIÓN
# =============================================================================

# Función para crear cuadros resumen de resultados
create_results_table <- function() {
  cat("=== CUADRO RESUMEN DE ELASTICIDADES ===\n")
  
  # Determinar qué modelos usar según cointegración
  use_ecm_import <- exists("ecm_import")
  use_ecm_export <- exists("ecm_export")
  
  # Tabla de Importaciones
  cat("\nIMPORTACIONES:\n")
  cat("Cointegración Engle-Granger:", eg_import$cointegrated, "\n")
  cat("Cointegración Gregory-Hansen:", gh_import$cointegrated, "\n")
  
  if(use_ecm_import) {
    cat("Modelo utilizado: ECM (Modelo de Corrección del Error)\n")
    # Extraer elasticidades de largo plazo
    lp_model <- if(gh_import$cointegrated) gh_import$best_model else eg_import$lm_model
    lp_coefs <- coef(lp_model)
    cat("Elasticidad-Ingreso de Largo Plazo (PIB):", round(lp_coefs[2], 3), "\n")
    cat("Elasticidad-Precio de Largo Plazo (TCR):", round(lp_coefs[3], 3), "\n")
    
    # Elasticidades de corto plazo del ECM
    sp_coefs <- coef(ecm_import$final_model)
    sp_names <- names(sp_coefs)
    
    if("dx1" %in% sp_names) {
      cat("Elasticidad-Ingreso de Corto Plazo (PIB):", round(sp_coefs["dx1"], 3), "\n")
    }
    if("dx2" %in% sp_names) {
      cat("Elasticidad-Precio de Corto Plazo (TCR):", round(sp_coefs["dx2"], 3), "\n")
    }
    
    # Velocidad de ajuste
    if("ect" %in% sp_names) {
      cat("Velocidad de Ajuste:", round(sp_coefs["ect"], 3), "\n")
    }
    
  } else if(exists("diff_import")) {
    cat("Modelo utilizado: Diferencias (No hay cointegración)\n")
    sp_coefs <- coef(diff_import$final_model)
    sp_names <- names(sp_coefs)
    
    if("dx1" %in% sp_names) {
      cat("Elasticidad-Ingreso de Corto Plazo (PIB):", round(sp_coefs["dx1"], 3), "\n")
    }
    if("dx2" %in% sp_names) {
      cat("Elasticidad-Precio de Corto Plazo (TCR):", round(sp_coefs["dx2"], 3), "\n")
    }
  }
  
  # Tabla de Exportaciones
  cat("\nEXPORTACIONES:\n")
  cat("Cointegración Engle-Granger:", eg_export$cointegrated, "\n")
  cat("Cointegración Gregory-Hansen:", gh_export$cointegrated, "\n")
  
  if(use_ecm_export) {
    cat("Modelo utilizado: ECM (Modelo de Corrección del Error)\n")
    # Extraer elasticidades de largo plazo
    lp_model <- if(gh_export$cointegrated) gh_export$best_model else eg_export$lm_model
    lp_coefs <- coef(lp_model)
    cat("Elasticidad-Ingreso de Largo Plazo (PIB Socios):", round(lp_coefs[2], 3), "\n")
    cat("Elasticidad-Precio de Largo Plazo (TCR):", round(lp_coefs[3], 3), "\n")
    
    # Elasticidades de corto plazo del ECM
    sp_coefs <- coef(ecm_export$final_model)
    sp_names <- names(sp_coefs)
    
    if("dx1" %in% sp_names) {
      cat("Elasticidad-Ingreso de Corto Plazo (PIB Socios):", round(sp_coefs["dx1"], 3), "\n")
    }
    if("dx2" %in% sp_names) {
      cat("Elasticidad-Precio de Corto Plazo (TCR):", round(sp_coefs["dx2"], 3), "\n")
    }
    
    # Velocidad de ajuste
    if("ect" %in% sp_names) {
      cat("Velocidad de Ajuste:", round(sp_coefs["ect"], 3), "\n")
    }
    
  } else if(exists("diff_export")) {
    cat("Modelo utilizado: Diferencias (No hay cointegración)\n")
    sp_coefs <- coef(diff_export$final_model)
    sp_names <- names(sp_coefs)
    
    if("dx1" %in% sp_names) {
      cat("Elasticidad-Ingreso de Corto Plazo (PIB Socios):", round(sp_coefs["dx1"], 3), "\n")
    }
    if("dx2" %in% sp_names) {
      cat("Elasticidad-Precio de Corto Plazo (TCR):", round(sp_coefs["dx2"], 3), "\n")
    }
  }
}



# =============================================================================
# 8. EJECUCIÓN FINAL Y RESULTADOS
# =============================================================================

cat("\n=== ANÁLISIS ECONOMÉTRICO COMPLETADO ===\n")

# Generar cuadro resumen de resultados
create_results_table()

cat("\n=== TP2 COMPLETADO ===\n")
