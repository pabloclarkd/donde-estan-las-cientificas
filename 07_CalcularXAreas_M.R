CalcularXAreas_M <- function(enoesvy, clas.car, periodo.enoe){
  
  # Calcular totales por sexo
  
  total_sexo <- enoesvy %>%
    filter(is.na(area) == FALSE & area != "99" & prof == 1) %>% 
    group_by(area, SEXO) %>% 
    summarise(TOTAL = survey_total(na.rm = TRUE,
                                   vartype = c("cv")))
  pea_sexo <- enoesvy %>%
    filter(is.na(area) == FALSE & area != "99" & prof == 1 & is.na(ACTIVIDAD) == FALSE) %>% 
    group_by(area, ACTIVIDAD, SEXO) %>% 
    summarise(TOTAL = survey_total(na.rm = TRUE,
                                   vartype = c("cv")))
  
  ocupacion_sexo <- enoesvy %>%
    filter(is.na(area) == FALSE & area != "99" & prof == 1 & is.na(OCUPACION) == FALSE) %>% 
    group_by(area, OCUPACION, SEXO) %>% 
    summarise(TOTAL = survey_total(na.rm = TRUE,
                                   vartype = c("cv")))
  
  formalidad_sexo <- enoesvy %>%
    filter(is.na(area) == FALSE & area != "99" & prof == 1 & is.na(FORMALIDAD) == FALSE) %>% 
    group_by(area, FORMALIDAD, SEXO) %>% 
    summarise(TOTAL = survey_total(na.rm = TRUE,
                                   vartype = c("cv")))
  
  por_sexo <- list(pea_sexo, ocupacion_sexo, formalidad_sexo)
  
  # Crear funcion para dar formato a cada df de resultados
  FormatoTotalesSexo <- function(df){
    colnames(df)[1] <- "AREA"
    df$INDICADOR <- colnames(df)[2]
    colnames(df)[2] <- "CATEGORIA"
    
    df$TIPO <- colnames(df)[4]
    colnames(df)[c(4, 5)] <- c("VALOR", "CV")
    
    df$CV <- round(df$CV, 2)
    df <- df %>% 
      select(AREA, INDICADOR, CATEGORIA, SEXO, TIPO, VALOR, CV) %>% 
      filter(is.nan(VALOR) == FALSE)
    
    return(df)
  }
  
  # Usar funcion, dar formato manual a resultados que faltan y unir todo en una sola df
  por_sexo <- lapply(por_sexo, FormatoTotalesSexo)
  
  total_sexo <- total_sexo %>%
    mutate(INDICADOR = "TOTAL",
           CATEGORIA = "total",
           TIPO = "TOTAL") %>%
    rename(VALOR = TOTAL,
           CV = TOTAL_cv,
           AREA = area) %>% 
    select(AREA, INDICADOR, CATEGORIA, SEXO, TIPO, VALOR, CV)
  
  por_sexo <- bind_rows(total_sexo, por_sexo)
  
  # Calcular salarios
  salario <- enoesvy %>%
    filter(is.na(area) == FALSE & area != "99" & prof == 1) %>% 
    group_by(area) %>% 
    summarise(PROMEDIO = survey_mean(SALARIO,
                                     na.rm = TRUE,
                                     vartype = c("cv")))
  
  salario_sexo <- enoesvy %>%
    filter(is.na(area) == FALSE & area != "99" & prof == 1) %>% 
    group_by(area, SEXO) %>% 
    summarise(PROMEDIO = survey_mean(SALARIO,
                                     na.rm = TRUE,
                                     vartype = c("cv")))
  
  # Usar funcion, dar formato manual a resultados que faltan y unir todo en una sola df
  
  salario_sexo <- salario_sexo %>%
    mutate(INDICADOR = "TOTAL",
           CATEGORIA = "total",
           TIPO = "PROMEDIO") %>%
    rename(VALOR = PROMEDIO,
           CV = PROMEDIO_cv,
           AREA = area) %>% 
    select(AREA, INDICADOR, CATEGORIA, SEXO, TIPO, VALOR, CV)
  
  salario <- salario %>%
    mutate(INDICADOR = "TOTAL",
           CATEGORIA = "total",
           TIPO = "PROMEDIO",
           SEXO = "ambos") %>%
    rename(VALOR = PROMEDIO,
           CV = PROMEDIO_cv,
           AREA = area) %>% 
    select(AREA, INDICADOR, CATEGORIA, SEXO, TIPO, VALOR, CV)
  
  salario_por_sexo <- bind_rows(salario, salario_sexo)
  
  por_sexo <- bind_rows(por_sexo, salario_por_sexo)
  
  # unir con nombres de area
  areas <- clas.car %>% select(cve.cam.amp, nom.cam.amp)
  areas <- unique(areas)
  areas$nom.cam.amp <- factor(areas$nom.cam.amp, levels = areas$nom.cam.amp)
  
  por_sexo <- left_join(por_sexo,
                        areas,
                        by = c("AREA" = "cve.cam.amp"))
  
  por_sexo <- por_sexo %>%
    ungroup() %>% 
    select(-"AREA") %>% 
    rename(AREA = nom.cam.amp) %>% 
    select(AREA, INDICADOR, CATEGORIA, SEXO, TIPO, VALOR, CV) %>% 
    arrange(INDICADOR, CATEGORIA, TIPO, SEXO, AREA) %>% 
    mutate(VALOR = round(VALOR, 0),
           PERIODO = periodo.enoe) %>% 
    filter(is.na(AREA) == FALSE)
  
  cat("Calculos por area completados\n")
  
  return(por_sexo)
}
