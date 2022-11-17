CalcularXSubAreas_M <- function(enoesvy, clas.car, periodo.enoe){
 
   # Calcular totales por sexo
  
  total_sexo <- enoesvy %>%
    filter(is.na(subarea) == FALSE & subarea != "999" & prof == 1 ) %>% 
    group_by(subarea, SEXO) %>% 
    summarise(TOTAL = survey_total(na.rm = TRUE,
                                   vartype = c("cv")))
  pea_sexo <- enoesvy %>%
    filter(is.na(subarea) == FALSE & subarea != "999" & prof == 1  & is.na(ACTIVIDAD) == FALSE) %>% 
    group_by(subarea, ACTIVIDAD, SEXO) %>% 
    summarise(TOTAL = survey_total(na.rm = TRUE,
                                   vartype = c("cv")))
  
  ocupacion_sexo <- enoesvy %>%
    filter(is.na(subarea) == FALSE & subarea != "999" & prof == 1  & is.na(OCUPACION) == FALSE) %>% 
    group_by(subarea, OCUPACION, SEXO) %>% 
    summarise(TOTAL = survey_total(na.rm = TRUE,
                                   vartype = c("cv")))
  
  por_sexo <- list(pea_sexo, ocupacion_sexo)
  
  # Crear funcion para dar formato a cada df de resultados
  FormatoTotalesSexo <- function(df){
    colnames(df)[1] <- "subarea"
    df$INDICADOR <- colnames(df)[2]
    colnames(df)[2] <- "CATEGORIA"
    
    df$TIPO <- colnames(df)[4]
    colnames(df)[c(4, 5)] <- c("VALOR", "CV")
    
    df$CV <- round(df$CV, 2)
    df <- df %>% 
      select(subarea, INDICADOR, CATEGORIA, SEXO, TIPO, VALOR, CV) %>% 
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
           subarea = subarea) %>% 
    select(subarea, INDICADOR, CATEGORIA, SEXO, TIPO, VALOR, CV)
  
  por_sexo <- bind_rows(total_sexo, por_sexo)
  
  # Calcular salarios
  salario <- enoesvy %>%
    filter(is.na(subarea) == FALSE & subarea != "999" & prof == 1 ) %>% 
    group_by(subarea) %>% 
    summarise(PROMEDIO = survey_mean(SALARIO,
                                     na.rm = TRUE,
                                     vartype = c("cv")))
  
  salario_sexo <- enoesvy %>%
    filter(is.na(subarea) == FALSE & subarea != "999" & prof == 1 ) %>% 
    group_by(subarea, SEXO) %>% 
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
           subarea = subarea) %>% 
    select(subarea, INDICADOR, CATEGORIA, SEXO, TIPO, VALOR, CV)
  
  salario <- salario %>%
    mutate(INDICADOR = "TOTAL",
           CATEGORIA = "total",
           TIPO = "PROMEDIO",
           SEXO = "ambos") %>%
    rename(VALOR = PROMEDIO,
           CV = PROMEDIO_cv,
           subarea = subarea) %>% 
    select(subarea, INDICADOR, CATEGORIA, SEXO, TIPO, VALOR, CV)
  
  salario_por_sexo <- bind_rows(salario, salario_sexo)
  
  por_sexo <- bind_rows(por_sexo, salario_por_sexo)
  
  # unir con nombres de subarea
  subareas <- clas.car %>% select(cve.cam.esp, nom.cam.esp)
  subareas <- unique(subareas)
  subareas$nom.cam.esp <- factor(subareas$nom.cam.esp, levels = subareas$nom.cam.esp)
  
  por_sexo <- left_join(por_sexo,
                        subareas,
                        by = c("subarea" = "cve.cam.esp"))
  
  por_sexo <- por_sexo %>%
    ungroup() %>% 
    select(-"subarea") %>% 
    rename(SUBAREA = nom.cam.esp) %>% 
    select(SUBAREA, INDICADOR, CATEGORIA, SEXO, TIPO, VALOR, CV) %>% 
    arrange(INDICADOR, CATEGORIA, TIPO, SEXO, SUBAREA) %>% 
    mutate(VALOR = round(VALOR, 0),
           PERIODO = periodo.enoe) %>% 
    filter(is.na(SUBAREA) == FALSE)
  
  cat("Calculos por subarea completados\n")
  
  return(por_sexo)
  

}