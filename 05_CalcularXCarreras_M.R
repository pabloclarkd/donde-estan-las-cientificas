CalcularXCarreras_M <- function(enoesvy, clas.car, periodo.enoe){
 
   # Calcular totales por sexo
  
  total_sexo <- enoesvy %>%
    filter(is.na(carrera) == FALSE & carrera != "9999" & prof == 1) %>% 
    group_by(carrera, SEXO) %>% 
    summarise(TOTAL = survey_total(na.rm = TRUE,
                                   vartype = c("cv")))
  pea_sexo <- enoesvy %>%
    filter(is.na(carrera) == FALSE & carrera != "9999" & prof == 1) %>% 
    group_by(carrera, ACTIVIDAD, SEXO) %>% 
    summarise(TOTAL = survey_total(na.rm = TRUE,
                                   vartype = c("cv")))
  
  por_sexo <- list(pea_sexo)
  
  # Crear funcion para dar formato a cada df de resultados
  FormatoTotalesSexo <- function(df){
    colnames(df)[1] <- "carrera"
    df$INDICADOR <- colnames(df)[2]
    colnames(df)[2] <- "CATEGORIA"
    
    df$TIPO <- colnames(df)[4]
    colnames(df)[c(4, 5)] <- c("VALOR", "CV")
    
    df$CV <- round(df$CV, 2)
    df <- df %>% 
      select(carrera, INDICADOR, CATEGORIA, SEXO, TIPO, VALOR, CV) %>% 
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
           carrera = carrera) %>% 
    select(carrera, INDICADOR, CATEGORIA, SEXO, TIPO, VALOR, CV)
  
  por_sexo <- bind_rows(total_sexo, por_sexo)
  
  # unir con nombres de subarea
  carreras <- clas.car %>% select(cve.cam.det, nom.cam.det)
  carreras <- unique(carreras)
  carreras$nom.cam.det <- factor(carreras$nom.cam.det, levels = carreras$nom.cam.det)
  
  por_sexo <- left_join(por_sexo,
                        carreras,
                        by = c("carrera" = "cve.cam.det"))
  
  por_sexo <- por_sexo %>%
    ungroup() %>% 
    select(-"carrera") %>% 
    rename(CARRERA = nom.cam.det) %>% 
    select(CARRERA, INDICADOR, CATEGORIA, SEXO, TIPO, VALOR, CV) %>% 
    arrange(INDICADOR, CATEGORIA, TIPO, SEXO, CARRERA) %>% 
    mutate(VALOR = round(VALOR, 0),
           PERIODO = periodo.enoe) %>% 
    filter(is.na(CARRERA) == FALSE)
  
  cat("Calculos por carrera completados\n")
  
  return(por_sexo)
  

}