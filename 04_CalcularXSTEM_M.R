CalcularXSTEM <- function(enoesvy, periodo.enoe){

# La funcion CreateSalariosCarreras realiza los calculos salariales y de indicadores del mercado laboral

   # Calcular totales por sexo
   
   total_sexo <- enoesvy %>%
      filter(is.na(STEM) == FALSE & prof == 1) %>% 
      group_by(STEM, SEXO) %>% 
      summarise(TOTAL = survey_total(na.rm = TRUE,
                                     vartype = c("cv")))
   pea_sexo <- enoesvy %>%
      filter(is.na(STEM) == FALSE & prof == 1 & is.na(ACTIVIDAD) == FALSE) %>% 
      group_by(STEM, ACTIVIDAD, SEXO) %>% 
      summarise(TOTAL = survey_total(na.rm = TRUE,
                                     vartype = c("cv")))
   
   posicion_sexo <- enoesvy %>%
      filter(is.na(STEM) == FALSE & prof == 1 & is.na(POSICION) == FALSE) %>% 
      group_by(STEM, POSICION, SEXO) %>% 
      summarise(TOTAL = survey_total(na.rm = TRUE,
                                     vartype = c("cv")))
   
   formalidad_sexo <- enoesvy %>%
      filter(is.na(STEM) == FALSE & prof == 1 & is.na(FORMALIDAD) == FALSE) %>% 
      group_by(STEM, FORMALIDAD, SEXO) %>% 
      summarise(TOTAL = survey_total(na.rm = TRUE,
                                     vartype = c("cv")))
   
   sector_sexo <- enoesvy %>%
      filter(is.na(STEM) == FALSE & prof == 1 & is.na(SECTOR) == FALSE) %>% 
      group_by(STEM, SECTOR, SEXO) %>% 
      summarise(TOTAL = survey_total(na.rm = TRUE,
                                     vartype = c("cv")))
   
   #pea_hijos <- enoesvy %>%
   #   filter(STEM == "STEM" & prof == 1 & is.na(ACTIVIDAD) == FALSE & is.na(HIJOS) == FALSE) %>% 
   #   group_by(HIJOS, ACTIVIDAD) %>% 
   #   summarise(TOTAL = survey_total(na.rm = TRUE,
   #                                  vartype = c("cv")))
   #
   #pea_edo_civil <- enoesvy %>%
   #   filter(STEM == "STEM" & prof == 1 & is.na(ACTIVIDAD) == FALSE & is.na(EDO_CIVIL) == FALSE & EDO_CIVIL != "no sabe") %>% 
   #   group_by(EDO_CIVIL, ACTIVIDAD) %>% 
   #   summarise(TOTAL = survey_total(na.rm = TRUE,
   #                                  vartype = c("cv")))
   #
   #formalidad_hijos <- enoesvy %>%
   #   filter(STEM == "STEM" & prof == 1 & is.na(FORMALIDAD) == FALSE & is.na(HIJOS) == FALSE) %>% 
   #   group_by(HIJOS, FORMALIDAD) %>% 
   #   summarise(TOTAL = survey_total(na.rm = TRUE,
   #                                  vartype = c("cv")))
   #
   #formalidad_civil <- enoesvy %>%
   #   filter(STEM == "STEM" & prof == 1 & is.na(FORMALIDAD) == FALSE & is.na(EDO_CIVIL) == FALSE & EDO_CIVIL != "no sabe") %>% 
   #   group_by(EDO_CIVIL, FORMALIDAD) %>% 
   #   summarise(TOTAL = survey_total(na.rm = TRUE,
   #                                  vartype = c("cv")))
   #
   por_sexo <- list(pea_sexo, posicion_sexo, formalidad_sexo, sector_sexo)
   #por_stem <- list(pea_hijos, pea_edo_civil, formalidad_hijos, formalidad_civil)
                    
   # Crear funcion para dar formato a cada df de resultados por sexo
   FormatoTotalesSexo <- function(df){
      colnames(df)[1] <- "STEM"
      df$INDICADOR <- colnames(df)[2]
      colnames(df)[2] <- "CAT1"
      
      df <- rename(df, CAT2 = SEXO)
      
      df$TIPO <- colnames(df)[4]
      colnames(df)[c(4, 5)] <- c("VALOR", "CV")
      
      df$CV <- round(df$CV, 2)
      df <- df %>% 
         select(STEM, INDICADOR, CAT1, CAT2, TIPO, VALOR, CV) %>% 
         filter(is.nan(VALOR) == FALSE)
      
      return(df)
   }
   
   # Crear funcion para dar formato a cada df de resultados por stem
   FormatoTotalesSTEM <- function(df){
      df$STEM <-  "STEM"
      
      df$INDICADOR <- colnames(df)[2]
      colnames(df)[2] <- "CAT1"
      colnames(df)[1] <- "CAT2"
      
      df$TIPO <- colnames(df)[3]
      colnames(df)[c(3, 4)] <- c("VALOR", "CV")
      
      df$CV <- round(df$CV, 2)
      df <- df %>% 
         select(STEM, INDICADOR, CAT1, CAT2, TIPO, VALOR, CV) %>% 
         filter(is.nan(VALOR) == FALSE)
      
      return(df)
   }
   
   # Usar funciones, dar formato manual a resultados que faltan y unir todo en una sola df
   por_sexo <- lapply(por_sexo, FormatoTotalesSexo)
   #por_stem <- lapply(por_stem, FormatoTotalesSTEM)
   
   total_sexo <- total_sexo %>%
      mutate(INDICADOR = "TOTAL",
             CAT1 = "total",
             TIPO = "TOTAL") %>%
      rename(CAT2 = SEXO,
             VALOR = TOTAL,
             CV = TOTAL_cv) %>% 
      select(STEM, INDICADOR, CAT1, CAT2, TIPO, VALOR, CV)
   
   #por_sexo <- bind_rows(total_sexo, por_sexo, por_stem)
   por_sexo <- bind_rows(total_sexo, por_sexo)
   
   por_sexo <- por_sexo %>%
      ungroup() %>%  
      select(STEM, INDICADOR, CAT1, CAT2, TIPO, VALOR, CV) %>% 
      arrange(INDICADOR, CAT1, TIPO, CAT2, STEM) %>% 
      mutate(VALOR = round(VALOR, 0),
             CV = round(CV, 2),
             PERIODO = periodo.enoe)
   
   # Calcular salarios
   salario <- enoesvy %>%
      filter(is.na(STEM) == FALSE & prof == 1) %>% 
      group_by(STEM) %>% 
      summarise(PROMEDIO = survey_mean(SALARIO,
                                       na.rm = TRUE,
                                       vartype = c("cv")))
   
   salario_sexo <- enoesvy %>%
      filter(is.na(STEM) == FALSE & prof == 1) %>% 
      group_by(STEM, SEXO) %>% 
      summarise(PROMEDIO = survey_mean(SALARIO,
                                       na.rm = TRUE,
                                       vartype = c("cv")))
   
   salario_formalidad_sexo <- enoesvy %>%
      filter(is.na(STEM) == FALSE & prof == 1 & is.na(FORMALIDAD) == FALSE) %>% 
      group_by(STEM, FORMALIDAD, SEXO) %>% 
      summarise(PROMEDIO = survey_mean(SALARIO,
                                       na.rm = TRUE,
                                       vartype = c("cv")))
   
   salario_por_sexo <- list(salario_formalidad_sexo)
   
   # Usar funcion, dar formato manual a resultados que faltan y unir todo en una sola df
   salario_por_sexo <- lapply(salario_por_sexo, FormatoTotalesSexo)
   
   salario_sexo <- salario_sexo %>%
      mutate(INDICADOR = "TOTAL",
             CAT1= "total",
             TIPO = "PROMEDIO") %>%
      rename(CAT2 = SEXO,
             VALOR = PROMEDIO,
             CV = PROMEDIO_cv) %>% 
      select(STEM, INDICADOR, CAT1, CAT2, TIPO, VALOR, CV)
   
   salario <- salario %>%
      mutate(INDICADOR = "TOTAL",
             CAT1 = "total",
             TIPO = "PROMEDIO",
             CAT2 = "ambos") %>%
      rename(VALOR = PROMEDIO,
             CV = PROMEDIO_cv) %>% 
      select(STEM, INDICADOR, CAT1, CAT2, TIPO, VALOR, CV)
   
   salario_por_sexo <- bind_rows(salario, salario_sexo, salario_por_sexo)
   
   salario_por_sexo <- salario_por_sexo %>%
      ungroup() %>%  
      select(STEM, INDICADOR, CAT1, CAT2, TIPO, VALOR, CV) %>% 
      arrange(INDICADOR, CAT1, TIPO, CAT2, STEM) %>% 
      mutate(VALOR = round(VALOR, 0),
             CV = round(CV, 2),
             PERIODO = periodo.enoe) %>% 
      filter(is.na(STEM) == FALSE)
   
   por_sexo <- bind_rows(por_sexo, salario_por_sexo)
   
   cat("Calculos STEM completados\n")

   return(por_sexo)
}
