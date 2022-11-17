CalcularXSTEM2 <- function(enoesvy, periodo.enoe){
   
   # La funcion CreateSalariosCarreras realiza los calculos salariales y de indicadores del mercado laboral
   
   # Calcular totales por sexo
   
   pea_hijos <- enoesvy %>%
      filter(SEXO == "mujer" & prof == 1 & is.na(ACTIVIDAD) == FALSE & is.na(HIJOS) == FALSE) %>% 
      group_by(STEM, HIJOS, ACTIVIDAD) %>% 
      summarise(TOTAL = survey_total(na.rm = TRUE,
                                     vartype = c("cv")))
   
   pea_edo_civil <- enoesvy %>%
      filter(SEXO == "mujer" & prof == 1 & is.na(ACTIVIDAD) == FALSE & is.na(EDO_CIVIL) == FALSE & EDO_CIVIL != "no sabe") %>% 
      group_by(STEM, EDO_CIVIL, ACTIVIDAD) %>% 
      summarise(TOTAL = survey_total(na.rm = TRUE,
                                     vartype = c("cv")))
   
   formalidad_hijos <- enoesvy %>%
      filter(SEXO == "mujer" & prof == 1 & is.na(FORMALIDAD) == FALSE & is.na(HIJOS) == FALSE) %>% 
      group_by(STEM, HIJOS, FORMALIDAD) %>% 
      summarise(TOTAL = survey_total(na.rm = TRUE,
                                     vartype = c("cv")))
   
   formalidad_civil <- enoesvy %>%
      filter(SEXO == "mujer" & prof == 1 & is.na(FORMALIDAD) == FALSE & is.na(EDO_CIVIL) == FALSE & EDO_CIVIL != "no sabe") %>% 
      group_by(STEM, EDO_CIVIL, FORMALIDAD) %>% 
      summarise(TOTAL = survey_total(na.rm = TRUE,
                                     vartype = c("cv")))
   
   por_stem <- list(pea_hijos, pea_edo_civil, formalidad_hijos, formalidad_civil)
   
   # Crear funcion para dar formato a cada df de resultados por sexo
   FormatoTotalesSexo <- function(df){
      df$INDICADOR <- colnames(df)[2]
      colnames(df)[2] <- "CAT1"
      colnames(df)[3] <- "CAT2"
      
      df$TIPO <- colnames(df)[4]
      colnames(df)[c(4, 5)] <- c("VALOR", "CV")
      
      df$CV <- round(df$CV, 2)
      df <- df %>% 
         select(STEM, INDICADOR, CAT1, CAT2, TIPO, VALOR, CV) %>% 
         filter(is.nan(VALOR) == FALSE)
      return(df)
   }
   
   
   # Usar funciones, dar formato manual a resultados que faltan y unir todo en una sola df
   por_stem <- lapply(por_stem, FormatoTotalesSexo)
   
   por_stem <- bind_rows(por_stem)
   
   cat("Calculos STEM completados\n")
   
   return(por_stem)
}
