#  La funcion CrearCalculosNiveles realiza los calculos poblacionales y de salario por nivel de estudio

CalcularXNiveles_M <- function(enoesvy, periodo.enoe){
   
   #guardar el orden de los niveles de estudio para dar formato a los resultados
   orden.nivel.estudio <- c("lice", "l.tec",  "prep", "secu", "prim", "ning")
   
   # Calcular totales por sexo
    
    total_sexo <- enoesvy %>%
       filter(is.na(m.esc) == FALSE) %>% 
       group_by(m.esc, SEXO) %>% 
       summarise(TOTAL = survey_total(na.rm = TRUE,
                                      vartype = c("cv")))
    pea_sexo <- enoesvy %>%
       filter(is.na(m.esc) == FALSE & is.na(ACTIVIDAD) == FALSE) %>% 
       group_by(m.esc, ACTIVIDAD, SEXO) %>% 
       summarise(TOTAL = survey_total(na.rm = TRUE,
                                      vartype = c("cv")))
    
    ocupacion_sexo <- enoesvy %>%
       filter(is.na(m.esc) == FALSE & is.na(OCUPACION) == FALSE) %>% 
       group_by(m.esc, OCUPACION, SEXO) %>% 
       summarise(TOTAL = survey_total(na.rm = TRUE,
                                      vartype = c("cv")))
    
   por_sexo <- list(pea_sexo, ocupacion_sexo)
   
   # Crear funcion para dar formato a cada df de resultados
   FormatoTotalesSexo <- function(df){
       colnames(df)[1] <- "ESC"
       df$INDICADOR <- colnames(df)[2]
       colnames(df)[2] <- "CATEGORIA"
       
       df$TIPO <- colnames(df)[4]
       colnames(df)[c(4, 5)] <- c("VALOR", "CV")
       
       df$CV <- round(df$CV, 2)
       df <- df %>% 
           select(ESC, INDICADOR, CATEGORIA, SEXO, TIPO, VALOR, CV) %>% 
           filter(is.nan(VALOR) == FALSE)
       
       return(df)
   }
   
   # Usar funcion, dar formato manual a resultados que faltan y unir todo en una sola df
   por_sexo <- lapply(por_sexo, FormatoTotalesSexo)
   
   total_sexo <- total_sexo %>%
       mutate(INDICADOR = "TOTAL",
              CATEGORIA = "TOTAL",
              TIPO = "TOTAL") %>%
       rename(VALOR = TOTAL,
              CV = TOTAL_cv,
              ESC = m.esc) %>% 
       select(ESC, INDICADOR, CATEGORIA, SEXO, TIPO, VALOR, CV)
   
   por_sexo <- bind_rows(total_sexo, por_sexo)
   
   # Calcular salarios
   salario <- enoesvy %>%
       filter(is.na(m.esc) == FALSE) %>% 
       group_by(m.esc) %>% 
       summarise(PROMEDIO = survey_mean(SALARIO,
                                        na.rm = TRUE,
                                        vartype = c("cv")))
   
   salario_sexo <- enoesvy %>%
       filter(is.na(m.esc) == FALSE) %>% 
       group_by(m.esc, SEXO) %>% 
       summarise(PROMEDIO = survey_mean(SALARIO,
                                        na.rm = TRUE,
                                        vartype = c("cv")))

    # Usar funcion, dar formato manual a resultados que faltan y unir todo en una sola df
   salario_sexo <- salario_sexo %>%
       mutate(INDICADOR = "TOTAL",
              CATEGORIA = "TOTAL",
              TIPO = "PROMEDIO") %>%
       rename(VALOR = PROMEDIO,
              CV = PROMEDIO_cv,
              ESC = m.esc) %>% 
       select(ESC, INDICADOR, CATEGORIA, SEXO, TIPO, VALOR, CV)
   
   salario <- salario %>%
       mutate(INDICADOR = "TOTAL",
              CATEGORIA = "TOTAL",
              TIPO = "PROMEDIO",
              SEXO = "AMBOS") %>%
       rename(VALOR = PROMEDIO,
              CV = PROMEDIO_cv,
              ESC = m.esc) %>% 
       select(ESC, INDICADOR, CATEGORIA, SEXO, TIPO, VALOR, CV)
   
   salario_por_sexo <- bind_rows(salario, salario_sexo)
   
   por_sexo <- bind_rows(por_sexo, salario_por_sexo)
   
   por_sexo$ESC <- factor(por_sexo$ESC, levels = orden.nivel.estudio)
   por_sexo <- por_sexo %>% 
       arrange(INDICADOR, CATEGORIA, TIPO, SEXO, ESC) %>% 
       mutate(VALOR = round(VALOR, 0),
              PERIODO = periodo.enoe)
   
   cat("Calculos por nivel completados\n")
   
   return(por_sexo)
   
}
