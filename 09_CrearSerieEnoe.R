#  Cargar paquetes 
library(dplyr)
library(tidyr)

#  cargar scripts
source("01_scripts\\01_ComparaMujeres.R")
version_nueva <- "cmpe2016"

# calcular serie de resultados anuales de la enoe y guardar los resultados en RDS
res12 <- ComparaMujeres(c(312, 412), guardar.res = "Si")
res13 <- ComparaMujeres(c(113, 213, 313, 413), guardar.res = "Si")
res14 <- ComparaMujeres(c(114, 214, 314, 414), guardar.res = "Si")
res15 <- ComparaMujeres(c(115, 214, 315, 415), guardar.res = "Si")
res16 <- ComparaMujeres(c(116, 216, 316, 416), guardar.res = "Si")
res17 <- ComparaMujeres(c(117, 217, 317, 417), guardar.res = "Si")
res18 <- ComparaMujeres(c(118, 218, 318, 418), guardar.res = "Si")
res19 <- ComparaMujeres(c(119, 219, 319, 419), guardar.res = "Si")
res20 <- ComparaMujeres(c(120, 320, 420), guardar.res = "Si")
res21 <- ComparaMujeres(c(121, 221, 321), guardar.res = "Si")

# importar resultados temporales
folder <- "03_resultados\\temporales"
res <- lapply(list.files(folder, pattern = ".RDS", full.names = TRUE), 
              readRDS)

# unir los resultados en una serie por tipo de calculos
niveles <- data.frame()
areas <- data.frame()
subareas <- data.frame()
stem <- data.frame()
carreras <- data.frame()

for(l in 1:length(res)){
   niveles <- bind_rows(niveles, res[[l]][[1]])
   areas <- bind_rows(areas, res[[l]][[2]])
   subareas <- bind_rows(subareas, res[[l]][[3]])
   stem <- bind_rows(stem, res[[l]][[4]])
   carreras <- bind_rows(carreras, res[[l]][[5]])
}

# dar formato a cada serie
niveles <- niveles %>% 
   mutate_at(vars(INDICADOR, SEXO, TIPO), tolower) %>% 
   mutate(PERIODO = paste("20", 
                          substring(PERIODO, 2, 3),
                          sep = ""))

areas <- areas %>% 
   mutate_at(vars(INDICADOR, SEXO, TIPO), tolower) %>% 
   mutate(PERIODO = paste("20", 
                          substring(PERIODO, 2, 3),
                          sep = ""))

subareas <- subareas %>% 
   mutate_at(vars(INDICADOR, SEXO, TIPO), tolower) %>% 
   mutate(PERIODO = paste("20", 
                          substring(PERIODO, 2, 3),
                          sep = ""))

stem <- stem %>% 
   mutate_at(vars(STEM, INDICADOR, TIPO), tolower) %>% 
   mutate(PERIODO = paste("20", 
                          substring(PERIODO, 2, 3),
                          sep = ""))

carreras <- carreras %>% 
   mutate_at(vars(INDICADOR, SEXO, TIPO), tolower) %>% 
   mutate(PERIODO = paste("20", 
                          substring(PERIODO, 2, 3),
                          sep = ""))

# unir en una lista y escribir
enoe <- list(niveles, areas, subareas, stem, carreras)
names(enoe) <- c("niveles", "areas", "subareas", "stem", "carreras")

saveRDS(enoe, "03_resultados\\SerieENOE.RDS")

