#   ComparaMujeres vincula las diferentes funciones usadas para los calculos de Compara Carreras con enfoque de genero y stem
periodo.enoe <- c(121, 221, 321); version_nueva <- "cmpe2016"
periodo.enoe <- 221; version_nueva <- "cmpe2016"


ComparaMujeres <- function(periodo.enoe,
                           guardar.res = c("No", "Si"),
                           version_nueva = c("cmpe2011", "cmpe2016")){              
   
   
   #  Guardar el argumento a pasar para guardar o no los resultados en el disco y calcular tops de carreras
   guardar <- match.arg(guardar.res)
   version_nueva <- match.arg(version_nueva, choices = c("cmpe2011", "cmpe2016"))
   
   #  Cargar los paquetes necesarios 
   library(dplyr)
   library(tidyr)
   library(srvyr)
   library(xlsx)
   library(stringr)
   
   #  Importar los scripts  
   source("01_scripts\\02_CrearEnoe_M.R")
   source("01_scripts\\99_CalcularXSTEM_2.R")
   source("01_scripts\\RealizarEquivalenciasCMPE_M.R")
   
   #  Importar las clasificaciones de carreras
   clas.car <- list()
   clas.car$c11 <- read.csv("02_fuentes\\cmpe 2011_stem.csv", encoding = "UTF-7")
   clas.car$c11 <- clas.car$c11 %>% mutate_all(as.character)
   
   clas.car$c16 <- read.csv("02_fuentes\\cmpe 2016_stem.csv", encoding = "UTF-7")
   clas.car$c16 <- clas.car$c16 %>% 
      mutate_all(as.character) %>% 
      mutate(cve.cam.amp = str_pad(cve.cam.amp, 2, "left", "0"),
             cve.cam.esp = str_pad(cve.cam.esp, 3, "left", "0"),
             cve.cam.det = str_pad(cve.cam.det, 4, "left", "0"))
   
   if(version_nueva == "cmpe2011"){
      clas.car <- clas.car$c11
   }
   
   if(version_nueva == "cmpe2016"){
      clas.car <- clas.car$c16
   }
   
   #  Importar archivo con deflactores al tercer trimestre de 2020
   fac_inf <- read.csv("02_fuentes\\deflactor_trimestral_121.csv", stringsAsFactors = FALSE)
   
   #  Crear funcion que contiene todos los calculos a realizar para cada periodo
   
   Calculos <- function(periodo.enoe, fac_inf, version.cmpe){
      
      cat(paste("Iniciar calculos para periodo", periodo.enoe, "\n", sep = " "))
      
      ## Importar tabla "sdem" de microdatos de la enoe
      sdem <-  readRDS(paste("02_fuentes\\enoe\\", 
                             "sdemt", 
                             periodo.enoe, 
                             ".RDS", 
                             sep = ""))
      
      ## Crear una lista para guardar los resultados y correr los scripts
      resultados <- NULL   
      
      enoe <- CrearEnoe_M(sdem, periodo.enoe, fac_inf, version_nueva, clas.car)
      
      enoesvy <- enoe %>% as_survey_design(ids = UPM, strata = EST_D, weights = FAC, nest = TRUE)
      options(survey.lonely.psu="adjust")
      
      resultados[[1]] <- CalcularXSTEM2(enoesvy, periodo.enoe)
      
      names(resultados) <- c("stem")
      
      cat(paste("Calculos realizados para periodo", periodo.enoe, "\n", sep = " "))
      
      return(resultados)
   }
   
   # Correr scripts de calculos para compara carreras y guardar los resultados en una lista llamada resultados, donde [[1]] es la dataframe de niveles de escolaridad, 
   # [[2]] son resultados por area, [[3]] por subarea, [[4]] carreras (licenciaturas) y [[5]] es de tecnico superior universitario y 
   if(length(periodo.enoe) == 1){
      
      resultados <- Calculos(periodo.enoe, fac_inf = fac_inf, version.cmpe = "cmpe2016")
      
   } else {
      
      ##Si el numero de periodo a calcular es mayor a  uno, realizar los calculos para cada periodo primero
      ##luego calcular, para cada dataframe de resultados, el promedio de cada dato en todos los periodos y guardar las dfs con los datos promediados en un lista
      
      if(length(periodo.enoe) > 1){
         
         ##crear la lista para guardar los resultados de cada periodo y para guardar los resultados promediados
         lista.res <- list()
         
         ##realizar los calculos de cada periodo
         lista.res <- lapply(periodo.enoe, Calculos, fac_inf = fac_inf, version.cmpe = "cmpe2016")
         
         ## unir los resultados de todos los periodos en una sola data frame por tipo de resultado
         res_juntos <- list(data.frame())
         
         for(l in 1:length(lista.res)){
            
            for(i in 1:length(lista.res[[l]])){
               
               res_juntos[[i]] <- bind_rows(res_juntos[[i]], lista.res[[l]][[i]])
               
            }
            
         }
         
         ## promediar los resultados
            
            res_juntos <- res_juntos[[1]] %>% 
               group_by_at(c(1:5)) %>% 
               summarise(VALOR = round(mean(VALOR, 
                                            na.rm = TRUE), 
                                       0),
                         CV = round(mean(CV, 
                                         na.rm = TRUE), 
                                    2))
         
         resultados <- res_juntos
         
      }
      
   }
   
   
   cat("calculos de compara mujeres finalizados\n")
   
   return(resultados)
   
} 

