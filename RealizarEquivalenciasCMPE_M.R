# RealizarEquivalenciasCMPE

library(stringr)
library(here)

## Este script realiza las equivalencias de claves de carreras entre la version 2011 de la CMPE y la version 2016, o viceversa

RealizarEquivalenciasCMPE_M <- function(claves, version_nueva){
   
   # Importar archivo con equivalencia entre versiones de cmpe
   equi <- read.csv(here("02_fuentes//", "equivalencias_cmpe2016a2011.csv"))
   equi <- mutate_all(equi, as.character)
   equi$cmpe2016 <- str_pad(equi$cmpe2016, 4, "left", "0")
   
   # Detectar cual version de cmpe existe en la columna de claves 
   
   DetectarVersion <- function(claves){
      
      claves_sin_na <- claves[is.na(claves) == FALSE]
         
      ## si el maximo numero de caracteres de las claves es 3, se revisa que no haya claves mas altas que la mayor de la cmpe2011
      if(max(nchar(claves_sin_na), na.rm = TRUE) == 3){
         
         if(max(claves_sin_na, na.rm = TRUE) > max(equi$cmpe2011, na.rm = TRUE)){
            
            version_existente <- "cmpe2016"
            
         }
         
         version_existente <- "cmpe2011"
      }
      
      ## si el maximo numero de caracteres de las claves es 4, la version_existente es cmpe2016
      if(max(nchar(claves_sin_na), na.rm = TRUE) == 4){
         
         version_existente <- "cmpe2016"
      }
      
      ## si el maximo numero de caracteres de las claves no es 3 o 4, la version_existente es desconocida
      if(max(nchar(claves_sin_na), na.rm = TRUE)  < 3 | max(nchar(claves_sin_na), na.rm = TRUE)  > 4){
         
         stop("No fue posible determinar la version_existente de CMPE")
      }
      
      return(version_existente)
   } 
   
   version_existente <- DetectarVersion(claves)
   
   # Realizar equivalencias
   
   RealizarEquivalencias <- function(version_nueva, version_existente, equi, claves){
      
      ## Si la version existente es igual a la version nueva, no se realizan modificaciones
      if(version_existente == version_nueva){
         
         claves_nuevas <- claves
         
      } else {
         
         ## Crear una dataframe con una columna con las claves introducidas 
         df <- data.frame(claves,
                          stringsAsFactors = FALSE)
         
         ## unir con la df de equivalencias, cambiando de columna segun la version del cmpe que corresponda, y seleccionar la columna de claves nuevas
         
         if(version_existente == "cmpe2011"){
            
            ### al pasar de cmpe2011 a cmpe 2016, 
            equi <- arrange(equi, cmpe2016, cmpe2011)
            
            ### unir
            
            Buscar <- function(clave_2011){
               
               ind <- grep(clave_2011, equi$cmpe2011)[1]
               
               clave_nueva <- equi$cmpe2016[ind]
               
               return(clave_nueva)
            }
            
            df$claves_nuevas <- apply(df, 1, Buscar)
            
            claves_nuevas <- df[, 2]
            
         }
         
         if(version_existente == "cmpe2016"){
            
            df$id <- seq.int(1, nrow(df))
            
            df <- left_join(df, 
                            equi, 
                            by = c("claves" = "cmpe2016"))
            
            df <- df %>% group_by(id) %>% 
               filter(id == min(id)) %>% 
               slice(1) %>% 
               ungroup()
         
            df <- select(df, -id)
            
            claves_nuevas <- as.data.frame(df)[, 2]
            
         }
         
      }
      
      return(claves_nuevas)
   }
   
   claves_nuevas <- RealizarEquivalencias(version_nueva, version_existente, equi, claves)
   
   return(claves_nuevas)
   
}
