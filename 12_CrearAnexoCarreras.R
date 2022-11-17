# crear anexo de planes de estudio por grupo de carreras

library(foreign)
library(readxl)
library(dplyr)
library(stringr)

# importar scripts
source("01_scripts\\RealizarEquivalenciasCMPE_M.R")

#  importar archivos
## formatos 911
car <- read_excel("C:\\Users\\pablo\\Google Drive\\IMCO\\Bases de datos\\Formatos 911\\ES\\BASES DE DATOS\\2020-2021/CARRERA_I2021_F1920.xlsx")

## clasificacion de carreras
clas.car <- read.csv("02_fuentes\\cmpe 2016_stem.csv", encoding = "UTF-7")
clas.car <- clas.car %>% 
   mutate_all(as.character) %>% 
   mutate(cve.cam.amp = str_pad(cve.cam.amp, 2, "left", "0"),
          cve.cam.esp = str_pad(cve.cam.esp, 3, "left", "0"),
          cve.cam.det = str_pad(cve.cam.det, 4, "left", "0"))

# sumar matricula por campo y nombre de carrera
cars <- car %>% 
   select(CV_GRUPO, V177, C_NOM_CARRERA) %>% 
   rename(MAT = V177,
          NOM = C_NOM_CARRERA) %>%
   mutate(NOM = str_to_sentence(NOM),
          CV_GRUPO = substring(CV_GRUPO, 1, 3)) %>% 
   group_by(CV_GRUPO, NOM) %>% 
   summarise(MAT_T = sum(MAT, na.rm = TRUE))

# seleccionar los cinco nombres con mas matricula de campo campo
nombres <- data.frame(CV_GRUPO = NULL,
                      NOM = NULL)

for(i in 1:length(unique(cars$CV_GRUPO))){
   campo <- filter(cars,
                   CV_GRUPO == unique(cars$CV_GRUPO)[i])
   
   campo <- arrange(campo, desc(MAT_T))
   campo <- campo[1:5, c("CV_GRUPO", "NOM")]
   
   # cambiar nombres
   campo$NOM <- str_replace(campo$NOM, "Licenciatura", "Lic.")
   campo$NOM <- str_replace(campo$NOM, "Licenciado", "Lic.")
   campo$NOM <- str_replace(campo$NOM, "Ingeniería", "Ing.")
   campo$NOM <- str_replace(campo$NOM, "Ingeniero", "Ing.")
   
   nombres <- bind_rows(nombres, campo)
}

nombres <- left_join(nombres,
                     unique(select(clas.car, nom.cam.amp, cve.cam.esp, nom.cam.esp, stem_subarea)),
                     by = c("CV_GRUPO" = "cve.cam.esp")) 

nombres <- nombres %>%
   rename(AREA = nom.cam.amp,
          SUBAREA = nom.cam.esp,
          PLANES_DE_ESTUDIO = NOM,
          STEM = stem_subarea) %>% 
   select(AREA, SUBAREA, PLANES_DE_ESTUDIO, STEM)

write.csv(nombres, "03_resultados//planes_estudio_por_grupo.csv")


