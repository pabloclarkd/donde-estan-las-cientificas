#

library(tidyr)
library(dplyr)
library(stringr)

# importar datos

## f911
f9 <- readRDS("03_resultados\\SerieF911.RDS")
areas <- f9$res_agrup_amp
subareas <- f9$res_agrup_esp
carreras <- f9$res_agrup_det

## clasificacion de carreras
clas.car <- read.csv("C:\\Users\\pablo\\Google Drive\\R\\Compara_carreras\\02_fuentes\\otros\\cmpe 2016_stem.csv", encoding = "UTF-7")
clas.car <- clas.car %>% 
   mutate_all(as.character) %>% 
   mutate(cve.cam.amp = str_pad(cve.cam.amp, 2, "left", "0"),
          cve.cam.esp = str_pad(cve.cam.esp, 3, "left", "0"),
          cve.cam.det = str_pad(cve.cam.det, 4, "left", "0"))

carreras <- left_join(carreras,
                      select(clas.car, cve.cam.det, stem),
                      by = c("CAM_DET_CVE" = "cve.cam.det"))

carreras$stem <- recode(carreras$stem,
                        "1" = "stem",
                        "0" = "otras")

## cargar archivos con las claves y nombres de entidades
ents <- read.csv("C:\\Users\\pablo\\Google Drive\\R\\Compara_carreras\\02_fuentes\\otros\\entidades.csv")
colnames(ents)[1] <- "id"
ents <- mutate(ents, id = str_pad(id, 2, "left", "0"),
               nombre = tolower(nombre))

carreras <- left_join(carreras,
                      ents,
                      by = c("ENT_CV" = "id"))

carreras <- rename(carreras,
                   STEM = stem,
                   ENTIDAD = nombre)

# analizar datos

## matricula de carrera por año
mat_car_serie <- carreras %>% 
   group_by(PERIODO, CAM_DET_NOM) %>% 
   summarise(MAT = sum(M, na.rm = TRUE),
             MAT_M = sum(M.M, na.rm = TRUE))

## matricula de de stem por sost
stem_mat <- carreras %>% 
   filter(is.na(STEM) == FALSE) %>% 
   group_by(PERIODO, SOST, STEM) %>% 
   summarise(MAT_H = sum(M.H, na.rm = TRUE),
             MAT_M = sum(M.M, na.rm = TRUE),
             NI_H = sum(NI.H),
             NI_M = sum(NI.M),
             MAT = MAT_H + MAT_H)

## matricula de de stem, 2020, por sexo
stem_2020 <- carreras %>% 
   filter(PERIODO == "2020" & is.na(STEM) == FALSE) %>% 
   group_by(STEM) %>% 
   summarise(MAT_H = sum(M.H, na.rm = TRUE),
             MAT_M = sum(M.M, na.rm = TRUE),
             NI_H = sum(NI.H),
             NI_M = sum(NI.M))

## matricula de de stem, 2020, por sexo y sostenimiento
stem_sost_sexo_2020 <- carreras %>% 
   filter(PERIODO == "2020" & is.na(STEM) == FALSE) %>% 
   group_by(STEM, SOST) %>% 
   summarise(MAT_H = sum(M.H, na.rm = TRUE),
             MAT_M = sum(M.M, na.rm = TRUE))

stem_sost_2020 <- carreras %>% 
   filter(PERIODO == "2020" & is.na(STEM) == FALSE) %>% 
   group_by(STEM, SOST) %>% 
   summarise(MAT = sum(M, na.rm = TRUE))

## porcentaje de stem por entidad, 2020
stem_ent_2020 <- carreras %>% 
   filter(PERIODO == "2020" & is.na(STEM) == FALSE) %>% 
   group_by(STEM, ENTIDAD) %>% 
   summarise(MAT = sum(M, na.rm = TRUE),
             MAT_M = sum(M.M, na.rm = TRUE))

mat_ent_2020 <- carreras %>% 
   filter(PERIODO == "2020") %>% 
   group_by(ENTIDAD) %>% 
   summarise(MAT_ENT = sum(M, na.rm = TRUE),
             MAT_M_ENT = sum(M.M, na.rm = TRUE))

stem_ent_2020 <- left_join(stem_ent_2020,
                           mat_ent_2020,
                           by = "ENTIDAD")

stem_ent_2020 <- stem_ent_2020 %>% 
   mutate(MAT_P = round((MAT / MAT_ENT) * 100,
                        1),
          MAT_M_P = round((MAT_M / MAT_M_ENT) * 100,
                        1),
          )

# matricula por sexo
mat_sexo <- areas %>% 
   group_by(PERIODO) %>% 
   summarise(M.M = sum(M.M, na.rm = TRUE),
             M.H = sum(M.H, na.rm = TRUE),
             M = sum(M, na.rm = TRUE)) %>% 
   mutate(M.M.P = round(M.M / M, 3),
          M.H.P = round(M.H / M, 3))

# estudiantes mujeres en tics por entidad
mat_tic_ent <- carreras %>% 
   filter(PERIODO == "2020" & CAM_AMP == "06") %>% 
   group_by(ENTIDAD) %>% 
   summarise(MAT_M_TIC = sum(M.M))

mat_ent_2020 <- carreras %>% 
   filter(PERIODO == "2020") %>% 
   group_by(ENTIDAD) %>% 
   summarise(MAT_M_ENT = sum(M.M, na.rm = TRUE))

mat_tic_ent <- left_join(mat_tic_ent,
                         mat_ent_2020,
                         by = "ENTIDAD")

mat_tic_ent <- mat_tic_ent %>% 
   mutate(MAT_M_TIC_P = round((MAT_M_TIC / MAT_M_ENT) * 100,
                              1))
write.csv(mat_tic_ent, "matricula_femenina_tic.csv")
