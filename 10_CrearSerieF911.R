# cargar paquetes
library(foreign)
library(readxl)
library(dplyr)
library(stringr)

# importar scripts
source("Mujeres STEM\\01_scripts\\RealizarEquivalenciasCMPE.R")

#  importar archivos
## formatos 911
folder <- "C:\\Users\\pablo\\Google Drive\\IMCO\\Bases de datos\\Formatos 911\\ES\\BASES DE DATOS\\"

f911 <- list()
f911$car20 <- read_excel(paste(sep = "", folder, "2020-2021/CARRERA_I2021_F1920.xlsx"))
f911$car19 <- read_excel(paste(sep = "", folder, "2019-2020\\CARRERA_I1920_F1818.xlsx"))
f911$car18 <- read_excel(paste(sep = "", folder, "2018-2019\\CARRERA_I1819_F1718.xlsx"), sheet = 3)
f911$car17 <- read_excel(paste(sep = "", folder, "2017-2018\\SUPERIOR LICENCIATURA INICIO 2017-2018.xlsx"))
f911$car16 <- read.dbf  (paste(sep = "", folder, "2016-2017\\KI9119A.dbf"))
f911$car15 <- read.dbf  (paste(sep = "", folder, "2015-2016\\KI9119A.dbf"))
f911$car14 <- read_excel(paste(sep = "", folder, "2014-2015\\911.9A 14-15.xlsx"))
f911$car13 <- read_excel(paste(sep = "", folder, "2013-2014\\911.9A 13-14.xlsx"))
f911$car12 <- read_excel(paste(sep = "", folder, "2012-2013\\911.9A 12-.13.xlsx"))
f911$car11 <- read_excel(paste(sep = "", folder, "2011-2012\\911.9A 11-12.xlsx"))

## clasificacion de carreras
clas.car <- read.csv("02_fuentes\\cmpe 2016_stem.csv", encoding = "UTF-7")
clas.car <- clas.car %>% 
   mutate_all(as.character) %>% 
   mutate(cve.cam.amp = str_pad(cve.cam.amp, 2, "left", "0"),
          cve.cam.esp = str_pad(cve.cam.esp, 3, "left", "0"),
          cve.cam.det = str_pad(cve.cam.det, 4, "left", "0"))

# claves y nombres de entidades
ents <- read.csv("02_fuentes\\entidades.csv")
colnames(ents)[1] <- "id"
ents <- mutate(ents, id = str_pad(id, 2, "left", "0"),
               nombre = tolower(nombre))

# filtar las bases de cada ciclo para seleccionar solo casos apropiados
f911$car11 <- f911$car11 %>% 
   filter()

# seleccionar y renombrar variables
## crear listas con los nombres originales y nuevos de las variables de cada archivo
### lista de variables a seleccionar en cada base 
var_sel <- list()
var_sel$car20 <- c("CV_ENT_INMUEBLE", "CONTROL", "CV_CARRERA", 
                   "V21", "V22", "V23", 
                   "V88", "V89", "V90", 
                   "V175", "V176", "V177")

var_sel$car19 <- c("cv_ent_inmueble", "CONTROL", "cv_carrera", 
                   "v21", "v22", "v23", 
                   "v88", "v89", "v90", 
                   "v175", "v176", "v177")

var_sel$car18 <- c("CV_ENT_INMUEBLE", "CONTROL", "CV_CARRERA", 
                   "V21", "V22", "V23", 
                   "V88", "V89", "V90", 
                   "V175", "V176", "V177")

var_sel$car17 <- c("CLAVECCT", "CONTROL2", "CARRERA", "NIVEL",
                   "S56", "S57", "S58", 
                   "S172H", "S172M", "S172", 
                   "S234H", "S234M", "S234")

var_sel$car16 <- c("CLAVECCT", "CONTROL", "CARRERA", "NIVEL",
                   "S56", "S57", "S58", 
                   "S172H", "S172M", "S172", 
                   "S234H", "S234M", "S234")

var_sel$car15 <- c("CLAVECCT", "CONTROL", "CARRERA", "NIVEL",
                   "S56", "S57", "S58", 
                   "S172H", "S172M", "S172", 
                   "S234H", "S234M", "S234")

var_sel$car14 <- c("CLAVECCT", "CONTROL", "CARRERA", "NIVEL",
                   "S56", "S57", "S58", 
                   "S172H", "S172M", "S172", 
                   "S234H", "S234M", "S234")

var_sel$car13 <- c("CLAVECCT", "CONTROL", "CARRERA", "NIVEL",
                   "S56", "S57", "S58", 
                   "S172H", "S172M", "S172", 
                   "S234H", "S234M", "S234")

var_sel$car12 <- c("CLAVECCT", "CONTROL", "CARRERA", "NIVEL",
                   "S56", "S57", "S58", 
                   "S172H", "S172M", "S172", 
                   "S234H", "S234M", "S234")

var_sel$car11 <- c("CLAVECCT", "CONTROL", "CARRERA",
                   "S56", "S57", "S58", 
                   "S172H", "S172M", "S172", 
                   "S234H", "S234M", "S234")

### listas de nuevos nombres para las variables de cada base 
var_renom <- list()
var_renom$car20 <- c("ENT_CV", "SOST", "CAR_CV",
                    "E.H", "E.M", "E", 
                    "NI.H", "NI.M", "NI", 
                    "M.H", "M.M", "M")

var_renom$car19 <- c("ENT_CV", "SOST", "CAR_CV",
                     "E.H", "E.M", "E", 
                     "NI.H", "NI.M", "NI", 
                     "M.H", "M.M", "M")

var_renom$car18 <- c("ENT_CV", "SOST", "CAR_CV",
                     "E.H", "E.M", "E", 
                     "NI.H", "NI.M", "NI", 
                     "M.H", "M.M", "M")

var_renom$car17 <- c("CCT", "SOST", "CAR_CV", "NIVEL",
                     "E.H", "E.M", "E", 
                     "NI.H", "NI.M", "NI", 
                     "M.H", "M.M", "M")

var_renom$car16 <- c("CCT", "SOST", "CAR_CV", "NIVEL",
                     "E.H", "E.M", "E", 
                     "NI.H", "NI.M", "NI", 
                     "M.H", "M.M", "M")

var_renom$car15 <- c("CCT", "SOST", "CAR_CV", "NIVEL",
                     "E.H", "E.M", "E", 
                     "NI.H", "NI.M", "NI", 
                     "M.H", "M.M", "M")

var_renom$car14 <- c("CCT", "SOST", "CAR_CV", "NIVEL",
                     "E.H", "E.M", "E", 
                     "NI.H", "NI.M", "NI", 
                     "M.H", "M.M", "M")

var_renom$car13 <- c("CCT", "SOST", "CAR_CV", "NIVEL",
                     "E.H", "E.M", "E", 
                     "NI.H", "NI.M", "NI", 
                     "M.H", "M.M", "M")

var_renom$car12 <- c("CCT", "SOST", "CAR_CV", "NIVEL",
                     "E.H", "E.M", "E", 
                     "NI.H", "NI.M", "NI", 
                     "M.H", "M.M", "M")

var_renom$car11 <- c("CCT", "SOST", "CAR_CV",
                     "E.H", "E.M", "E", 
                     "NI.H", "NI.M", "NI", 
                     "M.H", "M.M", "M")

## aplicar loop para seleccionar variables y cambiar sus nombres
for(l in 1:length(f911)){
   f911[[l]] <- f911[[l]] %>% select(var_sel[[l]])
   colnames(f911[[l]]) <- var_renom[[l]]
}

# dar formato a cada base
f911$car20 <- f911$car20 %>% 
   mutate(ENT_CV = str_pad(ENT_CV, 2, "left", "0"),
          SOST = recode(SOST,
                        "PÚBLICO" = "PUBLICO"),
          CAM_AMP = str_sub(CAR_CV, 2, 3),
          CAM_ESP = str_sub(CAR_CV, 2, 4),
          CAM_DET = str_sub(CAR_CV, 2, 5),
          PERIODO = "2020") %>% 
   mutate_at(vars(ENT_CV, SOST, CAR_CV, CAM_AMP, CAM_ESP, CAM_DET), as.character) %>%
   mutate_at(vars(E.H, E.M, E, NI.H, NI.M, NI, M.H, M.M, M), as.integer)

f911$car19 <- f911$car19 %>% 
   mutate(ENT_CV = str_pad(ENT_CV, 2, "left", "0"),
          SOST = recode(SOST,
                        "PÚBLICO" = "PUBLICO"),
          CAM_AMP = str_sub(CAR_CV, 2, 3),
          CAM_ESP = str_sub(CAR_CV, 2, 4),
          CAM_DET = str_sub(CAR_CV, 2, 5),
          PERIODO = "2019") %>% 
   mutate_at(vars(ENT_CV, SOST, CAR_CV, CAM_AMP, CAM_ESP, CAM_DET), as.character) %>%
   mutate_at(vars(E.H, E.M, E, NI.H, NI.M, NI, M.H, M.M, M), as.integer)

f911$car18 <- f911$car18 %>% 
   mutate(ENT_CV = str_pad(ENT_CV, 2, "left", "0"),
          SOST = recode(SOST,
                        "PÚBLICO" = "PUBLICO"),
          CAM_AMP = str_sub(CAR_CV, 2, 3),
          CAM_ESP = str_sub(CAR_CV, 2, 4),
          CAM_DET = str_sub(CAR_CV, 2, 5),
          PERIODO = "2018") %>% 
   mutate_at(vars(ENT_CV, SOST, CAR_CV, CAM_AMP, CAM_ESP, CAM_DET), as.character) %>%
   mutate_at(vars(E.H, E.M, E, NI.H, NI.M, NI, M.H, M.M, M), as.integer)

f911$car17 <- f911$car17 %>% 
   filter(NIVEL == "LICENCIATURA") %>% 
   mutate(ENT_CV = str_sub(CCT, 1, 2),
          SOST = recode(SOST,
                        "PARTICULAR" = "PRIVADO"),
          CAM_AMP = str_sub(CAR_CV, 2, 3),
          CAM_ESP = str_sub(CAR_CV, 2, 4),
          CAM_DET = str_sub(CAR_CV, 2, 5),
          PERIODO = "2017") %>% 
   select(-c(CCT, NIVEL)) %>% 
   mutate_at(vars(ENT_CV, SOST, CAR_CV, CAM_AMP, CAM_ESP, CAM_DET), as.character) %>%
   mutate_at(vars(E.H, E.M, E, NI.H, NI.M, NI, M.H, M.M, M), as.integer)

f911$car16 <- f911$car16 %>% 
   filter(NIVEL == "LICENCIATURA") %>%
   rename(CAR_CV_ORG = CAR_CV) %>% 
   mutate(ENT_CV = str_sub(CCT, 1, 2),
          SOST = recode(SOST,
                        "PARTICULAR" = "PRIVADO",
                        "AUTÓNOMO" = "PUBLICO",
                        "ESTATAL" = "PUBLICO",
                        "FEDERAL" = "PUBLICO",
                        "FEDERAL TRANSFERIDO" = "PUBLICO",),
          CAR_CV_ORG = str_sub(CAR_CV_ORG, 2, 4),
          CAR_CV = RealizarEquivalenciasCMPE(CAR_CV_ORG, 
                                             version_nueva = "cmpe2016"),
          CAM_AMP = str_sub(CAR_CV, 1, 2),
          CAM_ESP = str_sub(CAR_CV, 1, 3),
          CAM_DET = str_sub(CAR_CV, 1, 4),
          PERIODO = "2016") %>% 
   select(-c(CCT, NIVEL, CAR_CV_ORG)) %>% 
   mutate_at(vars(ENT_CV, SOST, CAR_CV, CAM_AMP, CAM_ESP, CAM_DET), as.character) %>%
   mutate_at(vars(E.H, E.M, E, NI.H, NI.M, NI, M.H, M.M, M), as.integer)

f911$car15 <- f911$car15 %>% 
   filter(NIVEL == "LICENCIATURA") %>%
   rename(CAR_CV_ORG = CAR_CV) %>% 
   mutate(ENT_CV = str_sub(CCT, 1, 2),
          SOST = recode(SOST,
                        "PARTICULAR" = "PRIVADO",
                        "AUTÓNOMO" = "PUBLICO",
                        "ESTATAL" = "PUBLICO",
                        "FEDERAL" = "PUBLICO",
                        "FEDERAL TRANSFERIDO" = "PUBLICO",),
          CAR_CV_ORG = str_sub(CAR_CV_ORG, 2, 4),
          CAR_CV = RealizarEquivalenciasCMPE(CAR_CV_ORG, 
                                             version_nueva = "cmpe2016"),
          CAM_AMP = str_sub(CAR_CV, 1, 2),
          CAM_ESP = str_sub(CAR_CV, 1, 3),
          CAM_DET = str_sub(CAR_CV, 1, 4),
          PERIODO = "2015") %>% 
   select(-c(CCT, NIVEL, CAR_CV_ORG)) %>% 
   mutate_at(vars(ENT_CV, SOST, CAR_CV, CAM_AMP, CAM_ESP, CAM_DET), as.character) %>%
   mutate_at(vars(E.H, E.M, E, NI.H, NI.M, NI, M.H, M.M, M), as.integer)

f911$car14 <- f911$car14 %>% 
   filter(NIVEL == "LICENCIATURA") %>%
   rename(CAR_CV_ORG = CAR_CV) %>% 
   mutate(ENT_CV = str_sub(CCT, 1, 2),
          SOST = recode(SOST,
                        "PARTICULAR" = "PRIVADO",
                        "AUTÓNOMO" = "PUBLICO",
                        "ESTATAL" = "PUBLICO",
                        "FEDERAL" = "PUBLICO",
                        "FEDERAL TRANSFERIDO" = "PUBLICO",),
          CAR_CV_ORG = str_sub(CAR_CV_ORG, 2, 4),
          CAR_CV = RealizarEquivalenciasCMPE(CAR_CV_ORG, 
                                             version_nueva = "cmpe2016"),
          CAM_AMP = str_sub(CAR_CV, 1, 2),
          CAM_ESP = str_sub(CAR_CV, 1, 3),
          CAM_DET = str_sub(CAR_CV, 1, 4),
          PERIODO = "2014") %>% 
   select(-c(CCT, NIVEL, CAR_CV_ORG)) %>% 
   mutate_at(vars(ENT_CV, SOST, CAR_CV, CAM_AMP, CAM_ESP, CAM_DET), as.character) %>%
   mutate_at(vars(E.H, E.M, E, NI.H, NI.M, NI, M.H, M.M, M), as.integer)

f911$car13 <- f911$car13 %>% 
   filter(NIVEL == "LICENCIATURA") %>%
   rename(CAR_CV_ORG = CAR_CV) %>% 
   mutate(ENT_CV = str_sub(CCT, 1, 2),
          SOST = recode(SOST,
                        "PARTICULAR" = "PRIVADO",
                        "AUTÓNOMO" = "PUBLICO",
                        "ESTATAL" = "PUBLICO",
                        "FEDERAL" = "PUBLICO",
                        "FEDERAL TRANSFERIDO" = "PUBLICO",),
          CAR_CV_ORG = str_sub(CAR_CV_ORG, 2, 4),
          CAR_CV = RealizarEquivalenciasCMPE(CAR_CV_ORG, 
                                             version_nueva = "cmpe2016"),
          CAM_AMP = str_sub(CAR_CV, 1, 2),
          CAM_ESP = str_sub(CAR_CV, 1, 3),
          CAM_DET = str_sub(CAR_CV, 1, 4),
          PERIODO = "2013") %>% 
   select(-c(CCT, NIVEL, CAR_CV_ORG)) %>% 
   mutate_at(vars(ENT_CV, SOST, CAR_CV, CAM_AMP, CAM_ESP, CAM_DET), as.character) %>%
   mutate_at(vars(E.H, E.M, E, NI.H, NI.M, NI, M.H, M.M, M), as.integer)

f911$car12 <- f911$car12 %>% 
   filter(NIVEL == "LICENCIATURA") %>%
   rename(CAR_CV_ORG = CAR_CV) %>% 
   mutate(ENT_CV = str_sub(CCT, 1, 2),
          SOST = recode(SOST,
                        "PARTICULAR" = "PRIVADO",
                        "AUTÓNOMO" = "PUBLICO",
                        "ESTATAL" = "PUBLICO",
                        "FEDERAL" = "PUBLICO",
                        "FEDERAL TRANSFERIDO" = "PUBLICO",),
          CAR_CV_ORG = str_sub(CAR_CV_ORG, 2, 4),
          CAR_CV = RealizarEquivalenciasCMPE(CAR_CV_ORG, 
                                             version_nueva = "cmpe2016"),
          CAM_AMP = str_sub(CAR_CV, 1, 2),
          CAM_ESP = str_sub(CAR_CV, 1, 3),
          CAM_DET = str_sub(CAR_CV, 1, 4),
          PERIODO = "2013") %>% 
   select(-c(CCT, NIVEL, CAR_CV_ORG)) %>% 
   mutate_at(vars(ENT_CV, SOST, CAR_CV, CAM_AMP, CAM_ESP, CAM_DET), as.character) %>%
   mutate_at(vars(E.H, E.M, E, NI.H, NI.M, NI, M.H, M.M, M), as.integer)

f911$car11 <- f911$car11 %>% 
   mutate(NIVEL = str_sub(CAR_CV, 1, 1)) %>% 
   filter(NIVEL == 5) %>%
   rename(CAR_CV_ORG = CAR_CV) %>% 
   mutate(ENT_CV = str_sub(CCT, 1, 2),
          SOST = recode(SOST,
                        "PARTICULAR" = "PRIVADO",
                        "AUTÓNOMO" = "PUBLICO",
                        "ESTATAL" = "PUBLICO",
                        "FEDERAL" = "PUBLICO",
                        "FEDERAL TRANSFERIDO" = "PUBLICO",),
          CAR_CV_ORG = str_sub(CAR_CV_ORG, 2, 4),
          CAR_CV = RealizarEquivalenciasCMPE(CAR_CV_ORG, 
                                             version_nueva = "cmpe2016"),
          CAM_AMP = str_sub(CAR_CV, 1, 2),
          CAM_ESP = str_sub(CAR_CV, 1, 3),
          CAM_DET = str_sub(CAR_CV, 1, 4),
          PERIODO = "2013") %>% 
   select(-c(CCT, NIVEL, CAR_CV_ORG)) %>% 
   mutate_at(vars(ENT_CV, SOST, CAR_CV, CAM_AMP, CAM_ESP, CAM_DET), as.character) %>%
   mutate_at(vars(E.H, E.M, E, NI.H, NI.M, NI, M.H, M.M, M), as.integer)

# unir todos los periodos y resumir datos por nivel de campo
res <- bind_rows(f911)

res_agrup_det <- res %>% 
   group_by(ENT_CV, SOST, CAM_DET, PERIODO) %>% 
   summarise(across(c(E.H, E.M, E, NI.H, NI.M, NI, M.H, M.M, M), sum, na.rm = TRUE)) %>% 
   arrange(PERIODO, ENT_CV, CAM_DET, SOST, desc(M))

res_agrup_esp <- res %>% 
   group_by(ENT_CV, SOST, CAM_ESP, PERIODO) %>% 
   summarise(across(c(E.H, E.M, E, NI.H, NI.M, NI, M.H, M.M, M), sum, na.rm = TRUE)) %>% 
   arrange(PERIODO, ENT_CV, CAM_ESP, SOST, desc(M))

res_agrup_amp <- res %>% 
   group_by(ENT_CV, SOST, CAM_AMP, PERIODO) %>% 
   summarise(across(c(E.H, E.M, E, NI.H, NI.M, NI, M.H, M.M, M), sum, na.rm = TRUE)) %>% 
   arrange(PERIODO, ENT_CV, CAM_AMP, SOST, desc(M))

# ajustar formato
## resultados por campo detallado
res_agrup_det <- left_join(res_agrup_det,
                           select(clas.car, cve.cam.det, nom.cam.det),
                           by = c("CAM_DET" = "cve.cam.det"))

res_agrup_det <- res_agrup_det %>% 
   mutate(CAM_AMP = str_sub(CAM_DET, 1, 2),
          CAM_ESP = str_sub(CAM_DET, 1, 3)) %>% 
   rename(CAM_DET_CVE = CAM_DET,
          CAM_DET_NOM = nom.cam.det)

## resultados por campo especifico
res_agrup_esp <- left_join(res_agrup_esp,
                           unique(select(clas.car, cve.cam.esp, nom.cam.esp)),
                           by = c("CAM_ESP" = "cve.cam.esp"))

res_agrup_esp <- res_agrup_esp %>% 
   mutate(CAM_AMP = str_sub(CAM_ESP, 1, 2)) %>% 
   rename(CAM_ESP_CVE = CAM_ESP,
          CAM_ESP_NOM = nom.cam.esp)

## resultados por campo amplio
res_agrup_amp <- left_join(res_agrup_amp,
                           unique(select(clas.car, cve.cam.amp, nom.cam.amp)),
                           by = c("CAM_AMP" = "cve.cam.amp"))

res_agrup_amp <- res_agrup_amp %>%  
   rename(CAM_AMP_CVE = CAM_AMP,
          CAM_AMP_NOM = nom.cam.amp)

# guardar en una sola lista y escribir
f911 <- list(res_agrup_amp, res_agrup_esp, res_agrup_det)
names(f911)<- c("res_agrup_amp", "res_agrup_esp", "res_agrup_det")

saveRDS(f911, "Mujeres STEM\\03_resultados\\SerieF911.RDS")
