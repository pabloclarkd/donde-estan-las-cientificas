#
library(tidyr)
library(dplyr)

# importar datos

enoe <- readRDS("03_resultados\\SerieENOE.RDS")

niveles <- enoe$niveles
areas <- enoe$areas
subareas <- enoe$subareas
stem <- enoe$stem
carreras <- enoe$carreras

clas.car <- read.csv("02_fuentes\\cmpe 2016_stem.csv", encoding = "UTF-7")

#  calcular datos
## total de lice por sexo, serie
Tot_lice_sexo <- function(){
   
   tot_lice <- niveles %>% 
      filter(ESC == "lice" & INDICADOR == "total" & SEXO != "ambos" & TIPO == "total" & PERIODO != "2012") %>% 
      slice(-c(6, 7)) %>% 
      select(-c(CV, ESC, INDICADOR, CATEGORIA, TIPO)) %>% 
      pivot_wider(values_from = VALOR,
                  names_from = SEXO)
      
}

## total por nivel en 2021
Tot_esc_2021 <- function(res){
   
   tot_esc_2021 <- res %>% 
      filter(INDICADOR == "total" & SEXO != "ambos" & TIPO == "total" & PERIODO == "2021") %>%
      group_by(ESC, SEXO) %>% 
      summarise(TOTAL = sum(VALOR, na.rm = TRUE))
   
   ## porcentaje de es en 2021
   sum(tot_esc_2021$TOTAL[tot_esc_2021$ESC %in% c("lice", "l.tec")]) / sum(tot_esc_2021$TOTAL)
   
   ## distribucion por sexo de es en 2021
   sum(tot_esc_2021$TOTAL[tot_esc_2021$ESC %in% c("lice", "l.tec") & tot_esc_2021$SEXO == "mujer"]) / sum(tot_esc_2021$TOTAL[tot_esc_2021$ESC %in% c("lice", "l.tec")])
   
   ## porcentaje por sexo con es en 2021
   sum(tot_esc_2021$TOTAL[tot_esc_2021$ESC %in% c("lice", "l.tec") & tot_esc_2021$SEXO == "mujer"]) / sum(tot_esc_2021$TOTAL[tot_esc_2021$SEXO == "mujer"])
   sum(tot_esc_2021$TOTAL[tot_esc_2021$ESC %in% c("lice", "l.tec") & tot_esc_2021$SEXO == "hombre"]) / sum(tot_esc_2021$TOTAL[tot_esc_2021$SEXO == "hombre"])
   
   return(tot_esc_2021)
}

tot_esc_2021 <- Tot_esc_2021(niveles)

## total por nivel en 2012
Tot_esc_2012 <- function(res){
   
   tot_esc_2012 <- res %>% 
      filter(INDICADOR == "total" & SEXO != "ambos" & TIPO == "total" & PERIODO == "2012") %>%
      group_by(ESC, SEXO) %>% 
      summarise(TOTAL = sum(VALOR, na.rm = TRUE))
   
   ## porcentaje de es en 2012
   sum(tot_esc_2012$TOTAL[tot_esc_2012$ESC %in% c("lice", "l.tec")]) / sum(tot_esc_2012$TOTAL)
   
   ## distribucion por sexo de es en 2012
   sum(tot_esc_2012$TOTAL[tot_esc_2012$ESC %in% c("lice", "l.tec") & tot_esc_2012$SEXO == "mujer"]) / sum(tot_esc_2012$TOTAL[tot_esc_2012$ESC %in% c("lice", "l.tec")])
   
   ## porcentaje por sexo con es en 2012
   sum(tot_esc_2012$TOTAL[tot_esc_2012$ESC %in% c("lice", "l.tec") & tot_esc_2012$SEXO == "mujer"]) / sum(tot_esc_2012$TOTAL[tot_esc_2012$SEXO == "mujer"])
   sum(tot_esc_2012$TOTAL[tot_esc_2012$ESC %in% c("lice", "l.tec") & tot_esc_2012$SEXO == "hombre"]) / sum(tot_esc_2012$TOTAL[tot_esc_2012$SEXO == "hombre"])
   
   return(tot_esc_2012)
}

tot_esc_2012 <- Tot_esc_2012(niveles)

## tasas promedio de crecimiento
CalcularTasasPromedioCrecimientoTotES <- function(tot_esc_2021, tot_esc_2012){
   
   ### tmac tot de es
   tot_es_2021 <- sum(tot_esc_2021$TOTAL[tot_esc_2021$ESC %in% c("lice", "l.tec")])
   tot_es_2012 <- sum(tot_esc_2012$TOTAL[tot_esc_2012$ESC %in% c("lice", "l.tec")])
   
   tmac_tot <- round((((tot_es_2021 / tot_es_2012) ^ (1 / 9)) - 1) * 100, 1)
   
   ### tmac de Mujeres con ES
   muj_es_2021 <- sum(tot_esc_2021$TOTAL[tot_esc_2021$ESC %in% c("lice", "l.tec") & tot_esc_2021$SEXO == "mujer"])
   muj_es_2012 <- sum(tot_esc_2012$TOTAL[tot_esc_2012$ESC %in% c("lice", "l.tec") & tot_esc_2012$SEXO == "mujer"])
   
   tmac_muj <- round((((muj_es_2021 / muj_es_2012) ^ (1 / 9)) - 1) * 100, 1)
   
   ### tmac de Hombres con ES
   hom_es_2021 <- sum(tot_esc_2021$TOTAL[tot_esc_2021$ESC %in% c("lice", "l.tec") & tot_esc_2021$SEXO == "hombre"])
   hom_es_2012 <- sum(tot_esc_2012$TOTAL[tot_esc_2012$ESC %in% c("lice", "l.tec") & tot_esc_2012$SEXO == "hombre"])
   
   tmac_hom <- round((((hom_es_2021 / hom_es_2012) ^ (1 / 9)) - 1) * 100, 1)
   
}

## total por area
### 2021
Tot_area_2021 <- function(areas){
   
   tot_area_2021 <- areas %>% 
      filter(is.na(AREA) == FALSE & INDICADOR == "total" & SEXO != "ambos" & TIPO == "total" & PERIODO == "2021") %>%
      group_by(AREA) %>% 
      summarise(TOTAL = sum(VALOR, na.rm = TRUE))
}

tot_area_2021 <- Tot_area_2021(areas)
tot_area_2021$TOTAL_P <- round((tot_area_2021$TOTAL / sum(tot_area_2021$TOTAL) * 100), 1)

### 2012
Tot_area_2012 <- function(areas){
   
   tot_area_2012 <- areas %>% 
      filter(INDICADOR == "total" & SEXO != "ambos" & TIPO == "total" & PERIODO == "2012") %>%
      group_by(AREA) %>% 
      summarise(TOTAL = sum(VALOR, na.rm = TRUE))
}

tot_area_2012 <- Tot_area_2012(areas)
tot_area_2012$TOTAL_P <- round((tot_area_2012$TOTAL / sum(tot_area_2012$TOTAL) * 100), 1)

### TMCA
tot_area <- left_join(rename(tot_area_2012,
                             TOTAL2012 = TOTAL,
                             P2012 = TOTAL_P),
                      rename(tot_area_2021,
                             TOTAL2021 = TOTAL,
                             P2021 = TOTAL_P),
                      by = "AREA")

tot_area <- tot_area %>% 
   mutate(TMCA = round((((TOTAL2021 / TOTAL2012) ^ (1/9)) - 1) * 100, 
                       1)
          )

## salario por area
sal_area_2021 <- areas %>% 
   filter(is.na(AREA) == FALSE & INDICADOR == "total" & SEXO != "ambos" & TIPO == "promedio" & PERIODO == "2021") %>% 
   select(AREA, SEXO, VALOR)

sal_area_2021 <- pivot_wider(sal_area_2021,
                             names_from = c(SEXO),
                             values_from = c(VALOR))

## mujeres por area
### 2021
Muj_area_2021 <- function(areas){
   
   muj_area_2021 <- areas %>% 
      filter(INDICADOR == "total" & SEXO == "mujer" & TIPO == "total" & PERIODO == "2021") %>%
      group_by(AREA) %>% 
      summarise(TOTAL = sum(VALOR, na.rm = TRUE))
}

muj_area_2021 <- Muj_area_2021(areas)
muj_area_2021$TOTAL_P <- round((muj_area_2021$TOTAL / sum(muj_area_2021$TOTAL) * 100), 1)

### 2012
muj_area_2012 <- function(areas){
   
   muj_area_2012 <- areas %>% 
      filter(INDICADOR == "total" & SEXO == "mujer" & TIPO == "total" & PERIODO == "2012") %>%
      group_by(AREA) %>% 
      summarise(TOTAL = sum(VALOR, na.rm = TRUE))
}

muj_area_2012 <- muj_area_2012(areas)
muj_area_2012$TOTAL_P <- round((muj_area_2012$TOTAL / sum(muj_area_2012$TOTAL) * 100), 1)

### TMCA
muj_area <- left_join(rename(muj_area_2012,
                             TOTAL2012 = TOTAL,
                             P2012 = TOTAL_P),
                      rename(muj_area_2021,
                             TOTAL2021 = TOTAL,
                             P2021 = TOTAL_P),
                      by = "AREA")

muj_area <- muj_area %>% 
   mutate(TMCA = round((((TOTAL2021 / TOTAL2012) ^ (1/9)) - 1) * 100, 
                       1)
   )

## hombrees por area
### 2021
Hom_area_2021 <- function(areas){
   
   hom_area_2021 <- areas %>% 
      filter(INDICADOR == "total" & SEXO == "hombre" & TIPO == "total" & PERIODO == "2021") %>%
      group_by(AREA) %>% 
      summarise(TOTAL = sum(VALOR, na.rm = TRUE))
}

hom_area_2021 <- Hom_area_2021(areas)
hom_area_2021$TOTAL_P <- round((hom_area_2021$TOTAL / sum(hom_area_2021$TOTAL) * 100), 1)

### 2012
Hom_area_2012 <- function(areas){
   
   hom_area_2012 <- areas %>% 
      filter(INDICADOR == "total" & SEXO == "hombre" & TIPO == "total" & PERIODO == "2012") %>%
      group_by(AREA) %>% 
      summarise(TOTAL = sum(VALOR, na.rm = TRUE))
}

hom_area_2012 <- Hom_area_2012(areas)
hom_area_2012$TOTAL_P <- round((hom_area_2012$TOTAL / sum(hom_area_2012$TOTAL) * 100), 1)

### TMCA
hom_area <- left_join(rename(hom_area_2012,
                             TOTAL2012 = TOTAL,
                             P2012 = TOTAL_P),
                      rename(hom_area_2021,
                             TOTAL2021 = TOTAL,
                             P2021 = TOTAL_P),
                      by = "AREA")

hom_area <- hom_area %>% 
   mutate(TMCA = round((((TOTAL2021 / TOTAL2012) ^ (1/9)) - 1) * 100, 
                       1)
   )

## mujeres por subarea STEM
### 2021
Muj_subarea_2021 <- function(subareas){
   
   muj_subarea_2021 <- subareas %>% 
      filter(INDICADOR == "total" & SEXO == "mujer" & TIPO == "total" & PERIODO == "2021") %>%
      group_by(SUBAREA) %>% 
      summarise(MUJERES = sum(VALOR, na.rm = TRUE)) 
}

muj_subarea_2021 <- Muj_subarea_2021(subareas)

muj_subarea_2021 <- left_join(muj_subarea_2021,
                              unique(select(clas.car, nom.cam.esp, stem_subarea)),
                              by = c("SUBAREA" = "nom.cam.esp"))

muj_subarea_2021 <- muj_subarea_2021 %>% 
   filter(stem_subarea == 1) %>% 
   select(-stem_subarea)

muj_subarea_2021$MUJERES_P <- round((muj_subarea_2021$MUJERES / sum(muj_subarea_2021$MUJERES)) * 100,
                                    1)

## hombrees por subarea
### 2021
Hom_subarea_2021 <- function(subareas){
   
   hom_subarea_2021 <- subareas %>% 
      filter(INDICADOR == "total" & SEXO == "hombre" & TIPO == "total" & PERIODO == "2021") %>%
      group_by(SUBAREA) %>% 
      summarise(HOMBRES = sum(VALOR, na.rm = TRUE))
}

hom_subarea_2021 <- Hom_subarea_2021(subareas)

hom_subarea_2021 <- left_join(hom_subarea_2021,
                              unique(select(clas.car, nom.cam.esp, stem_subarea)),
                              by = c("SUBAREA" = "nom.cam.esp"))

hom_subarea_2021 <- hom_subarea_2021 %>% 
   filter(stem_subarea == 1) %>% 
   select(-stem_subarea)

hom_subarea_2021$HOMBRES_P <- round((hom_subarea_2021$HOMBRES / sum(hom_subarea_2021$HOMBRES)) * 100,
                                    1)

subareas <- left_join(muj_subarea_2021,
                      hom_subarea_2021,
                      by = "SUBAREA")

##  salarios por area

##  salarios por sexo y area
### mujeres, 2021
Sal_muj_area_2021 <- function(areas){
   
   sal_muj_area_2021 <- areas %>% 
      filter(INDICADOR == "total" & SEXO == "mujer" & TIPO == "promedio" & PERIODO == "2021") %>% 
      select(AREA, VALOR) %>% 
      rename(SALARIO_MUJER = VALOR)
}

sal_muj_area_2021 <- Sal_muj_area_2021(areas)

##  salarios de hombres por area
### hombres,2021
Sal_hom_area_2021 <- function(areas){
   
   sal_hom_area_2021 <- areas %>% 
      filter(INDICADOR == "total" & SEXO == "hombre" & TIPO == "promedio" & PERIODO == "2021") %>% 
      select(AREA, VALOR) %>% 
      rename(SALARIO_HOMBRE = VALOR)
}

sal_hom_area_2021 <- Sal_hom_area_2021(areas)

### unir
salarios_area_2021 <- left_join(sal_muj_area_2021,
                                sal_hom_area_2021,
                                by = "AREA")

salarios_area_2021$BRECHA_SALARIAL <- round(salarios_area_2021$SALARIO_MUJER / salarios_area_2021$SALARIO_HOMBRE,
                                            2)

## salarios stem por sexo
Sal_stem_sexo <- function(stem){
   sal_stem_sexo <- sal_stem_sexo %>% 
      filter(STEM == "stem")
}

## total de stem por sexo
Sexo_stem <- function(stem){
   muj_stem <- stem %>% 
      filter(STEM == "stem" & INDICADOR == "total" & CAT2 == "mujer" & TIPO == "total") %>% 
      select(PERIODO, VALOR) %>% 
      rename(MUJERES = VALOR)
   
   hom_stem <- stem %>% 
      filter(STEM == "stem" & INDICADOR == "total" & CAT2 == "hombre" & TIPO == "total") %>% 
      select(PERIODO, VALOR) %>% 
      rename(HOMBRES = VALOR)
   
   sexo_stem <- left_join(muj_stem,
                          hom_stem,
                          by = "PERIODO")
   
   sexo_stem <- sexo_stem %>% 
      mutate(TOTAL = MUJERES + HOMBRES,
             MUJERES_P = round((MUJERES / TOTAL) * 100,
                               1)) %>% 
      select(-TOTAL)
}

sexo_stem <- Sexo_stem(stem)

tmca_stem_muj <- round((((sexo_stem$MUJERES[sexo_stem$PERIODO == "2021"] / sexo_stem$MUJERES[sexo_stem$PERIODO == "2012"]) ^ (1/9)) - 1) * 100,
                       1)

tmca_stem_hom <- round((((sexo_stem$HOMBRES[sexo_stem$PERIODO == "2021"] / sexo_stem$HOMBRES[sexo_stem$PERIODO == "2012"]) ^ (1/9)) - 1) * 100,
                       1)

## mujeres por stem
Mujeres_stem <- function(stem){
 
   mujeres_stem <- stem %>% 
    filter(INDICADOR == "total" & CAT2 == "mujer" & TIPO == "total") %>%
    filter(PERIODO != "2015" & VALOR != 7112823) %>% 
    select(PERIODO, STEM, VALOR)
   
   mujeres_stem <- pivot_wider(mujeres_stem,
                               id_cols = c(PERIODO),
                               names_from = c(STEM),
                               values_from = c(VALOR))
   
   mujeres_stem <- mujeres_stem %>% 
      mutate(TOTAL = otras + stem,
             STEM_P = round((stem / TOTAL) * 100, 1)) %>% 
      select(-TOTAL)
}

## Posicion por stem y sexo
Posicion_stem <- function(stem){
   
   posicion_mujer_stem <- stem %>% 
      filter(STEM == "stem" & INDICADOR == "posicion" & SEXO == "mujer" & TIPO == "total" & PERIODO == "2021") %>% 
      select(CATEGORIA, VALOR) %>% 
      rename(MUJERES = VALOR) %>% 
      mutate(MUJERES_P = round((MUJERES / sum(MUJERES)) * 100, 1))
   
   posicion_hombre_stem <- stem %>% 
      filter(STEM == "stem" & INDICADOR == "posicion" & SEXO == "hombre" & TIPO == "total" & PERIODO == "2021") %>% 
      select(CATEGORIA, VALOR) %>% 
      rename(HOMBRES = VALOR) %>% 
      mutate(HOMBRES_P = round((HOMBRES / sum(HOMBRES)) * 100, 1))
   
   posicion_stem <- left_join(posicion_mujer_stem,
                            posicion_hombre_stem,
                            by = "CATEGORIA")

}

## condicion por stem y sexo
Condicion_stem <- function(stem){
   
   condicion_mujer_stem <- stem %>% 
      filter(STEM == "stem" & INDICADOR == "condicion" & SEXO == "mujer" & TIPO == "total" & PERIODO == "2021") %>% 
      select(CATEGORIA, VALOR, CV) %>% 
      rename(MUJERES = VALOR,
             CV_MUJ = CV) %>% 
      mutate(MUJERES_P = round((MUJERES / sum(MUJERES)) * 100, 1))
   
   condicion_hombre_stem <- stem %>% 
      filter(STEM == "stem" & INDICADOR == "condicion" & SEXO == "hombre" & TIPO == "total" & PERIODO == "2021") %>% 
      select(CATEGORIA, VALOR, CV) %>% 
      rename(HOMBRES = VALOR,
             CV_HOM = CV) %>% 
      mutate(HOMBRES_P = round((HOMBRES / sum(HOMBRES)) * 100, 1))
   
   condicion_stem <- left_join(condicion_mujer_stem,
                            condicion_hombre_stem,
                            by = "CATEGORIA")
   
}

## sector por stem y sexo
Sector_stem <- function(stem){
   
   sector_mujer_stem <- stem %>% 
      filter(STEM == "stem" & INDICADOR == "sector" & SEXO == "mujer" & TIPO == "total" & PERIODO == "2021") %>% 
      select(CATEGORIA, VALOR, CV) %>% 
      rename(MUJERES = VALOR,
             CV_MUJ = CV) %>% 
      mutate(MUJERES_P = round((MUJERES / sum(MUJERES)) * 100, 1))
   
   sector_hombre_stem <- stem %>% 
      filter(STEM == "stem" & INDICADOR == "sector" & SEXO == "hombre" & TIPO == "total" & PERIODO == "2021") %>% 
      select(CATEGORIA, VALOR, CV) %>% 
      rename(HOMBRES = VALOR,
             CV_HOM = CV) %>% 
      mutate(HOMBRES_P = round((HOMBRES / sum(HOMBRES)) * 100, 1))
   
   sector_stem <- left_join(sector_mujer_stem,
                               sector_hombre_stem,
                               by = "CATEGORIA")
   
}

# carreras stem por  sexo
Car_stem <- function(carreras){
   
   car_stem <- carreras %>% 
      filter(INDICADOR == "TOTAL" & CV < 0.25)
}

car_stem <- Car_stem(carreras)

cars <- select(clas.car,
               nom.cam.det,
               stem)

car_stem <- left_join(car_stem,
                      select(clas.car,
                             nom.cam.det,
                             stem),
                      by = c("CARRERA" = "nom.cam.det"))

car_stem <- car_stem %>% 
   ungroup() %>% 
   filter(stem == 1)

total_stem_sexo <- car_stem %>% 
   group_by(SEXO) %>% 
   summarise(TOTAL_SEXO = sum(VALOR))

car_stem <- left_join(select(car_stem, CARRERA, SEXO, VALOR),
                      total_stem_sexo,
                      by = "SEXO")

car_stem <- car_stem %>% 
   mutate(PORCENTAJE = round((VALOR / TOTAL_SEXO) * 100,
                             1)) %>% 
   rename(TOTAL = VALOR)

car_stem <- pivot_wider(car_stem,
                        id_cols = CARRERA,
                        names_from = SEXO,
                        values_from = c(TOTAL, PORCENTAJE))

# carreras stem
car_sexo <- carreras %>% 
   filter(INDICADOR == "TOTAL" & CV < 0.25)

cars <- select(clas.car,
               nom.cam.det)

car_sexo <- left_join(car_sexo,
                      select(clas.car,
                             nom.cam.det),
                      by = c("CARRERA" = "nom.cam.det"))

total_car <- car_sexo %>%
   filter(is.na(CARRERA) == FALSE) %>% 
   group_by(CARRERA) %>% 
   summarise(TOTAL_CARRERA = sum(VALOR))

car_sexo <- left_join(select(car_sexo, CARRERA, SEXO, VALOR),
                      total_car,
                      by = "CARRERA")

car_sexo <- car_sexo %>% 
   mutate(PORCENTAJE = round((VALOR / TOTAL_CARRERA) * 100,
                             1)) %>% 
   rename(TOTAL = VALOR)

car_sexo <- pivot_wider(car_sexo,
                        id_cols = CARRERA,
                        names_from = SEXO,
                        values_from = c(TOTAL, PORCENTAJE))

# total por carrera 2013 y 2021
tot_car_13 <- carreras %>% 
   filter(INDICADOR == "total" & PERIODO == "2013"  & CV <= 0.15) %>% 
   group_by(CARRERA) %>% 
   summarise(TOTAL_13 = sum(VALOR, na.rm = TRUE))
   
tot_car_21 <- carreras %>% 
   filter(INDICADOR == "total" & PERIODO == "2021" & CV <= 0.15) %>% 
   group_by(CARRERA) %>% 
   summarise(TOTAL_21 = sum(VALOR, na.rm = TRUE))

tot_car <- left_join(tot_car_13,
                     tot_car_21,
                     by = "CARRERA")

tot_car <- tot_car %>% 
   mutate(CAMBIO_P = round(((TOTAL_21 - TOTAL_13) / TOTAL_13) * 100, 
                           1))

# posicion stem

