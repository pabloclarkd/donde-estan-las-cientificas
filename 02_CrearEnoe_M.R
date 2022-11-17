CrearEnoe_M <- function(sdem, periodo.enoe, fac_inf, version.nueva, clas.car){
   
# La funcion CreateEnoeTable toma como argumento la tabla sdem de los microdatos de inegi y crea la tabla enoe que contiene la informacion necesaria para los calculos de Compara Carreras

# filtrar los microdatos de sdem para seleccionar entrevistas definitivas, a residentes habiturales y a personas entre 15 y 65 años
## Convertir columna EDA en las tres tablas de a character y luego a numeric
   enoe <- sdem %>% 
      mutate(EDA = as.numeric(as.character(EDA))) %>% 
      filter(R_DEF == "00" & C_RES != "2" & (EDA > 14 & EDA <= 65))
      
# Si el trimestre es alguno en el que se utilizo la ENOEN, es necesario ajustar los nombres de las variables del diseño muestral 
   if(periodo.enoe %in% c("320", "420", "121", "221", "321")){
      enoe <- enoe %>% rename(EST_D = EST_D_TRI,
                              FAC = FAC_TRI) 
   }
   
# Crear columna de 'id' en las tablas con paste(coe1$CD_A, coe1$ENT, coe1$CON, coe1$V_SEL, coe1$N_HOG, coe1$H_MUD, coe1$N_REN, sep = "/") en sdemt
   enoe$ID <- paste(enoe$CD_A, enoe$ENT, enoe$CON, enoe$V_SEL, enoe$N_HOG, enoe$H_MUD, enoe$N_REN, sep = "/")
   
# Eliminar de las tablas las columnas que no se usen
   enoe <- select(enoe, 
                  ID, ENT, R_DEF, 
                  C_RES, EST, EST_D, 
                  UPM, SEX, EDA, 
                  CS_P13_1, CS_P13_2, CS_P14_C, 
                  CS_P15, CS_P16, FAC, 
                  CLASE1, CLASE2, CLASE3, 
                  POS_OCU, C_OCU11C, RAMA_EST1, 
                  RAMA_EST2, AMBITO2,  
                  TIP_CON, DUR_EST, 
                  HRSOCUP, INGOCUP, ING_X_HRS,
                  EMP_PPAL,  
                  SUB_O, DUR_EST,
                  DOMESTICO, N_HIJ, E_CON,
                  C_OCU11C, AMBITO2, ING7C,
                  ZONA)
   
# renombrar variables
   enoe <- rename(enoe, 
                  NIVEL.ESC = CS_P13_1, 
                  A.APROB = CS_P13_2, 
                  TER.EST = CS_P16, 
                  C.CAR = CS_P14_C,
                  SUBOCUPACION = SUB_O,
                  SECTOR = RAMA_EST2,
                  DURACION = DUR_EST,
                  ACTIVIDAD = CLASE1,
                  OCUPACION = CLASE2,
                  POSICION = POS_OCU,
                  FORMALIDAD = EMP_PPAL,
                  SEXO = SEX,
                  HIJOS_NUM = N_HIJ,
                  EDO_CIVIL = E_CON,
                  CONDICION = C_OCU11C,
                  TIPO.EMPRESA = AMBITO2,
                  INGRESO = INGOCUP,
                  INGRESO.NIV = ING7C)

# recodificar variables
   enoe <- enoe %>% mutate_at(vars(ACTIVIDAD, SECTOR, DURACION, DOMESTICO, OCUPACION, POSICION, CONDICION,  TIPO.EMPRESA, FORMALIDAD, INGRESO.NIV), 
                              na_if, y = "0")
   enoe <- enoe %>% mutate_at(vars(ACTIVIDAD, SECTOR, DURACION, DOMESTICO, OCUPACION, POSICION, CONDICION,  TIPO.EMPRESA, FORMALIDAD, INGRESO.NIV), 
                              na_if, y = "99")
   
   enoe$ACTIVIDAD <- recode(enoe$ACTIVIDAD, 
                            "1" = "activa", 
                            "2" = "no activa")
   
   enoe$OCUPACION <- recode(enoe$OCUPACION, 
                            "1" = "ocupado", 
                            "2" = "desocupado", 
                            "3" = "disponible", 
                            "4" = "no disponible")
   
   enoe$POSICION <- recode(enoe$POSICION, 
                           "1" = "subordinado", 
                           "2" = "empleador", 
                           "3" = "cuentapropia", 
                           "4" = "sin pago", 
                           "5" = "no especificado")
   
   enoe$FORMALIDAD <- recode(enoe$FORMALIDAD, 
                             "1" = "informal", 
                             "2" = "formal")
   
   enoe$SEXO <- recode(enoe$SEXO, 
                       "1" = "hombre", 
                       "2" = "mujer")
   
   enoe$SECTOR <- recode(enoe$SECTOR,
                         "1" = "agricultura y ganaderia",
                         "2" = "industria extractiva",
                         "3" = "industria manufacturera",
                         "4" = "construccion",
                         "5" = "comercio",
                         "6" = "restaurantes y alojamientos",
                         "7" = "transportes y comunicaciones",
                         "8" = "servicios profesionales, financieros y corporativos",
                         "9" = "servicios sociales",
                         "10" = "servicios diversos",
                         "11" = "gobierno y organismos internacionales")
   
   enoe$DURACION <- recode(enoe$DURACION, 
                           "1" = "ausentes con vinculo", 
                           "2" = "menos de 15", 
                           "3" = "de 15 a 34", 
                           "4" = "de 35 a 48", 
                           "5" = "mas de 48", 
                           "6" = "no especificado")
   
   enoe$DOMESTICO <- recode(enoe$DOMESTICO,
                            "1" = "solo pea",
                            "2" = "pea y estudian",
                            "3" = "pea y quehaceres domesticos",
                            "4" = "pea y apoyos al hogar",
                            "5" = "solo pea",
                            "6" = "solo pnea",
                            "7" = "pnea y estudian",
                            "8" = "pnea y quehaceres domesticos",
                            "9" = "pnea y apoyos al hogar",
                            "10" = "solo pnea")
   
   enoe$EDO_CIVIL <- recode(enoe$EDO_CIVIL,
                            "1" = "union libre",
                            "2" = "separado",
                            "3" = "divorciado",
                            "4" = "viudo",
                            "5" = "casado",
                            "6" = "soltero",
                            "9" = "no sabe")
   
   enoe$EDO_CIVIL <- recode(enoe$EDO_CIVIL,
                            "union libre" = "con pareja",
                            "separado"    = "sin pareja",
                            "divorciado"  = "sin pareja",
                            "viudo"       = "sin pareja",
                            "casado"      = "con pareja",
                            "soltero"     = "sin pareja",
                            "no sabe"     = "no sabe")
   
   enoe$CONDICION <- recode(enoe$CONDICION,
                         "1" = "profesionales y tecnicos",
                         "2" = "trabajadores de educacion",
                         "3" = "funcionarios y directivos",
                         "4" = "oficinistas",
                         "5" = "trabajadores industriales",
                         "6" = "comerciantes",
                         "7" = "operadores de transporte",
                         "8" = "trabajadores en servicios personales",
                         "9" = "vigilantes",
                         "10" = "trabajadores agropecuarios",
                         "11" = "no especificado")
   
   enoe$TIPO.EMPRESA <- recode(enoe$TIPO.EMPRESA,
                            "1" = "micronegocios",
                            "2" = "micronegocios",
                            "3" = "micronegocios",
                            "4" = "pequeño negocio",
                            "5" = "mediano negocio",
                            "6" = "grande negocio",
                            "7" = "gobierno",
                            "8" = "otros")
   
   enoe$INGRESO.NIV <- recode(enoe$INGRESO.NIV,
                                "1" = "hasta uno",
                                "2" = "mas de uno hasta dos",
                                "3" = "mas de dos y hasta tres",
                                "4" = "mas de tres y hasta cinco",
                                "5" = "mas de cinco",
                                "6" = "no recibe ingreso",
                                "7" = "no especificado")
   
# Codificar variables de mujer con o sin hijos
   enoe$HIJOS_NUM <- as.integer(as.character(enoe$HIJOS_NUM))
   enoe$HIJOS_NUM <- na_if(enoe$HIJOS_NUM, 99)
   enoe$HIJOS <- ifelse(enoe$HIJOS_NUM > 0,
                        "con hijos",
                        "sin hijos")
   
# Codificar maximo nivel de estudios completado
   enoe$A.APROB <- as.numeric(as.character(enoe$A.APROB))
   
   enoe$m.esc <- NA
   
   enoe$m.esc <- replace(enoe$m.esc, enoe$NIVEL.ESC == "04" & enoe$A.APROB == 3,  "prep")
   enoe$m.esc <- replace(enoe$m.esc, enoe$NIVEL.ESC == "07" & enoe$TER.EST == 2,  "prep")
   enoe$m.esc <- replace(enoe$m.esc, enoe$NIVEL.ESC == "07" & enoe$TER.EST == 9 & enoe$CS_P15 == 3,  "prep")
   enoe$m.esc <- replace(enoe$m.esc, (enoe$NIVEL.ESC == "05" | enoe$NIVEL.ESC == "06") & enoe$TER.EST == 2 & enoe$CS_P15 == "3",  "prep")
   
   enoe$m.esc <- replace(enoe$m.esc, enoe$NIVEL.ESC == "03" & enoe$A.APROB == 3,  "secu")
   enoe$m.esc <- replace(enoe$m.esc, enoe$NIVEL.ESC == "04" & (enoe$A.APROB < 3 | enoe$A.APROB == 9),  "secu")
   enoe$m.esc <- replace(enoe$m.esc, (enoe$NIVEL.ESC == "05" | enoe$NIVEL.ESC == "06") & enoe$TER.EST == 2 & enoe$CS_P15 == "2",  "secu")
   
   enoe$m.esc <- replace(enoe$m.esc, enoe$NIVEL.ESC == "02" & enoe$A.APROB == 6,  "prim")
   enoe$m.esc <- replace(enoe$m.esc, enoe$NIVEL.ESC == "03" & (enoe$A.APROB < 3 | enoe$A.APROB == 9),  "prim")
   enoe$m.esc <- replace(enoe$m.esc, (enoe$NIVEL.ESC == "05" | enoe$NIVEL.ESC == "06") & enoe$TER.EST == 2 & enoe$CS_P15 == "1",  "prim")
   
   enoe$m.esc <- replace(enoe$m.esc, enoe$NIVEL.ESC == "00" | enoe$NIVEL.ESC == "01",  "ning")
   enoe$m.esc <- replace(enoe$m.esc, enoe$NIVEL.ESC == "02" & (enoe$A.APROB < 6 | enoe$A.APROB == 9),  "ning")
   
   enoe$m.esc <- replace(enoe$m.esc, enoe$NIVEL.ESC == "06" & enoe$TER.EST == 1, "l.tec")
   
   enoe$m.esc <- replace(enoe$m.esc, enoe$NIVEL.ESC == "05" & enoe$TER.EST == 1,  "lice")
   enoe$m.esc <- replace(enoe$m.esc, enoe$NIVEL.ESC == "07" & enoe$TER.EST == 1,  "lice")
   enoe$m.esc <- replace(enoe$m.esc, (enoe$NIVEL.ESC == "08" | enoe$NIVEL.ESC == "09") & (enoe$TER.EST == 2 | enoe$TER.EST == 9),  "lice")
   
   enoe$m.esc <- replace(enoe$m.esc, (enoe$NIVEL.ESC == "09" | enoe$NIVEL.ESC == "08") & enoe$TER.EST == 1,  "lice")
   
   enoe$m.esc <- recode(enoe$m.esc,
                        "l.tec" = "lice")
   
   enoe$prof <- 0
   enoe$prof <- replace(enoe$prof, enoe$m.esc == "lice" | enoe$m.esc == "l.tec", 1)
   
   # codificar variable de grupo de edad de 30 años
   enoe$GRUPO_EDAD <- NA
   enoe$GRUPO_EDAD[enoe$EDA < 30] <- "menor de 30"
   enoe$GRUPO_EDAD[enoe$EDA >= 30] <- "30 o mas"

   ## Ajustar los datos de ingreso por inflacion
   AjustarInflacion <- function(indicador, fac_inf, trimestre){
      
      ###   obtener valor del deflactor correspondiente al trimestre
      fac_inf_trim <- fac_inf %>% filter(periodo == trimestre) %>% select(factor)
      factor <- fac_inf_trim$factor
      
      ### multiplicar los datos de ingreso por el factor
      
      #### si es un vector
      if(class(indicador) == "integer" | class(indicador) == "numeric"){
         
         indicador <- indicador / factor
         
      } else {
         
         stop("indicador debe ser un vector numerico o entero")
      }
      
      
   }
   
   enoe$INGRESO <- AjustarInflacion(enoe$INGRESO, fac_inf, periodo.enoe)
   
   ## Calcular los ingresos en multiplos del salario minimo (se usan valores del SM de 2020)
   enoe <- enoe %>% mutate(SALMIN = recode(as.character(ZONA), "1" = "185.56", "2" = "123.22"),
                           SALMIN = AjustarInflacion(as.numeric(SALMIN), fac_inf, periodo.enoe),
                           SALMINMES = as.numeric(SALMIN) * 30,
                           MULTSALMIN = round(INGRESO / SALMINMES, 3),
                           INGRESONIV = cut(MULTSALMIN,
                                            breaks = c(0, 0.0001, 0.999, 2, 3, 5, Inf),
                                            labels = c("sin ingresos", "menos de uno", "mas de uno y hasta dos", "mas de dos y hasta tres", "mas de tres y hasta cinco", "mas de cinco"),
                                            right = TRUE,
                                            include.lowest = TRUE,
                                            ordered_result = TRUE))
   
   enoe$INGRESONIV <- factor(enoe$INGRESONIV, 
                             levels = c(levels(enoe$INGRESONIV), "no especificado"))
   enoe$INGRESONIV <- replace(enoe$INGRESONIV, enoe$OCUPACION != "ocupado", NA)
   enoe$INGRESONIV <- replace(enoe$INGRESONIV, enoe$INGRESONIVORG == "no recibe ingreso", "sin ingresos")
   enoe$INGRESONIV <- replace(enoe$INGRESONIV, enoe$INGRESONIVORG == "no especificado", "no especificado")
   enoe$INGRESONIV <- replace(enoe$INGRESONIV, enoe$MULTSALMIN == 0, "no especificado")
   
   #establece condiciones para seleccionar los salarios a analizar
   enoe <- enoe %>% 
      mutate(SALARIO = ifelse(OCUPACION == "ocupado" & HRSOCUP >= 30 & INGRESO > 0,
                              INGRESO,
                              NA),
             SALARIO.HORA = ifelse(OCUPACION == "ocupado" & HRSOCUP >= 30 & INGRESO > 0,
                                   ING_X_HRS,
                                   NA),
             INGCERO = ifelse(OCUPACION == "ocupado",
                              ifelse(INGRESO == 0 | is.na(INGRESO),
                                     "ingreso cero",
                                     "con ingreso"),
                              NA))
   
   ## Si el trimestre despues del 321 (cuando se comenzo a usar la CMPE 2016), es necesario realizar la equivalencia de codigos de carreras
   if(periodo.enoe %in% c("321")){
      
      # guardar clave original
      enoe$C.CAR.org <- enoe$C.CAR
      
      enoe$C.CAR <- substring(enoe$C.CAR, 1, 4)
      
      # usar funcion para realizar equivalencias
      enoe$C.CAR <- RealizarEquivalenciasCMPE_M(as.character(enoe$C.CAR), version_nueva)
      
      if(version_nueva == "cmpe2011"){
         
         enoe$area <- substring(enoe$C.CAR, 2, 2)
         enoe$subarea <- substring(enoe$C.CAR, 2, 3)
         enoe$carrera <- substring(enoe$C.CAR, 2, 4)
         
      } else {
         
         enoe$area <- substring(enoe$C.CAR, 1, 2)
         enoe$subarea <- substring(enoe$C.CAR, 1, 3)
         enoe$carrera <- substring(enoe$C.CAR, 1, 4)
         
      }
   
   } else {
      
      # usar funcion para realizar equivalencias
      enoe$C.CAR <- substring(enoe$C.CAR, 2, 4)
      
      enoe$C.CAR <- RealizarEquivalenciasCMPE_M(as.character(enoe$C.CAR), version_nueva)
      
      if(version_nueva == "cmpe2011"){
         
         enoe$area <- substring(enoe$C.CAR, 2, 2)
         enoe$subarea <- substring(enoe$C.CAR, 2, 3)
         enoe$carrera <- substring(enoe$C.CAR, 2, 4)
      
      } else {
         
         enoe$area <- substring(enoe$C.CAR, 1, 2)
         enoe$subarea <- substring(enoe$C.CAR, 1, 3)
         enoe$carrera <- substring(enoe$C.CAR, 1, 4)
         
      }
      
      }
   
   # codificar una variable que identique carreras stem
   enoe$STEM <- NA
   enoe$STEM <- ifelse((enoe$prof == 1 | enoe$m.esc == "l.tec"),
                       "OTRAS",
                       NA)
   enoe$STEM <- ifelse((enoe$prof == 1 | enoe$m.esc == "l.tec") & enoe$carrera %in% clas.car$cve.cam.det[clas.car$stem == 1],
                       "STEM",
                       enoe$STEM)
   
   enoe <- select(enoe, -c(ENT, CLASE3, RAMA_EST1, TIP_CON)) 
   
   return(enoe)
}