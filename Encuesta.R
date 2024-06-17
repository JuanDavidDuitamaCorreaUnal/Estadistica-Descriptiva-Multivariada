#tratamiento de datos------------
library(readr)
library(dplyr)
datosTI <- read_delim("C:/Users/Pc/Desktop/EDM/total_tri_4_2022_pl.CSV", 
                      delim = ";", escape_double = FALSE, trim_ws = TRUE)



Indicador<-datosTI%>%select(DIRECTORIO)%>%group_by(DIRECTORIO)%>%select(DIRECTORIO)%>%as.vector()

Indicador<-Indicador$DIRECTORIO

S_y_E <- read_delim("GEIH/GEIH_Diciembre_2022_Marco_2018/CVS/Características generales, seguridad social en salud y educación.CSV", 
                                                                              delim = ";", escape_double = FALSE, trim_ws = TRUE)

library(stringr)
datosTI<-S_y_E%>%select(DIRECTORIO, SECUENCIA_P,ORDEN,P6083,P6081,P2057,MES)%>% 
  filter(MES==12)%>%
  right_join(datosTI,by=c('SECUENCIA_P'='SECUENCIA_P',"DIRECTORIO"="DIRECTORIO","ORDEN"="ORDEN"))%>%as.data.frame()


HogaresVivienda <- read_csv("~/HogaresVivienda.csv")
str(HogaresVivienda)
datosTI<-datosTI%>%left_join(HogaresVivienda,by=c('DIRECTORIO'='Directorio'))



colnames(datosTI)

str(datosTI)

str(datosTI)


datosTI$es










# Variables originales para días, horas al día y la variable indicadora
dias <- c(
  "P3131S2A1", "P3131S3A1", "P3132S1A1", "P3132S2A1", "P3133S1A1", "P3133S2A1",
  "P3134S1A1", "P3134S2A1", "P3135S1A1", "P3135S2A1", "P3136S1A1", "P3136S2A1",
  "P3137S1", "P3138S1", "P3139S1", "P3141S1", "P3142S1", "P3143S1", "P3144S1",
  "P3145S1", "P3502S1", "P3503S1", "P3504S1"
)

horas_dia<- c(
  "P3131S2A2", "P3131S3A2", "P3132S1A2", "P3132S2A2", "P3133S1A2", "P3133S2A2",
  "P3134S1A2", "P3134S2A2", "P3135S1A2", "P3135S2A2", "P3136S1A2", "P3136S2A2",
  "P3137S2", "P3138S2", "P3139S2", "P3141S2", "P3142S2", "P3143S2", "P3144S2",
  "P3145S2", "P3502S2", "P3503S2", "P3504S2"
)

indicadoras <- c( "P3131S2", "P3131S3", "P3132S1", "P3132S2", "P3133S1", "P3133S2",
                  "P3134S1", "P3134S2", "P3135S1", "P3135S2", "P3136S1", "P3136S2",
                  "P3137", "P3138", "P3139", "P3141", "P3142", "P3143", "P3144",
                  "P3145", "P3502", "P3503", "P3504")


datosTI1<-datosTI[,c("TAM_HOGAR","P3271","P6040","P6050","P6070","P6090","P6160","P6170","P3041",
                     "P3042S1","P400",'P6083','P6081','P2057',
                     'ClaseHogar', 'Depart' ,'TipoHogar' ,  'Luz'  , 'Gas' ,'Alcantarillado', 'Basura' ,'Acueducto', 'ProceAgua' ,'PropHogar', 'CantHogares','Estrato',"P408","P409","P414","P420","P413","P405")]
datosTI1<-rename(datosTI1,
                 SEXO = P3271,
                 EDAD = P6040,
                 PARENTESCO = P6050,
                 ESTADO_CIVIL = P6070,
                 AFILIACION_SEGURIDAD_SOCIAL = P6090,
                 SABE_LEER_ESCRIBIR = P6160,
                 ASISTE_INSTITUCION_EDUCATIVA = P6170,
                 INSTITUCION_EDUCATIVA = P3041,
                 ANIO_GRADO = P3042S1,
                 ACTIVIDAD_PRINCIPAL = P400,
                 VIVE_M=P6083,
                 VIVE_P=P6081,
                 CAMP=P2057
)


# Renombrar factores en las columnas específicas
datosTI1$SEXO <- factor(datosTI1$SEXO, levels = c(1, 2), labels = c("Hombre", "Mujer"))

datosTI1$PARENTESCO <- factor(datosTI1$PARENTESCO, levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13),
                              labels = c("Jefe (a) del hogar", "Pareja, esposo(a), cónyuge, compañero(a)", "Hijo(a), hijastro(a)", "Padre o madre", "Suegro(a)", "Hermano(a) o hermanastro(a)", "Yerno o nuera", "Nieto(a)", "Otro pariente", "Empleado(a) del servicio doméstico y sus parientes", "Pensionista", "Trabajador", "Otro no pariente"))

datosTI1$ESTADO_CIVIL <- factor(datosTI1$ESTADO_CIVIL, levels = c(1, 2, 3, 4, 5, 6),
                                labels = c("No esta casado(a) y vive en pareja hace menos de dos años", "No esta casado (a) y vive en pareja hace dos años o más", "Esta casado (a)", "Esta separado (a) o divorciado (a)", "Esta viudo (a)", "Esta soltero (a)"))

datosTI1$AFILIACION_SEGURIDAD_SOCIAL <- factor(datosTI1$AFILIACION_SEGURIDAD_SOCIAL, levels = c(1, 2, 9),
                                               labels = c("Sí", "No", "No sabe, no informa"))

datosTI1$SABE_LEER_ESCRIBIR <- factor(datosTI1$SABE_LEER_ESCRIBIR, levels = c(1, 2),
                                      labels = c("Sí", "No"))

datosTI1$ASISTE_INSTITUCION_EDUCATIVA <- factor(datosTI1$ASISTE_INSTITUCION_EDUCATIVA, levels = c(1, 2),
                                                labels = c("Sí", "No"))

datosTI1$INSTITUCION_EDUCATIVA <- factor(datosTI1$INSTITUCION_EDUCATIVA, levels = c(1, 2),
                                         labels = c("Publica", "Privada"))


datosTI1$ACTIVIDAD_PRINCIPAL <- factor(datosTI1$ACTIVIDAD_PRINCIPAL, levels = c(1, 2, 3, 4, 5, 6),
                                       labels = c("Trabajando", "Buscando trabajo", "Estudiando", "Oficios del hogar", "Incapacitado permanente para trabajar", "Otra actividad"))




# Realizar la suma de variables de días
datosTI1$SUMA_DIAS <- rowSums(datosTI[,dias], na.rm = TRUE)

# Realizar la suma de variables de horas al día
datosTI1$SUMA_HORAS_DIA <- rowSums(datosTI[,horas_dia], na.rm = TRUE)

# Realizar la suma de variables indicadoras
datosTI1$SUMA_INDICADORAS <- rowSums(sapply(datosTI[indicadoras], function(x) ifelse(x == 1, 1, 0)), na.rm = TRUE)

summary(datosTI1$SUMA_INDICADORAS)
str(datosTI)

missings <- function(x) return(sum(is.na(x)))
apply(datosTI1,2,missings)
library(dplyr)
#vemos cuantos hay de cada grupo

datosTI1%>%group_by(ACTIVIDAD_PRINCIPAL)%>% summarise(n=n())

# 1 - Trabajando
# 2 - Buscando trabajo
# 3 - Estudiando
# 4 - Oficios del hogar
# 5 - Incapacitado permanente para trabajar
# 6 - Otra actividad, ¿Cuál?

datosTI2<-datosTI1 %>%
  filter(ACTIVIDAD_PRINCIPAL=='Estudiando' & is.na(P405)) %>%
  as.data.frame()
set.seed(1234)

datosTI2<-datosTI2%>%
  sample_n(size=506,replace=FALSE)%>%
  as.data.frame()

datosTI3<-datosTI1 %>%
  filter(ACTIVIDAD_PRINCIPAL == 'Oficios del hogar') %>%
  as.data.frame()

datosTI11<-datosTI1 %>%
  filter(ACTIVIDAD_PRINCIPAL =='Trabajando' |ACTIVIDAD_PRINCIPAL =='Buscando trabajo')  %>%
  as.data.frame()


ACTIVIDAD_PRINCIPAL<-rep('Trabajando',506)

datosTI11<-datosTI11[,-11]
datosTI11<-cbind(datosTI11,ACTIVIDAD_PRINCIPAL)


TI<-rbind(datosTI11,datosTI2, datosTI3)



TI$Depart <- as.factor(TI$Depart)
TI$Luz <- as.factor(TI$Luz)
TI$Gas <- as.factor(TI$Gas)
TI$Alcantarillado <- as.factor(TI$Alcantarillado)
TI$Basura <- as.factor(TI$Basura)
TI$Acueducto <- as.factor(TI$Acueducto)
TI$PropHogar <- as.factor(TI$PropHogar)
TI$ACTIVIDAD_PRINCIPAL <- as.factor(TI$ACTIVIDAD_PRINCIPAL)
TI$ClaseHogar <- as.factor(TI$ClaseHogar)
TI$ProceAgua <- as.factor(TI$ProceAgua)
TI$TipoHogar <- as.factor(TI$TipoHogar)
summary(TI)
colnames(TI)
datosTI$P400 <-factor(datosTI$P400, levels = c(1, 2, 3, 4, 5, 6),
                    labels = c("Trabajando", "Buscando trabajo", "Estudiando", "Oficios del hogar", "Incapacitado permanente para trabajar", "Otra actividad"))
summary(TI)
str(TI)
hist(TI$TAM_HOGAR)
hist(TI$CantHogares)
hist(TI$ANIO_GRADO)
hist(TI$EDAD)
TI$Depart
TI$ClaseHogar
summary(as.factor(TI$Estrato))
# Primer intento de ACM----------
# Variables activas

# PARENTESCO VIVE_P   "SEXO" "ASISTE_INSTITUCION_EDUCATIVA"
# "TipoHogar" "ACTIVIDAD_PRINCIPAL "EDAD" "AFILIACION_SEGURIDAD_SOCIAL" "INSTITUCION_EDUCATIVA"
# ClaseHogar" "PropHogar"TAM_HOGAR 
# La afiliacion cambiarla por estrato cuando se tenga 
# Vive M CAMP Luz ESTADO_CIVIL SABE_LEER_ESCRIBIR Luz Gas 
# Alcantarillado Basura   Acueducto       ProceAgua  

# Toca recodificar 
hist(TI$EDAD)
hist(TI$TAM_HOGAR)
library(dplyr)
TI$CAMP[is.na(TI$CAMP)]=0

# se recategorizo Estado civil, parentesco, Vive M, VIVE P y campo

Primero<-TI%>%mutate(EDADC=case_when(EDAD <14  ~ "14-",
                                     EDAD > 15 ~ "15+",
                                         TRUE ~ "14-15"),
                     TAM_HOGARC=case_when(TAM_HOGAR <3 ~ "1-2",
                                          TAM_HOGAR > 4 ~ "4+",
                                          TRUE ~ "3-4"),
                     ESTADO_CIVIL=case_when(ESTADO_CIVIL== 'Esta soltero (a)' ~ 'soltero',
                                            TRUE~'OtroE'),
                     PARENTESCO=case_when(PARENTESCO=='Hijo(a), hijastro(a)'~'H',
                                          PARENTESCO=='Nieto(a)'~'N',
                                          TRUE~'OtroP'),
                     VIVE_M=case_when(VIVE_M==1~'sí',
                                      VIVE_M==0 |is.na(VIVE_M)~'no'
                                      ),
                     VIVE_P=case_when(VIVE_P==1~'sí',
                                      VIVE_P==0 | is.na(VIVE_P)~'no'
                                      ),
                     CAMP=case_when(CAMP==1~'sí',
                                    CAMP==9~'9'
                                    
                                     ),)%>%
  select(PARENTESCO,ASISTE=ASISTE_INSTITUCION_EDUCATIVA,ACTIVIDAD_PRINCIPAL,
         INSTITUCION_EDUCATIVA, PropHogar,SL= SABE_LEER_ESCRIBIR, TAM_HOGARC,EDADC,
         VIVE_M,CAMP,ESTADO_CIVIL, Luz,Gas,Alcantarillado,Basura,Acueducto,VIVE_P,
         ProceAgua)%>%as.data.frame()

Primero$EDADC<-as.factor(Primero$EDADC)
Primero$TAM_HOGARC<-as.factor(Primero$TAM_HOGARC)
Primero$ESTADO_CIVIL<-as.factor(Primero$ESTADO_CIVIL)
Primero$VIVE_M<-as.factor(Primero$VIVE_M)
Primero$PARENTESCO<-as.factor(Primero$PARENTESCO)
Primero$VIVE_P<-as.factor(Primero$VIVE_P)
Primero$CAMP<-as.factor(Primero$CAMP)
str(Primero)
Primero$ESTADO_CIVIL
summary(Primero)
library(FactoClass)
ayu$col.abs/100

sum(is.na(Primero$CAMP))

acm <-dudi.acm (Primero[,1:11], scannf = FALSE,nf=3)
barplot ( acm$eig)
ayu<-inertia.dudi(acm,,T)
ayu
abs(ayu$col.rel)/100
plot(acm)
plotfp(acm$cr,gg=TRUE)

plot(acm,Tcol = F)
plot(acm,Trow = F)

plot(acm,1,3)

############# Para analizar a mas detalle tomamos solo la parte de TI con los niños trabajando
str(TI)
TIT<-TI%>%filter(ACTIVIDAD_PRINCIPAL=="Trabajando")%>%as.data.frame()
TIT$P408<-as.factor(TIT$P408)
TIT$P420<-as.factor(TIT$P420)
TIT$P405<-as.factor(TIT$P405)

summary(TIT)

sum(is.na(sueldo))
# se desecha el numero de meses trabajado p414
# el sueldo o las ganancias estan medidas por p409 y p413 se escogio la que tenia menos NA

# se creo una categoria para saber si el trabajo era en su familia o no 
# esta categoria puede ser creada con P408 o p420
# se intento con p408 pero no esta muy claro como recodificar las categorias
# se trabaja ahora con p420 lo vamos a hacer en 3 categorias
# la primera es si es en la ciudad, la otra fuera, y la tercera si se tiene que desplazar 

# la razon para la que trabaja  se categorizo en estudio familia y otro
# estudio tiene muy poca frecuencia se pone junto a familia

#se quita insititucion educativa, muy poca frecuencia privada de igual manera asiste 

# las otras variables se recategorizan como se hizo arriba

# Trabaja Para????
Segundo<-TI%>%filter(ACTIVIDAD_PRINCIPAL=="Trabajando")%>%
  mutate(EDADC=case_when(           EDAD<=15~'15-',
                                   TRUE ~ "15+"),
                   TAM_HOGARC=case_when(TAM_HOGAR <=4 ~ "1-4",
                                        TRUE ~ "4+"),
                   ESTADO_CIVIL=case_when(ESTADO_CIVIL== 'Esta soltero (a)' ~ 'soltero',
                                          TRUE~'OtroE'),
                   PARENTESCO=case_when(PARENTESCO=='Hijo(a), hijastro(a)'~'H',
                                        PARENTESCO=='Nieto(a)'~'N',
                                        TRUE~'OtroP'),
                   VIVE_M=case_when(VIVE_M==1~'sí',
                                    VIVE_M==0~'no',
                                    TRUE~'NA'),
                   VIVE_P=case_when(VIVE_P==1~'sí',
                                    VIVE_P==0~'no',
                                    TRUE~'NA'),
                   CAMP=case_when(CAMP==1~'sí',
                                  CAMP==0~'no',
                                  TRUE~'NA'),
                    RAZON=case_when(P405==1| P405==2 | P405==3 ~'F', # familia
                                    TRUE~'Otra'),
         TRABAJA_PARA=case_when(P420==1 | P420==2| P420==3 |P420==7 |P420==9 ~'C' ,#Ciudad
                                P420==4 | P420==5|P420==6 ~'A', #Ambulante
                         TRUE~'Fuera'),
         SUELDO=case_when(P409<500000 | P409==500000 | is.na(P409) ~ '-500Mil',
                          P409>500000~'+500Mil')
         )%>% # los na se consideran de sueldo 0
  select(PARENTESCO,VIVE_P,SEXO,ASISTE=ASISTE_INSTITUCION_EDUCATIVA,
         ClaseHogar,PropHogar,TAM_HOGARC,EDADC, TRABAJA_PARA,RAZON,SUELDO,ESTADO_CIVIL,
         VIVE_M,CAMP,Luz, SL= SABE_LEER_ESCRIBIR,Luz,Gas,Alcantarillado,Basura,Acueducto,
         ProceAgua)%>%as.data.frame()
Segundo$EDADC<-as.factor(Segundo$EDADC)
Segundo$TAM_HOGARC<-as.factor(Segundo$TAM_HOGARC)
Segundo$ESTADO_CIVIL<-as.factor(Segundo$ESTADO_CIVIL)
Segundo$VIVE_M<-as.factor(Segundo$VIVE_M)
Segundo$PARENTESCO<-as.factor(Segundo$PARENTESCO)
Segundo$VIVE_P<-as.factor(Segundo$VIVE_P)
Segundo$CAMP<-as.factor(Segundo$CAMP)
Segundo$RAZON<-as.factor(Segundo$RAZON)
Segundo$TRABAJA_PARA<-as.factor(Segundo$TRABAJA_PARA)
Segundo$SUELDO<-as.factor(Segundo$SUELDO)
str(Segundo)
summary(Segundo)

acm2 <-dudi.acm (Segundo[,1:13], scannf = FALSE,nf=2)
barplot ( acm2$eig)
ayu2<-inertia.dudi(acm2,,T)
ayu2
plot(acm2)
plot(acm,Trow = F)
#hola