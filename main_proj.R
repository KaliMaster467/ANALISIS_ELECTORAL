install.packages("RColorBrewer")
library("RColorBrewer")
library(tidyverse)
library(esquisse)
library(readxl)
library(ggplot2)
library(gridExtra)
library(moments)
library(xlsx)
library(ggExtra)
library(readr)
library(rgdal)
library(foreign)
library(sf)

e2000 <- read_csv("2000_SEE_DIP_FED_MR_NAL_SEC.csv")
View(e2000)

e2006 <- read_csv("2006_SEE_DIP_FED_MR_NAL_SEC.csv")
View(e2006)

e2012 <- read_csv("2012_SEE_DIP_FED_MR_NAL_SEC.csv")
View(e2012)

e2018 <- read_csv("2018_SEE_DIP_FED_MR_NAL_SEC.csv")
View(e2000)

e2021 <- read_csv("2021_SEE_DIP_FED_MR_NAL_SEC.csv")
View(e2021)

cat_secciones <- read_excel("9_Catalogo de Secciones con Distrito Local.xls")
View(cat_secciones)

escolaridad <- read_excel("Detalle de Estratos (1).xlsx", 
                          col_types = c("numeric", "text", "numeric", 
                                        "numeric"))
View(escolaridad)
edades_sexo <- read_excel("DatosAbiertos-derfe-pdln_edms_re_20230428.xlsx")
View(edades_sexo)
##Crear frames exclusivamente de la Ciudad de México


e2018cm <- e2018 |> 
  filter(NOMBRE_ESTADO == "CIUDAD DE MÉXICO")



e2012cm <- e2012 |> 
  filter(NOMBRE_ESTADO == "DISTRITO FEDERAL")



e2006cm <- e2006 |> 
  filter(NOMBRE_ESTADO == "DISTRITO FEDERAL")

e2000cm <- e2000 |> 
  filter(NOMBRE_ESTADO == "DISTRITO FEDERAL")

e2021cm <- e2021 |> 
  filter(NOMBRE_ESTADO == "CIUDAD DE MÉXICO")

edades_sexo <- edades_sexo |> 
  filter( NOMBRE_ENTIDAD == "CIUDAD DE MEXICO")


##SACAR PROMEDIO POR SECCIONES
##LA TABLA BASE ES cat_secciones
base <- cat_secciones


base <- base |> 
  left_join(e2000cm |> 
               select(SECCION, AC), by = c("seccion" = "SECCION")) |> rename(PAN2000 = AC)
base <- base |> 
  left_join(e2006cm |> 
               select(SECCION, PAN), by = c("seccion" = "SECCION")) |> rename(PAN2006 = PAN)
base <- base |> 
  left_join(e2012cm |> 
               select(SECCION, PAN), by = c("seccion" = "SECCION")) |> rename(PAN2012 = PAN)
base <- base |> 
  left_join(e2018cm |> 
               select(SECCION, PAN), by = c("seccion" = "SECCION")) |> rename(PAN2018 = PAN)

#Sacamos el promedio de las votaciones presidenciales
base <- base |>
  mutate(VPANprom = round((PAN2000+PAN2006+PAN2012+PAN2018)/4))

base <- base |> rowwise() |>
  mutate(Desv = sd(c(PAN2000,PAN2006,PAN2012,PAN2018)))

#Modificar edades para tener menores de 29 años

edades_sexo <- edades_sexo |>
  mutate(Menores_29 = PADRON_18_HOMBRES+PADRON_18_MUJERES+PADRON_19_HOMBRES+
           PADRON_19_MUJERES+PADRON_20_24_HOMBRES+PADRON_20_24_MUJERES+
           PADRON_25_29_HOMBRES+PADRON_25_29_MUJERES) |>
  
  mutate(Hombres_m29 = PADRON_18_HOMBRES+PADRON_19_HOMBRES+
           PADRON_20_24_HOMBRES+
           PADRON_25_29_HOMBRES) |>
  
  mutate(Mujeres_m29 = PADRON_18_MUJERES+PADRON_19_MUJERES+
           PADRON_20_24_MUJERES+
           PADRON_25_29_MUJERES)


#Join de sexo y edad

base <- base |>
  left_join(edades_sexo |> 
              select(SECCION, PADRON_HOMBRES, PADRON_MUJERES, PADRON_ELECTORAL, Menores_29,
                     Hombres_m29, Mujeres_m29), by = c("seccion" = "SECCION")) 
#Join indice de escolaridad

base <- base |>
  left_join(escolaridad |> 
              select(Nombre, Valor), by = c("seccion" = "Nombre")) |> 
  rename(Escolaridad = Valor)

  
base_ponderada <- base 

xlsx::write.xlsx(base, file = "base_ponderada_dto.xlsx", row.names = T)
####Grafica de densidad por sección
frecuencia_voto <- base |>
  select(seccion, VPANprom)

frecuencia_voto |> mean(VPANprom, na.rm = T)

mean(frecuencia_voto, VPANprom, na.rm = TRUE)


##Gráfica de densidad de secciones
ggplot(frecuencia_voto) + 
  geom_density(aes( x = VPANprom), fill = "#02152b", alpha = 0.9, color = NA)+
  geom_vline(aes(xintercept = mean(frecuencia_voto$VPANprom, na.rm = T),
             color = "Cuantil 0.5"),
             linetype = "dashed",
             size = 1) +
  annotate("label", x = mean(frecuencia_voto$VPANprom, na.rm = T) + 60, y = .003,
           label = "Promedio de votos 
por sección CDMX (207.95)", angle = 90) + 
  labs(
    x = "Votos por sección",
    y = "Densidad",
    title = "Densidad votos promedio por sección PAN (CDMX) 
(Promedio 2000-2006-2012-2018)",
    subtitle = "Fuente: Datos abiertos del INE",
    caption = "Elaborado por Alberto Reyes Briseño"
  ) +
  theme_bw()

#Calculo de probabilidad voto por sección promedio en CDMX
prob_votos_seccion <- base |>
  group_by(VPANprom) |>
  tally()
ggplot(prob_votos_seccion) +
  geom_col(aes(x = VPANprom, y = n))

prob_votos_seccion <- prob_votos_seccion |>
  mutate(Probabilidad = n / sum(n))
##Gráfica de masa de probabilidad
ggplot(prob_votos_seccion) + 
  geom_col(aes(x = VPANprom, y = Probabilidad), fill = "#02152b") + 
  labs(
    x = "Votos promedio por sección",
    y = "P(x)",
    title = "Función Masa probabilidad de votos promedio
por sección PAN (CDMX)",
    subtitle = "Fuente: Datos abiertos INE",
    caption = "Elaborado por Alberto Reyes Briseño"
  ) + 
  theme_bw()

#Calculo de probabilidad acumulada
#Ejemplo de P(X >= 300)

prob_votos_seccion |>
  filter(VPANprom >= 300) |>
  summarise(
    "Probabilidad más de 160 votos" = 
      scales::percent(sum(Probabilidad))
  )
## FUNCIONES MASA DE PROBABILIDAD POR GRADO DE ESCOLARIDAD
# Creamos el DataFrame
grado <- base |> 
  select(seccion, VPANprom, Escolaridad)

grado <- grado |> 
  mutate(
    Grado = if_else(Escolaridad == 0, "SP", 
                    if_else(Escolaridad > 0 & Escolaridad < 7, "Primaria",
                            if_else(Escolaridad >= 7 & Escolaridad < 10, "Secundaria", 
                                    if_else(Escolaridad >= 10 & Escolaridad < 13, "Bachillerato", "Profesional"))))
  ) 

grado_masa <- grado |>
  group_by(VPANprom, Grado) |>
  tally() |>
  rename(Freq = n) 

ggplot(grado_masa) + 
  geom_histogram(aes(x = VPANprom, y = after_stat(density), fill=Grado), position = position_dodge())+
  geom_density(aes(x = VPANprom, fill = Grado), alpha = 0.18, color= NA) +
  scale_fill_brewer(palette = "Set1") +
  labs(
    x = "Votos promedio por sección",
    y = "P(x)",
    title = "Función Masa de probabilidad del número 
de votos del PAN por sección electoral (CDMX) dado el
grado escolar por sección",
    subtitle ="Fuente: Datos abiertos INE e INEGI",
    caption = "Elaborado por Alberto Reyes Briseño"
  ) + 
  theme_bw()

ggplot(grado_masa) + 
  geom_histogram(aes(x = VPANprom, y = after_stat(density), fill=Grado), position = position_dodge())+
  geom_density(aes(x = VPANprom, fill=Grado), alpha = 0.2, color = NA) + 
  facet_wrap(~Grado) + 
  scale_fill_brewer(palette = "Set1") +
  labs(
    x = "Votos promedio por sección",
    y = "P(x)",
    title = "Función Masa de probabilidad del número 
de votos del PAN por sección electoral (CDMX) dado el
grado escolar por sección",
    subtitle ="Fuente: Datos abiertos INE e INEGI",
    caption = "Elaborado por Alberto Reyes Briseño"
  ) + 
  theme_bw()

#Estadísiticos

grado_masa |>
  group_by(Grado) |>
  summarise(
    "Media" = mean(VPANprom, na.rm = T),
    "SD" = sd(VPANprom, na.rm = T),
    "Mediana" = median(VPANprom, na.rm = T),
    "Min" = min(VPANprom, na.rm = T),
    "Max" = max(VPANprom, na.rm = T)
  )
xlsx::write.xlsx(grado_masa |>
                   group_by(Grado) |>
                   summarise(
                     "Media" = mean(VPANprom, na.rm = T),
                     "SD" = sd(VPANprom, na.rm = T),
                     "Mediana" = median(VPANprom, na.rm = T),
                     "Min" = min(VPANprom, na.rm = T),
                     "Max" = max(VPANprom, na.rm = T)
                   ), file = "est_grado.xlsx", row.names = T)

  
##Proyeccion trinfos 2024
e21 <- read_csv("2021_SEE_DIP_FED_MR_NAL_SEC.csv")
View(e21)

e21cm <- e21 |>
  filter(NOMBRE_ESTADO == "CIUDAD DE MÉXICO")



## FUNCIONES MASA DE PROBABILIDAD DONDE LA MAYORIA ABSOLUTA SEA DE UN SEXO (50 + 1)

masa_sexo <- base


masa_sexo <- masa_sexo |>
  mutate(Sexo_mayoritario = if_else(
    PADRON_HOMBRES/PADRON_ELECTORAL >= .5, "H", "M"
  ))

##Numero de secciones que el hombre o la mujer es mayoría absoluta
masa_sexo |>
  group_by(VPANprom, Sexo_mayoritario) |>
  tally()

masa_sexo <- masa_sexo |>
  select(VPANprom, Sexo_mayoritario)

ggplot(masa_sexo) + 
  geom_histogram(aes(x = VPANprom, y = after_stat(density), fill = Sexo_mayoritario), 
                 position = position_dodge()) + 
  labs(
    x = "X: Votación promedio por sección PAN",
    y = "P(x)",
    title ="Función Masa de probabilidad de votos PAN por sección 
dado el sexo mayoritario (50% + 1)",
    subtitle = "Fuente: Datos abiertos del INE",
    caption = "Elaborado por Alberto Reyes Briseño"
  ) + 
  scale_fill_manual(values = c("H" = "lightblue", "M" = "#9A208C")) +
  theme_bw()
#Elaboramos la gráfica de probabilidad acumulada para cada sexo mayoritario
#Chat GPT

mh <- masa_sexo |>
  filter(Sexo_mayoritario == "H") |>
  group_by(VPANprom, Sexo_mayoritario) |>
  tally()
mh <- mh |> 
  mutate(prob = n / sum(mh$n))
cdf <- function(x, p){
  cumsum(p)
}

Fa <- cdf(mh$VPANprom, mh$prob)
funh <- data.frame(mh$VPANprom, Fa)

ggplot(funh) + 
  geom_step(aes( x = mh.VPANprom, y = Fa), color = "blue") + 
  labs(
    x = "X: Votos por sección electoral promedio PAN",
    y = "Probabilidad acumulada",
    title = "Función de probabilidad acumulada de votos PAN por
sección electoral ( 50 + 1 son Hombres)",
    subtitle = "Fuentes: Datos abiertos INE",
    caption = "Elaborado por Alberto Reyes Briseño"
  ) +
  theme_bw()

#Ahora para mujer

mm <- masa_sexo |>
  filter(Sexo_mayoritario == "M") |>
  group_by(VPANprom, Sexo_mayoritario) |>
  tally()
mm <- mm |> 
  mutate(prob = n / sum(mm$n))
cdf <- function(x, p){
  cumsum(p)
}

Fa <- cdf(mm$VPANprom, mm$prob)
funm <- data.frame(mm$VPANprom, Fa)

ggplot(funm) + 
  geom_step(aes( x = mm.VPANprom, y = Fa), color = "#9A208C") +
  labs(
    x = "X: Votos por sección electoral promedio PAN",
    y = "Probabilidad acumulada",
    title = "Función de probabilidad acumulada de votos PAN por
sección electoral ( 50 + 1 son Mujeres)",
    subtitle = "Fuentes: Datos abiertos INE",
    caption = "Elaborado por Alberto Reyes Briseño"
  ) + 
  theme_bw()

acu <- funh |>
  left_join(funm, by = c("mh.VPANprom" = "mm.VPANprom")) |>
  rename("H" = Fa.x) |>
  rename("M" = Fa.y)

ggplot(acu) + 
  geom_step(aes( x = mh.VPANprom, y = H, color = "H")) +
  geom_step(aes( x = mh.VPANprom, y = M, color = "M")) + 
  labs(
    x = "X: Votos por sección electoral promedio PAN",
    y = "Probabilidad acumulada",
    title = "Función de probabilidad acumulada de votos PAN por
sección electoral (Por sexo mayoritario)",
    subtitle = "Fuentes: Datos abiertos INE",
    caption = "Elaborado por Alberto Reyes Briseño"
  ) + 
  scale_color_manual(values = c("H" = "blue", "M" = "#9A208C")) +
  theme_bw()

##Densidades por distritos
##Hace la gráfica de todos los distritos
ggplot(base) + 
  geom_density(aes( x = VPANprom), fill = "#02108b" )+
  facet_wrap(~ distrito_local) +
  labs(
    x = "Votos por sección",
    y = "Densidad",
    title = "Densidad de votación promedio por sección electoral
(Por Distrito Electoral Local CDMX)
(Promedio 2000-2006-2012-2018)",
    subtitle = "Fuente: Datos abiertos del INE",
    caption = "Alberto Reyes Briseño"
  ) +
  theme_minimal()


#summarise

summarise_dtos <- base |>
  group_by(distrito_local) |>
  summarise(
    "Media" = mean(VPANprom, na.rm = T),
    "SD" = sd(VPANprom, na.rm = T),
    "Mediana" = median(VPANprom, na.rm = T),
    "Min" = min(VPANprom, na.rm = T),
    "Max" = max(VPANprom, na.rm = T)
  ) 

mean_dto <- summarise_dtos |>
  select(distrito_local, Media)
##Hacer tabla de promedios por distrito
xlsx::write.xlsx(mean_dto, file = "mean_dto.xlsx", row.names = T)
mean_dto <- mean_dto |> 
  pivot_wider(names_from = distrito_local, values_from = Media)
##Histograma media
  summarise_dtos |> 
    ggplot() + 
    geom_col(aes(x = distrito_local, y = Media), fill = summarise_dtos$distrito_local) + 
    scale_fill_brewer(palette = "Set1")+
    geom_hline(aes(yintercept = mean(Media, na.rm = T)),
               linetype = "dashed",
               color = "red") + 
    annotate("label", x = 20, y = 200,
             label = "Promedio de votación por distrito", angle = 90) + 
    labs(
      x = "Distrito local",
      y = "Votos por sección", 
      title = "Votos seccionales PAN promedio por Distrito Local", 
      subtitle = "Fuente: Datos abiertos INE", 
      caption = "Elaborado por Alberto Reyes Briseño"
    )  + 
    scale_x_continuous("Distrito local", labels = as.character(summarise_dtos$distrito_local), breaks = summarise_dtos$distrito_local)+
    theme_bw()
  
##Asimetría
asimetria <- base |>
  group_by(distrito_local) |>
  summarise(
    "Asimetría" = skewness(VPANprom, na.rm = T)
  ) 


#Calculo de probabilidad voto por sección promedio en CDMX
prob_votos_seccion <- base |>
  group_by(VPANprom) |>
  tally()
ggplot(prob_votos_seccion) +
  geom_col(aes(x = VPANprom, y = n))

prob_votos_seccion <- prob_votos_seccion |>
  mutate(Probabilidad = n / sum(n))

ggplot(prob_votos_seccion) + 
  geom_col(aes(x = VPANprom, y = Probabilidad))




##Densidad conjunta Votación promedio y menores de 29 años (CHAT GPT)

masa_con <- base |>
  select(VPANprom, Menores_29, PADRON_ELECTORAL)

masa_con <- masa_con |>
  mutate(Pormen = Menores_29 / PADRON_ELECTORAL)

med_con <- masa_con |>
  summarise("VPANprom" = mean(masa_con$VPANprom, na.rm = T),
            "Menores_29" = mean(masa_con$Menores_29, na.rm = T),
            "Pormen" = mean(masa_con$Pormen, na.rm = T))


mean(masa_con$VPANprom, na.rm = T)

## gráfica con marginales (votos por sección / % menores de 29)
p <- ggplot(masa_con, aes(VPANprom, Pormen)) +
  geom_point() + 
  geom_density2d() +
  geom_density_2d_filled(alpha = 0.5) + 
  geom_point(aes(color = "Media"), shape = 18, size = 5, data = med_con) + 
  labs(
    x = "Votos promedio por sección PAN",
    y = "Menores de 29 años por sección 
electoral (%)",
    title = "Relación voto y menores de 29 años 
por sección electoral PAN (CDMX)",
    subtitle = "Fuente: Datos abiertos INE",
    caption = "Elaborado por Alberto Reyes Briseño"
  ) +
  theme_bw()

# Agregar las distribuciones marginales
p2 <- ggMarginal(p, type = "density", fill = "#02108b", size = 3)

# Mostrar la gráfica
p2
##Correlación
cor(masa_con$VPANprom, masa_con$Pormen, use = "complete.obs")

# Crear la gráfica de densidad conjunta (votos PAN / Grado de escolaridad)

m <- ggplot(grado, aes(x = VPANprom, y = Escolaridad)) +
  geom_point() +
  xlim(0, 1000) +
  ylim(0, 20)

# contour lines
m + geom_density_2d() +
  geom_density_2d_filled(alpha = 0.5)+
  labs(
    x = "Votación promedio por sección PAN (CDMX)",
    y = "Coeficiente de grado escolar 
por sección electoral",
    title = "Distribución de probabilidad conjunta de votos PAN
y grado escolar por sección electoral",
    subtitle = "Fuente: Datos abiertos INE e INEGI",
    caption = "Elaborado por Alberto Reyes Briseño"
  )
#Correlación 
cor(grado$VPANprom, grado$Escolaridad, use = "complete.obs")

##Hacemos la comparación de conjuntas PAN-MORENA-MC (voto por sección / % menores de 
#29)

base_comp <- base |>
  select(seccion, VPANprom, Menores_29, PADRON_ELECTORAL)

base_comp <- base_comp |> 
  left_join(e2018cm, by = c("seccion" = "SECCION")) |>
  select(seccion, VPANprom, Menores_29, MORENA, PRI, MC, Menores_29, PADRON_ELECTORAL)|>
  rename(MORENA18 = MORENA, MC18 = MC, PRI18 = PRI) |>
  left_join(e2021cm, by = c("seccion" = "SECCION")) |>
  select(seccion, VPANprom, Menores_29, MORENA18, MC18, PRI18, MORENA, MC, PRI, Menores_29, PADRON_ELECTORAL) |>
  rename(MORENA21 = MORENA, MC21 = MC, PRI21 = PRI)

base_comp <- base_comp |>
  mutate(VMORprom = round((MORENA18 + MORENA21)/2),
         MC = round(MC18 + MC21) /2, 
         PRI = round(PRI18 + PRI21)) |>
  select(seccion, VPANprom, VMORprom, Menores_29, PADRON_ELECTORAL, MC, PRI) |>
  mutate(promEd = Menores_29/PADRON_ELECTORAL) |>
  rename(PAN = VPANprom, MORENA = VMORprom)

base_comp <- base_comp |>
  pivot_longer(cols=c("PAN", "MORENA", "MC", "PRI"),
               names_to = "partido",
               values_to = "votacion")

#GRAFICAMOS LAS DENSIDADES CONJUNTAS
par <- ggplot(base_comp, aes(votacion, promEd)) +
  geom_point() + 
  xlim(0, 700) +
  ylim(0, max(base_comp$promEd)) + 
  geom_density2d() +
  geom_density_2d_filled(contour_var = "ndensity") + 
  geom_smooth(method = lm, se = FALSE, color = "red") + 
  labs(
    x = "Votos promedio por sección",
    y = "Menores de 29 años por sección 
electoral (%)",
    title = "Relación voto por partido y % menores de 29 años 
por sección electoral (CDMX)",
    subtitle = "Fuente: Datos abiertos INE",
    caption = "Elaborado por Alberto Reyes Briseño"
  ) +
  facet_wrap(~ partido) + 
  theme_minimal()

# Mostrar la gráfica
par

####SUMMARY
sum_parti <- base_comp |>
    group_by(partido) |>
    summarise("Correlación" = cor(votacion, promEd, use = "complete.obs"))

xlsx::write.xlsx(sum_parti, file = "cor_partidos.xlsx", row.names = T)


##MAPA DE LA CDMX POR SECCIÓN DONDE SE MUESTRE SECCIONES % DE MENORES DE 29
mapa_shp <- st_read("SECCION/SECCION.shp")
head(mapa_shp)
mapa_shp <- mapa_shp |>
  subset(ENTIDAD == 9)
mapa_dbf <- read.dbf("SECCION/SECCION.dbf")

mapa_data <- left_join(mapa_shp, mapa_dbf, by = "SECCION") 

mapa_data <- mapa_data |>
  left_join(base, by = c("SECCION" = "seccion"))

mapa_shp_n <- mapa_shp |>
  left_join(base, by = c("SECCION" = "seccion")) |>
  filter(distrito_federal == 19 | distrito_federal == 8)
write_sf(mapa_shp_n, "ndistcoyo.shp")


mapa_edades_sec <- mapa_data %>% filter(ENTIDAD.y == 9) # filtrar por ENTIDAD = 9

mapa_edades_sec$distrito_local <- factor(mapa_edades_sec$distrito_local)
mapa_edades_sec$distrito_federal <- factor(mapa_edades_sec$distrito_federal)

mapa_edades_sec <- mapa_edades_sec |>
  mutate(porcentaje_edad = (Menores_29 / PADRON_ELECTORAL)*100)

mapita <- ggplot() + 
  geom_sf(data = mapa_edades_sec, aes(fill = porcentaje_edad), color = NA) +
  scale_fill_gradient(name = "Secciones electorales") + 
  labs(
    title = "Porcentaje de jóvenes por sección electoral en la CDMX",
    subtitle = "Fuente: Datos abiertos INE",
    caption = "Elaborado por Alberto Reyes Briseño"
  ) + 
  theme_void()

mapita

##En Coyoacán
mapa_edades_sec <- mapa_edades_sec |>
  filter(distrito_federal == 19 | distrito_federal == 8)

# Supongamos que tienes un objeto de datos espaciales llamado 'datos_espaciales'
write_sf(mapa_edades_sec, "ndistcoyo.shp")


mapita <- ggplot() + 
  geom_sf(data = mapa_edades_sec, aes(fill = porcentaje_edad), color = "black", size = .1) +
  scale_fill_gradient(name = "Secciones electorales 
(% de población menor a 29 años)") + 
  labs(
    title = "Porcentaje de jóvenes por sección electoral en Coyoacán",
    subtitle = "Fuente: Datos abiertos INE",
    caption = "Elaborado por Alberto Reyes Briseño"
  ) +
  theme_void()

mapita
##MAPA por escolaridad
mapa_entidad_9_coyo <- mapa_entidad_9_data |> filter(distrito_federal == 19 | distrito_federal == 8)

mapa_entidad_9_coyo <- mapa_entidad_9_coyo |> 
  mutate(
    Grado = if_else(Escolaridad == 0, "SP", 
                    if_else(Escolaridad > 0 & Escolaridad < 7, "Primaria",
                            if_else(Escolaridad >= 7 & Escolaridad < 10, "Secundaria", 
                                    if_else(Escolaridad >= 10 & Escolaridad < 13, "Bachillerato", "Profesional"))))
  ) 

ggplot() + 
  geom_sf(data = mapa_entidad_9_coyo, aes(fill = Escolaridad), color = "black") +
  scale_fill_gradient(name = "Distrito Federal") + 
  theme_void()





## MUESTRA MAPA DE LA CDMX (IGNORAR)
# Cargar las librerías necesarias



mapa_shp <- st_read("SECCION/SECCION.shp")
head(mapa_shp)
mapa_shp <- mapa_shp |>
  subset(ENTIDAD == 9)
mapa_dbf <- read.dbf("SECCION/SECCION.dbf")

mapa_data <- left_join(mapa_shp, mapa_dbf, by = "SECCION") 

mapa_data <- mapa_data |>
  left_join(base, by = c("SECCION" = "seccion"))


mapa_entidad_9_data <- mapa_data %>% filter(ENTIDAD.y == 9) # filtrar por ENTIDAD = 9

mapa_entidad_9_data$distrito_local <- factor(mapa_entidad_9_data$distrito_local)
mapa_entidad_9_data$distrito_federal <- factor(mapa_entidad_9_data$distrito_federal)

mapita <- ggplot() + 
  geom_sf(data = mapa_entidad_9_data, aes(fill = distrito_local), color = "black", size = .1) +
  scale_fill_discrete(name = "Distrito Local") + 
  theme_void()

mapita

mapa_entidad_9_coyo <- mapa_entidad_9_data |> filter(distrito_federal == 19 | distrito_federal == 8)

mapa_entidad_9_coyo <- mapa_entidad_9_coyo |> 
  mutate(
    Grado = if_else(Escolaridad == 0, "SP", 
                    if_else(Escolaridad > 0 & Escolaridad < 7, "Primaria",
                            if_else(Escolaridad >= 7 & Escolaridad < 10, "Secundaria", 
                                    if_else(Escolaridad >= 10 & Escolaridad < 13, "Bachillerato", "Profesional"))))
  ) 

ggplot() + 
  geom_sf(data = mapa_entidad_9_coyo, aes(fill = Escolaridad), color = "black") +
  scale_fill_gradient(name = "Distrito Federal") + 
  theme_void()



##SECCIONES votación pan

comparacion_votos <- mapa_entidad_9_data

comparacion_votos <- comparacion_votos |> 
  left_join(e2021cm, by = c("SECCION" = "SECCION")) 
  
comparacion_votos <- comparacion_votos |>
  mutate(porcentaje_seccion_PAN = PAN /TOTAL_VOTOS) |>
  mutate(porcentaje_seccion_MORENA = MORENA / TOTAL_VOTOS) |>
  mutate(porcentaje_seccion_PRD = PRD / TOTAL_VOTOS) |>
  mutate(porcentaje_seccion_PRI = PRI / TOTAL_VOTOS) |>
  mutate(porcentaje_seccion_MC = MC / TOTAL_VOTOS) |>
  mutate(porcentaje_seccion_PT = PT / TOTAL_VOTOS) |>
  mutate(porcentaje_seccion_PVEM = PVEM / TOTAL_VOTOS) |>
  mutate(Ganador = case_when(PAN > MORENA & PAN > PRD & PAN > PRI & PAN > MC & PAN > PT & PAN > PVEM ~ "PAN",
                   MORENA > PAN & MORENA > PRD & MORENA > PRI & MORENA > MC & MORENA > PT & MORENA > PVEM ~ "MORENA",
                   PRD > PAN & PRD > MORENA & PRD > PRI & PRD > MC & PRD > PT & PRD > PVEM ~ "PRD",
                   PRI > PAN & PRI > MORENA & PRI > PRD & PRI > MC & PRI > PT & PRI > PVEM ~ "PRI",
                   MC > PAN & MC > MORENA & MC > PRD & MC > PRI & MC > PT & MC > PVEM ~ "MC",
                   PT > PAN & PT > MORENA & PT > PRD & PT > PRI & PT > MC & PT > PVEM ~ "PT",
                   PVEM > PAN & PVEM > MORENA & PVEM > PRD & PVEM > PRI & PVEM > MC & PVEM > PT ~ "PVEM",
                   TRUE ~ "Empate"))
  


mapita <- ggplot() + 
  geom_sf(data = comparacion_votos|> group_by(distrito_federal) , aes(fill = Ganador), color = "black", size = .1) +
  scale_fill_manual(name = "Ganador", values = c("grey", "#9c0729", "#0069c0", "#FF0000", "#ECAB09", 
                                                 "green", "purple", "orange"),
                    labels = c("Empate", "MORENA", "PAN", "PRI", "PRD", "PVEM", "PT",
                               "MC"), 
                    breaks = c("Empate", "MORENA", "PAN", "PRI", "PRD", "PVEM", "PT",
                               "MC")) +
  labs(
    title =  "Ganador relativo por sección electoral en la 
alcaldía Coyoacán",
    subtitle = "Fuente: Datos abiertos INE",
    caption = "Elaborado por Alberto Reyes Briseño"
  )+
  theme_void()

mapita
##Probabilidad de que mas del 20% de la votación por sección sea de menores de 29 años

#P (X > .2) = SUMATORIA (N CHOOSE n * p^k(1-p))
#Primero hacemos el data frame
prob_voto_joven <- comparacion_votos
prob_voto_joven <- prob_voto_joven |>
  mutate(Proporcion_menores29 = Menores_29/PADRON_ELECTORAL) |>
  select(SECCION, LN.x, PAD.x, ID.y, entidad, distrito_local, 
         distrito_federal, PADRON_ELECTORAL, Menores_29, MUNICIPIO, municipio,
         TOTAL_VOTOS, LISTA_NOMINAL, geometry, Proporcion_menores29)
#Sacamos el promedio
mean(prob_voto_joven$Proporcion_menores29)
## función binomial

f2 <- function(padron, jovenes, votos){
  porcentaje_limite <- 0.25
  votos_totales <- votos
  votos_jovenes_minimo <- ceiling(porcentaje_limite * votos_totales)
  
  prob_acumulada <- 1 - pbinom(votos_jovenes_minimo - 1, votos_totales, jovenes / padron)
  
  return(prob_acumulada)
}

f2(1555, 390, 82)

#ejemplo probabilidad_seccion(.3, 1555, 390, 821)
#hacemos el for para sumar

#probabilidad_acumulada<- function (padron, menores29, votos_promedio){
 #   suma <- 0 
 #   for(i in c(.2, .3 , .4, .5, .6 , .7, .8, .9, 1)){
  #    suma <- suma + probabilidad_seccion(i, padron, menores29, votos_promedio)  
   # }
    
#return (suma)
#}

#probabilidad_acumulada(1555, 390, 82)

prob_voto_joven <- prob_voto_joven |> 
  mutate(probabilidad_acumulada_jovenes_voto = (f2(PADRON_ELECTORAL, Menores_29, TOTAL_VOTOS))*100) 
  

## Hacemos el mapeo
mapita <- ggplot() + 
  geom_sf(data = prob_voto_joven , aes(fill = probabilidad_acumulada_jovenes_voto), color = "black", size = .1) +
  scale_fill_gradient(name = "Sección (%)") +
  labs(
    title =  "Probabilidad de que más del 25% de los votos (CDMX)
sean de Menores de 29 años por sección electoral",
    subtitle = "Fuente: Datos abiertos INE",
    caption = "Elaborado por Alberto Reyes Briseño"
  )+
  theme_void()

mapita

##En Coyoacán
mapita <- ggplot() + 
  geom_sf(data = prob_voto_joven |> filter(municipio == 3), aes(fill = probabilidad_acumulada_jovenes_voto), color = "black", size = .1) +
  scale_fill_gradient(name = "Sección (%)") +
  labs(
    title =  "Probabilidad de que más del 25% de los votos
(Coyoacán) sean de Menores de 29 años por sección electoral",
    subtitle = "Fuente: Datos abiertos INE",
    caption = "Elaborado por Alberto Reyes Briseño"
  )+
  theme_void()

mapita
##
xlsx::write.xlsx(ls(), file = "listado_variables.xlsx")
sink("sesion.txt")

# Imprimir la información de la sesión
sessionInfo()

# Cerrar el archivo de texto
sink()

pnorm(-1.18616, 0, 1)
1-pchisq(19,19)
