###Probabilidad de que 1/3 de los votos por sección electoral sean de jóvenes 
##menores de 29 años

probabilidad_seccion <- function(padron, menores29, votos_promedio) {
  prop_menores29 <- menores29 / padron
  k <- round(votos_promedio / 3)
  
  probabilidad <- choose(votos_promedio, k) * prop_menores29^k * (1 - prop_menores29)^(votos_promedio - k)
  
  return(probabilidad)
}

probabilidad_seccion(1555, 390, 821)

prob_sec_joven <- e2021cm
prob_sec_joven <- prob_sec_joven |> 
  left_join(base, by = c("SECCION" = "seccion"))

prob_sec_joven <- prob_sec_joven |>
  mutate(Prob_jovenes = probabilidad_seccion(PADRON_ELECTORAL, Menores_29, TOTAL_VOTOS) ) |>
  left_join(comparacion_votos, by = c("SECCION" = "SECCION"))

prob_sec_joven |>
  group_by(distrito_local.x) |>
  summarise(
    "Menores29" = sum(Menores_29, na.rm = T),
    "Padron_electoral_dto" = sum(PADRON_ELECTORAL, na.rm = T),
    "VOTOS_PROMEDIO" = sum(TOTAL_VOTOS)
  ) 
mapita <- ggplot() + 
  geom_sf(data = prob_sec_joven |> filter(MUNICIPIO.y == 3), aes(geometry = geometry, fill = Prob_jovenes)) +
  scale_fill_gradient(name = "Ganador") +
  labs(
    title =  "Ganador por Distrito Federal (Sin alianzas)",
    subtitle = "Fuente: Datos abiertos INE",
    caption = "Elaborado por Alberto Reyes Briseño"
  )+
  theme_void()

mapita

##Tabla de resultados por distrito federal
resultados_federales <- comparacion_votos |>
  group_by(distrito_federal) |>
  summarise(
    "PAN" = sum(PAN, na.rm = T),
    "MORENA" = sum(MORENA, na.rm = T),
    "PRI" = sum(PRI, na.rm = T),
    "PRD" = sum(PRD, na.rm = T),
    "MC" = sum(MC, na.rm = T),
    "PVEM" = sum(PVEM, na.rm = T),
    "PT"= sum(PT, na.rm = T),
  ) |>
  mutate("Votos_totales" = PAN + MORENA + PRI + PRD + MC + PVEM + 
           PT) |>
  mutate("%PAN" = PAN/Votos_totales,
         "%MORENA" = MORENA/Votos_totales,
         "%PRI" = PRI/Votos_totales,
         "%PRD" = PRD/Votos_totales,
         "%PT" = PT/Votos_totales,
         "%PVEMA" = PVEM/Votos_totales,
         "%MC" = MC/Votos_totales) |>
  mutate("Ganador" = case_when(PAN > MORENA & PAN > PRD & PAN > PRI & PAN > MC & PAN > PT & PAN > PVEM ~ "PAN",
                               MORENA > PAN & MORENA > PRD & MORENA > PRI & MORENA > MC & MORENA > PT & MORENA > PVEM ~ "MORENA",
                               PRD > PAN & PRD > MORENA & PRD > PRI & PRD > MC & PRD > PT & PRD > PVEM ~ "PRD",
                               PRI > PAN & PRI > MORENA & PRI > PRD & PRI > MC & PRI > PT & PRI > PVEM ~ "PRI",
                               MC > PAN & MC > MORENA & MC > PRD & MC > PRI & MC > PT & MC > PVEM ~ "MC",
                               PT > PAN & PT > MORENA & PT > PRD & PT > PRI & PT > MC & PT > PVEM ~ "PT",
                               PVEM > PAN & PVEM > MORENA & PVEM > PRD & PVEM > PRI & PVEM > MC & PVEM > PT ~ "PVEM",
                               TRUE ~ "Empate")) |>
  mutate("VaXMexico" = PAN + PRI + PRD,
         "JHR" = MORENA + PT + PVEM,
         "pVPM" = VaXMexico/Votos_totales,
         "pJHR" = JHR/Votos_totales, 
         "Ganador_alianza" = if_else(
           pVPM - pJHR <= .07 & pVPM - pJHR >= -.07, "Empate", if_else(
             VaXMexico > JHR, "VxM", "JHR"
           ) 
         ))
##Grafica distritos (Partidos individuales)
mapita <- ggplot() + 
  geom_sf(data = resultados_federales, aes(fill = Ganador), color = "black", size = .1) +
  scale_fill_manual(name = "Ganador", values = c("grey", "#9c0729", "#0069c0", "#FF0000", "#ECAB09", 
                                                 "green", "purple"),
                    labels = c("Empate", "MORENA", "PAN", "PRI", "PRD", "PVEM", "PT",
                               "MC"), 
                    breaks = c("Empate", "MORENA", "PAN", "PRI", "PRD", "PVEM", "PT",
                               "MC")) +
  labs(
    title =  "Ganador por Distrito Federal (Sin alianzas)",
    subtitle = "Fuente: Datos abiertos INE",
    caption = "Elaborado por Alberto Reyes Briseño"
  )+
  theme_void()

mapita

##Gráfica distritos (alianzas)
mapita <- ggplot() + 
  geom_sf(data = resultados_federales, aes(fill = Ganador_alianza), color = "black", size = .1) +
  scale_fill_manual(name = "Ganador", values = c("grey", "#9c0729", "#0069c0"),
                    labels = c("Empate", "JHR", "VxM"), 
                    breaks = c("Empate", "JHR", "VxM")) +
  geom_sf_text(data = resultados_federales, aes(label = distrito_federal), color = "black", size = 3) +
  labs(
    title =  "Ganador por Distrito Federal (VxM vs. Morena y aliados)",
    subtitle = "Empate técnico +7%/-7% dif.
Fuente: Datos abiertos INE",
    caption = "Elaborado por Alberto Reyes Briseño"
  )+
  theme_void()

mapita

##ANALISIS CON CON DISTRITOS LOCALES

resultados_locales <- comparacion_votos |>
  group_by(distrito_local) |>
  summarise(
    "PAN" = sum(PAN, na.rm = T),
    "MORENA" = sum(MORENA, na.rm = T),
    "PRI" = sum(PRI, na.rm = T),
    "PRD" = sum(PRD, na.rm = T),
    "MC" = sum(MC, na.rm = T),
    "PVEM" = sum(PVEM, na.rm = T),
    "PT"= sum(PT, na.rm = T),
  ) |>
  mutate("Votos_totales" = PAN + MORENA + PRI + PRD + MC + PVEM + 
           PT) |>
  mutate("%PAN" = PAN/Votos_totales,
         "%MORENA" = MORENA/Votos_totales,
         "%PRI" = PRI/Votos_totales,
         "%PRD" = PRD/Votos_totales,
         "%PT" = PT/Votos_totales,
         "%PVEMA" = PVEM/Votos_totales,
         "%MC" = MC/Votos_totales) |>
  mutate("Ganador" = case_when(PAN > MORENA & PAN > PRD & PAN > PRI & PAN > MC & PAN > PT & PAN > PVEM ~ "PAN",
                               MORENA > PAN & MORENA > PRD & MORENA > PRI & MORENA > MC & MORENA > PT & MORENA > PVEM ~ "MORENA",
                               PRD > PAN & PRD > MORENA & PRD > PRI & PRD > MC & PRD > PT & PRD > PVEM ~ "PRD",
                               PRI > PAN & PRI > MORENA & PRI > PRD & PRI > MC & PRI > PT & PRI > PVEM ~ "PRI",
                               MC > PAN & MC > MORENA & MC > PRD & MC > PRI & MC > PT & MC > PVEM ~ "MC",
                               PT > PAN & PT > MORENA & PT > PRD & PT > PRI & PT > MC & PT > PVEM ~ "PT",
                               PVEM > PAN & PVEM > MORENA & PVEM > PRD & PVEM > PRI & PVEM > MC & PVEM > PT ~ "PVEM",
                               TRUE ~ "Empate")) |>
  mutate("VaXMexico" = PAN + PRI + PRD,
         "JHR" = MORENA + PT + PVEM,
         "pVPM" = VaXMexico/Votos_totales,
         "pJHR" = JHR/Votos_totales, 
         "Ganador_alianza" = if_else(
           pVPM - pJHR <= .07 & pVPM - pJHR >= -.07, "Empate", if_else(
             VaXMexico > JHR, "VxM", "JHR"
           ) 
         ))


xlsx::write.xlsx(resultados_locales, file = "dis.xlsx")
str(resultados_locales)
###
###Mapa de ganador por distrito local (partidos solos)
mapita <- ggplot() + 
  geom_sf(data = resultados_locales, aes(fill = Ganador), color = "black", size = .1) +
  scale_fill_manual(name = "Ganador", values = c("grey", "#9c0729", "#0069c0", "#FF0000", "#ECAB09", 
                                                 "green", "purple"),
                    labels = c("Empate", "MORENA", "PAN", "PRI", "PRD", "PVEM", "PT",
                               "MC"), 
                    breaks = c("Empate", "MORENA", "PAN", "PRI", "PRD", "PVEM", "PT",
                               "MC")) +
  labs(
    title =  "Ganador por Distrito Local (Sin alianzas)",
    subtitle = "Fuente: Datos abiertos INE",
    caption = "Elaborado por Alberto Reyes Briseño"
  )+
  theme_void()

mapita
##
##MAPA POR GANADOR DISTRITO LOCAL (ALIANZA)

mapita <- ggplot() + 
  geom_sf(data = resultados_locales, aes(fill = Ganador_alianza), color = "black", size = .1) +
  scale_fill_manual(name = "Ganador", values = c("grey", "#9c0729", "#0069c0"),
                    labels = c("Empate", "JHR", "VxM"), 
                    breaks = c("Empate", "JHR", "VxM")) +
  geom_sf_text(data = resultados_locales, aes(label = distrito_local), color = "black", size = 3) +
  labs(
    title =  "Ganador por Distrito Local (VxM vs. JHR)",
    subtitle = "Empate técnico +7%/-7% dif.
Fuente: Datos abiertos INE",
    caption = "Elaborado por Alberto Reyes Briseño"
  )+
  theme_void()

mapita
###
### VOTACIOIN POR ALCALDIA

resultados_alcaldias <- comparacion_votos |>
  group_by(MUNICIPIO.y) |>
  summarise(
    "PAN" = sum(PAN, na.rm = T),
    "MORENA" = sum(MORENA, na.rm = T),
    "PRI" = sum(PRI, na.rm = T),
    "PRD" = sum(PRD, na.rm = T),
    "MC" = sum(MC, na.rm = T),
    "PVEM" = sum(PVEM, na.rm = T),
    "PT"= sum(PT, na.rm = T),
  ) |>
  mutate("Votos_totales" = PAN + MORENA + PRI + PRD + MC + PVEM + 
           PT) |>
  mutate("%PAN" = PAN/Votos_totales,
         "%MORENA" = MORENA/Votos_totales,
         "%PRI" = PRI/Votos_totales,
         "%PRD" = PRD/Votos_totales,
         "%PT" = PT/Votos_totales,
         "%PVEMA" = PVEM/Votos_totales,
         "%MC" = MC/Votos_totales) |>
  mutate("Ganador" = case_when(PAN > MORENA & PAN > PRD & PAN > PRI & PAN > MC & PAN > PT & PAN > PVEM ~ "PAN",
                               MORENA > PAN & MORENA > PRD & MORENA > PRI & MORENA > MC & MORENA > PT & MORENA > PVEM ~ "MORENA",
                               PRD > PAN & PRD > MORENA & PRD > PRI & PRD > MC & PRD > PT & PRD > PVEM ~ "PRD",
                               PRI > PAN & PRI > MORENA & PRI > PRD & PRI > MC & PRI > PT & PRI > PVEM ~ "PRI",
                               MC > PAN & MC > MORENA & MC > PRD & MC > PRI & MC > PT & MC > PVEM ~ "MC",
                               PT > PAN & PT > MORENA & PT > PRD & PT > PRI & PT > MC & PT > PVEM ~ "PT",
                               PVEM > PAN & PVEM > MORENA & PVEM > PRD & PVEM > PRI & PVEM > MC & PVEM > PT ~ "PVEM",
                               TRUE ~ "Empate")) |>
  mutate("VaXMexico" = PAN + PRI + PRD,
         "JHR" = MORENA + PT + PVEM,
         "pVPM" = VaXMexico/Votos_totales,
         "pJHR" = JHR/Votos_totales, 
         "Ganador_alianza" = if_else(
           pVPM - pJHR <= .1 & pVPM - pJHR >= -.1, "Empate", if_else(
             VaXMexico > JHR, "VxM", "JHR"
           ) 
         ))
##
##MAPA POR GANADOR EN CADA ALCALDIA (SIN ALIANZA)
mapita <- ggplot() + 
  geom_sf(data = resultados_alcaldias, aes(fill = Ganador), color = "black", size = .1) +
  scale_fill_manual(name = "Ganador", values = c("grey", "#9c0729", "#0069c0", "#FF0000", "#ECAB09", 
                                                 "green", "purple"),
                    labels = c("Empate", "MORENA", "PAN", "PRI", "PRD", "PVEM", "PT",
                               "MC"), 
                    breaks = c("Empate", "MORENA", "PAN", "PRI", "PRD", "PVEM", "PT",
                               "MC")) +
  labs(
    title =  "Ganador por Distrito Local (Sin alianzas)",
    subtitle = "Fuente: Datos abiertos INE",
    caption = "Elaborado por Alberto Reyes Briseño"
  )+
  theme_void()

mapita
##
##MAPA GANADOR POR ALCALDIA (CON ALIANZAS)

mapita <- ggplot() + 
  geom_sf(data = resultados_alcaldias, aes(fill = Ganador_alianza), color = "black", size = .1) +
  scale_fill_manual(name = "Ganador", values = c("grey", "#9c0729", "#0069c0"),
                    labels = c("Empate", "JHR", "VxM"), 
                    breaks = c("Empate", "JHR", "VxM")) +
  labs(
    title =  "Ganador por alcaldía (VxM vs. JHR)",
    subtitle = "Empate técnico +10% / -10% dif.
Fuente: Datos abiertos INE",
    caption = "Elaborado por Alberto Reyes Briseño"
  )+
  theme_void()
mapita
## MAPA CON EL SEGUNDO LUGAR POR SECCION