#d1a1da1dad

library(tidyverse)


CAMPANA <- c("2017/18","2018/19","2019/20",
             "2017/18","2018/19","2019/20",
             "2017/18","2018/19","2019/20")
CULTIVO <-     c("Soja","Soja","Soja",
                 "Trigo","Trigo","Trigo",
                 "Maíz","Maíz","Maíz")
PRODUCCION <-      c(55,37.78,55.3,
                     18.4,18.5,19.46, 
                     49.5,43.46,57)
AREA <- c(16.8,17,17.2,
          6,6.3,6.6,
          7.5,7.2,9)

Datos <- data.frame(CAMPANA,CULTIVO,PRODUCCION,AREA)
Datos

str(Datos)
Datos %>% str()

summary(Datos)

#ctr+shift+M PIPE
#ALT + -  FLECHITA

Datos %>% 
  filter(CULTIVO == "Soja" , CULTIVO == "Trigo")

Datos %>% 
  filter(CULTIVO == "Soja" | PRODUCCION>40)


Datos %>% 
  rename(GRANO = CULTIVO)

Datos <- Datos %>% 
  rename(GRANO = CULTIVO)

Datos

View(Datos)
head(Datos)
tail(Datos)

Datos3 <- Datos %>% 
  select(-c("PRODUCCION","GRANO"))
Datos3


Datos %>%
  filter(GRANO == "Soja" & PRODUCCION>40) %>% 
  select("CAMPANA")

Datos <- Datos %>% 
  mutate(RINDE = PRODUCCION / AREA)
Datos


Datos$RINDE = Datos$RINDE*10
Datos

Datos <- Datos %>% 
  mutate(CODIGO = case_when(GRANO == "Soja"    ~ "S",
                            GRANO == "Trigo"       ~ "T",
                            TRUE ~ "M"))
Datos

Datos <- Datos %>% 
  arrange(AREA, PRODUCCION,RINDE)
Datos

Datos %>% 
  group_by(GRANO) %>% 
  summarise(RINDE_MAX = max(RINDE),
            RINDE_MIN = min(RINDE),
            RINDE_PROM = mean(RINDE))

Datos %>% 
  group_by(GRANO) %>% 
  summarise(RINDE_MAX = max(RINDE),
            RINDE_MIN = min(RINDE),
            RINDE_PROM = mean(RINDE))

Datos %>% 
  group_by(CAMPANA) %>%
  summarise(CAMPANA_PROM = mean(RINDE))


Clima <- data.frame(CAMPANA = c("2017/18","2018/19","2019/20"),
                    ENSO = c("NIÑO", "NIÑA", "NEUTRO"),
                    PRECIPITACIONES = c(500, 1200, 850))

Datos_join <- Datos %>% 
  left_join(Clima, by = "CAMPANA")
Datos_join

Datos_join2 <- left_join(Clima,Datos,  by = "CAMPANA")

Datos_join3 <- right_join(Clima,Datos,  by = "CAMPANA")


Datos_join %>% 
  group_by(ENSO,GRANO) %>% 
  summarise(RINDE_PROMEDIO = mean(RINDE))

library(datasets)
head(iris)


iris <- iris %>% 
  mutate(id = 1:nrow(.)) %>% 
  select(id, everything())  


iris_vertical <- iris %>% gather(., # el . llama a lo que esta atras del %>% 
                                 key   = fafafaf,
                                 value = Valores,
                                 2:5) #le indico qué columnas juntar
head(iris_vertical)

iris_horizontal <- iris_vertical %>%
  spread(. ,
         key   = fafafaf, # la llave es la variable que va a dar los nombres de columna
         value = Valores) # los valores con que se llenan las celdas
head(iris_horizontal)


write.csv(Datos,"datos.csv")


