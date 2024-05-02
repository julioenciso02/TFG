# Código final
pacman::p_load()
library(readr)
library(tidyverse)
library(kableExtra)
library(tm)
library(rvest)
library(tidytext)
library(sentimentr)
library(syuzhet)
library(quanteda)
datos <- read_csv("gunviolence.csv")
datos$state <- as.factor(datos$state)
datos$año <- substr(datos$date, 0,4)
datos$mes <- substr(datos$date, 6,7)
summary(as.factor(datos$año))



# --- AED-----
##---- Mapa incial-----
mapa_usa <- map_data("usa")

ggplot() +
  geom_polygon(data = mapa_usa, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_point(data = datos, aes(x = longitude, y = latitude), color = "red", size = 1, alpha = 0.5) +
  labs(x = "", y = "", title = "")+
  coord_cartesian(xlim = c(-130, -65), ylim = c(24, 50))+
  theme_void()

## ---- Meses y años -----
gun_violence_02 <- datos %>% filter(año > 2013)%>% 
  group_by(año, mes) %>% 
  summarize(total_incidents = n()) %>% 
  arrange(- total_incidents)
gun_viz_02 <- gun_violence_02 %>% 
  ggplot(aes(mes, total_incidents, color = año)) +
  geom_line(aes(group = año), lwd = 1) + geom_point(size = 2) + 
  ylab("Número de incidentes")+
  ylim(c(0,6000))+
  theme_bw()
gun_viz_02

##----- Summary muertos----
summary(datos$n_killed)

##----- Dias más muertes ---
dia <- datos %>% group_by(date)%>%summarise(muertes = sum(n_killed))%>%select(date,muertes)
colnames(dia)[1] <- "fecha"
dia$fecha <- format(dia$fecha, "%d/%m/%Y")
head(arrange(dia,desc(muertes)),5)%>%kbl(caption = "<b> <font color='black'> Tabla 1: Días con mayor número de muertes</font> <b>")%>%
  kable_styling(position = "float_left")

## -----Dias mas incidentes ----
dia2 <- datos %>% group_by(date)%>%summarise(incidentes = n())%>%select(date,incidentes)
colnames(dia2)[1] <- "fecha"
dia2$fecha <- format(dia2$fecha, "%d/%m/%Y")
head(arrange(dia2,desc(incidentes)),5)%>%kbl(caption = "<b> <font color='black'> Tabla 2: Días con mayor número de incidentes</font> <b>")%>%
  kable_styling(position = "float_left")

##----- Genero y tipo (datos long)----
expand_participant_info <- function(column_name, df) {
  df %>%
    select(incident_id, !!sym(column_name)) %>%
    mutate(row_id = row_number()) %>%
    filter(!(is.na(!!sym(column_name)))) %>%
    separate_rows(!!sym(column_name), sep = "\\|\\|") %>%
    separate(!!sym(column_name), into = c("participant_id", column_name), sep = "::", fill = "right") %>%
    mutate(participant_id = as.integer(participant_id),
           !!sym(column_name) := ifelse(is.na(!!sym(column_name)), NA, !!sym(column_name))) %>%
    arrange(row_id, participant_id)
}

vbles <- datos %>% select(starts_with("participant_")) %>% names()
vbles_exp <- list()

vbles_exp <- lapply(vbles, function(col) expand_participant_info(col, datos))
df_long <- reduce(vbles_exp, 
                  full_join, by = c("incident_id", "participant_id", "row_id")) %>%
  select(-row_id) %>%
  arrange(incident_id, participant_id)

df_long <- df_long[!is.na(df_long$participant_id),]

df_long$participant_age <- as.numeric(df_long$participant_age)
edad_errores <- df_long %>%filter(participant_age > 150)
# Existen dos observaciones con errores de imputación en la edad
df_long$participant_age[df_long$participant_age == 209] <- 33
df_long$participant_age[df_long$participant_age == 311] <- 31

df_long$participant_gender <- as.factor(df_long$participant_gender)
levels(df_long$participant_gender)

df_long$participant_gender[df_long$participant_gender == "Male, female"] <- "Female"
df_long <- droplevels(df_long)


tabla_genero_tipo <- df_long %>%
  group_by(participant_gender, participant_type) %>%  # Agrupar por género y tipo de participante
  summarise(Count = n()) %>%  # Contar el número de ocurrencias en cada combinación de género y tipo
  pivot_wider(names_from = participant_type, values_from = Count, names_prefix = "N_")

tabla_genero_tipo <- tabla_genero_tipo[1:2,]
colnames(tabla_genero_tipo) <- c("","Sospechoso","Víctima")


tabla_genero_tipo%>%kbl(caption = "<b> <font color='black'> Tabla 3: Distribución de participantes por género y tipo</font> <b>")%>%
  kable_classic(full_width = F, html_font = "Cambria")


##-----Edad participantes (Decidir y cambiar metodología)---- 
boxplot(df_long$participant_age, main = "", ylab = "Edad")
hist(df_long$participant_age, main = "", ylab = "Frecuencia",xlab = "Edad")


##---- Correlaciones y df_wide----
detach("package:plyr", unload = TRUE)
# SOlo si hago el heat map
victimas <- df_long %>%
  filter(participant_type == "Victim") %>%
  group_by(incident_id) %>%
  summarise(mean_age_victimas = mean(participant_age,na.rm = TRUE),
            percent_male_vict = sum(participant_gender == "Male")/n() *100,
            percent_female_vict = sum(participant_gender == "Female")/n() *100,
            percent_killed_vict = sum(participant_status == "Killed")/n() *100,
            percent_injured_vict = sum(participant_status == "Injured")/n() *100,
            percent_unharmed_vict = sum(participant_status == "Unharmed")/n() *100,
            num_vict = n())


suspects <- df_long %>%
  filter(participant_type == "Subject-Suspect") %>%
  group_by(incident_id) %>%
  summarise(mean_age_suspects = mean(participant_age,na.rm = TRUE),
            percent_male_susp = sum(participant_gender == "Male")/n() *100,
            percent_female_susp = sum(participant_gender == "Female")/n() *100,
            percent_killed_susp = sum(participant_status == "Killed")/n() *100,
            percent_injured_susp = sum(participant_status == "Injured")/n() *100,
            percent_unharmed_susp = sum(participant_status == "Unharmed")/n() *100,
            num_susp = n())


datos_wide <- datos%>%left_join(victimas, by = "incident_id")%>%
  left_join(suspects,by = "incident_id")
datos_wide$num_part <- datos_wide$num_vict + datos_wide$num_susp
cor(datos_wide$n_killed, datos_wide$mean_age_suspects,use = "complete.obs")
cor(datos_wide$n_killed, datos_wide$mean_age_victimas,use = "complete.obs")
cor(datos_wide$n_killed, datos_wide$percent_male_vict,use = "complete.obs")
cor(datos_wide$n_killed, datos_wide$num_part, use = "complete.obs")
cor(datos_wide$n_killed, datos_wide$percent_female_vict, use = "complete.obs")


##------ Análisis texto------
###----- Términos frecuentes-----
set.seed(123)  
datoss <- subset(datos, ! is.na(datos$notes))
datoss <- datoss[sample(nrow(datoss), nrow(datoss)*0.25), ]

datoss$notes <- tolower(datoss$notes)
# Tokenización
corpus <- Corpus(VectorSource(datoss$notes))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("en"))

# Paso 3: Contar la frecuencia de términos
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
term_frequency <- rowSums(m)
term_frequency <- sort(term_frequency, decreasing = TRUE)


barplot(term_frequency[1:20], las=2, col="lightblue", main="Términos más frecuentes", xlab="Términos", ylab="Frecuencia")

###------- Sentimientos y polaridad-------
nrc_sp2 <- get_sentiment_dictionary(dictionary = "nrc",language = "english") %>% 
  filter(!sentiment %in% c("positive","negative")) %>% 
  distinct()
nrc_sp2$word <- tolower(nrc_sp2$word)


lista_duplicados2 <- nrc_sp2 %>% 
  count(word,sort = TRUE) %>% 
  filter(n>1) %>% 
  pull(word)

nrc_sp2 <- nrc_sp2 %>% 
  filter(!word %in% lista_duplicados2)


notas <- datos[,c(1,19)]
notas$notes <- tolower(notas$notes)


tidy_notes <- notas %>% 
  unnest_tokens(word,notes) %>% 
  tibble()


emociones <- tidy_notes %>% 
  inner_join(nrc_sp2) %>%
  group_by(incident_id,sentiment) %>% 
  summarise(total=n())

emociones2 <- emociones%>%group_by(sentiment)%>%summarise(total = n())




trust_words <- filter(nrc_sp2, sentiment == "trust") 

trust_words_in_text <- tidy_notes %>%
  semi_join(trust_words, by = "word")

trust_words_count <- trust_words_in_text %>%
  count(word, sort = TRUE)
head(trust_words_count, 5)

corpus <- corpus(na.omit(datos$notes))

custody <- kwic(corpus, "custody", valuetype = "glob", window = 3)
head(custody, 6)

related <- kwic(corpus, "related", valuetype = "glob", window = 3)
set.seed(23)
 #Realmente connotación no de confianza por las palabras anteriores (gun, drug, gang...) 
related[sample(nrow(related), 5, replace = F)]


anger_words <- filter(nrc_sp2, sentiment == "anger") 

anger_words_in_text <- tidy_notes %>%
  semi_join(anger_words, by = "word")

anger_words_count <- anger_words_in_text %>%
  count(word, sort = TRUE)
head(anger_words_count, 5)


fear_words <- filter(nrc_sp2, sentiment == "fear") 

fear_words_in_text <- tidy_notes %>%
  semi_join(fear_words, by = "word")

fear_words_count <- fear_words_in_text %>%
  count(word, sort = TRUE)
head(fear_words_count, 5)




# Análisis de polaridad

nrc_sp <- get_sentiment_dictionary(dictionary = "nrc",language = "english") %>% 
  filter(sentiment %in% c("positive","negative")) %>% 
  distinct()
nrc_sp$word <- tolower(nrc_sp$word)


lista_duplicados <- nrc_sp %>% 
  count(word,sort = TRUE) %>% 
  filter(n>1) %>% 
  pull(word)

nrc_sp <- nrc_sp %>% 
  filter(!word %in% lista_duplicados)


polaridad <- tidy_notes %>% 
  inner_join(nrc_sp) %>%
  group_by(incident_id,sentiment) %>% 
  summarise(total=n())


polaridad_notes <- polaridad%>%group_by(sentiment)%>%summarise(total = n())
polaridad_notes
