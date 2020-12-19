# Analise de dados do Airbnb


# Carregando as bibliotecas ----
library(tidyverse)


setwd("F:/Ciência de Dados/Comunidade_de_estatistica/Marketing Analytics/projeto_final/marketing-analytics-estatidados")
getwd()


# Carregando os dados ----
dados <-read_csv2("base_de_dados_airbnb.csv")

dados <- dados %>%
  select(id, host_id, host_name, host_since, host_response_rate, host_response_time, 
         host_is_superhost, host_listings_count, host_has_profile_pic, host_identity_verified,
         neighbourhood_cleansed, property_type, room_type, accommodates, bathrooms_text, bedrooms, beds, 
         price, minimum_nights, maximum_nights, availability_30, availability_60, availability_90,
         availability_365,number_of_reviews, review_scores_rating, review_scores_accuracy, 
         review_scores_cleanliness, review_scores_checkin, review_scores_communication, 
         review_scores_location, review_scores_value, instant_bookable, calculated_host_listings_count,
         reviews_per_month)

head(dados)

# Analisando as variáveis ----

valores_por_tipo_de_quarto <- dados %>%
  group_by(room_type) %>% 
  summarise(preco_medio=mean(price), mediana_preco = median(price), desvio_padrao = sd(price), cv = (desvio_padrao/preco_medio))

valores_por_tipo_de_quarto



ggplot(dados, aes( x = room_type, y = price, fill = room_type)) +
  geom_boxplot() +
  labs(fill = "Tipo de quarto")
  

ggplot(dados, aes( x = price)) +
  geom_histogram() +
  facet_wrap(~room_type)

valores_por_localizacao <- dados %>% 
  group_by(neighbourhood_cleansed) %>% 
  summarise(preco_medio=mean(price), mediana_preco = median(price), desvio_padrao = sd(price), cv = (desvio_padrao/preco_medio))

valores_por_localizacao

