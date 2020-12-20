# Analise de dados do Airbnb


# Carregando as bibliotecas ----
library(tidyverse)
library(plotly)
library(gridExtra)
library(e1071)

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

# Preço
summary(dados$price)

# Observando o histograma do preço

preco <- ggplot(dados, aes(x = price)) +
  geom_histogram(bins = 200) +
  ggtitle("Histograma com a Frequência Simples do Preço antes do ajuste") +
  xlab("Preço") +
  ylab("Frequência Simples (Quantidade)") 

ggplotly(preco)


# Ajustando a variavel preço
dados_preco_ajustado <- dados %>%
  filter(price >= 100)

preco_ajustado <- ggplot(dados_preco_ajustado, aes(x = price)) +
  geom_histogram(bins = 200) +
  ggtitle("Histograma com a Frequência Simples do Preço depois do ajuste") +
  xlab("Preço") +
  ylab("Frequência Simples (Quantidade)") 

ggplotly(preco_ajustado)

# Colocando os histogramas lado a lado
grid.arrange(preco, preco_ajustado)


# Boxplot de preco
box_plot_preco_ajustado <- ggplot(dados_preco_ajustado, aes(x = "", y = price)) +
  geom_boxplot()

# Medidas de tendencia central
summary(dados_preco_ajustado$price)


# Medidas de dispersão
dados_preco_ajustado %>% 
  summarise(desvio_padrao = sd(price), coeficiente_var = desvio_padrao/mean(price)*100)

# Observando se há assimetria
skewness(dados_preco_ajustado$price) # skewness > 0 então positivo

# Observando o formato da curva
kurtosis(dados_preco_ajustado$price) # 0,77 < 3, entao platocurtica

# Preco vs localização



valores_por_tipo_de_quarto <- dados_preco_ajustado %>%
  group_by(room_type) %>% 
  summarise(preco_medio=mean(price), mediana_preco = median(price), desvio_padrao = sd(price), cv = (desvio_padrao/preco_medio)*100)

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

is.logical(dados_preco_ajustado$neighbourhood_cleased)


bairros_selecionados <- c("Leblon", "Ipanema", "Lagoa", "Gávea", "Jardim Botânico", "Recreio dos Bandeirantes", "Copacabana", "Freguesia (Jacarepaguá)", "Tijuca", "Leme",
                          "Humaitá", "Botafogo", "Flamengo", "São Conrado","Laranjeiras", "Cosme Velho")

dados_preco_ajustado$neighbourhood_cleansed <- dados_preco_ajustado$neighbourhood_cleansed %in% bairros_selecionados




bairros_selecionados %in% dados_preco_ajustado$neighbourhood_cleansed


