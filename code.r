# Analise de dados do Airbnb
install.packages("moments")
install.packages("moderndive")


# Carregando as bibliotecas ----
library(tidyverse)
library(plotly)
library(gridExtra)
library(e1071)
library(moments)
library(corrplot)
library(writexl)
library(moderndive)

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

# Preço-----
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




# Preço vs localização-----
valores_por_localizacao <- dados %>% 
  group_by(neighbourhood_cleansed) %>% 
  summarise(preco_medio=mean(price), mediana_preco = median(price), desvio_padrao = sd(price), cv = (desvio_padrao/preco_medio))

valores_por_localizacao


bairros_selecionados <- c("Leblon", "Ipanema", "Lagoa", "Gávea", "Jardim Botânico", "Recreio dos Bandeirantes", "Copacabana", "Freguesia (Jacarepaguá)", "Tijuca", "Leme",
                          "Humaitá", "Botafogo", "Flamengo", "São Conrado","Laranjeiras", "Cosme Velho", "Santa Tereza", "Centro", "Camorim", "Catete", "Maracanã", "Urca"
)

dados_preco_ajustado$bairros_selecionados <- dados_preco_ajustado$neighbourhood_cleansed %in% bairros_selecionados


(medidas_agrup_localizacao <- dados_preco_ajustado %>%
    group_by(bairros_selecionados) %>% 
    summarise(media = mean(price), mediana = median(price), desvio_padrao = sd(price),
              cv = desvio_padrao/media))

(medidas_agrup_localizacao <- dados_preco_ajustado %>%
    group_by(bairros_selecionados) %>% 
    summarize(media = mean(price), mediana = median(price), desvio_padrao = sd(price),
              cv = desvio_padrao/media, minimo = min(price), maximo = max(price)))

# Obtendo os quartis agrupando ----
p <- c(0.25,0.50,0.75)
p_names <- map_chr(p, ~paste0(.x*100, "%"))

p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% 
  set_names(nm = p_names)

p_funs

# ----
quartis_preco_agrp_bairro <- dados_preco_ajustado %>%
  group_by(bairros_selecionados) %>% 
  summarize_at(vars(price), funs(!!!p_funs))




ggplot(dados_preco_ajustado, aes(x = bairros_selecionados, y = price, fill = bairros_selecionados)) +
  geom_boxplot() +
  labs(fill = "É um bairro selecionado?") +
  ggtitle("Boxplot de preço agrupado por bairros selecionados")
  ylab("Preço") +
  xlab("Bairros Selecionados")
  

dados_preco_ajustado %>%
  group_by(bairros_selecionados) %>%
  summarize(skew = skewness(price), kurtosis = kurtosis(price))
  

# Preco vs tipo de propriedade-----

valores_por_tipo_de_quarto <- dados_preco_ajustado %>%
  group_by(room_type) %>% 
  summarise(minimo = min(price),preco_medio=mean(price), mediana_preco = median(price), maximo = max(price),  desvio_padrao = sd(price), cv = (desvio_padrao/preco_medio)*100)

valores_por_tipo_de_quarto



ggplot(dados, aes( x = room_type, y = price, fill = room_type)) +
  geom_boxplot() +
  labs(fill = "Tipo de quarto") +
  xlab("Tipo de quarto") +
  ylab("Preço")
  

ggplot(dados, aes( x = price)) +
  geom_histogram() +
  facet_wrap(~room_type)


quartis_prec_agrp_tipo_quarto <- dados_preco_ajustado %>%
  group_by(room_type) %>%
  summarize_at(vars(price), funs(!!!p_funs))

quartis_prec_agrp_tipo_quarto



# Calculando skewness e kurtosis do preço agrupado por tipo de quarto
dados_preco_ajustado %>% 
  group_by(room_type) %>%
  summarise(skew = skewness(price), kurtosis = kurtosis(price))


skewness(dados_preco_ajustado$room_type)


# Numero de reviews-----

#Substituindo os NAs por 0
dados_preco_ajustado$reviews_per_month[is.na(dados_preco_ajustado$reviews_per_month)] <- 0

#Pegando as medidas resumos
dados_preco_ajustado %>%
  summarize(media = mean(reviews_per_month), mediana = median(reviews_per_month),
            max = max(reviews_per_month), min = min(reviews_per_month),
            desvio_padrao = sd(reviews_per_month), cv = desvio_padrao/media * 100)

summary(dados_preco_ajustado$reviews_per_month)


# Curtose e assimetria
skewness(dados_preco_ajustado$reviews_per_month)
kurtosis(dados_preco_ajustado$reviews_per_month)




###

ggplot(dados_preco_ajustado, aes(x = reviews_per_month, y = price)) +
  geom_point()
# Pela lógica deveria haver uma correlação positiva, mas parece que o data frame
# é contaminado por muitos valores 0. Sera que nao tem importancia?

###

ggplot(dados_preco_ajustado, aes(x = reviews_per_month, y = price, color = bairros_selecionados)) +
  geom_point()


# reviews agrupado por room_type
dados_preco_ajustado %>%
  group_by(room_type) %>% 
  summarize(media = mean(reviews_per_month), mediana = median(reviews_per_month),
            max = max(reviews_per_month), min = min(reviews_per_month),
            desvio_padrao = sd(reviews_per_month), cv = desvio_padrao/media * 100)

quartis_preco_agrp_review_tipo_de_quarto <- dados_preco_ajustado %>%
  group_by(room_type) %>% 
  summarize_at(vars(reviews_per_month), funs(!!!p_funs))


# reviews agrupado por localização
dados_preco_ajustado %>%
  group_by(bairros_selecionados) %>% 
  summarize(media = mean(reviews_per_month), mediana = median(reviews_per_month),
            max = max(reviews_per_month), min = min(reviews_per_month),
            desvio_padrao = sd(reviews_per_month), cv = desvio_padrao/media * 100)

quartis_preco_agrp_review_tipo_de_bairros_selecionados <- dados_preco_ajustado %>%
  group_by(bairros_selecionados) %>% 
  summarize_at(vars(reviews_per_month), funs(!!!p_funs))


reviews_puro <- ggplot(dados_preco_ajustado, aes(x = reviews_per_month)) +
  geom_freqpoly() +
  ggtitle("Frequência de poligonos da quantidade de reviews por mês") +
  xlab("Quantiade de reviews por mês") +
  ylab("Contagem")

reviews_localizacao <- ggplot(dados_preco_ajustado, aes(x = reviews_per_month, color = bairros_selecionados)) +
  geom_freqpoly() +
  ggtitle("Frequência de poligonos da quantidade de reviews por mês agrupado por bairros selecionados") +
  xlab("Quantiade de reviews por mês") +
  ylab("Contagem")


reviews_room <- ggplot(dados_preco_ajustado, aes(x = reviews_per_month, color = room_type)) +
  geom_freqpoly() +
  ggtitle("Frequência de poligonos da quantidade de reviews por mês agrupado por tipo de quarto") +
  xlab("Quantiade de reviews por mês") +
  ylab("Contagem") +
  labs(fill = "Tipo de quarto")

grid.arrange(reviews_puro, reviews_localizacao, reviews_room)

# Mínimo de noites------

dados_preco_ajustado %>%
  summarize(media = mean(minimum_nights), mediana = median(minimum_nights),
            min = min(minimum_nights), max= max(minimum_nights),
            desvio_padrao = sd(minimum_nights), cv = desvio_padrao/media * 100)

summary(dados_preco_ajustado$minimum_nights)
table(dados_preco_ajustado$minimum_nights)

skewness(dados_preco_ajustado$minimum_nights)
kurtosis(dados_preco_ajustado$minimum_nights)


minimo_noite <- ggplot(dados_preco_ajustado, aes(x = minimum_nights)) +
  geom_freqpoly() +
  xlab("Mínimo de noites") +
  ylab("Contagem") +
  ggtitle("Frequência de polígonos do mínimo de noites")


minimo_bairro <- ggplot(dados_preco_ajustado, aes(x = minimum_nights, colour = bairros_selecionados)) +
  geom_freqpoly() +
  xlab("Mínimo de noites") +
  ylab("Contagem") +
  ggtitle("Frequência de polígonos do mínimo de noites agrupado por bairro selecionados")


minimo_room <- ggplot(dados_preco_ajustado, aes(x = minimum_nights, colour = room_type)) +
  geom_freqpoly(bins  = 15) +
  xlab("Mínimo de noites") +
  ylab("Contagem") +
  ggtitle("Frequência de polígonos do mínimo de noites agrupado por tipo de quarto") 


agrup_graf_noite <- grid.arrange(minimo_noite, minimo_bairro, minimo_room)
ggplotly(agrup_graf_noite)

ggplot(dados_preco_ajustado, aes(x = minimum_nights, y = price)) +
  geom_jitter() +
  xlab("Mínimo de noites") +
  ylab("Preço")


cor(dados_preco_ajustado$price, dados_preco_ajustado$minimum_nights)

# Disponibilidade em 365----

# Criando uma variavel em %, talvez faça mais sentido trabalhar com ela visto que o max
# que o Airbnb considera so 365 dias.

# Resumindo os dados
dados_preco_ajustado %>% 
  summarize(media = mean(availability_365), mediana = median(availability_365),
            min = min(availability_365), max = max(availability_365),
            desvio_padrao = sd(availability_365), cv = desvio_padrao/media * 100)

summary(dados_preco_ajustado$availability_365)


# assimetria e curtose
skewness(dados_preco_ajustado$availability_365)
e1071::skewness(dados_preco_ajustado$availability_365)
kurtosis(dados_preco_ajustado$availability_365)




dados_preco_ajustado <- dados_preco_ajustado %>%
  mutate(availability_365_perc = availability_365/365 * 100)

ggplot(dados_preco_ajustado, aes(x = availability_365)) +
  geom_histogram()


# Observando a dispersão
ggplot(dados_preco_ajustado, aes(x = availability_365, y = price)) +
  geom_point()

cor(dados_preco_ajustado$availability_365, dados_preco_ajustado$price)


vetor_correlacao <- dados_preco_ajustado %>%
  select(price, availability_365, minimum_nights) %>%
  rename(Preço = price, Disponibilidade_365 = availability_365, Mínimo_de_noites = minimum_nights)

cor_vetor <- cor(vetor_correlacao)

corrplot(cor_vetor)

corrplot(
  cor_vetor,
  method = 'color',
  cl.pos = 'b',
  type = 'lower',
  addgrid.col = 'white',
  addCoef.col = 'black',
  tl.col = 'black',
  tl.cex = 0.7,
  number.cex = 0.7,
  cl.cex = 0.7
)


# Criando arquivo xlsx para ler no gtrel
write_xlsx(dados_preco_ajustado, "dados.xlsx")


# Modelos
# Modelo price vs room type
modelo_simples_room <- lm(price~room_type, dados_preco_ajustado)

summary(modelo_simples_room)

get_regression_table(modelo_simples_room)

# Modelo price vs location
modelo_simples_localizacao <- lm(price~bairros_selecionados, dados_preco_ajustado)

summary(modelo_simples_localizacao)

get_regression_table(modelo_simples_localizacao)

# Modelo price vs reviews
modelo_simples_reviews <- lm(price~availability_365_perc, dados_preco_ajustado)

summary(modelo_simples_reviews)

get_regression_table(modelo_simples_reviews)

# Modelo price vs minimum nights
modelo_simples_noites <- lm(price~minimum_nights, dados_preco_ajustado)

summary(modelo_simples_noites)

get_regression_table(modelo_simples_noites)
