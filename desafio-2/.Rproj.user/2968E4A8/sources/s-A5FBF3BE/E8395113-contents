library(dplyr)
library(datasets)

#1.2 - Filtre a base de dados para temperaturas maiores que 70 F, selecione as colunas(temp, month e day) e ordene de maior a menor.
airquality %>%
  filter(Temp > 70) %>%
  select(Temp, Month, Day) %>%
  arrange(desc(Temp, Month, Day))

#1.3 - Crie uma variável chamada temp_c que é a temperatura em celcius e guarde no data frame.
airquality %>%
  mutate(temp_c = (Temp - 32) * 5/9 )

#1.4 - Renomeie as colunas para o português.
airquality %>%
  rename(Temperatura = Temp,
         Mes = Month,
         Dia = Day,
         Vento = Wind,
         Radiacao_Solar = Solar.R,
         Ozonio = Ozone)

#1.5 - Calcule a velocidade do vento(wind) média e mediana para cada mês e conte o número de observações mensais.
airquality %>%
  select(Wind, Month) %>%
  summarise(vel_vento_medio = mean(Wind),
         mediana_vento = median(Wind)) 

#1.6 - Sorteie aleatoriamente 20 observações e calcule a temperatura média.
airquality %>%
  sample_n(20) %>%
  summarise(temp_media = mean(Temp))

#1.7 - Sorteie aleatoriamente 40% da base e calcule a temperatura mediana.
airquality %>%
  sample_frac(0.4) %>%
  summarise(temp_mediana = median(Temp))

#1.8 - Calcule a temperatura máxima, minima e a amplitude térmica para cada mês, em graus celcius.
airquality %>%
  mutate(temp_celsius = (Temp - 32) * 5/9) %>%
  group_by(Month) %>%
  summarise(temp_max = max(temp_celsius, na.rm = TRUE),
            temp_min = min(temp_celsius, na.rm = TRUE),
            amplitude_termica = temp_max - temp_min)

#2.1 - Utilize a função clean_names para modificar o nome das colunas da tabela. Posteriormente, escreva um csv que contenha apenas as colunas nome, cargo e remuneracao_basica_bruta
salary <- read.csv('data/remuneracao_poa.csv', encoding = 'latin1', dec = ',') %>%
  clean_names(.) %>%
  select(nome, cargo, remuneracao_basica_bruta) %>%
  write.csv(., file = 'data/remuneracao_poa_novas_colunas.csv')

#2.2 - Ordene a base de dados pelo salário de forma decrescente e selecione os 5 maiores salários.
salary_2 <- read.csv('data/remuneracao_poa.csv', encoding = 'latin1', dec = ',') %>%
  clean_names(.) %>%
  select(remuneracao_basica_bruta) %>%
  arrange(desc(remuneracao_basica_bruta)) 

salary_2 %>%
  top_n(n = 5)

#2.3 - Escreva a base de dados em csv contendo as colunas nome, cargo e remuneracao_basica_bruta
salary_3 <- read.csv('data/remuneracao_poa.csv', encoding = 'latin1', dec = ',') %>%
  clean_names(.) %>%
  select(nome, cargo, remuneracao_basica_bruta) %>%
  write.csv(., file = 'data/remuneracao_nome_cargo_remuneracao')

#2.4 - Qual a média salarial da prefeitura
salary_4 <- read.csv('data/remuneracao_poa.csv', encoding = 'latin1', dec = ',') %>%
  select(Remuneração.básica.bruta) %>%
  summarise(media_salarial_bruta = mean(Remuneração.básica.bruta))

#2.3 - Qual a média e mediana de salários por cargo?
salary_5 <- read.csv('data/remuneracao_poa.csv', encoding = 'latin1', dec = ',') %>%
  group_by("Cargo Referência") %>%
  summarise(media = mean(Remuneração.básica.bruta),
            mediana = median(Remuneração.básica.bruta))
salary_5