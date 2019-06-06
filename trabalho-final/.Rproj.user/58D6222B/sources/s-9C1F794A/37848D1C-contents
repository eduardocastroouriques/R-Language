library("dplyr")
#%>%
library("tidyr")
#datas
library("lubridate")
library("forecast")
library("highcharter")
library("stringr")

db <- readRDS(file = 'data/gastos_cartao.RDS')  


#verifica tipo da coluna
glimpse(db)

#converte string para date, cria coluna mes
# 11/2013 - 02/2019
db_convertido <- db %>%
  mutate(data_transacao = dmy(data_transacao),
         mes = month(data_transacao),
         ano = year(data_transacao)) %>%
  arrange(ano, mes)

#db dilma
db_dilma <- db_convertido %>%
  filter(data_transacao >= as.Date("2011-1-01") & 
           data_transacao <= as.Date("2016-08-31"))

#db dilma somatorio
db_dilma_somatorio <- db_dilma %>%
  summarise(valor_gasto = sum(valor_transacao))

#db_temer
db_temer <- db_convertido %>%
  filter(data_transacao > as.Date("2016-08-31") & 
           data_transacao <= as.Date("2018-12-31"))

#db_temer somatorio
db_temer_somatorio <- db_temer %>%
  summarise(valor_gasto = sum(valor_transacao))

a <- seq(1,2,3,01,)
str_pad(a, 6, pad = "0")

#db dilma e db temer
db_dilma_e_temer <- db_convertido %>%
  filter(data_transacao >= as.Date("2011-1-01") & 
           data_transacao <= as.Date("2018-12-31")) %>%
  mutate(ano_mes = sprintf("name_%03d", paste(year(data_transacao), month(data_transacao))),
         presidente = ifelse(data_transacao <= "2016-08-31", "Dilma", "Temer"))

db_dilma_e_temer %>%
  filter(Presidente %in% c("Dilma", "Temer")) %>%
  ggplot(aes(x = ano_mes, y = valor_transacao)) +
  geom_line(aes(colour = Presidente))

glimpse(db_dilma_e_temer)

db_dilma_e_temer_somatorio <- db_dilma_e_temer %>%
  summarise(valor_gasto = sum(valor_transacao)) %>%
  arrange(ano_mes)

x <- forecast(db_dilma_e_temer_somatorio, h = 48, level = 95)
hchart(x)

db_dilma_e_temer_somatorio %>%
  hchart("line",
         name = "Desemprego",
         hcaes(x = ano_mes, y = valor_gasto),
         color = "#cc8800"
  )