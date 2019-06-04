library("dplyr")
#%>%
library("tidyr")
#datas
library("lubridate")
library("forecast")
library("ggplot2")
library("stringr")
library("plyr")

db <- readRDS('data/gastos_cartao.RDS')

#converte string para date, cria coluna mes
# 11/2013 - 02/2019
db_convertido <- db %>%
  mutate(data_transacao = dmy(data_transacao),
         mes = month(data_transacao),
         ano = year(data_transacao)
         )

#db dilma
db_dilma <- db_convertido %>%
  select(data_transacao, valor_transacao, mes, ano) %>%
  filter(data_transacao >= as.Date("2011-01-31") & 
           data_transacao <= as.Date("2016-08-31")) %>%
  transmute(valor = valor_transacao,
            ano_mes = paste(ano, mes, sep = "/")) %>%
  arrange(ano_mes)

#db temer
db_temer <- db_convertido %>%
  select(data_transacao, valor_transacao, mes, ano) %>%
  filter(data_transacao >= as.Date("2018-12-12") & 
           data_transacao <= as.Date("2019-01-01")) %>%
  transmute(valor = valor_transacao,
            ano_mes = paste(ano, mes, sep = "/"))
  
  valor_por_ano_mes <- db_temer %>%
  group_by(ano_mes) %>%
  summarise(valor_gasto = sum(valor))

db_temer_as_factor <- as.factor(db_temer)

p <- ggplot(db_temer_as_factor, aes(x=db_temer_as_factor$, y=len)) + 
  geom_boxplot()

db_temer %>%
  ggplot(aes(y = valor)) +
  geom_boxplot()