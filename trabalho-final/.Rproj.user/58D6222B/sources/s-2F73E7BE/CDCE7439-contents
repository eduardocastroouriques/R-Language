library("dplyr")
#%>%
library("tidyr")
#datas
library("lubridate")
library("forecast")
library("ggplot2")
library("stringr")
library("plyr")
library("scales")
library("highcharter")

db <- readRDS('data/gastos_cartao.RDS')

#converte string para date, cria coluna mes
# 11/2013 - 02/2019
db_convertido <- db %>%
  mutate(data_transacao = dmy(data_transacao),
         mes = month(data_transacao),
         ano = year(data_transacao)
  )

glimpse(db_convertido)
resumo <- aggregate(db_convertido$valor_transacao, by=list(nome_unidade=db_convertido$nome_orgao_superior), FUN=sum)

resumo %>%
  hchart(
    type = "column",
    hcaes(x     = nome_unidade,
          y     = x,
          color = nome_unidade
    )
  )