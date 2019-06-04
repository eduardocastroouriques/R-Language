library("dplyr")
#%>%
library("tidyr")
#datas
library("lubridate")
library("forecast")
library("ggplot2")
library("stringr")
library("plyr")
library("highcharter")
library("magrittr")

db <- readRDS('data/gastos_cartao.RDS')

db <- db %>%
  mutate(data_transacao = dmy(data_transacao))

periodo_dilma <- db %>%
  filter(data_transacao >= as.Date("2011-1-01") &
           data_transacao <= as.Date("2016-8-31"))


nome_orgao_x_contagem <- periodo_dilma %>% 
  select(nome_orgao_superior, valor_transacao, data_transacao) %>%
  filter(data_transacao >= as.Date("2011-1-01") &
           data_transacao <= as.Date("2016-8-31")) %>%
  group_by(nome_orgao_superior) %>%
  dplyr::summarise(count = n())

nome_orgao_x_contagem <- nome_orgao_x_contagem

nome_orgao_x_valor_gasto <- aggregate(periodo_dilma$valor_transacao, 
                                      by=list(nome_orgao_superior = periodo_dilma$nome_orgao_superior), 
                                      FUN=sum)


resumo <- merge(x = nome_orgao_x_contagem, y = nome_orgao_x_valor_gasto, by = "nome_orgao_superior", all = TRUE)
resumo <- resumo %>%
  mutate(percentual = count/sum(x))

resumo <- resumo %>%
  arrange(desc(x)) %>%
  mutate(index = seq.int(nrow(.)))


ggplot(resumo, aes(x =reorder(nome_orgao_superior, -x), y = x, fill = x)) +
  geom_bar(stat="identity") +
    scale_y_log10() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  xlab("Órgão Superior") + ylab("Valor Gasto (LOGx10): ") +
  theme(legend.position="none")







