library(tidyverse)
library(dplyr)
library(here)
library(writexl)
library(readxl)
library(tokenizers)
library(ggplot2)
library(rio)
library(quaestplots)
library(openxlsx)
library(lubridate)

# Definição Dicionário Geral de filtragem
dicio <- c("https", "para", "mais", "pelo","foram", "pela","entre",
           "como", "feira", "link", "segue", "hoje", "também", "todo", "toda",
           "todos", "todas", "serão", "será", "aquele", "aquela",
           "está", "estão", "sobre", "muito", "isso", "essa", "esse",
           "quem", "aqui", "anos", "minha", "pode", "porque", "pois", "quando", "antes", "depois", 
           "contra", "desde", "como", "depois", "foto", "agora", "fazer", "sempre",
           "muito", "muita", "muitos", "muitas", "amanhã", "hoje", "ontem", "semana",
           "eles", "elas", "coisa", "tipo", "quer", "cada", "outro", "tudo", "nada",
           "meus", "mesmo", "nesse", "nessa", "nisso", "ninguém", "dias", "fotos", "gente",
           "milhões", "bilhão", "milhão", "bilhões", "dezena", "dezenas", "centena",
           "centenas", "nesta", "neste", "nisto", "qual", "quais", "qualquer", "quaisquer",
           "seus", "suas", "teus", "tuas", "esta", "este", "estas", "estes", "isto",
           "nosso", "nossa", "você", "vocês", "vamos", "ainda", "logo", "onde", "aonde",
           "sabe", "nunca", "sempre", "novo", "novos", "nova", "novas", "além", "partir")

##### Análise Leite #############################################################
# Abrir posts totais
bd_leite <- read_xlsx(here("C:/Users/artur/OneDrive/Área de Trabalho/Quaest/Análise - Top Palavras/0.dados/govs/leite/leite.xlsx"))

# Alterar modelo de data para eliminar a hora
bd_leite$Date <- as.Date(bd_leite$Date)

# Filtrar datas das prévias
bd_leite_prev <- bd_leite %>%
  select(Date, Message) %>% 
  filter(Date >= "2021-09-20", Date <= "2021-11-27")

# Padronizar posts
bd_leite_prev$Message <- str_to_lower(bd_leite_prev$Message)

# Criando a lista com as palavras
palavras_leite <- strsplit(bd_leite_prev$Message, "\\W+")
palavras_leite <- unlist(palavras_leite)

# Contando a frequência de cada palavra usando o pacote "tokenizers"
bd_palavras_leite <- as.data.frame(tokenize_words(palavras_leite, lowercase = TRUE) %>%
                                     unlist() %>%
                                     table() %>%
                                     sort(decreasing = TRUE))

# Alterando nome das colunas
colnames(bd_palavras_leite) <- c("Palavras", "Freq")

# Filtrando palavras com mais de três letras, exceto PT
bd_palavras_leite <- subset(bd_palavras_leite, nchar(as.character(Palavras)) > 3 | 
                              Palavras == "rio" |
                              Palavras == "sul")

# Filtrando linhas que possuem um número em vez de uma palavra
bd_palavras_leite <- bd_palavras_leite %>%
  filter(!(str_detect(Palavras, "\\d")))

# Removendo palavras e hashtags inúteis manualmente
# Lista geral:
bd_palavras_leite <- filter(bd_palavras_leite, !(Palavras %in% dicio))

# Lista específica:
bd_palavras_leite <- filter(bd_palavras_leite, !(Palavras %in% c(
  "estamos", "temos", "precisamos",
  "governo_rs"
)))

# Filtrando palavras que foram usadas mais de 1 vez para termos uma lista
# dos termos mais importantes e criando ID
bd_palavras_leite <- bd_palavras_leite %>% 
                      filter(Freq > 1) %>% 
                      mutate(Cand = "Eduardo Leite")

# Definindo top 15
leite <- bd_palavras_leite %>% 
            select(Palavras, Freq) %>% 
            head(n = 15)

##### Análise Doria ########################################################
# Abrir posts
bd_doria <- read_xlsx(here("C:/Users/artur/OneDrive/Área de Trabalho/Quaest/Análise - Top Palavras/0.dados/govs/doria/doria.xlsx"))

# Alterar modelo de data para eliminar a hora
bd_doria$Date <- as.Date(bd_doria$Date)

# Filtrar datas das prévias
bd_doria_prev <- bd_doria %>%
  select(Date, Message) %>% 
  filter(Date >= "2021-09-20", Date <= "2021-11-27")


# Padronizar posts
bd_doria_prev$Message <- str_to_lower(bd_doria_prev$Message)

# Criando a lista com as palavras
palavras_doria <- strsplit(bd_doria_prev$Message, "\\W+")
palavras_doria <- unlist(palavras_doria)

# Contando a frequência de cada palavra usando o pacote "tokenizers"
bd_palavras_doria <- as.data.frame(tokenize_words(palavras_doria, lowercase = TRUE) %>%
                                     unlist() %>%
                                     table() %>%
                                     sort(decreasing = TRUE))

# Alterando nome das colunas
colnames(bd_palavras_doria) <- c("Palavras", "Freq")

# Filtrando palavras com mais de três letras, exceto PT
bd_palavras_doria <- subset(bd_palavras_doria, nchar(as.character(Palavras)) > 3)

# Filtrando linhas que possuem um número em vez de uma palavra
bd_palavras_doria <- bd_palavras_doria %>%
  filter(!(str_detect(Palavras, "\\d")))

# Removendo palavras e hashtags inúteis manualmente
# Lista geral:
bd_palavras_doria <- filter(bd_palavras_doria, !(Palavras %in% dicio))

# Lista específica:
bd_palavras_doria <- filter(bd_palavras_doria, !(Palavras %in% c(
  "vacinajá", "governosp", "estamos", "temos", "meio", "grande"
)))

# Filtrando palavras que foram usadas mais de 1 vez para termos uma lista
# dos termos mais importantes e criando ID
bd_palavras_doria <- bd_palavras_doria %>% 
                        filter(Freq > 1) %>% 
                        mutate(Cand = "João Doria")

# Definindo Top15
doria <- bd_palavras_doria %>% 
      select(Palavras, Freq) %>% 
      head(bd_palavras_doria, n = 15)

# Exporta
# Palavras totais durante prévias
lista_palavras <- list("João Doria" = bd_palavras_doria,
                       "Eduardo Leite" = bd_palavras_leite)
write.xlsx(lista_palavras, file = "palavras_previas.xlsx")

####### PLOTS ##################################################################
####### Grid ambos top-15 durante Prévias ########

# Função plot
plot_grid <- function(cand, titulo) {
  ggplot(cand) +
    aes(x = reorder(Palavras, Freq), y = Freq) +
    geom_bar(stat = "identity", fill = "#10ACB8", width = 0.9) +
    geom_text(aes(label=Freq), color = "white", vjust = 0.4, hjust = 1.3, size = 3) +
    ggtitle(titulo) +
    xlab("Palavras \n") +
    ylab("Frequência") +
    coord_flip() +
    tema_quaest_web() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 10, hjust = 1),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(face = "bold", size = 18, vjust = 0.7, hjust = 0.06),
          plot.caption = element_text(size = 10, face = "bold"))
}

# Plot ambos
plot_leite <- plot_grid(leite, "Eduardo Leite ")
plot_doria <- plot_grid(doria, "João Doria ")

# Grid
library(gridExtra)
library(grid)
grid_previas <- grid.arrange(plot_leite, plot_doria, ncol = 1)

# Exporta Grid
png("grid_top15_doriaXleite.png", width = 600, height = 500)
grid_previas
dev.off()

# Removendo...
rm(bd_doria, bd_leite, plot_doria, plot_leite, dicio, palavras_doria, palavras_leite, plot_grid)

###### Top 15 ambos no mesmo gráfico ############
# Top 15 com ID
top_doria_id <- head(bd_palavras_doria, n = 15)
top_leite_id <- head(bd_palavras_leite, n = 15)

# Juntando
agregado_top15 <- rbind(top_doria_id, top_leite_id)

# Plot Agregado Prévias
plot_previas <- ggplot(agregado_top15) +
  aes(x = reorder(Palavras, Freq), y = Freq, fill = Cand) +
  geom_col(position = position_dodge(0.9)) +
  scale_fill_manual(values = c("Eduardo Leite" = "darkblue", "João Doria" = "darkgreen")) +
  geom_text(aes(label=Freq), position = position_dodge(width = 0.9), color = "white",
            hjust = 1.4, vjust = 0.25, size = 2.5) +
  labs(title = "Top-15 palavras mais utilizadas pelos candidatos durante as Prévias do PSDB",
       subtitle = "Período analisado: 20/09/2021 a 27/11/2021\n",
       caption = "\n Fonte: Quaest Consultoria \n",
       y = "Frequência",
       fill = "Candidatos") + 
  coord_flip() +
  tema_quaest_web() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size = 11, hjust = 1, face = "bold"),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        plot.title = element_text(face = "bold", size = 14),
        plot.caption = element_text(size = 10, face = "bold"),
        legend.title = element_text(face = "bold"))

# Exporta
png("top_agregado_previas.png", width = 750, height = 600)
plot_previas
dev.off()

# Removendo...
rm(plot_previas, bd_doria_prev, bd_leite_prev)

# Plot evolução da palavra "Brasil" em ambos

# Abrir posts totais
bd_leite <- read_xlsx("leite/leite.xlsx")

# Alterar modelo de data para eliminar a hora
bd_leite$Date <- as.Date(bd_leite$Date)

# Selecionando Data e Tweet
bd_leite <- bd_leite %>% 
              select(Date, Message)

# Padronizando posts
bd_leite$Message <- str_to_lower(bd_leite$Message)

# Filtrando para apenas posts que contenham "brasil" e contando as datas 
bd_leite <- bd_leite %>% 
              filter(grepl("prévias|brasil|psdb", Message)) %>% 
              arrange(Date) %>% 
              count(Date)

# Plot
plot_brasil_leite <- ggplot(bd_leite) +
    aes(Date, n) +
    geom_line() +
    labs(title = "Eduardo Leite",
         subtitle = "Evolução dos tweets que possuem os termos 'prévias', 'psdb' ou 'brasil'")

# Abrir posts totais
bd_doria <- read_xlsx("doria/doria.xlsx")

# Alterar modelo de data para eliminar a hora
bd_doria$Date <- as.Date(bd_doria$Date)

# Selecionando Data e Tweet
bd_doria <- bd_doria %>% 
  select(Date, Message)

# Padronizando posts
bd_doria$Message <- str_to_lower(bd_doria$Message)

# Filtrando para apenas posts que contenham "brasil" e contando as datas 
bd_doria <- bd_doria %>% 
  filter(grepl("prévias|brasil|psdb", Message)) %>% 
  arrange(Date) %>% 
  count(Date)

# Plot
plot_brasil_doria <- ggplot(bd_doria) +
  aes(Date, n) +
  geom_line() +
  labs(title = "João Doria",
       subtitle = "Evolução dos tweets que possuem os termos 'prévias', 'psdb' ou 'brasil'")

plot_brasil_agregado <- grid.arrange(plot_brasil_doria, plot_brasil_leite, ncol = 1)

# Export
png("plot_brasil_previas.png", width = 700, height = 400)
plot_brasil_agregado
dev.off()
