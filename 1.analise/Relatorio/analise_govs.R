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
# Abrir posts
bd_leite <- read_xlsx(here("0.dados/govs/leite/leite.xlsx"))

# Padronizar posts
bd_leite$Message <- str_to_lower(bd_leite$Message)

# Criando a lista com as palavras
palavras_leite <- strsplit(bd_leite$Message, "\\W+")
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
# dos termos mais importantes
bd_palavras_leite <- filter(bd_palavras_leite, (Freq > 1))

# Exportando
write_xlsx(bd_palavras_leite, here("outputs", "palavras_leite.xlsx"))

# Remover análise leite
rm(bd_leite, palavras_leite)

##### Análise Zema ########################################################
# Abrir posts
bd_zema <- read_xlsx(here("0.dados/govs/zema/zema.xlsx"))

# Padronizar posts
bd_zema$Message <- str_to_lower(bd_zema$Message)

# Criando a lista com as palavras
palavras_zema <- strsplit(bd_zema$Message, "\\W+")
palavras_zema <- unlist(palavras_zema)

# Contando a frequência de cada palavra usando o pacote "tokenizers"
bd_palavras_zema <- as.data.frame(tokenize_words(palavras_zema, lowercase = TRUE) %>%
                                    unlist() %>%
                                    table() %>%
                                    sort(decreasing = TRUE))

# Alterando nome das colunas
colnames(bd_palavras_zema) <- c("Palavras", "Freq")

# Filtrando palavras com mais de três letras, exceto PT e PL
bd_palavras_zema <- subset(bd_palavras_zema, nchar(as.character(Palavras)) > 3)

# Filtrando linhas que possuem um número em vez de uma palavra
bd_palavras_zema <- bd_palavras_zema %>%
  filter(!(str_detect(Palavras, "\\d")))

# Removendo palavras e hashtags inúteis manualmente
# Lista geral:
  bd_palavras_zema <- filter(bd_palavras_zema, !(Palavras %in% dicio))

# Lista específica:
  bd_palavras_zema <- filter(bd_palavras_zema, !(Palavras %in% c(
    "maior", "estamos", "uixxzjnkuk", "vacinaminas", "estou", "temas"
  )))
  
# Filtrando palavras que foram usadas mais de 1 vez para termos uma lista
# dos termos mais importantes
bd_palavras_zema <- filter(bd_palavras_zema, (Freq > 1))

# Exportando
write_xlsx(bd_palavras_zema, here("outputs", "palavras_zema.xlsx"))

# Remover análise zema
rm(bd_zema, palavras_zema)

##### Análise Dino ########################################################
# Abrir posts
bd_dino <- read_xlsx(here("0.dados/govs/dino/dino.xlsx"))

# Padronizar posts
bd_dino$Message <- str_to_lower(bd_dino$Message)

# Criando a lista com as palavras
palavras_dino <- strsplit(bd_dino$Message, "\\W+")
palavras_dino <- unlist(palavras_dino)

# Contando a frequência de cada palavra usando o pacote "tokenizers"
bd_palavras_dino <- as.data.frame(tokenize_words(palavras_dino, lowercase = TRUE) %>%
                                         unlist() %>%
                                         table() %>%
                                         sort(decreasing = TRUE))

# Alterando nome das colunas
colnames(bd_palavras_dino) <- c("Palavras", "Freq")

# Filtrando palavras com mais de três letras, exceto PT
bd_palavras_dino <- subset(bd_palavras_dino, nchar(as.character(Palavras)) > 3 | Palavras == "pt")

# Filtrando linhas que possuem um número em vez de uma palavra
bd_palavras_dino <- bd_palavras_dino %>%
  filter(!(str_detect(Palavras, "\\d")))

# Removendo palavras e hashtags inúteis manualmente
# Lista geral:
bd_palavras_dino <- filter(bd_palavras_dino, !(Palavras %in% dicio))

# Lista específica:
bd_palavras_dino <- filter(bd_palavras_dino, !(Palavras %in% c(
  "governoma", "convido", "estamos", "agradeço", "sexta", "sábado", "acompanhem"
)))

# Filtrando palavras que foram usadas mais de 1 vez para termos uma lista
# dos termos mais importantes
bd_palavras_dino <- filter(bd_palavras_dino, (Freq > 1))

# Exportando
write_xlsx(bd_palavras_dino, here("outputs", "palavras_dino.xlsx"))

# Remover análise dino
rm(bd_dino, palavras_dino)

##### Análise Cláudio Castro ########################################################
# Abrir posts
bd_claudio <- read_xlsx(here("0.dados/govs/claudio/claudio.xlsx"))

# Padronizar posts
bd_claudio$Message <- str_to_lower(bd_claudio$Message)

# Criando a lista com as palavras
palavras_claudio <- strsplit(bd_claudio$Message, "\\W+")
palavras_claudio <- unlist(palavras_claudio)

# Contando a frequência de cada palavra usando o pacote "tokenizers"
bd_palavras_claudio <- as.data.frame(tokenize_words(palavras_claudio, lowercase = TRUE) %>%
                                    unlist() %>%
                                    table() %>%
                                    sort(decreasing = TRUE))

# Alterando nome das colunas
colnames(bd_palavras_claudio) <- c("Palavras", "Freq")

# Filtrando palavras com mais de três letras, exceto PT e PDT
bd_palavras_claudio <- subset(bd_palavras_claudio, nchar(as.character(Palavras)) > 3 | 
                             Palavras == "rio")

# Filtrando linhas que possuem um número em vez de uma palavra
bd_palavras_claudio <- bd_palavras_claudio %>%
  filter(!(str_detect(Palavras, "\\d")))

# Removendo palavras e hashtags inúteis manualmente
# Lista geral:
bd_palavras_claudio <- filter(bd_palavras_claudio, !(Palavras %in% dicio))

# Lista Específica:
bd_palavras_claudio <- filter(bd_palavras_claudio, !(Palavras %in% c(
  "semtempoaperder", "estamos", "pcerj", "pmerj", "agradeço", "temos"
)))

# Filtrando palavras que foram usadas mais de 1 vez para termos uma lista
# dos termos mais importantes
bd_palavras_claudio <- filter(bd_palavras_claudio, (Freq > 1))

# Exportando
write_xlsx(bd_palavras_claudio, here("outputs", "palavras_claudio.xlsx"))

# Remover análise claudio
rm(bd_claudio, palavras_claudio)

##### Análise Doria ########################################################
# Abrir posts
bd_doria <- read_xlsx(here("0.dados/govs/doria/doria.xlsx"))

# Padronizar posts
bd_doria$Message <- str_to_lower(bd_doria$Message)

# Criando a lista com as palavras
palavras_doria <- strsplit(bd_doria$Message, "\\W+")
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
"vacinajá", "governosp"
  )))
# Filtrando palavras que foram usadas mais de 1 vez para termos uma lista
# dos termos mais importantes
bd_palavras_doria <- filter(bd_palavras_doria, (Freq > 1))

# Exportando
write_xlsx(bd_palavras_doria, here("outputs", "palavras_doria.xlsx"))

# Remover análise doria
rm(bd_doria, palavras_doria)

##### Análise Camilo Santana ########################################################
# Abrir posts
bd_camilo <- read_xlsx(here("0.dados/govs/camilo/camilo.xlsx"))

# Padronizar posts
bd_camilo$Message <- str_to_lower(bd_camilo$Message)

# Criando a lista com as palavras
palavras_camilo <- strsplit(bd_camilo$Message, "\\W+")
palavras_camilo <- unlist(palavras_camilo)

# Contando a frequência de cada palavra usando o pacote "tokenizers"
bd_palavras_camilo <- as.data.frame(tokenize_words(palavras_camilo, lowercase = TRUE) %>%
                                     unlist() %>%
                                     table() %>%
                                     sort(decreasing = TRUE))

# Alterando nome das colunas
colnames(bd_palavras_camilo) <- c("Palavras", "Freq")

# Filtrando palavras com mais de três letras, exceto PT
bd_palavras_camilo <- subset(bd_palavras_camilo, nchar(as.character(Palavras)) > 3 |
                               Palavras == "pt")

# Filtrando linhas que possuem um número em vez de uma palavra
bd_palavras_camilo <- bd_palavras_camilo %>%
  filter(!(str_detect(Palavras, "\\d")))

# Removendo palavras e hashtags inúteis manualmente
# Lista geral:
bd_palavras_camilo <- filter(bd_palavras_camilo, !(Palavras %in% dicio))

# Lista específica:
bd_palavras_camilo <- filter(bd_palavras_camilo, !(Palavras %in% c(
  "cont", "temos", "pouco", "muito", "sendo"
)))

# Filtrando palavras que foram usadas mais de 1 vez para termos uma lista
# dos termos mais importantes
bd_palavras_camilo <- filter(bd_palavras_camilo, (Freq > 1))

# Exportando
write_xlsx(bd_palavras_camilo, here("outputs", "palavras_camilo.xlsx"))

# Remover análise camilo
rm(bd_camilo, palavras_camilo)

# Exportar excel consolidado
lista_palavras <- list("Camilo" = bd_palavras_camilo,
                       "Claudio Castro" = bd_palavras_claudio,
                       "Flávio Dino" = bd_palavras_dino,
                       "João Doria" = bd_palavras_doria,
                       "Eduardo Leite" = bd_palavras_leite,
                       "Romeu Zema" = bd_palavras_zema)
write.xlsx(lista_palavras, file = "palavras_agregado.xlsx")

# Remove tudo
rm(list = ls())

########## PLOTS #######################################
# Abrindo resultado geral
bd <- import_list("palavras_agregado.xlsx")

# Abrindo top 10 de cada cand
camilo <- head(bd$Camilo, n = 10)
claudio <- head(bd$`Claudio Castro`, n = 10)
dino <- head(bd$`Flávio Dino`, n = 10)
leite <- head(bd$`Eduardo Leite`, n = 10)
doria <- head(bd$`João Doria`, n = 10)
zema <- head(bd$`Romeu Zema`, n = 10)

# Criação da Função de Plot
plot_cand <- function(cand, titulo) {
  ggplot(cand) +
    aes(x = reorder(Palavras, Freq), y = Freq) +
    geom_bar(stat = "identity", fill = "#10ACB8", width = 0.8) +
    geom_text(aes(label=Freq), color = "white", vjust = 0.4, hjust = 1.3, size = 4.5) +
    ggtitle(titulo) +
    xlab("Palavras \n") +
    ylab("Frequência") +
    labs(caption = "Fonte: Quaest Consultoria \n Elaborado por: Artur Almeida") + 
    coord_flip() +
    tema_quaest() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 13, hjust = 1),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(face = "bold", size = 16, vjust = -0.8),
          plot.caption = element_text(size = 10, face = "bold"))
}

# Aplicando função para cada Cand
plot_camilo <- plot_cand(camilo, "Top-10 palavras mais utilizadas por Camilo Santana no ano de 2021 \n")
plot_claudio <- plot_cand(claudio, "Top-10 palavras mais utilizadas por Cláudio Castro no ano de 2021 \n")
plot_dino <- plot_cand(dino, "Top-10 palavras mais utilizadas por Fávio Dino no ano de 2021 \n")
plot_leite <- plot_cand(leite, "Top-10 palavras mais utilizadas por Eduardo Leite no ano de 2021 \n")
plot_doria <- plot_cand(doria, "Top-10 palavras mais utilizadas por João Doria no ano de 2021 \n")
plot_zema <- plot_cand(zema, "Top-10 palavras mais utilizadas por Romeu Zema no ano de 2021 \n")

# Criação Grid
library(gridExtra)
library(grid)

# Função de gráficos para grid
plot_grid <- function(cand, titulo) {
  ggplot(cand) +
    aes(x = reorder(Palavras, Freq), y = Freq) +
    geom_bar(stat = "identity", fill = "#10ACB8", width = 0.9) +
    geom_text(aes(label=Freq), color = "white", vjust = 0.4, hjust = 1.3, size = 3.8) +
    ggtitle(titulo) +
    xlab("Palavras \n") +
    ylab("Frequência") +
    coord_flip() +
    tema_quaest() +
    theme(axis.text.x = element_blank(),
          axis.text.y = element_text(size = 13, hjust = 1),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(face = "bold", size = 18, vjust = -3.2, hjust = 0.1),
          plot.caption = element_text(size = 10, face = "bold"))
}

# Formatação gráficos individuais
plot_camilo2 <- plot_grid(camilo, "Camilo Santana\n")
plot_claudio2 <- plot_grid(claudio, "Cláudio Castro\n")
plot_dino2 <- plot_grid(dino, "Fávio Dino\n")
plot_leite2 <- plot_grid(leite, "Eduardo Leite\n")
plot_doria2 <- plot_grid(doria, "João Doria\n")
plot_zema2 <- plot_grid(zema, "Romeu Zema\n")

grid_cands <- grid.arrange(plot_camilo2, plot_claudio2, plot_leite2, plot_dino2, plot_doria2, plot_zema2, 
             ncol = 3, bottom = textGrob("Fonte: Quaest Consultoria \nElaborado por: Artur Almeida ", 
                                         just = "right",
                                         x = 1,
                                         gp = gpar(fontface = "bold", fontsize = 10)
                                         ))
# Exporta tudo:
# Camilo
png("top10_camilo.png", width = 600, height = 500)
plot_camilo
dev.off()

# Cláudio Castro
png("top10_claudio.png", width = 600, height = 500)
plot_claudio
dev.off()

# Flávio Dino
png("top10_dino.png", width = 600, height = 500)
plot_dino
dev.off()

# Eduardo Leite
png("top10_leite.png", width = 600, height = 500)
plot_leite
dev.off()

# João Doria
png("top10_doria.png", width = 600, height = 500)
plot_doria
dev.off()

# Romeu Zema
png("top10_zema.png", width = 600, height = 500)
plot_zema
dev.off()

# Grid
png("grid_top10.png", width = 900, height = 510)
grid_cands
dev.off()

# Fim
rm(list = ls())
