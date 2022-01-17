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

##### Análise Lula #############################################################
# Abrir posts
bd_lula <- read_xlsx(here("0.dados/pres/lula/lula.xlsx"))

# Padronizar posts
bd_lula$Message <- str_to_lower(bd_lula$Message)

# Criando a lista com as palavras
palavras_lula <- strsplit(bd_lula$Message, "\\W+")
palavras_lula <- unlist(palavras_lula)

# Contando a frequência de cada palavra usando o pacote "tokenizers"
bd_palavras_lula <- as.data.frame(tokenize_words(palavras_lula, lowercase = TRUE) %>%
  unlist() %>%
  table() %>%
  sort(decreasing = TRUE))

# Alterando nome das colunas
colnames(bd_palavras_lula) <- c("Palavras", "Freq")

# Filtrando palavras com mais de três letras, exceto PT
bd_palavras_lula <- subset(bd_palavras_lula, nchar(as.character(Palavras)) > 3 | Palavras == "pt")

# Filtrando linhas que possuem um número em vez de uma palavra
bd_palavras_lula <- bd_palavras_lula %>%
                        filter(!(str_detect(Palavras, "\\d")))

# Removendo palavras e hashtags inúteis manualmente
# Lista geral:
bd_palavras_lula <- filter(bd_palavras_lula, !(Palavras %in% dicio))

# Lista específica:
bd_palavras_lula <- filter(bd_palavras_lula, !(Palavras %in% c(
  "equipelula", "stuckert", "ricardo",
  "ricardostuckert","76fatossobrelula"
  )))

# Filtrando palavras que foram usadas mais de 1 vez para termos uma lista
# dos termos mais importantes
bd_palavras_lula <- filter(bd_palavras_lula, (Freq > 1))

# Exportando
write_xlsx(bd_palavras_lula, here("outputs", "palavras_lula.xlsx"))

# Remover análise Lula
rm(bd_lula, palavras_lula)

##### Análise Bolsonaro ########################################################
# Abrir posts
bd_bolsonaro <- read_xlsx(here("0.dados/pres/bolsonaro/bolsonaro.xlsx"))

# Padronizar posts
bd_bolsonaro$Message <- str_to_lower(bd_bolsonaro$Message)

# Criando a lista com as palavras
palavras_bolsonaro <- strsplit(bd_bolsonaro$Message, "\\W+")
palavras_bolsonaro <- unlist(palavras_bolsonaro)

# Contando a frequência de cada palavra usando o pacote "tokenizers"
bd_palavras_bolsonaro <- as.data.frame(tokenize_words(palavras_bolsonaro, lowercase = TRUE) %>%
                                    unlist() %>%
                                    table() %>%
                                    sort(decreasing = TRUE))

# Alterando nome das colunas
colnames(bd_palavras_bolsonaro) <- c("Palavras", "Freq")

# Filtrando palavras com mais de três letras, exceto PT e PL
bd_palavras_bolsonaro <- subset(bd_palavras_bolsonaro, nchar(as.character(Palavras)) > 3 | 
                                  Palavras == "pt" |
                                  Palavras == "pl")

# Filtrando linhas que possuem um número em vez de uma palavra
bd_palavras_bolsonaro <- bd_palavras_bolsonaro %>%
  filter(!(str_detect(Palavras, "\\d")))

# Removendo palavras e hashtags inúteis manualmente
# Lista geral:
  bd_palavras_bolsonaro <- filter(bd_palavras_bolsonaro, !(Palavras %in% dicio))

# Lista específica:
  bd_palavras_bolsonaro <- filter(bd_palavras_bolsonaro, !(Palavras %in% c(
    "youtube", "govbr", "minsaude", "tarcisiogdf", "secomvc", "telegram", 
    "minfraestrutura", "defesagovbr", "exercitooficial", "live", "rogeriosmarinho",
    "mdregional_br", "fab_oficial", "justicagovbr"
  )))
  
# Filtrando palavras que foram usadas mais de 1 vez para termos uma lista
# dos termos mais importantes
bd_palavras_bolsonaro <- filter(bd_palavras_bolsonaro, (Freq > 1))

# Exportando
write_xlsx(bd_palavras_bolsonaro, here("outputs", "palavras_bolsonaro.xlsx"))

# Remover análise bolsonaro
rm(bd_bolsonaro, palavras_bolsonaro)

##### Análise Moro ########################################################
# Abrir posts
bd_moro <- read_xlsx(here("0.dados/pres/moro/moro.xlsx"))

# Padronizar posts
bd_moro$Message <- str_to_lower(bd_moro$Message)

# Criando a lista com as palavras
palavras_moro <- strsplit(bd_moro$Message, "\\W+")
palavras_moro <- unlist(palavras_moro)

# Contando a frequência de cada palavra usando o pacote "tokenizers"
bd_palavras_moro <- as.data.frame(tokenize_words(palavras_moro, lowercase = TRUE) %>%
                                         unlist() %>%
                                         table() %>%
                                         sort(decreasing = TRUE))

# Alterando nome das colunas
colnames(bd_palavras_moro) <- c("Palavras", "Freq")

# Filtrando palavras com mais de três letras, exceto PT
bd_palavras_moro <- subset(bd_palavras_moro, nchar(as.character(Palavras)) > 3 | Palavras == "pt")

# Filtrando linhas que possuem um número em vez de uma palavra
bd_palavras_moro <- bd_palavras_moro %>%
  filter(!(str_detect(Palavras, "\\d")))

# Removendo palavras e hashtags inúteis manualmente
# Lista geral:
bd_palavras_moro <- filter(bd_palavras_moro, !(Palavras %in% dicio))

# Lista específica:
bd_palavras_moro <- filter(bd_palavras_moro, !(Palavras %in% c(
  "estarei"
)))

# Filtrando palavras que foram usadas mais de 1 vez para termos uma lista
# dos termos mais importantes
bd_palavras_moro <- filter(bd_palavras_moro, (Freq > 1))

# Exportando
write_xlsx(bd_palavras_moro, here("outputs", "palavras_moro.xlsx"))

# Remover análise moro
rm(bd_moro, palavras_moro)

##### Análise Ciro ########################################################
# Abrir posts
bd_ciro <- read_xlsx(here("0.dados/pres/ciro/ciro.xlsx"))

# Padronizar posts
bd_ciro$Message <- str_to_lower(bd_ciro$Message)

# Criando a lista com as palavras
palavras_ciro <- strsplit(bd_ciro$Message, "\\W+")
palavras_ciro <- unlist(palavras_ciro)

# Contando a frequência de cada palavra usando o pacote "tokenizers"
bd_palavras_ciro <- as.data.frame(tokenize_words(palavras_ciro, lowercase = TRUE) %>%
                                    unlist() %>%
                                    table() %>%
                                    sort(decreasing = TRUE))

# Alterando nome das colunas
colnames(bd_palavras_ciro) <- c("Palavras", "Freq")

# Filtrando palavras com mais de três letras, exceto PT e PDT
bd_palavras_ciro <- subset(bd_palavras_ciro, nchar(as.character(Palavras)) > 3 | 
                             Palavras == "pt" |
                             Palavras == "pdt")

# Filtrando linhas que possuem um número em vez de uma palavra
bd_palavras_ciro <- bd_palavras_ciro %>%
  filter(!(str_detect(Palavras, "\\d")))

# Removendo palavras e hashtags inúteis manualmente
# Lista geral:
bd_palavras_ciro <- filter(bd_palavras_ciro, !(Palavras %in% dicio))

# Lista Específica:
bd_palavras_ciro <- filter(bd_palavras_ciro, !(Palavras %in% c(
  "cirogames", "youtube", "live", "link", "acompanhe", "vivo", "compartilhe",
  "assista", "prefirociro", "cironodatena"
)))

# Filtrando palavras que foram usadas mais de 1 vez para termos uma lista
# dos termos mais importantes
bd_palavras_ciro <- filter(bd_palavras_ciro, (Freq > 1))

# Exportando
write_xlsx(bd_palavras_ciro, here("outputs", "palavras_ciro.xlsx"))

# Remover análise ciro
rm(bd_ciro, palavras_ciro)

##### Análise Doria ########################################################
# Abrir posts
bd_doria <- read_xlsx(here("0.dados/pres/doria/doria.xlsx"))

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

# Exportar excel consolidado
lista_palavras <- list("Lula" = bd_palavras_lula,
                       "Bolsonaro" = bd_palavras_bolsonaro,
                       "Moro" = bd_palavras_moro,
                       "Ciro" = bd_palavras_ciro,
                       "Doria" = bd_palavras_doria)
write.xlsx(lista_palavras, file = "palavras_agregado.xlsx")

# Remove tudo
rm(list = ls())

########## PLOTS #######################################
# Abrindo resultado geral
bd <- import_list("C:/Users/artur/OneDrive/Área de Trabalho/Quaest/Análise - Top Palavras/2.documentos/pres/palavras_agregado.xlsx")

# Abrindo top 10 de cada cand
lula <- head(bd$Lula, n = 10)
bolsonaro <- head(bd$Bolsonaro, n = 10)
moro <- head(bd$Moro, n = 10)
ciro <- head(bd$Ciro, n = 10)
doria <- head(bd$Doria, n = 10)

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
          plot.caption = element_text(size = 10, face = "bold"),
          plot.background = element_rect(fill = "transparent"))
}

# Aplicando função para cada Cand
plot_lula <- plot_cand(lula, "Top-10 Palavras mais utilizadas por Lula no ano de 2021 \n")
plot_bolsonaro <- plot_cand(bolsonaro, "Top-10 Palavras mais utilizadas por Bolsonaro no ano de 2021 \n")
plot_moro <- plot_cand(moro, "Top-10 Palavras mais utilizadas por Sergio Moro no ano de 2021 \n")
plot_ciro <- plot_cand(ciro, "Top-10 Palavras mais utilizadas por Ciro Gomes no ano de 2021 \n")
plot_doria <- plot_cand(doria, "Top-10 Palavras mais utilizadas por João Doria no ano de 2021 \n")

# Exporta tudo:
# Lula
png("top10_lula.png", width = 550, height = 500)
plot_lula
dev.off()

# Bolsonaro
png("top10_bolsonaro.png", width = 550, height = 500)
plot_bolsonaro
dev.off()

# Moro
png("top10_moro.png", width = 550, height = 500)
plot_moro
dev.off()

# Ciro
png("top10_ciro.png", width = 550, height = 500)
plot_ciro
dev.off()

# Doria
png("top10_doria.png", width = 550, height = 500)
plot_doria
dev.off()

# Plot Grid
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
plot_lula2 <- plot_grid(lula, "Lula\n")
plot_bolsonaro2 <- plot_grid(bolsonaro, "Bolsonaro\n")
plot_moro2 <- plot_grid(moro, "Sergio Moro\n")
plot_ciro2 <- plot_grid(ciro, "Ciro Gomes\n")
plot_doria2 <- plot_grid(doria, "João Doria\n")

grid_cands <- grid.arrange(plot_lula2, plot_bolsonaro2, plot_moro2, plot_ciro2, plot_doria2, 
                           ncol = 3, bottom = textGrob("Fonte: Quaest Consultoria \nElaborado por: Artur Almeida ", 
                                                       just = "right",
                                                       x = 1,
                                                       gp = gpar(fontface = "bold", fontsize = 10)
                           ))
# Fim
rm(list = ls())