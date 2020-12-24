# pacotes necessários
library(data.table)
library(ggplot2)
library(dplyr)
library(grid)
library(stringr)
library(forcats)
library(kableExtra)
library(plotrix)
library(wordcloud)
library(RColorBrewer)
library(Amelia)
library(viridis)
library(paletteer)

# baixando csv com base de dados
df <- fread('original-dataset-20201117.csv')
head(df)

# criando um dataframe para tabela de descrição de variáveis
variaveis <- data.frame(variavel = c('date', 'home_team', 'away_team', 'home_score', 'away_score', 'tournament', 'city', 'country','neutral'),
                        descricao = c('data da partida', 'nome do time da casa', 'nome do time convidado', 'placar do time da casa (tempo normal de jogo + prorrogações. Pênaltis não considerados)', 'placar do time convidado (tempo normal de jogo + prorrogações. Pênaltis não considerados)', 'nome do torneio', 'nome da cidade onde a partida foi realizada', 'nome do país onde a partida foi realizada', 'VERDADEIRO/FALSO - indica se a partida foi realizada em um local neutro para as duas equipes'),
                        tipo_de_dados = c('IDate', 'Character','Character','Integer','Integer', 
                                          'Character', 'Character', 'Character', 'Logical'),
                        valores_permitidos = c('datas', 'texto', 'texto', 'números inteiros', 'números inteiros',
                                               'texto', 'texto', 'texto', 'valores lógicos'))

variaveis %>%
    rename('Variável' = variavel,
           'Descrição' = descricao,
           'Tipo de Dados' = tipo_de_dados,
           'Valores Permitidos' = valores_permitidos)

View(variaveis)

# analisando dataset
View(df)
str(df)
summary(df)

# criando novo dataset para transformações necessárias
novo_df <- df

# identificação de transformações necessárias

# verificando e acertando nomes escritos de formas diferentes
# times
# home_team
as.data.frame(table(novo_df$home_team))
# substituir
# CuraÃ§ao = Curaçao
# unir DR Congo e Congo
# unir India, India N e India S
# RÃ©union = Reunion
# SÃ£o TomÃ© and PrÃ­ncipe = San-Tome and Principe
novo_df$home_team <- novo_df$home_team %>%
  str_replace_all('CuraÃ§ao', 'Curaçao') %>%
  str_replace_all('DR Congo', 'Congo') %>%
  str_replace_all('India N', 'India') %>%
  str_replace_all('India S', 'India') %>%
  str_replace_all('RÃ©union', 'Reunion') %>%
  str_replace_all('SÃ£o TomÃ© and PrÃ­ncipe', 'San-Tome and Principe')
# outros que não mudaram (antigos países ou união de países condizentes com as datas dos jogos): 
# Czechoslovakia = depois se separou em Czech Republic e Slovakia
# Netherlands Antilles = conjunto de terras que originou Netherlands
# Serbia and Montenegro = depois se separou em Serbia e Montenegro

#away_team
as.data.frame(table(novo_df$away_team))
# substituir
# CuraÃ§ao = Curaçao
# unir DR Congo e Congo
# RÃ©union = Reunion
# SÃ£o TomÃ© and PrÃ­ncipe = San-Tome and Principe
novo_df$away_team <- novo_df$away_team %>%
  str_replace_all('CuraÃ§ao', 'Curaçao') %>%
  str_replace_all('DR Congo', 'Congo') %>%
  str_replace_all('India N', 'India') %>%
  str_replace_all('RÃ©union', 'Reunion') %>%
  str_replace_all('SÃ£o TomÃ© and PrÃ­ncipe', 'San-Tome and Principe')

# outros que não mudaram (antigos países ou união de países condizentes com as datas dos jogos): 
# Czechoslovakia = depois se separou em Czech Republic e Slovakia

# country
as.data.frame(table(novo_df$country))
# substituir
# unir DR Congo e Congo
# RÃ©union = Reunion
# SÃ£o TomÃ© and PrÃ­ncipe = San-Tome and Principe
novo_df$country <- novo_df$country %>%
  str_replace_all('DR Congo', 'Congo') %>%
  str_replace_all('RÃ©union', 'Reunion') %>%
  str_replace_all('SÃ£o TomÃ© and PrÃ­ncipe', 'San-Tome and Principe')

# outros que não mudaram (antigos países ou união de países condizentes com as datas dos jogos): 
# Czechoslovakia = depois se separou em Czech Republic e Slovakia
# Netherlands Antilles = conjunto de terras que originou Netherlands
# Serbia and Montenegro = depois se separou em Serbia e Montenegro

# tournament
as.data.frame(table(novo_df$tournament))
# substituir
# Copa AmÃ©rica = Copa América
# unir Euro e UEFA Euro
novo_df$tournament <- novo_df$tournament %>%
  str_replace_all('Copa AmÃ©rica', 'Copa América') %>%
  str_replace_all('Euro', 'UEFA Euro')

# city
as.data.frame(table(novo_df$city))
# dúvida prof

# alteração de tipo de data: "IDate" "Date" para "POSIXct" "POSIXt"
class(novo_df$date)
novo_df$date <- as.POSIXct(novo_df$date)
head(novo_df$date)
class(novo_df$date)

# As colunas que contém os times, países, torneios e cidades (home_team, away_team, country, tournament e city) devem ser convertidas para fatores.

novo_df$home_team <- as.factor(novo_df$home_team)
novo_df$away_team <- as.factor(novo_df$away_team)
novo_df$country <- as.factor(novo_df$country)
novo_df$tournament <- as.factor(novo_df$tournament)
novo_df$city <- as.factor(novo_df$city)

str(novo_df)


# nenhum valor NA foi encontrado
any(is.na(novo_df))

missmap(novo_df, 
        main = "Womens International Football Results - Mapa de Dados Missing",
        col = c("yellow", "black"), 
        legend = FALSE)

# RESUMO DOS DADOS
summary(novo_df)

# ANÁLISE EXPLORATÓRIA
# Criação de um dataframe completo para análises. Colunas inseridas:

novas_colunas <- data.frame(Coluna = c('Placar', 'Vencedor', 'Ano', 'Década'),
                            Descricao = c('Juntação de home_score e away_score, indicando o placar da partida', 'Informa o vencedor da partida', 'Informa o ano de realização da partida, últil para plotagens futuras', 'As décadas foram divididas em 4 com 10 anos cada e 1, a primeira, contendo 11 anos. Década 1 = 1969 - 1980, Década 2 = 1981 - 1990, Década 3 = 1991 - 2000, Década 4 = 2001 - 2010 e Década 5 = 2011 - 2020'))

novas_colunas %>%
  rename('Descrição' = Descricao) %>%
  kbl() %>%
  kable_paper() %>%
  column_spec(1, italic = T) %>%
  column_spec(2, width = "30em") %>%
  row_spec(0, bold = T)

completo <- novo_df %>%
  mutate(placar = paste(home_score, 'x', away_score)) %>%
  mutate(vencedor = ifelse(novo_df$home_score > novo_df$away_score, as.character(home_team), ifelse(novo_df$home_score == novo_df$away_score,  'empate', as.character(away_team)))) %>%
  mutate(ano = year(date)) %>%
  mutate(decada = ifelse(date <= '1980-12-31', 'Década 1', ifelse(date <= '1990-12-31', 'Década 2', ifelse(date <= '2000-12-31', 'Década 3', ifelse(date <= '2010-12-31', 'Década 4', 'Década 5')))))

completo$placar <- as.factor(completo$placar)
completo$vencedor <- as.factor(completo$vencedor)
completo$ano <- as.factor(completo$ano)
completo$decada <- as.factor(completo$decada)

str(completo)

# Placares e outliers

# moda
moda <- function(x){
    z <- table(as.vector(x))
    names(z)[z == max(z)]
}

paste('O placar que mais ocorreu foi o de ', moda(completo$placar),'.')

# heatmap
completo %>%
  count(home_score, away_score) %>%
  ggplot(aes(x = home_score, y = away_score)) + geom_tile(aes(fill = n), width=0.8, height=0.8) +
  scale_x_continuous(breaks = seq(0, 24, 2)) +
  scale_y_continuous(breaks = seq(0, 24, 2)) +
  labs(title = 'Ocorrência de placar',
       x = 'Gols - Time da Casa',
       y = 'Gols - Time Convidado',
       fill = 'Qtd de Partidas') + 
  scale_fill_viridis() +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(panel.background = element_blank())

# Gols marcados, sofridos e saldo de gols
# gols
gols_casa <- completo %>%
  group_by(home_team) %>%
  summarise(gols = sum(home_score))

gols_fora <- completo %>%
  group_by(away_team) %>%
  summarise(gols = sum(away_score))

gols <- inner_join(gols_casa, gols_fora, by = c('home_team' = 'away_team'))

gols %>%
  mutate(total = gols.x + gols.y) %>%
  select(home_team, total) %>%
  arrange(desc(total)) %>%
  rename('Equipe' = home_team,
         'Gols feitos' = total) %>%
  head(10) %>%
  kbl() %>% kable_paper()

gols %>%
  rename(team = 'home_team') %>%
  mutate(total = gols.x + gols.y) %>%
  select(team, total) %>%
  arrange(desc(total)) %>%
  ggplot() + geom_histogram(aes(total), fill = '#440154FF', alpha = .5) +
  guides(fill=F) +
  labs(title = 'Frequência totais de gols das equipes',
       x = 'Gols marcados',
       y = 'Ocorrência') +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

# saldo
saldo_casa <- completo %>%
  mutate(saldo = home_score - away_score) %>%
  group_by(home_team) %>%
  summarise(sum = sum(saldo)) %>%
  arrange(desc(sum))

saldo_fora <- completo %>%
  mutate(saldo = home_score - away_score) %>%
  group_by(away_team) %>%
  summarise(sum = sum(saldo)) %>%
  arrange(desc(sum))

saldos <- inner_join(saldo_casa, saldo_fora, by = c('home_team' = 'away_team'))

saldos %>%
  mutate(total = sum.x + sum.y) %>%
  select(home_team, total) %>%
  arrange(desc(total)) %>%
  rename('Equipe' = home_team,
         'Saldo de Gols' = total) %>%
  head(10) %>%
  kbl() %>% kable_paper()

saldos %>%
  rename(team = 'home_team') %>%
  mutate(total = sum.x + sum.y) %>%
  select(team, total) %>%
  arrange(desc(total)) %>%
  ggplot() + geom_histogram(aes(total), fill = '#440154FF', alpha = .5) +
  labs(title = 'Frequência de saldo de Gols das equipes',
       x = 'Saldo de gols',
       y = 'Ocorrência') +
  theme_classic() + theme(plot.title = element_text(hjust = 0.5))

# diferença de gols
cores_5 <- as.vector(paletteer_c("viridis::viridis", n = 5, direction = 1))

completo %>%
  mutate(gd = ifelse(home_score - away_score >= 0, home_score - away_score,  away_score - home_score)) %>%
  select(decada, gd) %>%
  arrange(desc(gd)) %>%
  ggplot(aes(decada, gd, color = decada)) + geom_boxplot(color = cores_5, outlier.colour = 'black', outlier.alpha = .10, outlier.size = 2) +
  labs(title = 'Diferença de gols por partida',
       subtitle = '                1969 até 1980                        1981 até 1990                        1991 até 2000                        2001 até 2010                        2011 até 2020',
       x = '',
       y = 'Gols')  +
  theme_classic() +
  theme(legend.position = "none") + theme(plot.title = element_text(hjust = 0.5))

# Vitórias e Derrotas

# analisando times mais e menos vitoriosos

mais_vitoriosos <- completo %>%
    group_by(vencedor) %>%
    summarise(vitorias = n()) %>%
    filter(vencedor != 'empate') %>%
    arrange(desc(vitorias)) %>%
    rename('Equipe' = vencedor,
           'Quantidade de Vitórias' = vitorias) %>%
    head(5)

menos_vitoriosos <- completo %>%
    group_by(vencedor) %>%
    summarise(vitorias = n()) %>%
    arrange(desc(vitorias)) %>%
    rename('Equipe' = vencedor,
           'Quantidade de Vitórias' = vitorias) %>%
    tail(5)

lista <- data.frame(Equipe = c(as.character(mais_vitoriosos$Equipe), as.character(menos_vitoriosos$Equipe)),
                    qtd_vitorias = c(mais_vitoriosos$`Quantidade de Vitórias`, menos_vitoriosos$`Quantidade de Vitórias`))
lista <- lista %>%
  rename('Quantidade de Vitórias' = qtd_vitorias)

# análise de desempenho dos times mais e menos vitoriosos (absoluto e relativo):

# média de times mais vitoriosos e menos vitoriosos

top5_vitorias <- c('Germany', 'Norway', 'United States', 'Sweden', 'China PR')
top5_derrotas <- c('Samoa', 'Serbia and Montenegro', 'Tahiti', 'Tajikistan', 'Uganda') 
cores_3 <- as.vector(paletteer_c("viridis::viridis", n = 3, direction = 1))

completo %>%
  filter(home_team %in% top5_vitorias | away_team %in% top5_vitorias) %>%
  mutate(classificador = ifelse(vencedor %in% top5_vitorias, 'Vitória', ifelse(vencedor == 'empate', 'Empate', 'Derrota'))) %>%
  group_by(classificador) %>%
  summarise(qtd = n()) %>%
  ggplot(aes(x = fct_reorder(classificador, qtd, .desc = T), y = qtd, fill = classificador)) + geom_col(fill = cores_3) +
  guides(fill =F) +
  labs(title = 'Top 5 Mais Vitoriosos',
       subtitle = 'Análise de Desempenho',
       x = 'Situação da Partida',
       y = 'Ocorrência') + 
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

derrotados <- completo %>%
  filter(home_team %in% top5_derrotas | away_team %in% top5_derrotas) %>%
  mutate(classificador = ifelse(vencedor %in% top5_derrotas, 'Vitória', ifelse(vencedor == 'empate', 'Empate', 'Derrota'))) %>%
  group_by(classificador) %>%
  summarise(qtd = n()) %>%
  mutate(Percentual = (qtd / sum(qtd)) * 100)

legenda_percentual <- paste(derrotados$classificador,' ' ,round(derrotados$Percentual,2), '%')

pie3D(derrotados$Percentual, labels = legenda_percentual ,explode = 0.5,
      col = cores_3,
      main = 'Análise de Desempenho Top 5 menos vitoriosos')

# time mais vitorioso fora de casa

completo %>%
    filter(neutral == F) %>%
    select(away_team, vencedor) %>%
    mutate(time_fora = ifelse(as.character(away_team) == as.character(vencedor), as.character(away_team), 'nada')) %>%
    group_by(time_fora) %>%
    filter(time_fora != 'nada') %>%
    summarise(qtd = n()) %>%
    arrange(desc(qtd)) %>%
    rename('Equipe' = time_fora,
           'Vitórias fora de casa' = qtd)

# mapa de palavras - país

df_country <- as.data.frame(table(completo$country))
cores_mapa_palavras <- paletteer_c("viridis::viridis", n = 143, direction = 1)
wordcloud(words = df_country$Var1, freq = df_country$Freq, random.order = FALSE, rot.per = 0.35 ,colors = cores_mapa_palavras)

# time que mais venceu quando o local era neutro pras duas equipes
cores_10 <- as.vector(paletteer_c("viridis::viridis", n = 10, direction = 1))

completo %>%
  filter(neutral == T) %>%
  count(vencedor) %>%
  arrange(desc(n)) %>%
  filter(vencedor != 'empate') %>%
  head(10) %>%
  ggplot(aes(x = fct_reorder(vencedor, n, .desc = F), y = n, fill = vencedor)) +
  geom_col(fill = cores_10) + guides(fill = F) +
  labs(title ='Países mais vitoriosos em locais neutros',
       x = '',
       y = 'Vitórias') +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()


# Times e países mais participativos

# qtd de partidas por década
completo %>%
    group_by(decada, ano) %>%
    mutate(ano = as.character(ano)) %>%
    mutate(ano = as.numeric(ano)) %>%
    filter(vencedor != 'empate') %>%
    summarise(qtd = n()) %>%
    ggplot(aes(x = ano, y = qtd)) + geom_line(aes(group = decada, colour = decada)) +
    labs(title = 'Quantidades de partidas por década', x = '', y = 'Partidas') +
    theme_classic() +
    scale_color_manual('', values = cores_5) +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = seq(1960, 2020, 10))

# continentes mais participativos

times <- data.frame(time = c(as.character(completo$home_team), as.character(completo$away_team)), decada = c(as.character(completo$decada), as.character(completo$decada)), stringsAsFactors = F)

times_contagem_decada <- times %>%
  group_by(time, decada) %>%
  summarise(qtd = n()) %>%
  arrange(desc(qtd))

continentes <- fread('countryContinent.csv') %>% select(country, continent)


times_contagem_decada_continente <- full_join(times_contagem_decada, continentes, by = c('time' = 'country'))

# montagem do gráfico
times_contagem_decada_continente %>%
  filter(!is.na(continent)) %>%
  ggplot(aes(x = decada, y = qtd, fill = continent)) + geom_bar(stat = 'identity') +
  labs(title = 'Continentes mais participativos', x = '', y = 'Quantitade de Partidas') +
  scale_x_discrete(limits = c('Década 1', 'Década 2', 'Década 3', 'Década 4', 'Década 5')) +
  scale_fill_discrete(name = '') +
  scale_fill_manual("Continentes", values = cores_5) +
  theme_classic() + 
  theme(plot.title = element_text(hjust = 0.5))

# analisando países que mais participaram - mapa

# contagem de partidas por equipe
times_contagem <- times %>%
  group_by(time) %>%
  summarise(qtd = n()) %>%
  arrange(desc(qtd))
# 20 equipes mais participativas
top_20 <- head(times_contagem,20)

# baixando dataset com latitudes e longitudes
latlong <- fread('long_lat.csv') %>% select(latitude, longitude, country)
head(latlong)

# latitudes e longitudes dos 20 países mais participativos
top_20_latlong <- latlong %>% filter(country %in% top_20$time)

# gerando mapa com 20 países mais parrticipativos
mapa <- map_data("world")
graf <- ggplot() + 
    geom_polygon(data = mapa, aes(x=long, y = lat, group = group), fill = NA, color = "black") + 
    coord_fixed(1.3) + labs(title = 'Países mais participativos')

graf + 
    geom_point(data = top_20_latlong, aes(x = longitude, y = latitude), color = "#440154FF", size = 5) +
    geom_point(data = top_20_latlong, aes(x = longitude, y = latitude), color = "#FDE725FF", size = 5) + labs(x = '', y = '')
