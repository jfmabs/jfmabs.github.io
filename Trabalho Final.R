# CARREGAMENTO E LIMPEZA DOS DADOS ----

install.packages("tidyr")
library(tidyr)
install.packages("dplyr")
library(dplyr)
install.packages("readr")
library(readr)
install.packages("usethis")
library(usethis)
install.packages("gh")
library(gh)
install.packages("git2r")
library(git2r)
edit_r_environ()
gitcreds::gitcreds_get()
.rs.restartR()
gh::gh("GET /user")

## DADOS CRIMINAIS ------

# Carregando base de crimes do Anuário Brasileiro de Segurança Pública 

caminho_arquivo <- "/Users/jfmabs/Downloads/uf.csv.gz"

dados_crimes <- read.csv(gzfile(caminho_arquivo))
dados_crimesteste <- read.csv(gzfile(caminho_arquivo))

## Criando variável com o nome "mandato" referindo-se aos quatro
## anos de atuação no governo

dados_crimes <- dados_crimes %>%
  mutate(mandato = case_when(
    ano %in% 2011:2014 ~ "2011-2014",
    ano %in% 2015:2018 ~ "2015-2018",
    ano %in% 2019:2022 ~ "2019-2022",
    ano %in% 2023:2026 ~ "2023-2026",
    TRUE ~ NA_character_ # caso existam anos fora desses intervalos
  ))

## Limpando casos de NA, em mandatos que não se enquadram no intervalo
## investigado.

dados_crimes <- dados_crimes %>%
  filter(!is.na(mandato))

## Agregando as taxas criminais para a média dos 4 anos de cada mandato

crimes_agregados <- dados_crimes %>%
  group_by(sigla_uf, mandato) %>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = T)))

mean_exclude <- function(x) {
  x <- x[!is.na(x) & x != 0 & !is.nan(x)] # Excluir 0, NaN e NA
  if(length(x) == 0) return(NA) # Retornar NA se não houver valores válidos
  mean(x)
}

crimes_agregados <- dados_crimes %>%
  group_by(sigla_uf, mandato) %>%
  summarise(across(where(is.numeric), ~ mean_exclude(.x)))


## DELETAR Excluindo a coluna "ano", que não importa mais, pois analisamos apenas o mandato

crimes_agregados <- subset(crimes_agregados, select = -ano)

## Criando um dataframe com nomes e siglas dos estados, para inserir na
## base de dados dos crimes

estados <- data.frame(sigla_uf = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", "SP", "SE", "TO"),
                      estado = c("Acre", "Alagoas", "Amapá", "Amazonas", "Bahia", "Ceará", "Distrito Federal", "Espírito Santo", "Goiás", "Maranhão", "Mato Grosso", "Mato Grosso do Sul", "Minas Gerais", "Pará", "Paraíba", "Paraná", "Pernambuco", "Piauí", "Rio de Janeiro", "Rio Grande do Norte", "Rio Grande do Sul", "Rondônia", "Roraima", "Santa Catarina", "São Paulo", "Sergipe", "Tocantins"))

crimes_agregados <- crimes_agregados %>%
  left_join(estados, by = "sigla_uf")


## BRAZILIAN LEGISLATIVE SURVEY ----

## Nos dados dos partidos, coletado do BLS retirar casos mais antigos
## que 2009 


dados_partidos <- read_csv("BLS9_full.csv") %>% filter(wave >= 2009)

## Selecionar as variáveis de interesse

dados_partidos2 <- dados_partidos %>% 
  select(lrclass, wave, party_survey, 
         party_elected, pcfiscal, pcinterv,
         pcredist, pcvalues, lrlmr, kinzo,
         lrpmdb, lrdem, lrpcb, lrpcdob, lrpds,
         lrpfl, lrpl, lrpp, lrpp_ppb, lrpps,
         lrpr, lrpsb, lrpsd, lrpsdb, lrpsol,
         lrpt, lrptb, lrsd, lrpsc, lrpros, 
         lrpv, lrmdb, lrpsl, lrcid, lrrep,
         lrpode, lrnovo, lrpdt) 

# Substituindo valores de -999.0 para NA

dados_partidos2 <- dados_partidos2 %>%
  mutate_all(~ ifelse(. == -999.0, NA, .))

# Adaptar anos a períodos de mandato e transformar "NaN" em NA numérico

dados_partidos3 <- dados_partidos2 %>%
  mutate(wave = case_when(
    wave == 2009 ~ "2011-2014",
    wave == 2013 ~ "2015-2018",
    wave == 2017 ~ "2019-2022",
    wave == 2021 ~ "2023-2026",
    TRUE ~ as.character(wave) # Manter os outros anos como estão
  ))

dados_partidos4 <- dados_partidos3 %>%
  mutate(across(everything(), ~ ifelse(.x == "NaN", NA_real_, .x)))


# Calculando média dos respondentes por período

media_por_wave <- dados_partidos4 %>%
  group_by(wave) %>%
  summarise_all(mean, na.rm = T)

# Deletando variáveis que não importam mais

media_por_wave <- media_por_wave %>%
  select(-party_survey, -party_elected, -pcfiscal, -pcinterv,
         -pcredist, -pcvalues, -kinzo, -lrclass,
         -lrlmr)

# Unindo dados do PMDB e MDB e excluindo a variável do PMDB

valores_PMDB <- media_por_wave$lrpmdb[1:3]

media_por_wave$lrmdb[1:3] <- valores_PMDB

media_por_wave <- media_por_wave %>%
  select(-lrpmdb, -lrpcb, -lrpds, -lrpp, -lrpfl, -lrrep)


# Mudando o nome das colunas pra algo que eu consiga entender bem


zucco_class <- media_por_wave %>%
  rename(DEM = lrdem, 
         PCdoB = lrpcdob, 
         PL = lrpl, PP = lrpp_ppb, PPS = lrpps,
         PR = lrpr, PSB = lrpsb, PSD = lrpsd, PSDB = lrpsdb,
         PSOL = lrpsol, PT = lrpt, PTB = lrptb, SDD = lrsd,
         PSC = lrpsc, PROS = lrpros, PV = lrpv, MDB = lrmdb,
         PSL = lrpsl, CIDADANIA = lrcid,
         PODE = lrpode, NOVO = lrnovo, PDT = lrpdt)


# Reorganizando o banco


zucco_class_longo <- pivot_longer(zucco_class, 
                                  cols = -wave,  # Todas as colunas exceto a coluna "wave"
                                  names_to = "partido",  # Nome da nova coluna que conterá os nomes dos partidos
                                  values_to = "ideologia_zucco")  # Nome da nova coluna que conterá os valores de ideologia


## BOLOGNESI ----

# Carregar dados da base de Bolognesi

bolognesi <- read.table("BD_survey_partyideology_brasil_expert_2018_HARVARD_DV.tab", 
                        header = TRUE, sep = "\t", quote = "", fill = TRUE)

# Carregar base de dados de governadores, seus mandatos e partidos


dados_governadores2 <- read_csv("dados governadores2 - Página1.csv")

# Calculando a média dos valores do survey de Bolognesi de acordo
# com os partidos

média_bolognesi <- bolognesi %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE)))


# MESCLANDO BASES ----

## Unindo as bases com dados dos governadores, mandatos e partidos, com os períodos
## que correspondem às médias das taxas de crimes

crimes_governadores <- left_join(crimes_agregados, 
    dados_governadores2, by = c("mandato" = "ano", "estado" = "estado"))

## Excluindo colunas que não importam ou estão duplicadas

crimes_governadores <- crimes_governadores %>%
    select(-sigla_estado, -ideologia_part, -apoio_gov)



# CORRIGINDO ERROS INICIAIS DOS BANCOS ----

## Os bancos não apresentaram os nomes e partidos dos governadores da PB, RR e AL
## provavelmente devido a um erro na codificação dos mandatos


# ADICIONANDO VALORES FALTANTES - PARAÍBA

crimes_governadores$governador <- ifelse(crimes_governadores$estado == "Paraíba" & 
 (crimes_governadores$mandato == "2011-2014" 
 | crimes_governadores$mandato == "2015-2018"), 
  "Ricardo Vieira Coutinho", crimes_governadores$governador)

crimes_governadores$partido <- ifelse(crimes_governadores$estado == "Paraíba" & 
  (crimes_governadores$mandato == "2011-2014" | 
  crimes_governadores$mandato == "2015-2018"), 
 "PSDB", crimes_governadores$partido)


# ADICIONANDO VALORES FALTANTES - ALAGOAS

crimes_governadores$governador <- ifelse(crimes_governadores$estado == "Alagoas" & 
(crimes_governadores$mandato == "2011-2014" | crimes_governadores$mandato == "2015-2018"), 
ifelse(crimes_governadores$mandato == "2011-2014", 
"Teotônio Brandão Vilela Filho", "Renan Filho"), crimes_governadores$governador)

crimes_governadores$partido <- ifelse(crimes_governadores$estado == "Alagoas" & 
(crimes_governadores$mandato == "2011-2014" | crimes_governadores$mandato == "2015-2018"),
ifelse(crimes_governadores$mandato == "2011-2014", "PSDB", "MDB"), crimes_governadores$partido)


# ADICIONANDO VALORES FALTANTES - RORAIMA

crimes_governadores$governador <- ifelse(crimes_governadores$estado == "Roraima" 
& (crimes_governadores$mandato == "2011-2014" | crimes_governadores$mandato == "2015-2018"),
ifelse(crimes_governadores$mandato == "2011-2014", "José de Anchieta Júnior",
       "Antonio Denarium"), crimes_governadores$governador)


crimes_governadores$partido <- ifelse(crimes_governadores$estado == "Roraima" &
(crimes_governadores$mandato == "2011-2014" | crimes_governadores$mandato == "2015-2018"),
ifelse(crimes_governadores$mandato == "2011-2014", "PMDB", "PSL"),
crimes_governadores$partido)

## Os dados de bolognesi geraram uma base de dados com 80 colunas, em que
## cada coluna apresentava um partido e o valor da ideologia.
## Seria mais intuitivo transformar em uma base verticalizada, com uma coluna
## intitulada "partido" e outra "ideologia", para análise posterior

# INVERTENDO DADOS

bolognesi_invertido <- t(média_bolognesi)
colnames(bolognesi_invertido) <- bolognesi_invertido[1, ]
bolognesi_invertido <- bolognesi_invertido[-1, ]


bolognesi_total <- média_bolognesi[, -(1:2)]
bolognesi_vertical <- pivot_longer(bolognesi_total, cols = 
                                     everything(), names_to = "partido", 
                                   values_to = "valores_bolognesi")


## Notou-se que na base de crimes e governadores unidos, o partido MDB estava
## intitulado MDB, devido ao ano do mandato. 
## Modificamos para MDB como padronização

crimes_governadores <- mutate(crimes_governadores, partido = 
                        ifelse(partido == "PMDB", "MDB", partido))

## O mesmo ocorreu com o Progressistas, que foi simplificado para PP

bolognesi_vertical <- mutate(bolognesi_vertical, partido =
                               ifelse(partido == "Progressistas", "PP", 
                                      partido))

## Adicionamos uma coluna à base crimes_governadores, contendo o valor da
## ideologia por partido, de acordo com bolognesi.

gov.crim.ide <- merge(crimes_governadores, 
                             bolognesi_vertical, by = "partido", 
                             all.x = TRUE)

gov.crim.ide <- merge(crimes_governadores, bolognesi_vertical, 
                      by = "partido", all.x = TRUE)


## Agora vou adicionar uma coluna nova no banco para a classificação
## do Cesar Zucco

gov_crim_ide_ideologia <- merge(gov.crim.ide, zucco_class_longo,
                                by.x = c("partido", "mandato"),
                                by.y = c("partido", "wave"),
                                all.x = TRUE)

gov_crim_ide_ideologia <- gov_crim_ide_ideologia %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))


##  Retirando valores 0 da base para NA

dados_crimes <- dados_crimes %>% mutate(across(everything(), as.character))
dados_crimes[dados_crimes == 0] <- NA

## Teste de hipótese

populacao_ano <- ipeadata_20_06_2024_10_18_ %>%
  select(Sigla, Estado, "2014", "2018", "2022")

anos_referencia <- list('2011-2014' = 2014,
                        '2015-2018' = 2018,
                        '2019-2022' = 2022)

populacao_ano <- rename(populacao_ano, sigla = sigla_uf)

populacao_long <- populacao_ano %>%
  pivot_longer(cols = starts_with("20"),  # Seleciona as colunas de anos
               names_to = "Ano",          # Nome para a coluna de anos
               values_to = "Populacao")  # Nome para a coluna de população

populacao_long <- populacao_long %>%
  mutate(Ano_modificado = case_when(
    Ano == "2014" ~ "2011-2014",
    Ano == "2018" ~ "2015-2018",
    Ano == "2022" ~ "2019-2022",
    TRUE ~ Ano  # Para outros casos, manter o valor original
  ))

populacao_long <- populacao_long %>%
  select(-Ano)

populacao_long <- rename(populacao_long, ano = Ano_modificado)

populacao_crimes <- gov_crim_ide_ideologia %>%
  left_join(populacao_long, by = c("sigla_uf" = "Sigla", "mandato" = "ano"))

taxas_crimes <- populacao_crimes %>%
  mutate(across(starts_with("quantidade_"), ~ . / Populacao * 100)) %>%
  select(partido, mandato, sigla_uf, valores_bolognesi, ideologia_zucco, despesa_empenhada_seguranca_publica, starts_with("quantidade_"))

install.packages("ggplot2")
library(ggplot2)
install.packages("stats")
library(stats)

intervalos <- c(0, 1.5, 3, 4.5, 5.5, 7, 8.5, 10)
rotulos <- c("Extrema-Esquerda", "Esquerda", "Centro-Esquerda", "Centro", "Centro-Direita", "Direita", "Extrema-Direita")

taxas_crimes$categoria_bolognesi <- cut(taxas_crimes$valores_bolognesi, breaks = intervalos, labels = rotulos)

modelo <- lm(cbind(quantidade_morte_intervencao_policial_civil_servico 
                   ) ~ cbind(
                     categoria_bolognesi), data = taxas_crimes)

modelo <- lm(cbind(
                   quantidade_morte_intervencao_policial_militar_servico,
                   quantidade_policial_militar_morto_confronto_servico,
                   despesa_empenhada_seguranca_publica
                   ) ~ cbind(
                     ideologia_zucco, mandato), data = taxas_crimes2)


summary(modelo2)

modelo2 <- lm(cbind(
                   quantidade_morte_intervencao_policial_militar_servico) ~ cbind(
                                                         ideologia_zucco), data = taxas_crimes)

modelo3 <- lm(cbind(despesa_empenhada_seguranca_publica,
                    quantidade_populacao_sistema_penitenciario) ~ cbind(categoria_bolognesi, ideologia_zucco) + factor(mandato), data = taxas_crimes)

summary(modelo3)

modelo4 <- lm(quantidade_populacao_sistema_penitenciario ~ 
                categoria_bolognesi + factor(mandato), data = taxas_crimes)
summary(modelo4)

taxas_crimes2 <- taxas_crimes %>%
  arrange(sigla_uf, mandato)

taxas_crimes2 <- taxas_crimes2 %>%
  group_by(sigla_uf) %>%
  mutate(mudanca_ideologica = ifelse(lag(categoria_bolognesi) == categoria_bolognesi, 0, 1))

taxas_crimes2 <- taxas_crimes2 %>%
  group_by(sigla_uf) %>%
  mutate(
    transicao_ideologica = case_when(
      lag(categoria_bolognesi) %in% c("Extrema-Esquerda", "Esquerda", "Centro") & 
        categoria_bolognesi %in% c("Direita", "Extrema-Direita") ~ 1,
      lag(categoria_bolognesi) == "Direita" & categoria_bolognesi == "Extrema-Direita" ~ 1,
      TRUE ~ 0
    )
  )
