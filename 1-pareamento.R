# ==============================================================================
# 1. CONFIGURAÇÕES INICIAIS E PACOTES
# ==============================================================================

library(MatchIt)
library(cobalt)
library(dplyr)
library(ggplot2)
library(sf)
library(readxl)
library(readr)
library(stringr)
library(janitor)
library(openxlsx)
library(writexl)

# Garantir que a pasta OUTPUTS exista no diretório de trabalho
dir.create("OUTPUTS", showWarnings = FALSE)

# ==============================================================================
# 2. LEITURA E TRATAMENTO DOS DADOS DO CENSO (2010)
# ==============================================================================

# Dados Básicos: Domicílios, Pessoas e Renda Média
basico <- read_delim("DADOS/infos_2010/Basico_SP1.csv", 
                     delim = ";", escape_double = FALSE, 
                     col_types = cols(Cod_setor = col_character(), 
                                      V001 = col_number(), V002 = col_number(),
                                      V009 = col_number()), 
                     locale = locale(decimal_mark = ",", grouping_mark = "."), 
                     trim_ws = TRUE) %>% 
  select(Cod_setor, V001, V002, V009) %>% 
  rename("n_dom" = "V001", "n_pess" = "V002", "med_rend_dom" = "V009") %>% 
  arrange(Cod_setor)

# Responsável Renda: Cálculo de % com renda de até 3 salários mínimos
responsavel_renda <- read_delim("DADOS/infos_2010/ResponsavelRenda_SP1.csv", 
                                delim = ";", escape_double = FALSE, 
                                col_types = cols(Cod_setor = col_character(), 
                                                 V001 = col_number(), V002 = col_number(),
                                                 V003 = col_number(), V004 = col_number(),
                                                 V010 = col_number(), V020 = col_number()), 
                                locale = locale(decimal_mark = ",", grouping_mark = "."), 
                                trim_ws = TRUE) %>% 
  select(Cod_setor, V001, V002, V003, V004, V010, V020) %>%
  mutate(pct_resp_rend_3_sm_menos = (V001 + V002 + V003 + V004 + V010) / V020) %>% 
  select(Cod_setor, pct_resp_rend_3_sm_menos)

# Unificando base principal
df <- left_join(basico, responsavel_renda, by = "Cod_setor")

# Responsável 01 e 02: Proporção de mulheres e taxa de alfabetização
responsavel01 <- read_delim("DADOS/infos_2010/Responsavel01_SP1.csv", 
                            delim = ";", escape_double = FALSE, col_types = cols(Cod_setor = col_character(), V001 = col_number()), 
                            locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE) %>% 
  select(Cod_setor, V001)

responsavel02 <- read_delim("DADOS/infos_2010/Responsavel02_SP1.csv", 
                            delim = ";", escape_double = FALSE, col_types = cols(Cod_setor = col_character(), V001 = col_number(), V093 = col_number()), 
                            locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE) %>% 
  select(Cod_setor, V001, V093) %>% 
  left_join(responsavel01, by = "Cod_setor") %>% 
  mutate(pct_resp_fem = V001.y / V001.x,
         pct_resp_alfa = V093 / V001.x) %>% 
  select(Cod_setor, pct_resp_fem, pct_resp_alfa)

df <- left_join(df, responsavel02, by = "Cod_setor")

# Pessoa 03: Proporção de pessoas Pretas, Pardas, Indígenas e Amarelas
pessoa03 <- read_delim("DADOS/infos_2010/Pessoa03_SP1.csv", 
                       delim = ";", escape_double = FALSE, col_types = cols(Cod_setor = col_character(), V001 = col_number(),
                                                                            V003 = col_number(), V004 = col_number(),
                                                                            V005 = col_number(), V006 = col_number()), 
                       locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE) %>% 
  select(Cod_setor, V001, V003, V004, V005, V006) %>% 
  mutate(pct_pess_ppia = (V003 + V004 + V005 + V006) / V001) %>% 
  select(Cod_setor, pct_pess_ppia)

df <- left_join(df, pessoa03, by = "Cod_setor")

# Responsável 02 (Idade): Cálculo ponderado da idade média do responsável
responsavel02_idade <- read_delim("DADOS/infos_2010/Responsavel02_SP1.csv", col_types = cols(Cod_setor = col_character()),
                                  delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), 
                                  trim_ws = TRUE) %>% 
  select(c(1, 3:94)) %>%
  mutate_at(2:93, as.numeric)

# Ponderação das idades pelas colunas
j <- 3
for (i in 10:100) {
  responsavel02_idade[, j] <- responsavel02_idade[, j] * i
  j <- j + 1
}

responsavel02_idade$med_resp_idade <- rowSums(responsavel02_idade[, 3:93]) / responsavel02_idade$V001
df <- left_join(df, select(responsavel02_idade, Cod_setor, med_resp_idade), by = "Cod_setor")

# Domicílio 01: Proporção de domicílios com saneamento adequado
domicilio01 <- read_delim("DADOS/infos_2010/Domicilio01_SP1.csv", 
                          delim = ";", escape_double = FALSE, col_types = cols(Cod_setor = col_character(),
                                                                               V002 = col_number(), V024 = col_number()), 
                          locale = locale(decimal_mark = ",", grouping_mark = "."), trim_ws = TRUE) %>% 
  select(Cod_setor, V002, V024) %>% 
  mutate(pct_dom_san = (V024) / V002) %>% 
  select(Cod_setor, pct_dom_san)

df <- left_join(df, domicilio01, by = "Cod_setor")

# ==============================================================================
# 3. GEOPROCESSAMENTO E MATCHING (PROPENSITY SCORE)
# ==============================================================================

# Definição e georreferenciamento dos CEUs reais (Grupo de Tratamento) com buffer de 1.5km
ceus_1o_lote <- data.frame(
  CEU = c("CIDADE ADEMAR", "CIDADE LIDER", "ERMELINO MATARAZZO", "GRAJAU", "IMPERADOR"),
  LAT = c(-23.671953969833766, -23.55450294404546, -23.48831440932225, -23.762362435094627, -23.588001220820445),
  LONG = c(-46.667613661425506, -46.50667942942734, -46.48120861060095, -46.690674388765686, -46.51585052050288)
) %>%
  st_as_sf(coords = c("LONG", "LAT"), crs = 4326) %>%
  st_transform(31983) %>%
  st_buffer(1500)

# Geração de 5.000 pontos fictícios (Grupo de Controle)
set.seed(24844452)
n_amostra <- 5000
lat_min <- -23.692778
lat_max <- -23.47507
lon_min <- -46.825
lon_max <- -46.365

pontos_ficticios <- data.frame(
  CEU = sprintf("%05d", 1:n_amostra),
  LAT = runif(n_amostra, lat_min, lat_max),
  LONG = runif(n_amostra, lon_min, lon_max)
) %>%
  st_as_sf(coords = c("LONG", "LAT"), crs = 4326) %>%
  st_transform(31983)

# Intersecção de pontos fictícios com a malha territorial e aplicação de buffer
malha_aux <- read_sf("DADOS/malha_2010/35SEE250GC_SIR_sp_utm_corrigida.shp") %>%
  st_transform(31983)

pontos_ficticios_sp <- pontos_ficticios %>%
  st_intersection(malha_aux) %>%
  select(CEU) %>%
  st_buffer(1500)

# Leitura e integração da base completa de CEUs para filtro de controle
aux_ceus <- read_csv("DADOS/infos_ceus/CEUs_georref.csv", 
                     locale = locale(decimal_mark = ",", grouping_mark = ".")) %>%
  select(1:3) %>%
  rename("LAT" = "Latitude", "LONG" = "Longitude")

aux_ceus <- rbind(aux_ceus,
                  data.frame(CEU = c("CIDADE ADEMAR", "CIDADE LIDER", "ERMELINO MATARAZZO", "GRAJAU", "IMPERADOR"),
                             LAT = c(-23.671953969833766, -23.55450294404546, -23.48831440932225, -23.75413816348209, -23.588001220820445),
                             LONG = c(-46.667613661425506, -46.50667942942734, -46.48120861060095, -46.698470595134644, -46.51585052050288))) %>%
  st_as_sf(coords = c("LONG", "LAT"), crs = 4326) %>%
  st_transform(31983) %>%
  st_buffer(1500)

# Remoção de pontos de controle que se sobrepõem a CEUs reais
intersecao_ceus <- pontos_ficticios_sp %>% st_intersection(aux_ceus)
pontos_ficticios_sp <- pontos_ficticios_sp %>% subset(!(CEU %in% intersecao_ceus$CEU))

# Junção da malha cartográfica com dados socioeconômicos
malha <- read_sf("DADOS/malha_2010/35SEE250GC_SIR_sp_utm_corrigida.shp") %>%
  st_transform(31983) %>%
  left_join(df, by = c("CD_GEOCODI" = "Cod_setor"))

# Flag de Tratamento (1 = Real, 0 = Fictício) e união das bases de buffer
ceus_1o_lote$TRAT <- 1
pontos_ficticios_sp$TRAT <- 0

ceus_1o_lote <- rbind(
  ceus_1o_lote[, c("CEU", "geometry", "TRAT")],
  pontos_ficticios_sp[, c("CEU", "geometry", "TRAT")]
)

# Intersecção espacial dos setores censitários dentro dos buffers estipulados
intersecao <- malha %>% st_intersection(ceus_1o_lote)
setores_cortados <- malha %>% 
  filter(apply(st_intersects(geometry, st_buffer(ceus_1o_lote, 1500), sparse = FALSE), 1, any)) %>% 
  filter(!apply(st_within(geometry, st_buffer(ceus_1o_lote, 1500), sparse = FALSE), 1, all))

intersecao <- bind_rows(intersecao, setores_cortados)

# Agregação do perfil socioeconômico médio por área de influência
perfil_ceus <- intersecao %>%
  group_by(CEU, TRAT) %>%
  summarise(
    n_dom = sum(n_dom, na.rm = TRUE),
    n_pess = sum(n_pess, na.rm = TRUE),
    med_rend_dom = mean(med_rend_dom, na.rm = TRUE),
    pct_resp_rend_3_sm_menos = mean(pct_resp_rend_3_sm_menos, na.rm = TRUE),
    pct_resp_fem = mean(pct_resp_fem, na.rm = TRUE),
    pct_resp_alfa = mean(pct_resp_alfa, na.rm = TRUE),
    pct_pess_ppia = mean(pct_pess_ppia, na.rm = TRUE),
    med_resp_idade = mean(med_resp_idade, na.rm = TRUE),
    pct_dom_san = mean(pct_dom_san, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  st_drop_geometry() %>%
  na.omit()

# Remoção manual de pontos de controle com anomalias
outliers <- c('03096', '03004', '00482', '00611', '03564', '00686', '03432', '01421', '03601', '02447', '04240', 
              '01235', '03108', '01386', '03998', '03332', '00437', '03696', '03806', '03771', '00504', '04002', '04322')
perfil_ceus <- subset(perfil_ceus, !(CEU %in% outliers))

# Execução do Matching usando vizinho mais próximo
mod_match <- matchit(
  as.numeric(TRAT) ~ med_rend_dom + pct_resp_rend_3_sm_menos + pct_resp_fem + 
    pct_resp_alfa + pct_pess_ppia + med_resp_idade + pct_dom_san,
  method = "nearest",
  data = perfil_ceus
)

perfil_ceus_m <- match.data(mod_match)

# Anexando informações da subclasse pareada de volta aos CEUs tratados
ceus_1o_lote <- ceus_1o_lote %>%
  left_join(select(perfil_ceus_m, CEU, subclass), by = "CEU")

# ==============================================================================
# 4. SORTEIO E AMOSTRAGEM DE SETORES
# ==============================================================================

# Cálculo da Probabilidade Proporcional ao Tamanho (PPT) por setor
sorteio <- intersecao %>% 
  subset(CEU %in% perfil_ceus_m$CEU) %>% 
  select(CEU, CD_GEOCODI, n_pess) %>% 
  group_by(CEU) %>% 
  mutate(n_pess_CEU = sum(n_pess, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(PPT = n_pess / n_pess_CEU) %>% 
  select(CEU, CD_GEOCODI, PPT) %>% 
  na.omit()

# Sorteio amostral: Ermelino Matarazzo necessita de amostragem maior que os demais
aux <- data.frame()

for (i in sort(unique(sorteio$CEU))) {
  set.seed(1234)
  if (i == "ERMELINO MATARAZZO") {
    aux_1 <- sample_n(subset(sorteio, CEU == i), 36, weight = sorteio$PPT[sorteio$CEU == i], replace = TRUE)
    aux_1$setor_escolhido <- "Sim"
    aux_1$setor_escolhido[13:36] <- "Reserva" # Excedente vai como setor reserva
  } else {
    aux_1 <- sample_n(subset(sorteio, CEU == i), 24, weight = sorteio$PPT[sorteio$CEU == i], replace = TRUE)
    aux_1$setor_escolhido <- "Sim"
    aux_1$setor_escolhido[13:24] <- "Reserva" # Excedente vai como setor reserva
  }
  aux <- rbind(aux, aux_1)
}

aux <- st_drop_geometry(aux)

# Junção das flags de sorteio à base principal
sorteio <- sorteio %>% left_join(select(aux, CEU, CD_GEOCODI, setor_escolhido), by = c("CEU", "CD_GEOCODI"))
sorteio$setor_escolhido[is.na(sorteio$setor_escolhido)] <- "Não"

# Adição das descrições textuais dos setores sorteados
descricao <- read_excel("DADOS/infos_2010/Descrição_Municipio_SP_Capital.xls") %>% 
  clean_names() %>% 
  select(geocodigo, ponto_inicial, descricao_do_perimetro)

aux <- aux %>% left_join(descricao, by = c("CD_GEOCODI" = "geocodigo"))

# Readequando projeção geográfica final para WGS84
st_crs(sorteio) <- 31983
sorteio <- st_transform(sorteio, crs=4326)

# ==============================================================================
# 5. FILTRO ESPACIAL: REMOÇÃO DE SETORES SOBREPOSTOS A FAVELAS
# ==============================================================================

# Leitura e projeção da malha de favelas
malha_favela <- read_sf("DADOS/SIRGAS_SHP_favela/SIRGAS_SHP_favela.shp")
st_crs(malha_favela) <- 31983
malha_favela <- st_transform(malha_favela, crs=4326)

# Separação dos setores por categoria de seleção
setores_escolhidos <- sorteio %>% filter(setor_escolhido == "Sim") %>% distinct()
setores_reserva <- sorteio %>% filter(setor_escolhido == "Reserva") %>% distinct()

# Intersecção geográfica entre setores selecionados e polígonos de favela
intersecao_favela_setores_escolhidos <- st_intersection(setores_escolhidos, malha_favela)
interseccao_favela_setores_reserva <- st_intersection(setores_reserva, malha_favela)

# Remoção ativa dos códigos de setor que apresentaram intersecção
setores_escolhidos_sem_favela <- setores_escolhidos %>% 
  filter(!CD_GEOCODI %in% intersecao_favela_setores_escolhidos$CD_GEOCODI)

setores_reserva_sem_favela <- setores_reserva %>% 
  filter(!CD_GEOCODI %in% interseccao_favela_setores_reserva$CD_GEOCODI)

# Reconstrução da base limpa consolidada
setores_sem_favela <- bind_rows(setores_escolhidos_sem_favela, setores_reserva_sem_favela) %>%
  distinct(CD_GEOCODI, .keep_all = TRUE)

# ==============================================================================
# 6. EXPORTAÇÃO E OUTPUTS DOS DADOS 
# ==============================================================================

# Integração final da tabela de descrição legível para os pesquisadores/entrevistadores
descricao_setores <- read_excel("DADOS/Descrição dos Setores.xls")

resultado <- merge(setores_sem_favela, descricao_setores, by.x = "CD_GEOCODI", by.y = "Geocodigo", all.x = TRUE)
resultado <- resultado[, c("CD_GEOCODI", "CEU", "setor_escolhido", "ponto inicial", "Descrição do perímetro")]
resultado <- resultado %>% mutate(geometry = as.character(geometry)) # Conversão para texto para suportar Excel

# Escrita dos arquivos tabulares
write_xlsx(resultado, "OUTPUTS/resultado.xlsx")
write_xlsx(aux, "OUTPUTS/setores_1o_lote.xlsx")
write.csv2(aux, "OUTPUTS/setores_1o_lote.csv", fileEncoding = "UTF-8", row.names = F)

# Agrupamento da malha por CEU e exportação do RData para rotina de cotas
setores_agrupados <- setores_sem_favela %>%
  group_by(CEU) %>%
  summarise(geometry = first(geometry), .groups = "drop")

save.image(file = "OUTPUTS/workspace.RData")