# ==============================================================================
# 1. CONFIGURAÇÕES INICIAIS E PACOTES
# ==============================================================================

library(tidyverse)
library(sf)
library(writexl)

# Carrega o workspace gerado no script anterior (contém setores_agrupados, setores_sem_favela, etc.)
load(file = "OUTPUTS/workspace.RData")

# ==============================================================================
# 2. LEITURA E TRATAMENTO DOS DADOS DEMOGRÁFICOS (CENSO 2010)
# ==============================================================================

# Dados Básicos: Seleção de domicílios, pessoas e renda média
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

# Pessoa 11 (Homens): Agrupamento populacional masculino por faixas etárias
pessoa11 <- read_delim("DADOS/infos_2010/Pessoa11_SP1.csv", col_types = cols(Cod_setor = col_character()),
                       delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), 
                       trim_ws = TRUE) %>% 
  select(c(1, 3, 24, 37:136)) %>%
  mutate_at(2:103, as.numeric) %>% 
  rename("n_homem" = "V001")

# Cálculo das faixas de idade (Homens)
pessoa11$n_homem_18mais <- pessoa11$n_homem - rowSums(select(pessoa11, V022:V051))
pessoa11$n_homem_18a24  <- rowSums(select(pessoa11, V052:V058))
pessoa11$n_homem_25a34  <- rowSums(select(pessoa11, V059:V068))
pessoa11$n_homem_35a44  <- rowSums(select(pessoa11, V069:V078))
pessoa11$n_homem_45a59  <- rowSums(select(pessoa11, V079:V093))
pessoa11$n_homem_60mais <- rowSums(select(pessoa11, V094:V134))

# Consolidação da base masculina
pessoa11 <- pessoa11 %>% select(Cod_setor, n_homem_18mais, n_homem_18a24, n_homem_25a34, n_homem_35a44, n_homem_45a59, n_homem_60mais)
df <- basico %>% left_join(pessoa11, by = "Cod_setor")

# Pessoa 12 (Mulheres): Agrupamento populacional feminino por faixas etárias
pessoa12 <- read_delim("DADOS/infos_2010/Pessoa12_SP1.csv", col_types = cols(Cod_setor = col_character()),
                       delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",", grouping_mark = "."), 
                       trim_ws = TRUE) %>% 
  select(c(1, 3, 24, 37:136)) %>%
  mutate_at(2:103, as.numeric) %>% 
  rename("n_mulher" = "V001")

# Cálculo das faixas de idade (Mulheres)
pessoa12$n_mulher_18mais <- pessoa12$n_mulher - rowSums(select(pessoa12, V022:V051))
pessoa12$n_mulher_18a24  <- rowSums(select(pessoa12, V052:V058))
pessoa12$n_mulher_25a34  <- rowSums(select(pessoa12, V059:V068))
pessoa12$n_mulher_35a44  <- rowSums(select(pessoa12, V069:V078))
pessoa12$n_mulher_45a59  <- rowSums(select(pessoa12, V079:V093))
pessoa12$n_mulher_60mais <- rowSums(select(pessoa12, V094:V134))

# Consolidação da base feminina e união com df principal
pessoa12 <- pessoa12 %>% select(Cod_setor, n_mulher_18mais, n_mulher_18a24, n_mulher_25a34, n_mulher_35a44, n_mulher_45a59, n_mulher_60mais)
df <- df %>% left_join(pessoa12, by = "Cod_setor")

# ==============================================================================
# 3. GEOPROCESSAMENTO E RECORTES ESPACIAIS
# ==============================================================================

# Integração da malha cartográfica com os dados demográficos
malha <- read_sf("DADOS/malha_2010/35SEE250GC_SIR_sp_utm_corrigida.shp") %>%
  st_transform(31983) %>%
  left_join(df, by = c("CD_GEOCODI" = "Cod_setor"))

# Criação de área de influência (Buffer de 1km) ao redor dos agrupamentos de setores
setores_agrupados <- setores_agrupados %>% st_transform(31983)
buffer_1km <- st_buffer(setores_agrupados, dist = 1000)

# Filtragem apenas dos setores contidos dentro do buffer criado
setores_dentro_buffer <- malha %>% 
  st_join(buffer_1km, join = st_within) %>% 
  filter(!is.na(CEU))

# Separação das variáveis essenciais para o cálculo das cotas
resultante <- setores_dentro_buffer %>%
  select(CEU, CD_GEOCODI, n_dom, n_pess, med_rend_dom, 
         n_homem_18mais, n_homem_18a24, n_homem_25a34, n_homem_35a44, n_homem_45a59, n_homem_60mais, 
         n_mulher_18mais, n_mulher_18a24, n_mulher_25a34, n_mulher_35a44, n_mulher_45a59, n_mulher_60mais)

# ==============================================================================
# 4. CÁLCULO DAS COTAS AMOSTRAIS E CORREÇÃO DE PESOS
# ==============================================================================

# Agregação do perfil sociodemográfico a nível de CEU
perfil_ceus <- resultante %>%
  group_by(CEU) %>% 
  summarise(n_homem_18mais  = sum(n_homem_18mais, na.rm = T),
            n_mulher_18mais = sum(n_mulher_18mais, na.rm = T),
            n_pess_18a24    = sum(n_homem_18a24, na.rm = T) + sum(n_mulher_18a24, na.rm = T),
            n_pess_25a34    = sum(n_homem_25a34, na.rm = T) + sum(n_mulher_25a34, na.rm = T),
            n_pess_35a44    = sum(n_homem_35a44, na.rm = T) + sum(n_mulher_35a44, na.rm = T),
            n_pess_45a59    = sum(n_homem_45a59, na.rm = T) + sum(n_mulher_45a59, na.rm = T),
            n_pess_60mais   = sum(n_homem_60mais, na.rm = T) + sum(n_mulher_60mais, na.rm = T)) %>%
  mutate(n_pess            = n_homem_18mais + n_mulher_18mais,
         pct_homem_18mais  = n_homem_18mais / n_pess,
         pct_mulher_18mais = n_mulher_18mais / n_pess,
         pct_pess_18a24    = n_pess_18a24 / n_pess,
         pct_pess_25a34    = n_pess_25a34 / n_pess,
         pct_pess_35a44    = n_pess_35a44 / n_pess,
         pct_pess_45a59    = n_pess_45a59 / n_pess,
         pct_pess_60mais   = n_pess_60mais / n_pess) %>% 
  select(CEU, n_pess,
         pct_homem_18mais, pct_mulher_18mais, 
         pct_pess_18a24, pct_pess_25a34, pct_pess_35a44, pct_pess_45a59, pct_pess_60mais) %>% 
  st_drop_geometry()

# Aplicação de fatores de correção temporal/demográfica nas faixas etárias
perfil_ceus$pct_pess_18a24_cor  <- perfil_ceus$pct_pess_18a24 * 0.69
perfil_ceus$pct_pess_25a34_cor  <- perfil_ceus$pct_pess_25a34 * 0.79
perfil_ceus$pct_pess_35a44_cor  <- perfil_ceus$pct_pess_35a44 * 1.06
perfil_ceus$pct_pess_45a59_cor  <- perfil_ceus$pct_pess_45a59 * 1.10
perfil_ceus$pct_pess_60mais_cor <- perfil_ceus$pct_pess_60mais * 1.42

# Normalização das porcentagens corrigidas para fecharem em 100%
perfil_ceus$soma_cor <- rowSums(perfil_ceus[, 10:14])

perfil_ceus$pct_pess_18a24_cor  <- perfil_ceus$pct_pess_18a24_cor / perfil_ceus$soma_cor
perfil_ceus$pct_pess_25a34_cor  <- perfil_ceus$pct_pess_25a34_cor / perfil_ceus$soma_cor
perfil_ceus$pct_pess_35a44_cor  <- perfil_ceus$pct_pess_35a44_cor / perfil_ceus$soma_cor
perfil_ceus$pct_pess_45a59_cor  <- perfil_ceus$pct_pess_45a59_cor / perfil_ceus$soma_cor
perfil_ceus$pct_pess_60mais_cor <- perfil_ceus$pct_pess_60mais_cor / perfil_ceus$soma_cor

# Transformação das porcentagens em número absoluto de entrevistas (Base = 1065)
perfil_ceus$n_entrevistas_homem_18mais  <- round(perfil_ceus$pct_homem_18mais * 1065, 0)
perfil_ceus$n_entrevistas_mulher_18mais <- round(perfil_ceus$pct_mulher_18mais * 1065, 0)
perfil_ceus$n_entrevistas_pess_18a24    <- round(perfil_ceus$pct_pess_18a24_cor * 1065, 0)
perfil_ceus$n_entrevistas_pess_25a34    <- round(perfil_ceus$pct_pess_25a34_cor * 1065, 0)
perfil_ceus$n_entrevistas_pess_35a44    <- round(perfil_ceus$pct_pess_35a44_cor * 1065, 0)
perfil_ceus$n_entrevistas_pess_45a59    <- round(perfil_ceus$pct_pess_45a59_cor * 1065, 0)
perfil_ceus$n_entrevistas_pess_60mais   <- round(perfil_ceus$pct_pess_60mais_cor * 1065, 0)

# Exporta o planejamento das cotas
write_xlsx(perfil_ceus, "OUTPUTS/cotas_1o_lote.xlsx")

# ==============================================================================
# 5. DISTRIBUIÇÃO DAS ENTREVISTAS POR SETOR (CÁLCULO DE METAS)
# ==============================================================================

# Integra os cálculos aos setores sem favela e conta a quantidade de setores por CEU
setores_com_entrevistas <- setores_sem_favela %>%
  left_join(perfil_ceus, by = "CEU") %>%
  group_by(CEU) %>%
  mutate(n_setores = n()) %>%
  ungroup()

# Divide as cotas do CEU igualmente entre os seus respectivos setores
setores_com_entrevistas <- setores_com_entrevistas %>%
  mutate(
    entrevistas_homem_18mais  = round(n_entrevistas_homem_18mais / n_setores),
    entrevistas_mulher_18mais = round(n_entrevistas_mulher_18mais / n_setores),
    entrevistas_18a24         = round(n_entrevistas_pess_18a24 / n_setores),
    entrevistas_25a34         = round(n_entrevistas_pess_25a34 / n_setores),
    entrevistas_35a44         = round(n_entrevistas_pess_35a44 / n_setores),
    entrevistas_45a59         = round(n_entrevistas_pess_45a59 / n_setores),
    entrevistas_60mais        = round(n_entrevistas_pess_60mais / n_setores)
  ) %>%
  select(CEU, CD_GEOCODI,
         entrevistas_homem_18mais, entrevistas_mulher_18mais,
         entrevistas_18a24, entrevistas_25a34, entrevistas_35a44,
         entrevistas_45a59, entrevistas_60mais)

# ==============================================================================
# 6. EXPORTAÇÃO E OUTPUTS FINAL
# ==============================================================================

# Cruza as metas de entrevistas com os dados reais de domicílios/pessoas do setor
domicilio_pessoas <- merge(setores_com_entrevistas, df[, c("Cod_setor", "n_dom", "n_pess")],
                           by.x = "CD_GEOCODI", by.y = "Cod_setor", all.x = TRUE)

# Calcula o impacto da amostra em relação à população total do setor
domicilio_pessoas$perceutal_entrevistados_total_pessoas <- 
  ((domicilio_pessoas$entrevistas_homem_18mais + domicilio_pessoas$entrevistas_mulher_18mais) / 
     domicilio_pessoas$n_pess) * 100 

# Remove geometria (sf) para permitir a exportação em formato Excel
domicilio_pessoas_sem_geom <- st_drop_geometry(domicilio_pessoas)

# Salva arquivo com metas detalhadas para campo
write_xlsx(domicilio_pessoas_sem_geom, "OUTPUTS/domicilio_pessoas.xlsx")