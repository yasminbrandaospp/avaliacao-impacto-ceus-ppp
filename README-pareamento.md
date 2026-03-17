# Avaliação de Impacto dos CEUs PPP: Pareamento e Sorteio de Setores Censitários

Este repositório contém o script principal (`1-pareamento.R`) responsável pelo desenho amostral da pesquisa de campo para a Avaliação de Impacto dos Centros Educacionais Unificados (CEUs) construídos via Parceria Público-Privada (PPP) na cidade de São Paulo.

## 🎯 Objetivo do Script
O código realiza o pareamento espacial e estatístico (*Propensity Score Matching*) para encontrar áreas de controle (pontos fictícios) sociodemograficamente comparáveis às áreas de influência dos CEUs reais. Em seguida, realiza um sorteio proporcional ao tamanho (PPT) dos setores censitários que serão visitados pela equipe de campo, excluindo áreas com sobreposição a favelas.

---

## 🛠️ Pré-requisitos e Configuração

Para executar este script, você precisará ter o **R** instalado, além das bibliotecas listadas abaixo.

### Instalação de Pacotes
Você pode instalar os pacotes necessários rodando o seguinte comando no console do R:
```R
install.packages(c("MatchIt", "cobalt", "dplyr", "ggplot2", "sf", "readxl", "leaflet", "readr", "stringr", "janitor", "RVAideMemoire", "car", "rstatix", "openxlsx", "writexl"))
