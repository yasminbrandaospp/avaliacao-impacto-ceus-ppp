# Avaliação de Impacto dos CEUs PPP: Distribuição de Cotas de Entrevista

Este repositório contém o script secundário (`2-cotas_entrevista.R`) da avaliação de impacto dos Centros Educacionais Unificados (CEUs) PPP na cidade de São Paulo. 

## 🎯 Objetivo do Script
Após o sorteio das rotas e setores censitários no script de pareamento, este código calcula **quem** deve ser entrevistado em cada local. Ele dimensiona cotas de entrevistas por perfil demográfico (sexo e faixa etária) para garantir representatividade estatística. O script também aplica pesos de correção (pós-estratificação) para atualizar os dados defasados do Censo de 2010 frente às projeções demográficas contemporâneas de São Paulo (Censo 2022).

---

## 🛠️ Pré-requisitos e Configuração

Este código é o "passo 2" da esteira analítica. Ele depende do arquivo de memória (`workspace.RData`) gerado ao final da execução do script `1-pareamento.R`.

### Instalação de Pacotes
Certifique-se de ter os seguintes pacotes instalados no seu ambiente R:
```R
install.packages(c("tidyverse", "sf", "writexl"))
