# GLM e GAM: Modelos Lineares Generalizados e Aditivos
### Repositório da Disciplina

## 1. Sobre este Repositório

Este repositório armazena todos os materiais de estudo, *scripts* de aulas práticas e relatórios desenvolvidos para a disciplina de **Modelos Lineares Generalizados (GLM) e Modelos Aditivos Generalizados (GAM)**, do Programa de Pós-Graduação em Ecologia da UFRN.

O objetivo principal é servir como um portfólio e um guia de consulta para a aplicação prática desses modelos estatísticos na análise de dados, com foco principal em dados ecológicos e biológicos.

## 2. Conteúdo do Repositório

Os materiais estão organizados nas seguintes pastas:

* `/Dados`: Contém os conjuntos de dados (ex: `.rds`, `.csv`) utilizados nas análises das aulas práticas. Inclui dados como `palmerpenguins_extended.rds` e `fisheries.rds`.
* `/Scripts_R`: Armazena os *scripts* de R (`.R`) e R Markdown (`.Rmd`) desenvolvidos em cada aula prática.
* `/Relatorios`: Contém os relatórios finais (`.html`) gerados a partir dos arquivos R Markdown, detalhando o passo a passo, os resultados e a interpretação dos modelos.

## 3. Ferramentas e Principais Pacotes R

Todas as análises são desenvolvidas na linguagem **R** e apresentadas usando **R Markdown**. Os principais pacotes utilizados incluem:

* **Modelagem:**
    * `glmmTMB`: Para ajuste de GLMs Gama e Log-Normal.
    * `VGAM`: Para ajuste do modelo multinomial (`vglm`).
    * `nnet`: Alternativa para o modelo multinomial (`multinom`).
    * `mgcv`: (Principal pacote para GAMs).
* **Diagnóstico e Validação:**
    * `DHARMa`: Para diagnóstico de resíduos simulados.
    * `performance`: Para checagem geral dos pressupostos do modelo.
* **Manipulação e Visualização:**
    * `dplyr` e `tidyr`: Para manipulação e organização dos dados.
    * `ggplot2`: Para visualização exploratória dos dados.
    * `ggeffects`: Para visualização dos efeitos marginais e predições dos modelos.
