
<p align="center">

<picture>
  <!-- Image displayed when the user is in Dark Mode -->
  <source media="(prefers-color-scheme: dark)" srcset="Figuras/Logo_course_repo.png" width="600" height="300">
  <!-- Image displayed when the user is in Light Mode (and fallback) -->
  <img alt="Describe your figure here" src="Figuras/Logo_course_repo_white.png" width="600" height="300">
</picture>

</p>




-------


<br>


### Acessando o material da disciplina

Este repositório contém todo o material da disciplina ministrada para o Programa de Pós-Graduação em Ecologia (PPGECO - UFRN) durante o segundo semestre de 2026.

A página da disciplina foi toda construída usando apenas o [R Markdown][], com o código fonte acessível nos arquivos `Rmd`. Para gerar o site, você precisará instalar (ou atualizar) os pacotes `rmarkdown` e `knitr`.

- Para acessar o material da disciplina, siga as instruções abaixo:

   - (1) Copie (ou fork) esse repositório
   - (2) Apague o diretório `site_libs/`
   - (3) Abra o R nesse diretório, carregue os pacotes e renderize o site com
   `render_site()`, conforme indicado abaixo
   - (4) Clique no arquivo *index.html* e abra-o com a opção *browser*

<br>

```r

# Compilando o site da disciplina
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
library(knitr)
library(rmarkdown)
render_site()


```

### Licença

O conteúdo deste repositório, das páginas, e do material da disciplina está está disponível por meio da [Licença Creative Commons 4.0][] (Atribuição/NãoComercial/PartilhaIgual).

![Licença Creative Commons 4.0](Figuras/CC_by-nc-sa_88x31.png)


[Licença Creative Commons 4.0]: https://creativecommons.org/licenses/by-nc-sa/4.0/deed.pt_BR
[R Markdown]: http://rmarkdown.rstudio.com
