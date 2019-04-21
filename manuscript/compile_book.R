#!/usr/bin/Rscript

# ==============================================================================
# author          :Ghislain Vieilledent
# email           :ghislain.vieilledent@cirad.fr, ghislainv@gmail.com
# web             :https://ghislainv.github.io
# license         :CC-BY-SA 4.0
# ==============================================================================

# Libraries
library(bookdown)
library(knitr)
library(kableExtra)

# Working directory
setwd("manuscript")

# Bookdown
# pdf
options(knitr.table.format="latex")
pdf_format <- bookdown::pdf_document2(citation_package="natbib", fig_caption=TRUE, keep_tex=FALSE,
                                      latex_engine="pdflatex", number_sections=TRUE, toc=FALSE,
                                      includes=list(in_header="header.tex", before_body="doc_prefix.tex"))
params <- list(title="",author="",date="")
bookdown::render_book("index.Rmd", output_format=pdf_format)

# html
# Don't indicate output_format to take into account YAML options
options(knitr.table.format="html")
# Dynamic YAML options
title_html <- "It's not just poverty: unregulated domestic market and weak law enforcement explain unceasing deforestation in Western Madagascar"
author_html <- "Ghislain~Vieilledent$^{*,1,2,3}$, Clovis~Grinand$^4$, Miguel~Pedrono$^5$, Tsiky~Rabetrano$^6$, Jean-Roger~Rakotoarijaona$^7$, Bruno~Rakotoarivelo$^7$, Fety~A.~Rakotomalala$^4$, Linjanantenaina~Rakotomalala$^8$, Andriamandimbisoa~Razafimpahanana$^6$, José~Ralison$^9$, and Marie~Nourtier$^4$"
date_html <- format(Sys.time(), "%d %B, %Y")
params <- list(title=title_html,author=author_html ,date=date_html)
bookdown::render_book("index.Rmd")
