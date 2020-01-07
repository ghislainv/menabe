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
bookdown::render_book("index.Rmd", output_format=pdf_format, clean=TRUE)

# html
# Don't indicate output_format to take into account YAML options
options(knitr.table.format="html")
# Dynamic YAML options
title_html <- "It's not just poverty: unregulated global market and bad governance explain unceasing deforestation in Western Madagascar"
author_html <- "Ghislain Vieilledent, Marie Nourtier, Clovis Grinand, Miguel Pedrono, Alison Clausen, Tsiky Rabetrano, Jean-Roger Rakotoarijaona, Bruno Rakotoarivelo, Fety A. Rakotomalala, Linjanantenaina Rakotomalala, Andriamandimbisoa Razafimpahanana, José M. Ralison, and Frédéric Achard"
date_html <- format(Sys.time(), "%d %B, %Y")
params <- list(title=title_html,author=author_html, date=date_html)
bookdown::render_book("index.Rmd")

# End
                      