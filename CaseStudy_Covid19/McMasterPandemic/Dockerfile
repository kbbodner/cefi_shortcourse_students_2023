FROM rocker/verse:latest

ENV RENV_VERSION 1.0.0
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

WORKDIR /project
COPY renv.lock renv.lock

ENV RENV_PATHS_LIBRARY renv/library

RUN R -e "renv::restore()"




