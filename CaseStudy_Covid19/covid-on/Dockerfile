FROM rocker/verse:latest


RUN apt-get update \
	&& apt-get install -y --no-install-recommends apt-utils ed libnlopt-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/

# Install rstan
RUN install2.r --error --deps TRUE \
    rstan \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# Global site-wide config -- neeeded for building packages
RUN mkdir -p $HOME/.R/ \
    && echo "CXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function -flto -ffat-lto-objects  -Wno-unused-local-typedefs \n" >> $HOME/.R/Makevars

# Config for rstudio user
RUN mkdir -p $HOME/.R/ \
    && echo "CXXFLAGS=-O3 -mtune=native -march=native -Wno-unused-variable -Wno-unused-function -flto -ffat-lto-objects  -Wno-unused-local-typedefs -Wno-ignored-attributes -Wno-deprecated-declarations\n" >> $HOME/.R/Makevars \
    && echo "rstan::rstan_options(auto_write = TRUE)\n" >> /home/rstudio/.Rprofile \
    && echo "options(mc.cores = parallel::detectCores())\n" >> /home/rstudio/.Rprofile

RUN R -e 'install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))'

# Install rstan dependencies and other necessary packages
RUN install2.r --error --deps TRUE \
    rstan \
	loo \
	bayesplot \
	tidybayes \
    rstanarm \
    rstantools \
    brms \
    here \
    shinystan \
    remotes \
    tmbstan \
    && rm -rf /tmp/downloaded_packages/ /tmp/*.rds

# Install McMasterPandemic
RUN R -e "remotes::install_github('mac-theobio/McMasterPandemic@tmb-condense')"





