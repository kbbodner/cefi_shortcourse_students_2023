FROM rocker/tidyverse:devel

RUN apt update
RUN apt install software-properties-common -y

RUN add-apt-repository ppa:marutter/rrutter4.0 -y
RUN add-apt-repository ppa:c2d4u.team/c2d4u4.0+ -y
RUN apt update
RUN apt install r-cran-rstan -y

RUN apt install libudunits2-dev -y
RUN apt install libgdal-dev -y

RUN R -e 'Sys.setenv(DOWNLOAD_STATIC_LIBV8 = 1)'

# install developer tools
RUN R -e 'install.packages(c("devtools","remotes","testthat","here","styler"))'

# install Bayes packages
RUN R -e 'install.packages(c("rstan","tidybayes","bayesplot","rstantools","brms"))'

# install covidseir (depracated)
# RUN R -e 'remotes::install_github("seananderson/covidseir")'

# install bcdata
RUN R -e 'remotes::install_github("bcgov/bcdata")'

EXPOSE 8787

CMD ["/init"]