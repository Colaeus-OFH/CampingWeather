FROM rocker/shiny:4.2.1
ENV RENV_CONFIG_REPOS_OVERRIDE https://packagemanager.rstudio.com/cran/latest

RUN apt-get update -qq && \ 
  apt-get install -y --no-install-recommends \
    gdal-bin \
    libcurl4-openssl-dev \
    libgdal-dev \
    libgeos-dev \
    libicu-dev \
    libpng-dev \
    libproj-dev \
    libsodium-dev \
    libssl-dev \
    make \
    pandoc \
    zlib1g-dev && \
  apt-get clean && \ 
  rm -rf /var/lib/apt/lists/*
COPY shiny_renv.lock renv.lock
RUN Rscript -e "install.packages('renv')"
RUN Rscript -e "renv::restore()"
COPY  /srv/shiny-server/
EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
