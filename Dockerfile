FROM rocker/r-ver:4.4.2

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libudunits2-dev \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY DESCRIPTION NAMESPACE LICENSE ./
COPY R ./R
COPY inst ./inst
COPY tests ./tests
COPY README.md .

RUN R -q -e "install.packages(c('remotes', 'pak'))"
RUN R -q -e "pak::pkg_install(c('arrow', 'DBI', 'duckdb', 'furrr', 'future', 'httr2', 'jsonlite', 'logger', 'purrr', 'rlang', 'stringr', 'tibble', 'tidyr', 'xml2', 'yaml', 'testthat', 'withr', 'shiny'))"
RUN R -q -e "pak::local_install('.')"

EXPOSE 3838

CMD ["R", "-q", "-e", "shiny::runApp('app', host = '0.0.0.0', port = 3838)"]

