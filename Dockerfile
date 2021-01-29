FROM rstudio/plumber:latest

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    #libmariadbd-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libudunits2-dev \
    libgdal-dev \
    odbc-postgresql

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# install renv & restore packages
RUN install2.r lubridate dplyr DBI RPostgres pool plumber dbplyr

COPY plumber.R ./plumber.R

EXPOSE 8000

USER 1001

ENTRYPOINT ["R", "-e", \
"plumber_api <- plumber::plumb('plumber.R'); \
plumber_api$run(host = '0.0.0.0', port= 8000)"]

CMD ["./plumber.R"]