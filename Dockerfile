# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# System libraries required by the app's R packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    build-essential \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev &&\
    rm -rf /var/lib/apt/lists/*

COPY dependencies.R .
RUN R --slave --no-restore -e 'source("dependencies.R")'

COPY /app ./app

# expose port
EXPOSE 3838

USER shiny
# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3838)"]
