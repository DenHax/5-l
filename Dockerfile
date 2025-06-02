FROM rocker/shiny-verse:latest

RUN R -e "install.packages(c('ggplot2', 'dplyr', 'xts', 'DT', 'jsonlite', 'igraph'), repos='https://cloud.r-project.org/')"

COPY app.R /srv/shiny-server/

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
