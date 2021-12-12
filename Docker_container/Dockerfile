
FROM openanalytics/r-base

MAINTAINER Tobias Verbeke "tobias.verbeke@openanalytics.eu"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.1 \
    && rm -rf /var/lib/apt/lists/*

# system library dependency for the shiny app
RUN apt-get update && apt-get install -y \
    libmpfr-dev \
    && rm -rf /var/lib/apt/lists/*

# install packages
RUN R -e "install.packages(c('shiny', 'rmarkdown', 'DT', 'data.table','shinythemes' ,'shinyWidgets', 'DT', 'tidyverse', 'shinydashboard', 'shinydashboardPlus','data.table', 'fresh','shinyjs', 'shinyBS','openxlsx'), repos='https://cloud.r-project.org/')"

#'dplyr','tibble','readr'
## install dependencies if required
#RUN R -e "install.packages('DT', repos='https://cloud.r-project.org/')"

################################################################################
# Python is not required
################################################################################

#RUN apt-get update && \
#    apt-get install -y python3 python3-pip && \
#    rm -rf /var/lib/apt/lists/*
#
## Dash and dependencies
#RUN pip3 install matplotlib

# copy the app to the image
RUN mkdir /root/app/
COPY app /root/app/

COPY Rprofile.site /usr/lib/R/etc/
#RUN mkdir /root/data/
#COPY data /root/data/
#RUN mkdir /root/www/
#COPY www /root/www/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/app')"]


# docker build -t duplicate-finder .
# docker run -it -p 3838:3838 duplicate-finder
# docker network create virtual-net
# docker build . -t sp-example
# docker run -d -v /var/run/docker.sock:/var/run/docker.sock --net virtual-net -p 8080:8080 sp-example
# "C:\Users\mam\Documents\R\R-4.1.0\bin\Rscript.exe" -e "shiny::runApp('app', launch.browser = TRUE)