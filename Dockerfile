FROM rocker/shiny:4.4.3

RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
		git \
		cmake \
		default-jdk \
		libglpk-dev \
		libicu-dev \
		libmagick++-dev \
		libnode-dev \
		libssl-dev \
		libxml2-dev \
		perl \
	&& R CMD javareconf \
	&& apt-get clean && rm -rf /var/lib/apt/lists/*

WORKDIR /srv/shiny-server/bmdx2

COPY renv.lock ./renv.lock

RUN R -e "install.packages('renv', repos = c(CRAN = 'https://cran.rstudio.com'))"

RUN R -e "renv::consent(provided = TRUE); renv::restore(clean = TRUE)"

COPY report.Rmd run_app.R server.R ui.R /srv/shiny-server/bmdx2/
COPY server_modules/ /srv/shiny-server/bmdx2/server_modules/
COPY ui_functions/  /srv/shiny-server/bmdx2/ui_functions/
COPY shiny-server.conf  /etc/shiny-server/shiny-server.conf

EXPOSE 3838

CMD ["Rscript", "/srv/shiny-server/bmdx2/run_app.R"]
