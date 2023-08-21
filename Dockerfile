FROM quay.io/mojanalytics/rshiny:3.5.1

ENV PATH="/opt/shiny-server/bin:/opt/shiny-server/ext/node/bin:${PATH}"
ENV SHINY_APP=/srv/shiny-server
ENV NODE_ENV=production

WORKDIR /srv/shiny-server

# ENV SHINY_GAID <your google analytics token here>

# Add environment file individually so that next install command
# can be cached as an image layer separate from application code
ADD environment.yml environment.yml

RUN conda env update --file environment.yml -n base

 RUN R -e "install.packages('waffle', repos = 'https://cinc.rud.is')"

# Add shiny app code
ADD . .

USER 998
CMD analytics-platform-shiny-server
EXPOSE 9999
