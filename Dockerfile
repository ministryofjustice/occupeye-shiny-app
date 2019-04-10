FROM quay.io/mojanalytics/rshiny:3.5.1
# FROM quay.io/mojanalytics/rshiny@sha256:4501e2af32f915aa2f2b652f07ff61d76d35c723843f3c4c1fe6e253d4463d7a
SHELL ["/bin/bash", "-c"]
ENV AWS_DEFAULT_REGION eu-west-1
WORKDIR /srv/shiny-server

# Add environment file individually so that next install command
# can be cached as an image layer separate from application code
ADD environment.yml environment.yml

# Install packrat itself then packages from packrat.lock
RUN conda env update --file environment.yml -n base

## -----------------------------------------------------
## Uncomment if still using packrat alongside conda
## Install packrat itself then packages from packrat.lock
# ADD packrat packrat
# RUN R -e "install.packages('packrat'); packrat::restore()"
## ------------------------------------------------------

# Add shiny app code
ADD . .

# temporarily run this way until we can figure out how to run shiny-server in conda
CMD R -e "library(shiny); runApp(host='0.0.0.0', port=80)"

#CMD bash