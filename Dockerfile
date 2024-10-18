# The base docker image
FROM ghcr.io/ministryofjustice/analytical-platform-rshiny-open-source-base:1.3.0

# ** Optional step: only if some of R pakcages requires the system libraries which are not covered by base image
#   the one in the example below has been provided in base image.
# RUN apt-get update \
#   && apt-get install -y --no-install-recommends \
#     libglpk-dev


# use renv for packages
ADD renv.lock renv.lock

# Install R packages
RUN R -e "install.packages('renv'); renv::restore()"

# ** Optional step: only if the app requires python packages
# Make sure reticulate uses the system Python
 ENV RETICULATE_PYTHON="/usr/bin/python3"
# ensure requirements.txt exists (created automatically when making a venv in renv)
 COPY requirements.txt requirements.txt
 RUN python3 -m pip install -r requirements.txt

# Add shiny app code
ADD . .

USER 998