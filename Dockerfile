FROM ghcr.io/ministryofjustice/analytical-platform-rshiny-open-source-base:1.3.0

# Make sure reticulate uses the system Python
ENV RETICULATE_PYTHON="/usr/bin/python3"

WORKDIR /srv/shiny-server

# Install python3 and essentials
RUN apt-get update \
  && apt-get install -y \
    python3 \
    python3-pip \
    python3-venv \
    python3-dev

# Add R package requirements and install
ADD renv.lock renv.lock
RUN R --vanilla -s -e 'renv::restore()'

# Add Python package requirements and install
COPY requirements.txt .
RUN python3 -m pip install -r requirements.txt

# Add shiny app code etc
COPY . .
RUN chown shiny:shiny /srv/shiny-server

USER 998

CMD analytics-platform-shiny-server
EXPOSE 9999
