FROM ghcr.io/ministryofjustice/analytical-platform-rshiny-open-source-base:1.3.0

ENV STRINGI_DISABLE_PKG_CONFIG=true \
  AWS_DEFAULT_REGION=eu-west-1 \
  TZ=Etc/UTC \
  LC_ALL=C.UTF-8


WORKDIR /srv/shiny-server

# Cleanup shiny-server dir
RUN rm -rf ./*

# Make sure the directory for individual app logs exists
RUN mkdir -p /var/log/shiny-server

# Make sure reticulate uses the system Python
ENV RETICULATE_PYTHON="/usr/bin/python3" \
  SHINY_LOG_LEVEL=TRACE

# Install python3 and essentials
RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    libglpk-dev \
  && apt-get install -y \
    python3 \
    python3-pip \
    python3-venv \
    python3-dev \
    libxml2 \
    pandoc

# Add R package requirements and install
ADD renv.lock renv.lock

RUN R -e "install.packages('renv'); renv::restore()"

# Add Python package requirements and install
COPY requirements.txt .
RUN python3 -m pip install -r requirements.txt

# Add shiny app code etc
COPY . .
RUN chown shiny:shiny /srv/shiny-server

USER 998

CMD analytics-platform-shiny-server
EXPOSE 9999
