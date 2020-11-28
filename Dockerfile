FROM rocker/tidyverse:4.0.3 as builder

ARG EVALUATOR_VERSION
ENV BUILD_DATE=2020-11-27

LABEL org.opencontainers.image.licenses="MIT" \
      org.opencontainers.image.source="https://github.com/davidski/collector" \
      org.opencontainers.image.documentation="https://collector.tidyrisk.org" \
      maintainer="David F. Severski <davidski@deadheaven.com>" \
      org.openctainers.image.authors="David F. Severski <davidski@deadheaven.com>"

# collector doesn't need shiny
#RUN export ADD=shiny && bash /etc/cont-init.d/add

COPY . /src/
WORKDIR /src

RUN apt-get update \
    && apt-get install -y zlib1g-dev libproj-dev libcairo2-dev libmagick++-dev \
    && install2.r --deps=TRUE remotes \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

# Silence pragma warnings for RcppEigen
# Reference https://github.com/kaskr/adcomp/issues/277#issuecomment-400191014
COPY /scripts/Makevars /root/.R/Makevars

RUN apt-get clean \
    && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

VOLUME /data

EXPOSE 8787
EXPOSE 3838
