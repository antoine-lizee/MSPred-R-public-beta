## This dockerfile is used to build the image that is used to run the code.
# It ensures package version consistency and reproducibility

## Commands:
# Build:       docker build -t antoinelizee/mspred . --no-cache --progress plain
# Run (dev):   docker run -e DISABLE_AUTH=true -e ROOT=TRUE -p 8787:8787 -d --rm --name mspred -v "$PWD:/mspred" antoinelizee/mspred
#      with:   docker exec -ti mspred /bin/bash
# Run (repro): TBD

## Resources
# See https://github.com/rocker-org/rocker-versioned

ARG R_VERSION=3.3.3

FROM rocker/tidyverse:${R_VERSION}

# Install java for XLConnect
RUN apt-get update && \
    DEBIAN_FRONTEND=noninteractive \
    apt-get -y install default-jre-headless default-jdk libbz2-dev liblzma-dev libpcre3-dev libicu-dev && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

COPY install_packages.R mspred/install_packages.R
RUN Rscript /mspred/install_packages.R
COPY . mspred
