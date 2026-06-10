## ReefCloud FRK Model Application
## Uses reefcloud-base-image from ECR with all R dependencies pre-installed
## Build time: ~5-10 minutes (vs 60-90 minutes for base image)

ARG BASE_IMAGE_VERSION=2024.11.1
ARG ECR_REGISTRY=255329909679.dkr.ecr.ap-southeast-2.amazonaws.com

FROM ${ECR_REGISTRY}/reefcloud-base-image:${BASE_IMAGE_VERSION}

ARG IMAGE_VERSION
LABEL maintainer="ReefCloud Team <reefcloud@aims.gov.au>"
LABEL description="ReefCloud FRK/INLA spatial-temporal modeling application"
LABEL version="${IMAGE_VERSION}"
LABEL base-image="reefcloud-base-image:${BASE_IMAGE_VERSION}"

## Install AWS CLI v2
RUN apt-get update && apt-get install -y --no-install-recommends \
  curl \
  unzip \
  && curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip" \
  && unzip awscliv2.zip \
  && ./aws/install --install-dir /usr/local/aws-cli --bin-dir /usr/local/bin \
  && rm -rf awscliv2.zip aws \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/* \
  && which aws \
  && aws --version \
  && ls -la /usr/local/bin/aws

## Ensure PATH includes /usr/local/bin
ENV PATH="/usr/local/bin:${PATH}"

## OpenBLAS threading: MUST be 1 on Fargate.
## Multi-threaded OpenBLAS (>=2) causes "singular matrix" errors in FRK's
## Cholesky decomposition on Fargate vCPUs (confirmed with 4 threads).
## Single-threaded is stable but makes MACROALGAE slow (~10h).
ENV OPENBLAS_NUM_THREADS=1

## Copy R package source files
COPY R/ /tmp/reefCloudPackage/R/
COPY inst/ /tmp/reefCloudPackage/inst/
COPY DESCRIPTION /tmp/reefCloudPackage/DESCRIPTION
COPY NAMESPACE /tmp/reefCloudPackage/NAMESPACE
COPY VERSION /tmp/reefCloudPackage/inst/VERSION

## Install reefCloudPackage from local source (includes all optimizations)
RUN R -e "remotes::install_local('/tmp/reefCloudPackage', upgrade = 'never', dependencies = FALSE, force = TRUE);" \
  && rm -rf /tmp/reefCloudPackage

## Copy application files
COPY 00_main.R /home/project/
COPY scripts/entrypoint.sh /home/project/
COPY VERSION /home/project/VERSION

## Create data directories
RUN mkdir -p /data \
  && mkdir -p /data/raw /data/primary /data/processed /data/modelled \
  && mkdir -p /data/outputs/tier /data/outputs/site \
  && chmod +x /home/project/entrypoint.sh

WORKDIR /home/project

## Verification
RUN R -e "cat('=== Application Image Build Complete ===\n'); \
  cat('reefCloudPackage version:', as.character(packageVersion('reefCloudPackage')), '\n'); \
  cat('status version:', as.character(packageVersion('status')), '\n'); \
"

## Set entrypoint
ENTRYPOINT ["/home/project/entrypoint.sh"]

## Default command (run the model)
CMD ["--help"]
