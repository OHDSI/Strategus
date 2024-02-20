FROM ohdsi/broadsea-hades:4.2.1
LABEL Anthony Sena <sena@ohdsi.org>

# install OS dependencies including java and python 3
# NOTE: Added iputils-ping since api.github.com was timing out locally
RUN apt-get update && apt-get install -y r-cran-rjava openjdk-11-jdk libsecret-1-0 libsodium23 libxml2-dev libglpk-dev liblzma-dev libbz2-dev libncurses5-dev curl python3-dev python3.venv \
&& R CMD javareconf \
&& rm -rf /var/lib/apt/lists/*

# Workaround to allow for communication on J&J laptop
COPY ZscalerRootCA.crt /root/ZscalerRootCA.crt
RUN cat /root/ZscalerRootCA.crt >> /etc/ssl/certs/ca-certificates.crt
COPY ZscalerRootCA.crt /usr/local/share/ca-certificates
RUN update-ca-certificates

# install utility R packages - use the latest versions of the packages 
# which is why we specify the repo argument
RUN install2.r --error --skipinstalled --repos "https://packagemanager.posit.co/cran/latest" \
    ParallelLogger \
    rJava \
    SqlRender \
    DatabaseConnector \
&& rm -rf /tmp/download_packages/ /tmp/*.rds

# NOTE: Installing RMM and Strategus from source since remotes::install_github is failing
RUN --mount=type=secret,id=build_github_pat \
    cp /usr/local/lib/R/etc/Renviron /tmp/Renviron \
    && echo "GITHUB_PAT=$(cat /run/secrets/build_github_pat)" >> /usr/local/lib/R/etc/Renviron \
    && echo "INSTATIATED_MODULES_FOLDER=/home/ohdsi/strategus/modules" >> /usr/local/lib/R/etc/Renviron.site \
    && echo "DATABASECONNECTOR_JAR_FOLDER=/opt/hades/jdbc_drivers" >> /usr/local/lib/R/etc/Renviron \
    && R -e "remotes::install_github(repo = 'OHDSI/Strategus', upgrade = 'always')" \
    && cp /tmp/Renviron /usr/local/lib/R/etc/Renviron

# RUN --mount=type=secret,id=build_github_pat \
#     && echo "GITHUB_PAT=$(cat /run/secrets/build_github_pat)" >> /usr/local/lib/R/etc/Renviron \
#     && R <<EOF
#     print(Sys.getenv("INSTANTIATED_MODULES_FOLDER")) 
#     sampleAnalysisSpecifications <- system.file("testdata/analysisSpecification.json", package = "Strategus")
#     print(sampleAnalysisSpecifications)
#     analysisSpecifications <- ParallelLogger::loadSettingsFromJson(
#         fileName = sampleAnalysisSpecifications
#     )
#     Strategus::ensureAllModulesInstantiated(analysisSpecifications)
#     EOF

