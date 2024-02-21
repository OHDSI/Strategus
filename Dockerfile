FROM ohdsi/broadsea-hades:4.2.1
LABEL Anthony Sena <sena@ohdsi.org>

# install OS dependencies including java and python 3
# NOTE: Added iputils-ping since api.github.com was timing out locally
RUN apt-get update && apt-get install -y r-cran-rjava openjdk-11-jdk libsecret-1-0 libsodium23 libxml2-dev libglpk-dev liblzma-dev libbz2-dev libncurses5-dev curl python3-dev python3.venv \
&& R CMD javareconf \
&& rm -rf /var/lib/apt/lists/*

# Workaround to allow for communication on J&J laptop
# COPY ZscalerRootCA.crt /root/ZscalerRootCA.crt
# RUN cat /root/ZscalerRootCA.crt >> /etc/ssl/certs/ca-certificates.crt
# COPY ZscalerRootCA.crt /usr/local/share/ca-certificates
# RUN update-ca-certificates

# install utility R packages - use the latest versions of the packages 
# which is why we specify the repo argument
RUN install2.r --error --skipinstalled --repos "https://packagemanager.posit.co/cran/latest" \
    ParallelLogger \
    rJava \
    SqlRender \
    DatabaseConnector \
&& rm -rf /tmp/download_packages/ /tmp/*.rds

# Installing Strategus & Strategus Modules - NOTE that the module path is in 2 places for now
RUN echo "INSTATIATED_MODULES_FOLDER=/home/ohdsi/strategus/modules" >> /usr/local/lib/R/etc/Renviron
ENV GITHUB_PAT=$GITHUB_PAT
RUN R <<EOF
Sys.setenv(INSTANTIATED_MODULES_FOLDER = "/home/ohdsi/strategus/modules")
remotes::install_github(repo = 'OHDSI/Strategus')
analysisSpecifications <- ParallelLogger::loadSettingsFromJson(fileName = system.file('testdata/analysisSpecification.json', package = 'Strategus'))
Strategus::ensureAllModulesInstantiated(analysisSpecifications)
EOF