FROM ubuntu:jammy-20230126
MAINTAINER kontrapunkt@uclmail.net
ENV SS_MF_DB_DIR="/usr/local/metfrag_db"
ENV SS_MF_JAR /usr/local/bin/MetFragCommandLine.jar

RUN apt-get update
RUN apt-get install -y --no-install-recommends apt-utils r-base r-recommended g++ gfortran
RUN apt-get install -y --no-install-recommends curl wget libxml2-dev libssl-dev libssh2-1-dev zlib1g-dev
RUN apt-get install -y --no-install-recommends xdg-utils automake autoconf gcc make pkg-config
RUN apt-get install -y --no-install-recommends net-tools git-all default-jre-headless zlib1g zlib1g-dev 
RUN apt-get install -y --no-install-recommends libnetcdf* openbabel libhdf5-1* libhdf5-hl-1*
RUN apt-get install -y --no-install-recommends r-cran-rcolorbrewer r-cran-data.table r-cran-biocmanager r-cran-tidyverse r-cran-cowplot
RUN apt-get install -y --no-install-recommends r-cran-curl r-cran-promises r-cran-yaml
RUN apt-get install -y --no-install-recommends r-cran-dt r-cran-future
RUN apt-get install -y --no-install-recommends r-cran-rjava r-cran-rsvg r-cran-png
RUN apt-get install -y --no-install-recommends r-cran-rmarkdown r-cran-shiny r-cran-htmltools
RUN apt-get install -y --no-install-recommends r-cran-rcdklibs r-cran-rcdk r-cran-fingerprint
RUN apt-get install -y --no-install-recommends r-bioc-affy r-bioc-affyio
RUN apt-get install -y --no-install-recommends r-cran-devtools
RUN apt-get install -y --no-install-recommends libpng16-16 libpng-tools libpng-dev libpng++-dev
RUN rm -rf /var/lib/apt/lists/*
RUN rm -rf /tmp/*

# Add MetFrag.
RUN curl -LJO https://gitlab.lcsb.uni.lu/eci/shinyscreen_needs/-/raw/master/MetFragCommandLine-2.5.0.jar
# RUN curl -LJO https://github.com/ipb-halle/MetFragRelaunched/releases/download/v.2.5.0/MetFragCommandLine-2.5.0.jar
RUN mv MetFragCommandLine-2.5.0.jar /usr/local/bin/MetFragCommandLine.jar
RUN chmod 0555 /usr/local/bin/MetFragCommandLine.jar

# Add PCL.
RUN curl -LJO https://zenodo.org/record/7576412/files/PubChemLite_exposomics_20230127.csv
RUN mkdir /usr/local/metfrag_db
RUN chmod 0555 /usr/local/metfrag_db
RUN mv PubChemLite_exposomics_20230127.csv /usr/local/metfrag_db/PubChemLite_exposomics.csv 

RUN MAKEFILE=j R -e 'install.packages("enviPat", dependencies=TRUE)'
RUN MAKEFILE=j R -e 'BiocManager::install(c("Rhdf5lib","mzR","MSnbase","RMassBank"),update=F)'
RUN MAKEFILE=j R -e 'devtools::install_git("https://github.com/CDK-R/rinchi", dependencies=F)'
RUN MAKEFILE=j R -e 'devtools::install_git("https://github.com/schymane/RChemMass", dependencies=F)'
