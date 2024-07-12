# Use a base image with R pre-installed
FROM ubuntu:jammy-20230126

# Set non-interactive frontend and timezone
ENV DEBIAN_FRONTEND=noninteractive
ENV TZ=Etc/UTC
ENV SS_CPU 2

# Install system dependencies
RUN apt-get update && apt-get install -y --no-install-recommends \
    apt-utils \
    r-base \
    r-recommended \
    g++ \
    gfortran \
    curl \
    wget \
    libxml2-dev \
    libssl-dev \
    libssh2-1-dev \
    zlib1g-dev \
    xdg-utils \
    automake \
    autoconf \
    gcc \
    make \
    pkg-config \
    net-tools \
    git-all \
    default-jre-headless \
    default-jdk \
    libpng16-16 \
    libpng-tools \
    libpng-dev \
    libpng++-dev \
    libnetcdf* \
    openbabel \
    libhdf5-1* \
    libhdf5-hl-1* \
    r-cran-rcolorbrewer \
    r-cran-data.table \
    r-cran-biocmanager \
    r-cran-tidyverse \
    r-cran-cowplot \
    r-cran-curl \
    r-cran-promises \
    r-cran-yaml \
    r-cran-dt \
    r-cran-future \
    r-cran-rjava \
    r-cran-rsvg \
    r-cran-png \
    r-cran-rmarkdown \
    r-cran-shiny \
    r-cran-htmltools \
    r-cran-rcdklibs \
    r-cran-rcdk \
    r-cran-fingerprint \
    r-bioc-affy \
    r-bioc-affyio \
    r-cran-devtools

# Clean up
RUN rm -rf /var/lib/apt/lists/* /tmp/*

# Install R packages
RUN R -e 'install.packages("enviPat", dependencies=TRUE)'
RUN R -e 'BiocManager::install(c("Rhdf5lib","mzR","MSnbase","RMassBank"), update=FALSE)'
RUN R -e 'devtools::install_git("https://github.com/schymane/RChemMass", dependencies=FALSE)'

# Set JAVA_HOME
ENV JAVA_HOME=/usr/lib/jvm/default-java
ENV PATH=$JAVA_HOME/bin:$PATH

# Clean up
RUN apt-get clean && rm -rf /var/lib/apt/lists/*

# Create a user and directories
RUN useradd -ms /bin/bash ssuser
USER root
WORKDIR /home/ssuser
RUN mkdir -p top_data_dir projects users
RUN chown -R ssuser:ssuser /home/ssuser

# Copy and build the Shiny app
ADD . shinyscreen/
RUN R CMD build shinyscreen
RUN R CMD INSTALL shinyscreen

# Switch back to ssuser
USER ssuser
RUN cp shinyscreen/runme /home/ssuser/runme
RUN chmod u+x /home/ssuser/runme

# Initialize the app with absolute paths
RUN R -e 'library(shinyscreen); setwd("~"); init(top_data_dir="/home/ssuser/top_data_dir", projects="/home/ssuser/projects", users_dir="/home/ssuser/users", no_structure_plots=TRUE, save=TRUE, merge=FALSE)'

# Expose the port and set the entrypoint
EXPOSE 3838
ENTRYPOINT ["/home/ssuser/runme"]
CMD ["app"]

