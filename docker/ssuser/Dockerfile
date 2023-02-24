FROM gitlab.lcsb.uni.lu:4567/eci/shinyscreen:base
MAINTAINER todor.kondic@uni.lu
RUN useradd -ms /bin/bash ssuser
USER ssuser
WORKDIR /home/ssuser
RUN mkdir top_data_dir
RUN mkdir projects
RUN mkdir metfrag_dbs
RUN mkdir users
