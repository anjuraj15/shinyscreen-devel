FROM gitlab.lcsb.uni.lu:4567/eci/shinyscreen:ssuser
MAINTAINER todor.kondic@uni.lu
EXPOSE 5432
ENV SS_MF_DB="PubChemLite_exposomics.csv"
ENV SS_CPU 2
ADD . shinyscreen/
RUN R CMD build shinyscreen
RUN R CMD INSTALL shinyscreen
RUN useradd -ms /bin/bash ssuser
RUN cp shinyscreen/runme /home/ssuser/runme
RUN cp -R shinyscreen /home/ssuser
RUN chmod u+x /home/ssuser/runme
RUN chown ssuser /home/ssuser/runme
RUN chown -R ssuser /home/ssuser/shinyscreen
USER ssuser
WORKDIR /home/ssuser
RUN mkdir top_data_dir
RUN mkdir projects
RUN mkdir metfrag_dbs
RUN mkdir users
# RUN mkdir -p isb401/top_data_dir/isb401
# RUN mkdir -p isb401/projects/isb401
# WORKDIR isb401/top_data_dir/isb401
# RUN curl -LJO https://zenodo.org/record/3666069/files/ISB401_AA_Std_pos.mzML?download=1
# RUN curl -LJO https://zenodo.org/record/3666069/files/ISB401_KO_pos.mzML?download=1
# RUN curl -LJO https://zenodo.org/record/3666069/files/ISB401_WT_pos.mzML?download=1
RUN R -e 'library(shinyscreen);setwd("~");init(top_data_dir="~/top_data_dir",projects="~/projects",users_dir="~/users",metfrag_db_dir=Sys.getenv("SS_MF_DB_DIR"),metfrag_jar="/usr/local/bin/MetFragCommandLine.jar",save=T,merge=F)'
ENTRYPOINT ["/home/ssuser/runme"]
