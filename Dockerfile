FROM gitlab.lcsb.uni.lu:4567/eci/shinyscreen/dep/ssuser:latest
MAINTAINER todor.kondic@uni.lu
EXPOSE 5432
ENV SS_MF_DB="PubChemLite_exposomics.csv"
ENV SS_CPU 2
ADD . shinyscreen/
RUN R CMD build shinyscreen
RUN R CMD INSTALL shinyscreen
RUN cp shinyscreen/runme /home/ssuser/runme
RUN cp -R shinyscreen /home/ssuser
RUN chmod u+x /home/ssuser/runme
RUN chown ssuser /home/ssuser/runme
RUN chown -R ssuser /home/ssuser/shinyscreen
RUN R -e 'library(shinyscreen);setwd("~");init(top_data_dir="~/top_data_dir",projects="~/projects",users_dir="~/users",metfrag_db_dir=Sys.getenv("SS_MF_DB_DIR"),metfrag_jar="/usr/local/bin/MetFragCommandLine.jar",no_structure_plot=T,save=T,merge=F)'
ENTRYPOINT ["/home/ssuser/runme"]
CMD ["app"]
