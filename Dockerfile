FROM kontrapunkt/shinyscreen_base:trials
MAINTAINER kontrapunkt@uclmail.net
EXPOSE 5432
ENV SS_CPU 2
ADD . shinyscreen/
RUN R CMD build shinyscreen
RUN R CMD INSTALL shinyscreen
RUN useradd -ms /bin/bash ssuser
RUN cp shinyscreen/runme /home/ssuser/runme
RUN chmod u+x /home/ssuser/runme
RUN chown ssuser /home/ssuser/runme
USER ssuser
WORKDIR /home/ssuser
RUN mkdir top_data_dir
RUN mkdir projects
RUN mkdir metfrag_dbs
RUN mkdir users

RUN R -e 'library(shinyscreen);setwd("~");init(top_data_dir="~/top_data_dir",projects="~/projects",users_dir="~/users",metfrag_db_dir=Sys.getenv("SS_MF_DB_DIR"),metfrag_jar="/usr/local/bin/MetFragCommandLine.jar",save=T,merge=F)'
ENTRYPOINT ["/home/ssuser/runme"]
