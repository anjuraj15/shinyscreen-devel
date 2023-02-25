FROM gitlab.lcsb.uni.lu:4567/eci/shinyscreen/dep/ssuser:latest
MAINTAINER todor.kondic@uni.lu
WORKDIR isb401/top_data_dir/isb401
RUN curl -LJO https://zenodo.org/record/3666069/files/ISB401_AA_Std_pos.mzML
RUN curl -LJO https://zenodo.org/record/3666069/files/ISB401_KO_pos.mzML
RUN curl -LJO https://zenodo.org/record/3666069/files/ISB401_WT_pos.mzML
WORKDIR /home/ssuser/
RUN curl -LJO https://gitlab.lcsb.uni.lu/eci/isb401/-/archive/master/isb401-master.tar.gz
RUN tar -xf isb401-master.tar.gz
RUN mkdir top_data_dir/isb401
RUN mkdir projects/isb401
RUN cp isb401-master/shinyscreen_files/*.csv /home/ssuser/projects/isb401