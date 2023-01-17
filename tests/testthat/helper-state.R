
STATE_DATA = readRDS(system.file("testdata","test-state.rds",package="shinyscreen"))

## Create a project that contains the metfrag dir structure and
## inherits `summ' and `extr' from a real project state (STATE_DATA).
make_dummy_mf_project <- function()  {
    withr::with_options(list(fn_jar=Sys.getenv("METFRAG_JAR"),
                             fn_db_dir=Sys.getenv("METFRAG_DB_DIR"),
                             fn_db = Sys.getenv("METFRAG_DB"),
                              p_dir = tempfile(pattern="shinyscreen_dummy_mf_proj")),
    {
        fn_jar = norm_path(getOption("fn_jar"))
        if (!file.exists(fn_jar)) fn_jar = ""
        fn_db_dir = norm_path(getOption("fn_db_dir"))
        if (!file.exists(fn_db_dir)) fn_db_dir= ""
        p_dir = norm_path(getOption("p_dir"))
        dir.create(p_dir)
        data.table::fwrite(data.table(),file.path(p_dir,"NT_set.csv"))
    
        eo = envopts(metfrag_db_dir=fn_db_dir,
                     metfrag_jar=fn_jar)

        x  = metfrag_conf(STATE_DATA)
        x$conf$metfrag$param$SampleName = "testsample"
        x$conf$metfrag$param$MetFragDatabaseType = "LocalCSV"
        x$conf$metfrag$db_file = basename(getOption("fn_db"))
        x$conf$metfrag$scores = list(FragmenterScore=1.0,
                                     OfflineIndividualMoNAScore=1.0,
                                     PubMedCount=1.0,
                                     PatentCount=1.0,
                                     AnnoTypeCount=1.0)

        x$conf$metfrag$cand_parameters = c("Identifier","CompoundName")
        x$conf$metfrag$collect_candidates = c("Identifier")

        yaml::write_yaml(x=x$conf,file=file.path(p_dir,"conf-state.yaml"))
        m = new_project(project = p_dir,
                        envopts = eo)

        kkk = STATE_DATA$out$tab$summ[,.(ii=.GRP),by=key(STATE_DATA$out$tab$summ)][,ii:=NULL][1:4]
        
        m$out$tab$summ = STATE_DATA$out$tab$summ[kkk,on=key(STATE_DATA$out$tab$summ),nomatch=NULL]
        m$extr = STATE_DATA$extr
        m
    })
}
    

