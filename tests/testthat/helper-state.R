
STATE_DATA = readRDS(system.file("testdata","test-state.rds",package="shinyscreen"))

## Create a project that contains the metfrag dir structure and
## inherits `summ' and `extr' from a real project state (STATE_DATA).
make_dummy_mf_project <- function()  {

    tst = gen_test_project()
    troot = tst$dirs$root
    etmp = envopts_from_dirs(tst$dirs)
    save_envopts(etmp,dir=troot)
    fn_db =basename(Sys.getenv("SS_MF_DB"))
 
    
    eo = init(metfrag_db_dir=Sys.getenv("SS_MF_DB_DIR"),
              metfrag_jar=Sys.getenv("SS_MF_JAR"),
              java_bin=Sys.which("java"),
              merge=T,
              conf_dir=troot)
    p_dir = tst$ppath
 
    data.table::fwrite(data.table(),file.path(tst$ppath,"NT_set.csv"))    
    x = config_test_project(test_project=tst,mf=T,fn_mf_db=fn_db)

    m = new_project(project = tst$project,
                    envopts = eo)
    
    kkk = STATE_DATA$out$tab$summ[,.(ii=.GRP),by=key(STATE_DATA$out$tab$summ)][,ii:=NULL][1:4]
    
    m$out$tab$summ = STATE_DATA$out$tab$summ[kkk,on=key(STATE_DATA$out$tab$summ),nomatch=NULL]
    m$extr = STATE_DATA$extr
    m
    
}
    

