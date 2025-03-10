test_that("Do adducts affect MetFrag config generation correctly?",{
    skip_if_not(file.exists(Sys.getenv("SS_MF_JAR")),"Environment variable SS_MF_JAR does not contain a path to MetFrag jar package.")
    withr::with_tempdir({
        opts = list(fn_jar=Sys.getenv("SS_MF_JAR"),
                    fn_db_dir=Sys.getenv("SS_MF_DB_DIR"),
                    fn_db = Sys.getenv("SS_MF_DB"))

        ## Create test project directory structure.
        tst = gen_test_project()
        troot = tst$dirs$root

        ## Create suitable envopts.
        etmp = envopts_from_dirs(tst$dirs)
        save_envopts(etmp,dir=troot)
        eo = init(metfrag_db_dir=opts$fn_db_dir,
                  metfrag_jar=opts$fn_jar,merge=T,
                  conf_dir=troot)

        ## Write test config.
        o = config_test_project(tst,mf=T,fn_mf_db=basename(opts$fn_db))
        pproj = tst$ppath
        
        m = new_project(tst$project,envopts = eo)

        spec = data.table(mz=c(100.123,200.456,300.789),
                          intensity = c(1000.,2000.,3000.))
        
        cfconf = character(length(METFRAG_ADDUCTS))
        cfspec = character(length(METFRAG_ADDUCTS))
        names(cfconf) = METFRAG_ADDUCTS
        names(cfspec) = METFRAG_ADDUCTS
        for (adduct in METFRAG_ADDUCTS) {
            res = write_metfrag_config(param = m$run$metfrag$param,
                                       path = m$run$metfrag$path,
                                       subpaths = m$run$metfrag$subpaths,
                                       db_file = m$run$metfrag$db_file,
                                       stag = paste0("a_",adduct,"_a"),
                                       adduct = adduct,
                                       ion_mz = 777.7789,
                                       spec = spec)
            fconf = file.path(m$run$metfrag$path,res["f_conf"])
            fspec = file.path(m$run$metfrag$path,res["f_spec"])
            x = readChar(fconf,nchars=file.size(fconf))
            y = gsub(paste0("LocalDatabasePath = ",m$run$metfrag$db_file),"",x)
            cfconf[adduct] = y 


            
        }
        names(cfconf) = NULL
        expect_snapshot(cfconf)


        

    })
    
})

## TODO: FIXME: Obsoleted.
## ok_return_val("metfrag_run",{
##     skip_if_not(file.exists(Sys.getenv("SS_MF_JAR")),"Environment variable SS_MF_JAR does not contain a path to MetFrag jar package.")
##     m = make_dummy_mf_project()
                
##     withr::with_dir(m$run$metfrag$path,{
##             stagtab = metfrag_get_stag_tab(m$out$tab$summ[ms2_sel == T])

##             ftab = metfrag_run(param = m$run$metfrag$param,
##                                path = m$run$metfrag$path,
##                                subpaths = m$run$metfrag$subpaths,
##                                db_file = m$run$metfrag$db_file,
##                                stag_tab = stagtab, ms2 = m$db$extr$cgm$ms2,
##                                runtime=m$run$metfrag$runtime,
##                                java_bin=m$run$metfrag$java_bin,
##                                nproc = 2)

##             expect_snapshot(ftab)

##             for (f in ftab[,f_res]) {
##                 expect_true(file.exists(file.path(m$run$metfrag$path,
##                                                   m$run$metfrag$subpaths['results'],
##                                                   f)))
##             }

##             x = summarise_metfrag_results(param = m$conf$metfrag$param,
##                                           path = m$run$metfrag$path,
##                                           subpaths = m$run$metfrag$subpaths,
##                                           cand_parameters = m$conf$metfrag$cand_parameters,
##                                           db_scores = m$conf$metfrag$database_scores,
##                                           int_scores = m$conf$metfrag$intrinsic_scores,
##                                           collect_candidates= m$conf$metfrag$collect_candidates,
##                                           file_tab = ftab)
            
##             expect_snapshot(x)


                                 
##     })
## })
