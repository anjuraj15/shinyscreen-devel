test_that("Do adducts affect MetFrag config generation correctly?",{
    skip_if_not(file.exists(Sys.getenv("METFRAG_JAR")),"Environment variable METFRAG_JAR does not contain a path to MetFrag jar package.")
    withr::with_tempdir({
        opts = list(fn_jar=Sys.getenv("METFRAG_JAR"),
                    fn_db_dir=Sys.getenv("METFRAG_DB_DIR"),
                    fn_db = Sys.getenv("METFRAG_DB"))
                    
        o = new_conf()
        eo = envopts(metfrag_db_dir=opts$fn_db_dir,
                     metfrag_jar=opts$fn_jar)
        o$conf$metfrag$db_file = basename(opts$fn_db)
        yaml::write_yaml(o$conf,file='conf-state.yaml')
        pproj = getwd()
        m = new_project(pproj,envopts = eo)

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
                                       db_path = m$run$metfrag$db_path,
                                       stag = paste0("a_",adduct,"_a"),
                                       adduct = adduct,
                                       ion_mz = 777.7789,
                                       spec = spec)
            fconf = file.path(m$run$metfrag$path,res["f_conf"])
            fspec = file.path(m$run$metfrag$path,res["f_spec"])
            x = readChar(fconf,nchars=file.size(fconf))
            y = gsub(paste0("LocalDatabasePath = ",m$run$metfrag$db_path),"",x)
            cfconf[adduct] = y 


            
        }
        names(cfconf) = NULL
        expect_snapshot(cfconf)


        

    })
    
})

ok_return_val("metfrag_run",{
    skip_if_not(file.exists(Sys.getenv("METFRAG_JAR")),"Environment variable METFRAG_JAR does not contain a path to MetFrag jar package.")
    m = make_dummy_mf_project()
                
    withr::with_dir(m$run$metfrag$path,{
            stagtab = metfrag_get_stag_tab(m$out$tab$summ[ms2_sel == T])

            ftab = metfrag_run(param = m$run$metfrag$param,
                               path = m$run$metfrag$path,
                               subpaths = m$run$metfrag$subpaths,
                               db_path = m$run$metfrag$db_path,
                               stag_tab = stagtab, ms2 = m$extr$ms2,
                               runtime=m$run$metfrag$runtime,
                               java_bin=m$run$metfrag$java_bin,
                               nproc = 2)

            expect_snapshot(ftab)

            for (f in ftab[,f_res]) {
                expect_true(file.exists(file.path(m$run$metfrag$path,
                                                  m$run$metfrag$subpaths['results'],
                                                  f)))
            }

            x = summarise_metfrag_results(param = m$conf$metfrag$param,
                                          path = m$run$metfrag$path,
                                          subpaths = m$run$metfrag$subpaths,
                                          cand_parameters = m$conf$metfrag$cand_parameters,
                                          db_scores = m$conf$metfrag$database_scores,
                                          int_scores = m$conf$metfrag$intrinsic_scores,
                                          collect_candidates= m$conf$metfrag$collect_candidates,
                                          file_tab = ftab)
            
            expect_snapshot(x)


                                 
    })
})
