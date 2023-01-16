test_that("Do adducts affect MetFrag config generation correctly?",{

    withr::with_tempdir({
        o = new_conf()
        dir.create("db_dir")
        saveRDS("","db_dir/dbfile.csv")
        saveRDS("","mf.jar")
        o$conf$metfrag$db_file = "dbfile.csv"
        eo = envopts(metfrag_db_dir="db_dir",
                     metfrag_jar="mf.jar")
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
            res = write_metfrag_config(param = m$conf$metfrag$param,
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

            ftab = metfrag_run(param = m$conf$metfrag$param,
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

                                 
    })
})

## ok_return_val("metfrag_on_state",{
##     ## skip_if_not(file.exists(Sys.getenv("METFRAG_JAR")),"Environment variable METFRAG_JAR does not contain a path to MetFrag jar package.")
##     ## m = make_dummy_mf_project()
##     ## res = metfrag_on_state(m)
##     ## withr::with_dir(m$run$metfrag$path,{
##     ##     fc_i = readChar(res$targets[1,files],nchars=file.size(res$targets[1,files]))
##     ##     fc_i = gsub(paste0("LocalDatabasePath = ",m$run$metfrag$db_path),"",fc_i)
##     ##     expect_snapshot(fc_i)
    
##     ##     fs_i = readChar(res$targets[3,files],nchars=file.size(res$targets[3,files]))
##     ##     fs_i = gsub(paste0("LocalDatabasePath = ",m$run$metfrag$db_path),"",fs_i)
##     ##     expect_snapshot(fs_i)
    
##     ##     fc_l = readChar(res$targets[.N-2,files],nchars=file.size(res$targets[.N-2,files]))
##     ##     fc_l = gsub(paste0("LocalDatabasePath = ",m$run$metfrag$db_path),"",fc_l)
##     ##     expect_snapshot(fc_l)

##     ##     fs_l = readChar(res$targets[.N,files],nchars=file.size(res$targets[.N,files]))
##     ##     fs_l = gsub(paste0("LocalDatabasePath = ",m$run$metfrag$db_path),"",fs_l)
##     ##     expect_snapshot(fs_l)
##     ## })
## })




## test_that("MetFrag example works.",{
##     skip_if_not(file.exists(Sys.getenv("METFRAG_JAR")),"Environment variable METFRAG_JAR does not contain a path to MetFrag jar package.")
##     skip_if_offline()
##     withr::with_tempdir({
##         runtime = path.expand(Sys.getenv("METFRAG_JAR"))
##         fn_conf = system.file("testdata/example_parameter_file.txt",package = "shinyscreen")
##         fn_peaks = system.file("testdata/example_data.txt",package = "shinyscreen")
##         fn_log = "metfrag.log"
##         file.copy(fn_conf,basename(fn_conf))
##         file.copy(fn_peaks,basename(fn_peaks))

##         metfrag_run(fn_jar = runtime,
##                     fn_conf = basename(fn_conf),
##                     fn_log = fn_log)

##         content = readChar(fn_log,nchars=file.size(fn_log))
##         expect_true(grepl(r"(0 candidate\(s\) discarded during processing due to errors)",content))
##     })
## })


