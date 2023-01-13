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
                                       stag = paste0("a_",adduct,"_a"),
                                       adduct = adduct,
                                       ion_mz = 777.7789,
                                       spec = spec)
            fconf = file.path(m$run$metfrag$path,res["f_conf"])
            fspec = file.path(m$run$metfrag$path,res["f_spec"])
            
            cfconf[adduct] = readChar(fconf,nchars=file.size(fconf))


            
        }
        names(cfconf) = NULL
        expect_snapshot(cfconf)


        

    })
    
})

test_that("Function get_metfrag_targets behaves as expected.",{

    summ = STATE_DATA$out$tab$summ
    ms2 = STATE_DATA$extr$ms2
    res = get_metfrag_targets(summ=summ,
                              ms2=ms2)
    
    expect_snapshot(head(res,5))
    expect_snapshot(tail(res,5))
  
})

ok_return_val("metfrag_on_state",{
    skip_if_not(file.exists(Sys.getenv("METFRAG_JAR")),"Environment variable METFRAG_JAR does not contain a path to MetFrag jar package.")
    m = make_dummy_mf_project()
    res = metfrag_on_state(m)
    withr::with_dir(m$run$metfrag$path,{
        fc_i = readChar(res[1,V1],nchars=file.size(res[1,V1]))
        expect_snapshot(fc_i)
    
        fs_i = readChar(res[3,V1],nchars=file.size(res[3,V1]))
        expect_snapshot(fs_i)
    
        fc_l = readChar(res[.N-2,V1],nchars=file.size(res[.N-2,V1]))
        expect_snapshot(fc_l)

        fs_l = readChar(res[.N,V1],nchars=file.size(res[.N,V1]))
        expect_snapshot(fs_l)
    })
})


test_that("MetFrag example works.",{
    skip_if_not(file.exists(Sys.getenv("METFRAG_JAR")),"Environment variable METFRAG_JAR does not contain a path to MetFrag jar package.")
    skip_if_offline()
    withr::with_tempdir({
        runtime = path.expand(Sys.getenv("METFRAG_JAR"))
        fn_conf = system.file("testdata/example_parameter_file.txt",package = "shinyscreen")
        fn_peaks = system.file("testdata/example_data.txt",package = "shinyscreen")
        fn_log = "metfrag.log"
        file.copy(fn_conf,basename(fn_conf))
        file.copy(fn_peaks,basename(fn_peaks))

        metfrag_run(fn_jar = runtime,
                    fn_conf = basename(fn_conf),
                    fn_log = fn_log)

        content = readChar(fn_log,nchars=file.size(fn_log))
        expect_true(grepl(r"(0 candidate\(s\) discarded during processing due to errors)",content))
    })
})


