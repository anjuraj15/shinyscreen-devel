test_that("Test state creation",{

    withr::with_tempdir({
        ## new_empty_project
        ppath = file.path('test','project')
        dir.create(ppath,recursive=T)
        res = shinyscreen::new_empty_project(ppath)
        testthat::expect_true(dir.exists(res$run$paths$project))
        testthat::expect_false(res$run$metfrag$available)
        
        ## new_project
        testthat::expect_error(shinyscreen::new_project(ppath),class="conf-file-absent")
        
        ## Switch metfrag on when conditions met.
        conf = res$conf
        mf_jar = "bingo.jar"
        saveRDS(" ",mf_jar)
        mf_db = "bingo.csv"
        saveRDS(" ",mf_db)
        conf$metfrag$runtime = norm_path(mf_jar)
        conf$metfrag$db_path = norm_path(mf_db)
        testthat::expect_true(is_metfrag_available(conf))
        
        yaml::write_yaml(conf,file.path(ppath,FN_CONF))
        x = shinyscreen::new_project(ppath)
        testthat::expect_true(x$run$metfrag$available)
    })
})



test_that("pack_ms2_w_summ",{
    summ = STATE_DATA$out$tab$summ
    ms2 = STATE_DATA$extr$ms2
    res = pack_ms2_w_summ(summ,ms2)
    expect_identical(res[1,adduct],'[M+H]+')
    expect_identical(res[1,tag],'AA')
    expect_identical(res[1,ID],'100')
    expect_equal(res[1,an],5413)
    expect_equal(res[1,mz],182.0816)

    expect_identical(res[29,adduct],'[M+H]+')
    expect_identical(res[29,tag],'WT')
    expect_identical(res[29,ID],'112')
    expect_equal(res[29,an],2838)
    expect_equal(res[29,mz],268.1045)
    
    
    
})

