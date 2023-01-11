test_that("Test envopts",{
    ## Test bad mf db dir.
    expect_error(envopts(metfrag_db_dir="notexist"),class="mf-db-dir-absent")

    ## Test bad mf db jar.
    expect_error(envopts(metfrag_jar="notexist"),class="mf-jar-absent")
})

test_that("Test empty project creation.",{
    withr::with_tempdir({

        ## new_empty_project
        ppath = file.path('test','project')
        dir.create(ppath,recursive=T)
        eo = envopts()
        res = new_empty_project(project=ppath,envopts=eo)
        expect_true(dir.exists(res$run$paths$project))
        expect_false(res$run$metfrag$cando_metfrag)
    })
})

test_that("Test full project creation.",{
     withr::with_tempdir({

        ppath = file.path('test','project')
        dir.create(ppath,recursive=T)
        eo = envopts()
        res = new_empty_project(project=ppath,envopts=eo)
        
        ## Switch metfrag on when conditions met.
        conf = res$conf
        db_dir = "mfdb"
        dir.create(db_dir)
        mf_jar = "bingo.jar"
        saveRDS(" ",mf_jar)
        mf_db = file.path(db_dir,"bingo.csv")
        saveRDS(" ",mf_db)
        conf$metfrag$db_file = basename(mf_db)
        yaml::write_yaml(conf,file.path(ppath,FN_CONF))
        eo = envopts(metfrag_db_dir=db_dir,
                     metfrag_jar=mf_jar)
        x = new_project(ppath,envopts=eo)
        expect_true(x$run$metfrag$cando_metfrag)
        expect_true(x$run$metfrag$cando_local)
        expect_true(x$run$metfrag$cando_remote)
        expect_true(dir.exists(file.path(x$run$metfrag$path,"results")))
        expect_true(dir.exists(file.path(x$run$metfrag$path,"log")))
        expect_true(dir.exists(file.path(x$run$metfrag$path,"config")))
})})

test_that("Test bad project path.", {
    withr::with_tempdir({
    ## Test bad project path.
    dir.create("test")
    badprojpath = file.path('test','noproject')
    expect_error(new_project(badprojpath,envopts=envopts()),class="project-absent")
})})

test_that("Test bad mf_db path.", {
    withr::with_tempdir({
        
    ppath = file.path('test','project')
    dir.create(ppath,recursive=T)
    eo = envopts()
    res = new_empty_project(project=ppath,envopts=eo)
    
    conf = res$conf
    db_dir = "mfdb"
    dir.create(db_dir)
    mf_jar = "bingo.jar"
    saveRDS(" ",mf_jar)
    mf_db = file.path(db_dir,"bingo.csv")
    saveRDS(" ",mf_db)
    conf$metfrag$db_file = "notexist.csv"
    yaml::write_yaml(conf,file.path(ppath,FN_CONF))
    eo = envopts(metfrag_db_dir=db_dir,
                 metfrag_jar=mf_jar)
    expect_error(new_project(ppath,envopts=eo),class="metfrag-db-file-absent")
        
})})



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

