test_that("Test make_metfrag_panel (local and remote)",{
    withr::with_tempdir({
        mfdb = "demodb.csv"
        mfdbdir = "demodir"
        dir.create(mfdbdir)
        mfjar = "demomf.jar"
        saveRDS("",file.path(mfdbdir,mfdb))
        saveRDS("",mfjar)

        eo = envopts(metfrag_db_dir = mfdbdir,
                     metfrag_jar = mfjar)

        xx = make_metfrag_panel(eo)
        res = sapply(METFRAG_DATABASE_TYPE,function (p) {grepl(p,xx[[6L]])})
        expect_true(all(res))
    })
})

test_that("Test make_metfrag_panel (remote)",{
    withr::with_tempdir({
        mfjar = "demomf.jar"
        saveRDS("",mfjar)
        eo = envopts(metfrag_jar = mfjar)
        xx = make_metfrag_panel(eo)
        res = sapply(METFRAG_REMOTE_DATABASE_TYPE,function (p) {grepl(p,xx[[6L]])})
        expect_true(all(res))
        
        res2 = sapply(METFRAG_LOCAL_DATABASE_TYPE,function (p) {grepl(p,xx[[6L]])})
        expect_false(any(res2))
    })
})

test_that("Test make_metfrage_panel (disabled)",{
    eo = envopts()
    xx = make_metfrag_panel(eo)
    expect_true(length(xx)==1L)
})
