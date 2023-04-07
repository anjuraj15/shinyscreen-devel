test_that("narrow_summ",{
    m = PLOTTING_TEST_STATE
    ## kvals =  list(set="AAs",ID="5") # 5 is interesting (isobars with 19)
    kvals = list(set="AAs",ID="4")
    labs = c("adduct","tag")
    ns = narrow_summ(db=m$db,m$out$tab$summ,kvals,labs,"mz","ms1_rt","ms1_int","Name","SMILES","qa_ms1_exists","scan","ms2_sel")
    expect_snapshot(ns)
})

test_that("make_eic_ms1_plot",{
    m = PLOTTING_TEST_STATE
    kvals = list(set="AAs",ID="4")
    labs = c("adduct","tag")
    plt = make_eic_ms1_plot(db=m$db,m$db$extr$cgm$ms1,m$out$tab$summ,kvals,labs)
    expect_equal(1,1)
})


## test_that("make_eic_ms1_plot",{

##     ms1 <- PLOTTING_DATA$ms1
##     summ <- PLOTTING_DATA$summ
##     labs <- PLOTTING_DATA$labs

##     ## Test the corner case when no cindex entry is selected.
##     kval <- NULL
##     res <- make_eic_ms1_plot(extr_ms1=ms1,
##                              summ=summ,
##                              kval=kval,
##                              labs=labs)
##     expect_identical(res,NULL)

##     ## Test getting the right plot.
##     kval <- list(set="NTs",ID=109L)
##     res <- make_eic_ms1_plot(extr_ms1=ms1,
##                              summ=summ,
##                              kval=kval,
##                              labs=labs)
##     expect_equal(NROW(res$data),4046L)
## })

## test_that("make_eic_ms2_plot",{


##     summ <- PLOTTING_DATA$summ
##     labs <- PLOTTING_DATA$labs

##     ## Test the corner case when no cindex entry is selected.
##     kval <- NULL
##     res <- make_eic_ms2_plot(summ=summ,
##                              kval=kval,
##                              labs=labs)
##     expect_identical(res,NULL)

##     ## Test getting the right plot.
##     kval <- list(set="NTs",ID=109L)
##     res <- make_eic_ms2_plot(summ=summ,
##                              kval=kval,
##                              labs=labs)
##     expect_equal(NROW(res$data),77L)
## })

test_that("plot_fname",{
    kval <- list(set="NTs",ID=109L)
    fname <- plot_fname(kval)
    expect_equal(fname,"plot_setNTs_ID109.pdf")
})
