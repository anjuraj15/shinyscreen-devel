test_that("make_eic_ms1_plot",{

    ms1 <- PLOTTING_DATA$ms1
    summ <- PLOTTING_DATA$summ
    labs <- PLOTTING_DATA$labs

    ## Test the corner case when no cindex entry is selected.
    kval <- NULL
    res <- make_eic_ms1_plot(extr_ms1=ms1,
                             summ=summ,
                             kval=kval,
                             labs=labs)
    expect_identical(res,NULL)

    ## Test getting the right plot.
    kval <- list(set="NTs",ID=109L)
    res <- make_eic_ms1_plot(extr_ms1=ms1,
                             summ=summ,
                             kval=kval,
                             labs=labs)
    expect_equal(NROW(res$data),4046L)
})

test_that("make_eic_ms2_plot",{


    summ <- PLOTTING_DATA$summ
    labs <- PLOTTING_DATA$labs

    ## Test the corner case when no cindex entry is selected.
    kval <- NULL
    res <- make_eic_ms2_plot(summ=summ,
                             kval=kval,
                             labs=labs)
    expect_identical(res,NULL)

    ## Test getting the right plot.
    kval <- list(set="NTs",ID=109L)
    res <- make_eic_ms2_plot(summ=summ,
                             kval=kval,
                             labs=labs)
    expect_equal(NROW(res$data),77L)
})

test_that("plot_fname",{
    kval <- list(set="NTs",ID=109L)
    fname <- plot_fname(kval)
    expect_equal(fname,"plot_setNTs_ID109.pdf")
})
