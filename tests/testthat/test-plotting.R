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
    cd = define_colrdata(m$out$tab$comp,labs)
    plt = make_eic_ms1_plot(db=m$db,m$out$tab$summ,kvals,labs,colrdata=cd)
    expect_snapshot(plt$labels)
})

test_that("make_eic_ms2_plot",{
    m = PLOTTING_TEST_STATE
    kvals = list(set="AAs",ID="4")
    labs = c("adduct","tag")
    cd = define_colrdata(m$out$tab$comp,labs)
    plt = make_eic_ms2_plot(db=m$db,m$out$tab$summ,kvals,labs,colrdata=cd)
    expect_snapshot(plt$labels)
})

test_that("make_spec_ms2_plot",{
    m = PLOTTING_TEST_STATE
    kvals = list(set="AAs",ID="4")
    labs = c("adduct","tag")
    cd = define_colrdata(m$out$tab$comp,labs)
    plt = make_spec_ms2_plot(db=m$db,m$out$tab$summ,kvals,labs,colrdata=cd)
    expect_snapshot(plt$labels)
})

test_that("plot_fname",{
    kval <- list(set="NTs",ID=109L)
    fname <- plot_fname(kval)
    expect_equal(fname,"plot_setNTs_ID109.pdf")
})
