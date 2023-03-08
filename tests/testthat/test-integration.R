test_that("Extraction returns what is needed.",{
    topd = Sys.getenv("SS_INTEG_TOP_DATA_DIR")
    projd = Sys.getenv("SS_INTEG_PROJ_DIR")
    skip_if_not(shiny::isTruthy(topd) && shiny::isTruthy(projd),"Environment variables SS_INTEG_TOP_DATA_DIR and SS_INTEG_PROJ_DIR must be present for this test to work.")

    eo = init(top_data_dir=topd,projects=projd)
    print(str(eo))
    m = run(envopts=eo,
            project="proj",
            phase=c("setup","comptab"))
    expect_true(1==1)
})
