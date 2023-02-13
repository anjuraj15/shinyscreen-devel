test_that("Preparation for app start",{withr::with_tempdir({
    dbef = "bef"
    dir.create("bef")
    ddata = "topdata"
    dir.create(ddata)
    dproj = "proj"
    dir.create(dproj)
    ddbdir = "dbdir"
    dir.create(ddbdir)
    mrt = "mrt.fake.jar"
    saveRDS("",file=mrt)
    expect_error(prepare_app(dir_before=dbef,
                             projects="absent",
                             top_data_dir=ddata),
                 class="projects-absent")
    expect_error(prepare_app(dir_before=dbef,
                             projects=dproj,
                             top_data_dir="absent"),
                 class="top-data-dir-absent")

})})
