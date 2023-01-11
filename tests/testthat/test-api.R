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
                             projects=dproj,
                             top_data_dir=ddata,
                             metfrag_db_dir=ddbdir,
                             metfrag_runtime="absent.jar"),
                 class="mf-jar-absent")
    
    expect_error(prepare_app(dir_before=dbef,
                             projects="absent",
                             top_data_dir=ddata,
                             metfrag_db_dir=ddbdir,
                             metfrag_runtime=mrt),
                 class="projects-absent")
    expect_error(prepare_app(dir_before=dbef,
                             projects=dproj,
                             top_data_dir="absent",
                             metfrag_db_dir=ddbdir,
                             metfrag_runtime=mrt),
                 class="top-data-dir-absent")
    expect_error(prepare_app(dir_before=dbef,
                             projects=dproj,
                             top_data_dir=ddata,
                             metfrag_db_dir="absent",
                             metfrag_runtime=mrt),
                 class="mf-db-dir-absent")
})})
