ok_return_val <- function(fun_name,...) {
    testthat::test_that(paste("function",fun_name,"returns correct result."),...)
}

gen_test_dirs <- function() {

    tname = tempfile("shiny_test_dirs")
    dirs = list(metfrag_db_dir = file.path(tname,"mfdb"))
    dirs$top_data_dir = file.path(tname,"topdatadir")
    dirs$projects = file.path(tname,"projects")
    dir.create(tname)
    for (dr in dirs) dir.create(dr)
    dirs$root = tname
    fn_db = system.file("testdata","example_db.csv",package="shinyscreen")
    dirs
}


gen_test_project <- function() {
    dirs = gen_test_dirs()
    project = file.path(dirs$projects,"test_project")
    dir.create(project)
    
}
