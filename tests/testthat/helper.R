ok_return_val <- function(fun_name,...) {
    testthat::test_that(paste("function",fun_name,"returns correct result."),...)
}

gen_test_dirs <- function() {
    ## Create shinyscreen directories.
    tname = tempfile("shiny_test_dirs")
    dirs = list(metfrag_db_dir = file.path(tname,"mfdb"))

    dirs$top_data_dir = file.path(tname,"topdatadir")
    dirs$projects = file.path(tname,"projects")
    dirs$users_dir = file.path(tname,"users")
    dirs$mfjardir = file.path(tname,"jardir")
    
    dir.create(tname)
    for (dr in dirs) dir.create(dr)
    dirs$root = tname
    fn_db = system.file("testdata","example_db.csv",package="shinyscreen")

    mfjar = file.path(dirs$mfjardir,"metfrag.jar")
    mfjava = file.path(dirs$mfjardir,"java")
    saveRDS(NULL,file=mfjar)
    saveRDS(NULL,file=mfjava)

    file.copy(fn_db,dirs$top_data_dir)
    dirs
}


gen_test_project <- function() {
    dirs = gen_test_dirs()
    project = file.path(dirs$projects,"test_project")
    dir.create(project)
    
}

trim_tmp_paths_envopts <- function(x) {
    y=list()
    for (n in setdiff(names(x),"metfrag")) {
        y[[n]] = basename(x[[n]])
    }
    
    mfchrs = setdiff(names(x$metfrag),"max_proc")
    for (n in mfchrs) {
        y$metfrag[[n]]=basename(x$metfrag[[n]])
    }
    y$metfrag$max_proc = x$metfrag$max_proc
    y
}
