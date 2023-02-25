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
    project = "test_project"
    data_dir = "test_data"
    ppath=file.path(dirs$projects,project)
    pdata=file.path(dirs$top_data_dir,data_dir)
    dir.create(ppath)
    dir.create(pdata)
    list(dirs=dirs,
         project=project,
         data_dir=data_dir,
         ppath=ppath)
}

config_test_project <- function(test_project,mf=F,fn_mf_db="",o=new_conf()) {

    ## Shell we do MetFrag?
    if (mf) {
         o$conf$metfrag$param$SampleName = "testsample"
         
         if (nchar(fn_mf_db)>0L) {
             o$conf$metfrag$db_file = fn_mf_db

             o$conf$metfrag$param$MetFragDatabaseType = "LocalCSV"
             o$conf$metfrag$intrinsic_scores = list(FragmenterScore=1.0,
                                                    OfflineIndividualMoNAScore=1.0)
             o$conf$metfrag$database_scores = list(PubMed_Count=1.0,
                                                   Patent_Count=1.0,
                                                   AnnoTypeCount=1.0)
             
             o$conf$metfrag$cand_parameters = c("Identifier","CompoundName")
             o$conf$metfrag$collect_candidates = c("Identifier")
         }
   
    }
    o$conf$paths$data = test_project$data_dir
    yaml::write_yaml(o$conf,file=file.path(test_project$ppath,'conf-state.yaml'))
    o
}
    
envopts_from_dirs <- function(dirs) {
    init(projects=dirs$projects,
         top_data_dir=dirs$top_data_dir,
         users_dir=dirs$users_dir,
         metfrag_db_dir=dirs$metfrag_db_dir,
         metfrag_jar = file.path(dirs$mfjardir,"metfrag.jar"),
         java_bin = file.path(dirs$mfjardir,"java"),
         metfrag_max_proc=2L,
         merge=F,
         save=F)
}

trim_tmp_paths_envopts <- function(x) {
    y=list()
    for (n in setdiff(names(x),c("metfrag","no_structure_plots"))) {
        y[[n]] = basename(x[[n]])
    }
    
    mfchrs = setdiff(names(x$metfrag),"max_proc")
    for (n in mfchrs) {
        y$metfrag[[n]]=basename(x$metfrag[[n]])
    }
    y$metfrag$max_proc = x$metfrag$max_proc
    y$no_structure_plots = y$no_structure_plots
    y
}

trim_tmp_paths_run <- function(run){
    run$paths = sapply(run$paths,basename)
    run$metfrag$path = basename(run$metfrag$path)
    run$metfrag$db_file = basename(run$metfrag$db_file)
    run$metfrag$runtime = basename(run$metfrag$runtime)
    run$metfrag$java_bin = basename(run$metfrag$java_bin)
    run
}
