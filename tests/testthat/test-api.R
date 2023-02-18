test_that("Test basic initialisation.",{
    dirs = gen_test_dirs()

    ## This is how it is supposed to work.
    x = init(projects=dirs$projects,
             top_data_dir=dirs$top_data_dir,
             users_dir=dirs$users_dir,
             metfrag_db_dir=dirs$metfrag_db_dir,
             metfrag_jar = file.path(dirs$mfjardir,"metfrag.jar"),
             java_bin = file.path(dirs$mfjardir,"java"),
             metfrag_max_proc=3L,
             merge=F,
             save=F)
    
    ## Function `gen_test_dirs' creates shinyscreen root structure in
    ## a temp dir. We need to strip the random part for comparison.
    y = trim_tmp_paths_envopts(x)
    expect_snapshot(y)
    
    ## What happens if directories are wrong.
    expect_error(init(projects="absent",
                      top_data_dir=dirs$top_data_dir,
                      users_dir=dirs$users_dir,
                      metfrag_db_dir=dirs$metfrag_db_dir,
                      metfrag_jar = file.path(dirs$mfjardir,"metfrag.jar"),
                      java_bin = file.path(dirs$mfjardir,"java"),
                      metfrag_max_proc=3L,
                      merge=F,
                      save=F),
                 class="projects-dir-absent")
    expect_error(init(projects=dirs$projects,
                      top_data_dir="absent",
                      users_dir=dirs$users_dir,
                      metfrag_db_dir=dirs$metfrag_db_dir,
                      metfrag_jar = file.path(dirs$mfjardir,"metfrag.jar"),
                      java_bin = file.path(dirs$mfjardir,"java"),
                      metfrag_max_proc=3L,
                      merge=F,
                      save=F),
                 class="top-data-dir-absent")
    expect_error(init(projects=dirs$projects,
                      top_data_dir=dirs$top_data_dir,
                      users_dir="absent",
                      metfrag_db_dir=dirs$metfrag_db_dir,
                      metfrag_jar = file.path(dirs$mfjardir,"metfrag.jar"),
                      java_bin = file.path(dirs$mfjardir,"java"),
                      metfrag_max_proc=3L,
                      merge=F,
                      save=F),
                 class="users-dir-absent")
    
    expect_error(init(projects=dirs$projects,
                      top_data_dir=dirs$top_data_dir,
                      users_dir="absent",
                      metfrag_db_dir=dirs$metfrag_db_dir,
                      metfrag_jar = file.path(dirs$mfjardir,"metfrag.jar"),
                      java_bin = file.path(dirs$mfjardir,"java"),
                      metfrag_max_proc=3L,
                      merge=F,
                      save=F),
                 class="users-dir-absent")
    
    expect_error(init(projects=dirs$projects,
                      top_data_dir=dirs$top_data_dir,
                      users_dir=dirs$users_dir,
                      metfrag_db_dir="absent",
                      metfrag_jar = file.path(dirs$mfjardir,"metfrag.jar"),
                      java_bin = file.path(dirs$mfjardir,"java"),
                      metfrag_max_proc=3L,
                      merge=F,
                      save=F),
                 class="mf-db-dir-absent")

    expect_error(init(projects=dirs$projects,
                      top_data_dir=dirs$top_data_dir,
                      users_dir=dirs$users_dir,
                      metfrag_db_dir=dirs$metfrag_db_dir,
                      metfrag_jar = "absent",
                      java_bin = file.path(dirs$mfjardir,"java"),
                      metfrag_max_proc=3L,
                      merge=F,
                      save=F),
                 class="mf-jar-absent")
    
    expect_error(init(projects=dirs$projects,
                      top_data_dir=dirs$top_data_dir,
                      users_dir=dirs$users_dir,
                      metfrag_db_dir=dirs$metfrag_db_dir,
                      metfrag_jar = file.path(dirs$mfjardir,"metfrag.jar"),
                      java_bin = "absent",
                      metfrag_max_proc=3L,
                      merge=F,
                      save=F),
                 class="java-bin-absent")
    
    expect_error(init(projects=dirs$projects,
                      top_data_dir=dirs$top_data_dir,
                      users_dir=dirs$users_dir,
                      metfrag_db_dir=dirs$metfrag_db_dir,
                      metfrag_jar = dirs$metfrag_db_dir,
                      java_bin = file.path(dirs$mfjardir,"java"),
                      metfrag_max_proc=3.45,
                      merge=F,
                      save=F),
                 class="metfrag-max-proc-not-an-int")
    
        

})

test_that("Test saving configuration.",{
    dirs = gen_test_dirs()
    troot = dirs$root

    e = init(projects=dirs$projects,
             top_data_dir=dirs$top_data_dir,
             users_dir=dirs$users_dir,
             metfrag_db_dir=dirs$metfrag_db_dir,
             metfrag_jar = file.path(dirs$mfjardir,"metfrag.jar"),
             java_bin = file.path(dirs$mfjardir,"java"),
             metfrag_max_proc=3L,
             merge=F,
             save=T,
             conf_dir=troot)

    eold = load_envopts(dir=troot)
    y = trim_tmp_paths_envopts(eold)
    expect_snapshot(y)
})


test_that("Test merging configuration.",{
    dirs = gen_test_dirs()
    troot = dirs$root

    e = init(projects=dirs$projects,
             top_data_dir=dirs$top_data_dir,
             users_dir=dirs$users_dir,
             metfrag_db_dir=dirs$metfrag_db_dir,
             metfrag_jar = file.path(dirs$mfjardir,"metfrag.jar"),
             java_bin = file.path(dirs$mfjardir,"java"),
             metfrag_max_proc=3L,
             merge=F,
             save=T,
             conf_dir=troot)

    enew = init(metfrag_max_proc=5L,conf_dir=troot)
    y = trim_tmp_paths_envopts(x=enew)
    expect_snapshot(y)
})
