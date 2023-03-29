test_that("Test empty project creation.",{

    ## Generate test environment and a test project directory.
    proj = gen_test_project()
    
    eo = envopts_from_dirs(proj$dirs)
    
    res = new_empty_project(project=proj$project,envopts=eo)
    res$run = trim_tmp_paths_run(res$run)
    expect_snapshot(res)
})

test_that("pack_ms2_w_summ",{
    summ = STATE_DATA$out$tab$summ
    ms2 = STATE_DATA$db$extr$cgm$ms2
    res = pack_ms2_w_summ(summ,ms2)
    expect_snapshot(res[1])
    expect_snapshot(res[2])
})


test_that("pack_project",{
    z = tempfile(pattern="project_",fileext=".tar.gz")
    proj = gen_test_project()
    eo = envopts_from_dirs(proj$dirs)
    res = new_empty_project(project=proj$project,envopts=eo)
    pack_project(m=res,fn_arch=z)
    tmpdir = tempfile()
    dir.create(tmpdir)
    fls = withr::with_dir(tmpdir,{
        untar(z)
        list.files(recursive=T)
    })
    expect_snapshot(fls)
})

test_that("join_compound_lists with empty input returns empty output",{
    x = join_compound_lists(character(0))
    expect_true(nrow(x)==0L)
})

test_that("process_cmpd_sets",{

    ## Test case when no base filename is the same.
    cmpdl = data.table(ID=1:5,ORIG=c("f1.csv","f1.csv","f2.csv","f3.csv","f3.csv"),set=c(NA_character_,NA_character_,"aks","se31","se32"))
    x = process_cmpd_sets(cmpdl)
    expect_snapshot(x)

    ## Test case with similar base filenames.
    cmpdl = data.table(ID=1:5,ORIG=c("b/f2.csv","a/f1.csv","q/f/g/f2.csv","d/e/f/f3.csv","m/n/q/f3.csv"),set=c(NA_character_,NA_character_,"aks","se31","se32"))
    x = process_cmpd_sets(cmpdl)
    expect_snapshot(x)

    ## Test semi-filled cmpd.
    cmpdl1 = data.table(ID=1:5,ORIG=rep("a/fn.csv",5),set=c(NA_character_,NA_character_,"set","set2",NA_character_))
    cmpdl2 = data.table(ID=6:9,ORIG=rep("b/fn.csv",4),set=c(NA_character_,"set2","set2",NA_character_))
    cmpdl = rbindlist(list(cmpdl1,cmpdl2))
    x = process_cmpd_sets(cmpdl)
    expect_snapshot(x)
    
    ## Empty input.
    cmpdl = EMPTY_CMPD_LIST
    x = process_cmpd_sets(cmpdl)
    expect_snapshot(x)


})
