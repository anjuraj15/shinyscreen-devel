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
    ms2 = STATE_DATA$extr$ms2
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
