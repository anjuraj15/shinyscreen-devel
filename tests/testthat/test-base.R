test_that("gen_fname_slug",{

    r1 = gen_fname_slug("file_name.csv")
    expect_true(r1=="file_name")
    r2 = gen_fname_slug(".file.name.csv")
    expect_true(r2==".file.name")
    r3 = gen_fname_slug("file name    with blanks.x.y.csv")
    expect_true(r3=="file_name_with_blanks.x.y")
    r3 = gen_fname_slug("file    name____with blanks____x.y.csv")
    expect_true(r3=="file_name_with_blanks_x.y")
})

test_that("uniqy_slugs",{
    inp = c('f1','f2','f2','f2','f3','f4','f4','f5','f3','f6')
    out = uniqy_slugs(inp)
    expect_snapshot(out)
})
