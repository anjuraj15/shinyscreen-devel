test_that("filetag_add_file works properly",{
    input1=list(file=character(),
                tag=character())
    output1=filetag_add_file(input1,
                               c("file1.mzML",
                                 "file2.mzML",
                                 "file3.mzML"))

    expect_snapshot(output1)

    input2=output1
    input2$tag[[2]]='XY'

    output2=filetag_add_file(input2,c("file4.mzML"))
    expect_snapshot(output2)
    input3=output2
    output3=filetag_add_file(input3,c("file2.mzML"))
    expect_snapshot(output3)
})

test_that("datatab_update_tags works properly",{
    tab1=list(tag=character(0),
              adduct=character(0),
              set=character(0),
              file=character(0))
    out1 = datatab_update_tags(tab=tab1,
                               tags=character(0),
                               files=character(0))
    expect_snapshot(out1)

    out2 = datatab_update_tags(tab=tab1,
                               tags=c("A","B"),
                               files=c("f1.f","f2.f"))
    
    expect_snapshot(out2)

    tab2 = out2
    out3 = datatab_update_tags(tab=tab1,
                               tags=c("X","B"),
                               files=c("f1.f","f2.f"))
    expect_snapshot(out3)


    tab3 = out3
    out4 = datatab_update_tags(tab=tab3,
                               tags=c("Y","Z"),
                               files=c("fx.f","fy.f"))
    expect_snapshot(out4)

    
    
})
