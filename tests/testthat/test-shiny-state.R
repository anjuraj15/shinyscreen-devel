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
