test_that("pack_ms2_w_summ",{
    summ = STATE_DATA$out$tab$summ
    ms2 = STATE_DATA$extr$ms2
    res = pack_ms2_w_summ(summ,ms2)
    expect_identical(res[1,adduct],'[M+H]+')
    expect_identical(res[1,tag],'AA')
    expect_identical(res[1,ID],'100')
    expect_equal(res[1,an],5413)
    expect_equal(res[1,mz],182.0816)

    expect_identical(res[29,adduct],'[M+H]+')
    expect_identical(res[29,tag],'WT')
    expect_identical(res[29,ID],'112')
    expect_equal(res[29,an],2838)
    expect_equal(res[29,mz],268.1045)
    
    
    
})
