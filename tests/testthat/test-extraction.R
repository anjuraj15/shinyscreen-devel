## TODO: FIXME: the NA case, is this correct behaviour? 
test_that("Test find_ms1_max behaviour.",{
    rt = c(1.,1.5,1.7,3.,5.6,7.,7.1,8.,9.5,10.)
    intensity= c(NA,NA,NA,0.1,0.2,0.3,0.19,0.09,NA,NA)
    x = find_ms1_max(rt,intensity,0.9,11.)

    expect_equal(x[[1]],7.)
    expect_equal(x[[2]],0.3)

    rt = c(1.,1.5,1.7,3.,5.6,7.,7.1,8.,9.5,10.)
    intensity= c(NA,NA,NA,0.1,0.3,0.2,0.3,0.4,0.25,NA)
    x = find_ms1_max(rt,intensity,c(3.,7.1),c(7.,10.))
    expect_equal(x[1,1],5.6)
    expect_equal(x[1,2],8.)
    expect_equal(x[2,1],0.3)
    expect_equal(x[2,2],0.4)
    x = find_ms1_max(rt,intensity,1.,1.7)
    ## expect_true(is.na(x)) # This, or
    ## expect_equal(length(x),0L) # ... that?


})
