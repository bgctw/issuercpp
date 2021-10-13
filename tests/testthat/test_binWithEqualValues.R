#require(testthat)
context(".binWithEqualValues")
# Furher context: fCheckColNames, fCheckColNumeric, fCheckOutsideRange

if( !exists(".whichValueGreaterEqual") ) .whichValueGreaterEqual <- issuercpp:::.whichValueGreaterEqual
if( !exists(".binWithEqualValuesBalanced") ) .binWithEqualValuesBalanced <- issuercpp:::.binWithEqualValuesBalanced

test_that(".whichValueGreaterEqual",{
			x <- 1:10
			expect_equal( .whichValueGreaterEqual(x, 5L), 5L )
			expect_equal( .whichValueGreaterEqual(x, 11L), NA_integer_ )
			expect_equal( .whichValueGreaterEqual(x, 5L, 3L), 5L )
			expect_equal( .whichValueGreaterEqual(x, 7L, 3L), 7L )
			expect_equal( .whichValueGreaterEqual(x, 3L, 3L), 3L )
			expect_equal( .whichValueGreaterEqual(x, 1L, 3L), 3L ) # srarting at 3
			expect_equal( .whichValueGreaterEqual(x, 1L), 1L )
			expect_equal( .whichValueGreaterEqual(x, 10, 10L), 10L )
			expect_equal( .whichValueGreaterEqual(x, 10, 11L), NA_integer_ )
		})

test_that(".binWithEqualValuesBalanced",{
			expect_equal( .binWithEqualValuesBalanced(1:10, 3L), c(1,1,1,2,2,2,3,3,3,3) )
			expect_equal( .binWithEqualValuesBalanced(c(rep(1,4),2:7), 3L), c(rep(1,4),2,2,3,3,3,3) ) # one 2 replaced by 1
			expect_equal( .binWithEqualValuesBalanced(c(rep(1,4),2,2,3,3,3), 3L), c(rep(1,4),2,2,3,3,3) ) # one 2 replaced by 1
			expect_equal( .binWithEqualValuesBalanced(c(rep(1,7),2:4), 3L), c(rep(1,7),3,3,3) ) # group 2 disappeard
		})


.tmp.benchmark <- function(){

}


