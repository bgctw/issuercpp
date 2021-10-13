#' Call the C function causing issues
#'
#' @return result of the C function
#' @export
call_cfunc <- function(){
  .whichValueGreaterEqual(1:3,1,2)
}


.binWithEqualValuesBalanced <- function(
  ### Create a binning factor with shortening following bins
	x				        ##<< sorted numeric vector to sort into bins
	, nBin			    ##<< intended number of bins
	, tol = 1e-8		##<< distance between successive values of x that
	  ## are treated to be equal
	, isBinSizeFloorUsed = TRUE	##<< set to FALSE to postpone rounding on
	  ## start and end values
) {
	if (nBin == 1L) return(integer(length(x)))
	binSize <- length(x) / nBin
	##details<<
	## Equal values of x end up in the same bin
	## It shortens the following bins
	## By not taking the floor, a better distribution of
	## samples across bins is achieved.
	## But here keep it due to compatibility to C-Code.
	if (isBinSizeFloorUsed) binSize <- floor(binSize)
	breaksX <- which(diff(x) > tol) + 1
	binId <- rep(1L, length(x))
	iBreak <- 1L	# index from which to seek next break
	#iClass <- 2L
	for (iClass in 2L:as.integer(nBin)) {
		start0 <- round((iClass - 1) * binSize) + 1
		iBreak <- .whichValueGreaterEqual(breaksX, start0, iStart = iBreak)
		start1 <- breaksX[iBreak]
		# find next uStar change at or after position start0
		#start1Slow <- breaksX[breaksX >= start0][1]
		binId[start1:length(x)] <- iClass
	}
	##value<< integer vector of same length as x, with unique value for each bin
	binId
}

#' @useDynLib issuercpp _issuercpp_whichValueGreaterEqualC
#' @import Rcpp
.whichValueGreaterEqual <- function(
	### search first element in an integer vector that is larger
	x			##<< increasingly sorted numeric vector to search
	, threshold	##<< integer scalar: searched element will need to
	  ## be greater or equal as this argument
	, iStart = 1L	##<< index in vector to start search
) {
	##author<< TW
	# see tests / test_binWithEqualValues.R
	#which(x >= threshold)[1]
  #
	# for performance reasons call a c ++ function that loops across the vector
	#
	# cannot generate C function with dot
	# Rcpp::compileAttributes() generates a function without leading dot,
	# need to adjust by hand afterwards
	# or otherwise export but make sure its documented
	##details<<
	## searches a sorted integer vector for the next element
	## that is >= a threshold in fast C-code
	#wutz211013: breaks ci - stalls - no cue - revert to R
  ans <- whichValueGreaterEqualC(
	  as.integer(x), as.integer(threshold), as.integer(iStart) )
  return(ans)
  # if (iStart > length(x)) return(NA_integer_)
  #  return(iStart - 1 + which(x[iStart:length(x)] >= threshold)[1])
  ##value<<
	## Scalar integer: first index in x, that is >= iStart,
	## and whose value x[i] is >= threshold.
	## If no index was found, returns NA
}
