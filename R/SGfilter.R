#'@import torch
NULL

#' Calculate the matrix of paramaters for Savitsky-Golay Filter
#' @param M window half-width
#' @param N polynomial order
#'
#' @return the pseudo-inverse of the Valdemonde matrix
#'
#' @export

SG.parameters = function(N,M){
  stopifnot("N must be an integer" = is.whole(N))
  stopifnot("M must be an integer" = is.whole(M))

  if(N>=2*M){
    stop("For smoothing to be effective, N<2M")
  }

  #calculate the Valdemondo matrix
  A = matrix(0, nrow = 2*M + 1, ncol = N+1)
  index = 1

  for(n in -M:M){
    A[index,] = n ^ (0:N)

    index = index +1
  }

  #calculate the pseudo-inverse of A
  A = torch_tensor(A)
  tA = A$t()

  H = torch_matmul(tA, A)
  H = torch_inverse(H)
  H = torch_matmul(H, tA)

  return(H)
}


#' Savitzki-Golay filter for FTIR
#'
#' @param spr object of class spr
#' @param M window half-width
#' @param N polynomial order
#' @param H pseudo-inverse of the Valdemondo matrix. Overrides given values of N,M.
#' @param padding if TRUE, add a padding of length M
#'
#' @export

SG.filter = function(spr, N, M, H, padding = TRUE){
  if(!is.spr(spr)) stop("spr must be of class spectra")
  if(!is.logical(padding)) stop("padding must be logical")

  if(missing(H)){
    if(missing(N)|missing(M)){
      stop("If H is not provided, N and M must be.")
    }

    H = SG.parameters(N,M)
  }else{
    N = nrow(H) - 1
    M = (ncol(H) - 1) / 2
  }

  #extract relevant data
  h = torch_tensor(H[1,])
  x = spr@response

  #add padding if needed
  if(padding){
    pad1 = rep(mean(x[1:M]), M)
    pad2 = rep(mean(x[(length(x)-M) : (length(x))]), M)

    x = c(pad1, x, pad2)
  }
  x = torch_tensor(x)


  #perform discrete convolution
  x = x$view(c(1, 1, -1))
  h = h$view(c(1, 1, -1))


  y = torch_conv1d(x, h, stride = 1)

  y = as.numeric(y$squeeze(1)$squeeze(1))
  x = as.numeric(x$squeeze(1)$squeeze(1))

  if(!padding){
    x = x[(M+1) : (length(x)-M)]
    wvn = spr@spectralAxis[(M+1) : (length(x)-M)]
  }else{
    wvn = spr@spectralAxis
  }

  spr@spectralAxis = wvn
  spr@response = y
  spr@history = append(spr@history, paste0("Filter: SG ", N, "x", M))

  return(spr)
}
