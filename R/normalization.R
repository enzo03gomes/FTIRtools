#' Normalize FTIR spectra
#' @param spr object of class spectra
#' @param type normalization type. Options are "L-norm", "area", "max" and "SNV"
#' @param order Order of L-norm. Used only if type == "L-norm"
#' @param target If type == "max", target is the new maximum value; if type == "SNV", target is the new total area
#'
#'@export

spr.norm = function(spr, type = "L-norm", order = 2, target = 1){
  if(!is.spr(spr)) stop("spr must be of class spectra")
  if(!any(c("L-norm", "area", "max", "SNV") == type)) stop("Invalid type")
  if(!is.numeric(order) | length(order) != 1) stop("order must be a single numeric")
  if(!is.numeric(target) | length(target) != 1) stop("target must be a single numeric")

  #extract response and wavenumber
  y = spr@response
  x = spr@spectralAxis

  #perform normalisation
  switch (type,
          "L-norm" = {
            Y = torch_tensor(y)

            n = as.numeric(torch_sum(torch_pow(Y, order)))
            normY = y / ((n) ^ (1 / order))

          },
          "area" = {
            dx = diff(x)
            yavg = (y[-length(y)] + y[-1])/2

            n = sum(dx * yavg)
            normY = y * target / n


          },
          "max" = {
            m = max(y)
            normY = y * target / m

          },
          "SNV" = {
            m = mean(y)
            s = sd(y)
            normY = (y - m) * target / s

          }
  )

  #return spectra
  spr@response = normY
  if(type == "L-norm"){
    spr@history = append(spr@history, paste0("Normalisation: L", order))
  }else{
    spr@history = append(spr@history, paste0("Normalisation: ", type))
  }

  return(spr)
}

