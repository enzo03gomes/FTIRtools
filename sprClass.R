#setup new class
  setClass("spectra", representation = list(spectralAxis = "numeric",
                                            response = "numeric",
                                            axis = "character",
                                            units = "character",
                                            sample = "character",
                                            history = "character"))
  
  
  
  
  
  
  as.spr = function(data, axis, units, sample, history = ""){
    if(missing(data)) stop("Missing data")
      if(length(data) != 2) stop("Incorrectly formated data")
      if(length(data[[1]])!=length(data[[2]])) stop("Length of data vectors must be equal")
    
    if(missing(axis)) stop("Missing axis")
      if(!any(c("wavenumber", "wavelength", "frequency") == axis[1])) stop("Invalid axis")
      if(!any(c("absorbance", "transmittance", "SB") == axis[2])) stop("Invalid axis")
    
    if(missing(units)) stop("Missing units")
      if(length(units) != 2 | !is.character(units)) stop("Incorrectly formatted units")
    
      names(units) = c("spectralAxis", "response")
    
    
    if(missing(sample)){
      sample = deparse(substitute(data))  
    }else{
      sample = tryCatch(as.character(sample),
                        error = function(e){
                          stop("Invalid sample name")
                        })
    }
    
    
    
    new("spectra", spectralAxis = data[[1]],
                   response = data[[2]],
                   axis = axis,
                   units = units,
                   sample = sample,
                   history = history)
  }
  
  
  
  
  
  
  setMethod("show", "spectra", function(object){
    cat("Spectra:             ", object@sample, "\n")
    cat("axis:                ", object@axis[2], " ~ ", object@axis[1], "\n")
    cat("Scan range:          ", round(min(object@spectralAxis),2), "to", round(max(object@spectralAxis), 2), "\n")
    cat("Response range:      ", round(min(object@response),2), "to", round(max(object@response),2), "\n")
    #cat("Filter:              ", object@filterType,"\n")
    
    if(object@history[length(object@history)] != ""){
      cat("Change history:\n")
      
      for(i in 2:length(object@history)){
        cat("                (",i-1,") ", object@history[i], "\n", sep = "")
      }  
      
    }else{
      cat("Change history: none\n") 
    }
    
  })
  
  
  
  
  
  
  is.spr = function(object){
    if(class(object)[1] == "spectra"){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
  
  
  
  
  
  