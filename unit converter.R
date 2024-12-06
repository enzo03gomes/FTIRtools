#'remove padding on FTIR data 
#'
#'@param spr object of class "spectra"
#'@export
  pad.remove = function(spr){
    if(!is.spr(spr)) stop("spr must be of class spectra")
    
    remove = which(spr@response == 0)
    
    if(length(remove) != 0){
      spr@spectralAxis = spr@spectralAxis[-remove]
      spr@response = spr@response[-remove]
    }
    
    spr@history = append(spr@history, "remove padding")
    return(spr)
  }
  
  

#' Convert Single Beam to Absorbance
#' 
#' @param spr object of class "spectra"
#' @param background object of class "spectra" with beackground measurement
  SB2abs = function(spr, background){
    if(!is.spr(spr)) stop("spr must be of class spectra")
    if(!is.spr(background)) stop("background must be of class spectra")
    
    if(spr@axis[2] == "SB"){
      spr@response = spr@response / background@response  
      spr@axis[2] = "absorbance"
      spr@history = append(spr@history, "SingleBeam to absorbance")
    }
    
    return(spr)
  }
  
    
#'convert transmittance to absorbance
#'
#'@param spr object of class "spectra"
#'@export
  trans2abs = function(spr){
    if(!is.spr(spr)) stop("spr must be of class spectra")
    
    if(spr@axis[2] == "transmittance"){
      spr@response = -log10(spr@response / 100)
      spr@axis[2] = "absorbance"
      spr@units[2] = "unitless"
      spr@history = append(spr@history, "transmittance to absorbance")
    }
    
    
    return(spr) 
  }

  

#'convert absorbance to transmittance
#'
#'@param spr object of class "spectra"
#'@export  
  abs2trans = function(spr){
    if(!is.spr(spr)) stop("spr must be of class spectra")
    
    if(spr@axis[2] == "absorbance"){
      spr@response = -(10^spr@response)*100
      spr@axis[2] = "transmittance"
      spr@units[2] = "%"
      spr@history = append(spr@history, "absorbance to transmittance")
    }
    
    return(spr)
  }


#'convert wavenumber to wavelengt
#'
#'@param spr object of class "spectra"
#'@export 
  wavenum2lenght = function(spr){
    if(!is.spr(spr)) stop("spr must be of class spectra")
    
    if(spr@axis[1] == "wavenumber"){
      spr@response = 1/spr@response
      spr@axis[1] = "wavelength"
      spr@units[1] = "cm"
      spr@history = append(spr@history, "wavenumber to wavelenght")
    }
    
    return(spr)
  }
  
  
#'convert wavelength to wavenumber
#'
#'@param spr object of class "spectra"
#'@export 
  wavelength2num = function(spr){
    if(!is.spr(spr)) stop("spr must be of class spectra")
    
    if(spr@axis[1] == "wavelength"){
      spr@response = 1/spr@response
      spr@axis[1] = "wavenumber"
      spr@units[1] = "/cm"
      spr@history = append(spr@history, "wavelength to wavenumber")
    }
    
    return(spr)
  }  
  
  
#'Convert wavelength to frequency
#'
  wavelength2freq = function(spr){
    if(!is.spr(spr)) stop("spr must be of class spectra")
    c = 299792458*10^2 #cm/s
    
    if(spr@axis[1] == "wavelength"){
      spr@response = c / spr@response
      spr@axis[1] = "frequency"
      spr@units[1] = "Hz"
      spr@history = append(spr@history, "wavelength to frequency")
    }
    
    return(spr)
    
  }
  
  
  
#'Convert wavelength to frequency
#'
  wavenum2freq = function(spr){
    if(!is.spr(spr)) stop("spr must be of class spectra")
    c = 299792458*10^2 #cm/s
    
    if(spr@axis[1] == "wavenumber"){
      spr@response = c * spr@response
      spr@axis[1] = "frequency"
      spr@units[1] = "Hz"
      spr@history = append(spr@history, "wavenumber to frequency")
    }
    
    return(spr)
  }
  