##' @include abstracts.R parameters.R QhatModel.homo.normal.linear.AR2.R QhatModel.homo.normal.linear.fire.R
##' @export
QhatModel.homo.normal.linear.AR2.fire <- setClass(
  # Set the name for the class
  "QhatModel.homo.normal.linear.AR2.fire",

  package='hydroState',

  contains=c('QhatModel.homo.normal.linear.AR2','QhatModel.homo.normal.linear.fire'),

  # Set the default values for the slots. (optional)
  prototype=list(
    parameters =  new('parameters',c('mean.a0', 'mean.a1','mean.AR1','mean.AR2','std.a0', 'Kuczera.Lmax','Kuczera.K','Kuczera.tlag'),c(1,1,1,1,1,1,1,1))
  )
)

# Valid object?
validObject <- function(object) {
  TRUE

}
setValidity("QhatModel.homo.normal.linear.AR2.fire", validObject)

# Initialise object
#setGeneric(name="initialize",def=function(.Object,input.data){standardGeneric("initialize")})
setMethod("initialize","QhatModel.homo.normal.linear.AR2.fire", function(.Object, input.data, use.truncated.dist=T, transition.graph=matrix(T,2,2),
                                                                    state.dependent.mean.a0=T, state.dependent.mean.a1=F, state.dependent.mean.trend=NA,
                                                                    state.dependent.mean.AR1=F, state.dependent.mean.AR2=F,
                                                                    state.dependent.std.a0=T) {
  .Object@input.data <- input.data
  .Object@use.truncated.dist = use.truncated.dist
  .Object@nStates = ncol(transition.graph)


  # Set the number of parameter values per parameter name and set up model terms for mean and standard deviation and trend.
  if (is.na(state.dependent.mean.trend)) {
    parameter.length <- as.numeric(c(state.dependent.mean.a0, state.dependent.mean.a1, state.dependent.mean.AR1,state.dependent.mean.AR2,
                                     state.dependent.std.a0,0,0,0)) * (.Object@nStates-1) + 1
    .Object@parameters = new('parameters', c('mean.a0', 'mean.a1', 'mean.AR1', 'mean.AR2','std.a0','Kuczera.Lmax','Kuczera.K','Kuczera.tlag'), parameter.length)
  } else {
    parameter.length <- as.numeric(c(state.dependent.mean.a0, state.dependent.mean.a1, state.dependent.mean.trend, state.dependent.mean.AR1,
                                     state.dependent.mean.AR2, state.dependent.std.a0,0,0,0)) * (.Object@nStates-1) + 1
    .Object@parameters = new('parameters', c('mean.a0', 'mean.a1','mean.trend', 'mean.AR1', 'mean.AR2','std.a0','Kuczera.Lmax','Kuczera.K','Kuczera.tlag'), parameter.length)
  }

  validObject(.Object)
  .Object
}
)

setMethod(f="is.stationary",signature=c("QhatModel.homo.normal.linear.AR2.fire"),definition=function(.Object)
{
  # Get object parameter list
  parameters = getParameters(.Object@parameters)

  # Find roots of AR quadratic equations.
  AR1 <- parameters$mean.AR1
  if (length(AR1)==1)
    AR1 = rep(AR1, .Object@nStates)

  AR2 <- parameters$mean.AR2
  if (length(AR2)==1)
    AR2 = rep(AR2, .Object@nStates)

  AR.polynomial.coeffs = cbind(rep(1, .Object@nStates), -AR1, -AR2)
  AR.polynomial.coeffs = unique(AR.polynomial.coeffs)

  # Find Roots and check if OUTSIDE the unit circle ie stable AR model
  for (i in 1:nrow(AR.polynomial.coeffs)) {
    # get roots
    AR.polynomial.roots = polyroot(as.vector(AR.polynomial.coeffs[i,]))

    # If unit circle, the return false for AR being non-stationary
    if (any(abs(AR.polynomial.roots)<=1))
      return(FALSE)
  }
  return(TRUE)

}
)

setMethod(f="getMean",signature=c("QhatModel.homo.normal.linear.AR2.fire","data.frame"),definition=function(.Object, data)
        {
          #return(getMean.AR2(.Object, data))
          fire.impact = getKuczera(.Object)
          return(getMean.AR2(.Object, data)-fire.impact)
        }
)

