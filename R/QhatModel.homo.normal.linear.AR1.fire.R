##' @include abstracts.R QhatModel.homo.normal.linear.AR1.R QhatModel.homo.normal.linear.Kuczera.impact.R
##' @export
QhatModel.homo.normal.linear.AR1.fire <- setClass(
  # Set the name for the class
  "QhatModel.homo.normal.linear.AR1.fire",

  package='hydroState',

  contains=c('QhatModel.homo.normal.linear.AR1','QhatModel.homo.normal.linear.Kuczera.impact'),

  # Set the default values for the slots. (optional)
  prototype=list(
    parameters =  new('parameters',c('mean.a0', 'mean.a1','mean.AR1','std.a0','Kuczera.Lmax','Kuczera.K','Kuczera.tlag'),c(1,1,1,1,1,1,1))
  )
)

# Valid object?
validObject <- function(object) {
  TRUE

}
setValidity("QhatModel.homo.normal.linear.AR1.fire", validObject)

# Initialise object
#setGeneric(name="initialize",def=function(.Object,input.data){standardGeneric("initialize")})
setMethod("initialize","QhatModel.homo.normal.linear.AR1.fire", function(.Object, use.truncated.dist=T, input.data, transition.graph=matrix(T,2,2),
                                                                    state.dependent.mean.a0=T, state.dependent.mean.a1=F,state.dependent.mean.trend=NA,state.dependent.mean.AR1=F, state.dependent.std.a0=T) {
  .Object@input.data <- input.data
  .Object@use.truncated.dist = use.truncated.dist
  .Object@nStates = ncol(transition.graph)

  # Set the number of parameter values per parameter name and set up model terms for mean and standard deviation and trend.
  if (is.na(state.dependent.mean.trend)) {
    parameter.length <- as.numeric(c(state.dependent.mean.a0, state.dependent.mean.a1, state.dependent.mean.AR1,
                                     state.dependent.std.a0,0,0,0)) * (.Object@nStates-1) + 1
    .Object@parameters = new('parameters', c('mean.a0', 'mean.a1', 'mean.AR1', 'std.a0','Kuczera.Lmax','Kuczera.K','Kuczera.tlag'), parameter.length)
  } else {
    parameter.length <- as.numeric(c(state.dependent.mean.a0, state.dependent.mean.a1, state.dependent.mean.trend, state.dependent.mean.AR1,
                                     state.dependent.std.a0)) * (.Object@nStates-1) + 1
    .Object@parameters = new('parameters', c('mean.a0', 'mean.a1', 'mean.trend','mean.AR1', 'std.a0','Kuczera.Lmax','Kuczera.K','Kuczera.tlag'), parameter.length)
  }

  validObject(.Object)
  .Object
}
)

setGeneric(name="is.stationary",def=function(.Object) {standardGeneric("is.stationary")})
setMethod(f="is.stationary",signature=c("QhatModel.homo.normal.linear.AR1.fire"),definition=function(.Object)
{
  # Get object parameter list
  parameters = getParameters(.Object@parameters)

  if (all(parameters$mean.AR1 >-1 & parameters$mean.AR1 <1)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
)


# Calculate the transformed flow at the mean annual precip
setMethod(f="getMean",signature=c("QhatModel.homo.normal.linear.AR1.fire","data.frame"),definition=function(.Object, data) {
          #return(getMean.AR1(.Object, data))
          fire.impact = getKuczera(.Object)
          return(getMean.AR1(.Object, data)-fire.impact)
  }
)

