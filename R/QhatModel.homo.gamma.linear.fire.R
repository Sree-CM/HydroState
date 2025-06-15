##' @include abstracts.R  QhatModel.homo.normal.linear.fire.R  QhatModel.homo.gamma.linear.R
##' @export
QhatModel.homo.gamma.linear.fire <- setClass(
  # Set the name for the class
  "QhatModel.homo.gamma.linear.fire",

  package='hydroState',

  contains=c('QhatModel.homo.normal.linear.fire','QhatModel.homo.gamma.linear'),

    # Set the default values for the slots. (optional)
  prototype=list(
    input.data = data.frame(year=c(0),month=c(0),precipitation=c(0),fire =c(0)),
    nStates = Inf,
    use.truncated.dist=F,
    parameters = new('parameters',c('mean.a0', 'mean.a1','std.a0','Kuczera.Lmax','Kuczera.K','Kuczera.tlag'),c(1,1,1,1,1,1))
  )
)


# # Initialise object
#setGeneric(name="initialize",def=function(.Object,input.data){standardGeneric("initialize")})
setMethod("initialize","QhatModel.homo.gamma.linear.fire", function(.Object, input.data, transition.graph=matrix(T,2,2),state.dependent.mean.a0=T,
                                                                      state.dependent.mean.a1=F, state.dependent.mean.trend=NA,state.dependent.std.a0=T) {
  .Object@input.data <- input.data
  .Object@use.truncated.dist <- F
  .Object@nStates = ncol(transition.graph)

  # Set the number of parameter values per parameter name.
  parameter.length <- as.numeric(c(state.dependent.mean.a0, state.dependent.mean.a1, state.dependent.std.a0,0,0,0)) * (.Object@nStates-1) + 1

  # Set up model terms for mean and standard deviation.
  .Object@parameters = new('parameters', c('mean.a0', 'mean.a1', 'std.a0','Kuczera.Lmax','Kuczera.K','Kuczera.tlag'), parameter.length)
  validObject(.Object)
  .Object
}
)


