##' @include abstracts.R parameters.R QhatModel.homo.normal.linear.R
##' @export
QhatModel.homo.normal.linear.fire <- setClass(  #class
  # Set the name for the class
  "QhatModel.homo.normal.linear.fire",

  package='hydroState',

  contains=c('QhatModel.homo.normal.linear'),#Parent file


  # Set the default values for the slots. (optional)
  prototype=list(
    input.data = data.frame(year=c(0),month=c(0),precipitation=c(0), fire =c(0)),
    nStates = Inf,
    use.truncated.dist=T,
    parameters = new('parameters',c('mean.a0', 'mean.a1','std.a0','Kuczera.Lmax','Kuczera.K','Kuczera.tlag'),c(1,1,1,1,1,1))

  )
)


# Initialise object
#setGeneric(name="initialize",def=function(.Object,input.data){standardGeneric("initialize")})
setMethod("initialize","QhatModel.homo.normal.linear.fire", function(.Object, input.data, use.truncated.dist=T, transition.graph=matrix(T,2,2),
                                                                state.dependent.mean.a0=T, state.dependent.mean.a1=F, state.dependent.mean.trend=NA, state.dependent.std.a0=T) {
  .Object@input.data <- input.data

  .Object@use.truncated.dist = use.truncated.dist

  .Object@nStates = ncol(transition.graph)

  # Set the number of parameter values per parameter name and set up model terms for mean and standard deviation and trend.
  if (is.na(state.dependent.mean.trend)) {
    parameter.length <- as.numeric(c(state.dependent.mean.a0, state.dependent.mean.a1, state.dependent.std.a0,1,1,1)) * (.Object@nStates-1) + 1
    .Object@parameters = new('parameters', c('mean.a0', 'mean.a1', 'std.a0','Kuczera.Lmax','Kuczera.K','Kuczera.tlag'), parameter.length)
  } else {
    parameter.length <- as.numeric(c(state.dependent.mean.a0, state.dependent.mean.a1, state.dependent.mean.trend, state.dependent.std.a0,1,1,1)) * (.Object@nStates-1) + 1
    .Object@parameters = new('parameters', c('mean.a0', 'mean.a1', 'mean.trend', 'std.a0', 'Kuczera.Lmax','Kuczera.K','Kuczera.tlag'), parameter.length)
  }

  validObject(.Object)
  .Object
}
)


# Calculate the transformed flow at the mean annual precip
setGeneric(name="getMean",def=function(.Object, data) {standardGeneric("getMean")})
setMethod(f="getMean",signature=c("QhatModel.homo.normal.linear.fire","data.frame"),definition=function(.Object, data)
{

            # Get object parameter list
            parameters = getParameters(.Object@parameters)

            ncols.a1 = length(parameters$mean.a1)
            ncols.a0 = length(parameters$mean.a0)
            ncols.Kuczera.tlag = length(parameters$Kuczera.tlag)
            ncols.trend = 0
            if ('mean.trend' %in% names(parameters)) {
              ncols.trend = length(parameters$mean.trend)
            }
            nrows = length(data$Qhat.precipitation);
            ncols.max = max(c(ncols.a0 ,ncols.a1, ncols.trend, ncols.Kuczera.tlag))

            if (ncols.max > .Object@nStates)
              stop(paste('The number of parameters for each term of the mean model must must equal 1 or the number of states of ',.Object@nStates))

            # Check which terms are uniform for all states and whic terms are unique
            # to each state.
            if (ncols.a0==1 || ncols.a0==.Object@nStates) {
              a0.est = matrix(rep(parameters$mean.a0,each=nrows),nrows,.Object@nStates);
            } else if (ncols.a0<.Object@nStates) {
              stop(paste('The number of parameters for the a0 term of the mean model must must equal 1 or the number of states of ',.Object@nStates))
            }
            if (ncols.a1==1 || ncols.a1==.Object@nStates) {
              a1.est = matrix(rep(parameters$mean.a1,each=nrows),nrows,.Object@nStates);
            } else if (ncols.a1<.Object@nStates) {
              stop(paste('The number of parameters for the a1 term of the mean model must must equal 1 or the number of states of ',.Object@nStates))
            }
            if (ncols.trend==1 || ncols.trend==.Object@nStates) {
              trend.est = matrix(rep(parameters$mean.trend,each=nrows),nrows,.Object@nStates);
            } else {
              trend.est = 0
            }

            time.vals = matrix(data$year - data$year[1],nrows,.Object@nStates)
            precip.data = matrix(data$Qhat.precipitation,nrows,.Object@nStates);

            # Calculate the non-AR1 componants
            a0.est <- 100 * a0.est
            Qhat.model <- precip.data * a1.est + a0.est + time.vals * trend.est

          #Calculate Kuczera curve fire impact
            fire.year = which(.Object@input.data$fire==1)
            fire.Qhat <- matrix(0, nrows, length(fire.year))
            ind =0
            tlag=.Object@parameters@Kuczera.tlag@value
            Lmax =.Object@parameters@Kuczera.Lmax@value
            K =.Object@parameters@Kuczera.K@value
            for (i in fire.year){
              # getting the no. of years by deducing the lag years
              lagged_year = fire.year+tlag
              years.postfire = pmax(0,data$years - fire.year.tlag)
              ind=ind+1
              fire.Qhat[,ind] =Lmax*K*years.postfire*exp(1-K*years.postfire)
              }
            Qhat.model =Qhat.model-colSums(fire.Qhat)
            return(Qhat.model)
          }
)
