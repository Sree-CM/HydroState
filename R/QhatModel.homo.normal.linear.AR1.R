##' @include abstracts.R QhatModel.homo.normal.linear.R
##' @export
QhatModel.homo.normal.linear.AR1 <- setClass(
  # Set the name for the class
  "QhatModel.homo.normal.linear.AR1",

  package='hydroState',

  contains=c('QhatModel.homo.normal.linear'),

  # Set the default values for the slots. (optional)
  prototype=list(
    parameters =  new('parameters',c('mean.a0', 'mean.a1','mean.AR1','std.a0'),c(1,1,1,1))
  )
)

# Valid object?
validObject <- function(object) {
  TRUE

}
setValidity("QhatModel.homo.normal.linear.AR1", validObject)

# Initialise object
#setGeneric(name="initialize",def=function(.Object,input.data){standardGeneric("initialize")})
setMethod("initialize","QhatModel.homo.normal.linear.AR1", function(.Object, use.truncated.dist=T, input.data, transition.graph=matrix(T,2,2),
                                                                    state.dependent.mean.a0=T, state.dependent.mean.a1=F,state.dependent.mean.trend=NA,state.dependent.mean.AR1=F, state.dependent.std.a0=T) {
  .Object@input.data <- input.data
  .Object@use.truncated.dist = use.truncated.dist
  .Object@nStates = ncol(transition.graph)

  # Set the number of parameter values per parameter name and set up model terms for mean and standard deviation and trend.
  if (is.na(state.dependent.mean.trend)) {
    parameter.length <- as.numeric(c(state.dependent.mean.a0, state.dependent.mean.a1, state.dependent.mean.AR1,
                                     state.dependent.std.a0)) * (.Object@nStates-1) + 1
    .Object@parameters = new('parameters', c('mean.a0', 'mean.a1', 'mean.AR1', 'std.a0'), parameter.length)
  } else {
    parameter.length <- as.numeric(c(state.dependent.mean.a0, state.dependent.mean.a1, state.dependent.mean.trend, state.dependent.mean.AR1,
                                     state.dependent.std.a0)) * (.Object@nStates-1) + 1
    .Object@parameters = new('parameters', c('mean.a0', 'mean.a1', 'mean.trend','mean.AR1', 'std.a0'), parameter.length)
  }

  validObject(.Object)
  .Object
}
)

setGeneric(name="is.stationary",def=function(.Object) {standardGeneric("is.stationary")})
setMethod(f="is.stationary",signature=c("QhatModel.homo.normal.linear.AR1"),definition=function(.Object)
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
setMethod(f="getMean",signature=c("QhatModel.homo.normal.linear.AR1","data.frame"),definition=function(.Object, data) {
          return(getMean.AR1(.Object, data))
  }
)

# Calculate the transformed flow at the mean annual precip
setGeneric(name="getMean.AR1",def=function(.Object, data) {standardGeneric("getMean.AR1")})
setMethod(f="getMean.AR1",signature=c("QhatModel.homo.normal.linear.AR1","data.frame"),definition=function(.Object, data)
          {
            # Find time points with finite PRECIPITATION  data
            filt <- is.finite(data$Qhat.precipitation)
            data <- data[filt,]

            # Get object parameter list
            parameters = getParameters(.Object@parameters)

            ncols.a1 = length(parameters$mean.a1)
            ncols.a0 = length(parameters$mean.a0)
            ncols.trend = 0
            if ('mean.trend' %in% names(parameters)) {
              ncols.trend = length(parameters$mean.trend)
            }
            ncols.AR1 = length(parameters$mean.AR1)
            nrows = length(data$Qhat.precipitation);
            ncols.max = max(c(ncols.a0 ,ncols.a1,ncols.trend,ncols.AR1))
            if (ncols.max > .Object@nStates)
                stop(paste('The number of parameters for each term of the mean model must must equal 1 or the number of states of ',.Object@nStates))

            # Check the AR1 model is stationary.
            if (!is.stationary(.Object)){
              Qhat.model <- matrix(Inf, length(filt), .Object@nStates)
              return(Qhat.model)
            }

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
            if (ncols.AR1==1 || ncols.AR1 ==.Object@nStates) {
              AR1.est = matrix(rep(parameters$mean.AR1,each=nrows),nrows,.Object@nStates);
            } else if (mean.AR1<.Object@nStates) {
              stop(paste('The number of parameters for the a0 term of the mean model must must equal 1 or the number of states of ',.Object@nStates))
            }
            if (ncols.trend==1 || ncols.trend==.Object@nStates) {
              trend.est = matrix(rep(parameters$mean.trend,each=nrows),nrows,.Object@nStates);
            } else {
              trend.est = matrix(rep(0,each=nrows),nrows,.Object@nStates);
            }

            time.vals = matrix(data$year - data$year[1],nrows,.Object@nStates)
            precip.data = matrix(data$Qhat.precipitation,nrows,.Object@nStates);

            # Calculate the mean with the AR1 componants
            a0.est <- 100 * a0.est
            Qhat.model <- matrix(0, nrows, .Object@nStates)
            Qhat.model[1,] <- precip.data[1,] * a1.est[1,] + a0.est[1,] + time.vals[1,] * trend.est[1,]
            for (i in 2:nrows) {
              Qhat.model[i,] <- precip.data[i,] * a1.est[i,] + a0.est[i,] + Qhat.model[i-1,] * AR1.est[i,] + time.vals[i,] * trend.est[i,]
            }

            Qhat.model.NAs = matrix(NA,length(filt),.Object@nStates)
            Qhat.model.NAs[filt,] <- Qhat.model

            # message(paste('...DBG getMean.AR1 nrows Qhat.model.NAs:',nrow(Qhat.model.NAs)))

            return(Qhat.model.NAs)
            print(Qhat.model)
          }
)
