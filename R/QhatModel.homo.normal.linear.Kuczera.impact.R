##' @include abstracts.R parameters.R QhatModel.homo.normal.linear.R
##' @export
QhatModel.homo.normal.linear.Kuczera.impact <- setClass(  #class
  # Set the name for the class
  "QhatModel.homo.normal.linear.Kuczera.impact",

  package='hydroState',

  contains=c('QhatModel.homo.normal.linear'),#Parent file


  # Set the default values for the slots. (optional)]
  prototype=list(
    input.data = data.frame(year=c(0),month=c(0),precipitation=c(0), fire =c(0)),
    nStates = Inf,
    use.truncated.dist=T,
    parameters = new('parameters',c('mean.a0', 'mean.a1','std.a0','Kuczera.Lmax','Kuczera.K','Kuczera.tlag'),c(1,1,1,1,1,1))

  )
)


# Initialise object
#setGeneric(name="initialize",def=function(.Object,input.data){standardGeneric("initialize")})
setMethod("initialize","QhatModel.homo.normal.linear.Kuczera.impact", function(.Object, input.data, use.truncated.dist=T, transition.graph=matrix(T,2,2),
                                                                state.dependent.mean.a0=T, state.dependent.mean.a1=F, state.dependent.mean.trend=NA, state.dependent.std.a0=T) {
  .Object@input.data <- input.data

  .Object@use.truncated.dist = use.truncated.dist

  .Object@nStates = ncol(transition.graph)

  # Set the number of parameter values per parameter name and set up model terms for mean and standard deviation and trend.
  if (is.na(state.dependent.mean.trend)) {
    parameter.length <- as.numeric(c(state.dependent.mean.a0, state.dependent.mean.a1, state.dependent.std.a0,0,0,0)) * (.Object@nStates-1) + 1
    .Object@parameters = new('parameters', c('mean.a0', 'mean.a1', 'std.a0','Kuczera.Lmax','Kuczera.K','Kuczera.tlag'), parameter.length)
  } else {
    parameter.length <- as.numeric(c(state.dependent.mean.a0, state.dependent.mean.a1, state.dependent.mean.trend, state.dependent.std.a0,0,0,0)) * (.Object@nStates-1) + 1
    .Object@parameters = new('parameters', c('mean.a0', 'mean.a1', 'mean.trend', 'std.a0', 'Kuczera.Lmax','Kuczera.K','Kuczera.tlag'), parameter.length)
  }

  validObject(.Object)
  .Object
}
)


# Calculate the transformed flow at the mean annual precip
#setGeneric(name="getMean",def=function(.Object, data) {standardGeneric("getMean")})
setMethod(f="getMean",signature=c("QhatModel.homo.normal.linear.Kuczera.impact","data.frame"),definition=function(.Object, data)
{

            # Get object parameter list
            parameters = getParameters(.Object@parameters)

            ncols.a1 = length(parameters$mean.a1)
            ncols.a0 = length(parameters$mean.a0)
            ncols.trend = 0
            if ('mean.trend' %in% names(parameters)) {
              ncols.trend = length(parameters$mean.trend)
            }
            nrows = length(data$Qhat.precipitation);
            ncols.max = max(c(ncols.a0 ,ncols.a1, ncols.trend))

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
            Qhat.fire <- getKuczera(.Object)
            Qhat.model =Qhat.model - Qhat.fire
            #saving qhat values to a csv
            #write.table(Qhat.model, file = "intermediate_values.csv", sep = ",", col.names = !file.exists("intermediate_values.csv"), row.names = FALSE)
            #print( Qhat.model)
            return(Qhat.model)

          }
 )
# Creating separate function for Kuczera component
setGeneric(name="getKuczera",def=function(.Object) {standardGeneric("getKuczera")})
setMethod(f="getKuczera",signature=c("QhatModel.homo.normal.linear.Kuczera.impact"),definition=function(.Object)
{

            # Get object parameter list
            parameters = getParameters(.Object@parameters)

            #Calculate Kuczera curve fire impact
            fire.year = which(.Object@input.data$fire==1)
            nrows = nrow(.Object@input.data);
            fire.Qhat <- matrix(0, nrows, length(fire.year))
            ind =0
            #The Kuczera parameters
            tlag = parameters$Kuczera.tlag
            #print(tlag)
            Lmax = parameters$Kuczera.Lmax
            K = parameters$Kuczera.K
            for (i in fire.year){
              # getting the no. of years by deducing the lag years
              # Message here in the console
              #print(paste("Processing iteration", i))
              #print(paste("Year of fire is  ", fire.year))
              lagged_year = .Object@input.data$year[i] +tlag

              #Adding a new parameter for considering percentage area
              #BA = .Object@input.data$area_burnt[i]

              #print(paste("Year when reduction starts  ", lagged_year))
              years.postfire =  pmax(0,.Object@input.data$year -lagged_year)
              #print yeaes.postfire
              #print(paste("No.of years after fire reduction started  ", years.postfire))
              ind=ind+1
              fire.Qhat[,ind] =Lmax*K*years.postfire*exp(1-K*years.postfire)
              }
            Qhat.fire =rowSums(fire.Qhat)
            #print(paste("Reduction in flow due to fire", Qhat.fire))
            #print(Qhat.fire)
            return(Qhat.fire)
            #plot(.Object@input.data$year,Qhat.fire)
          }
 )
