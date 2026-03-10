##' @include abstracts.R parameters.R QhatModel.homo.normal.linear.R

#' @export getMean

QhatModel.wapaba.normal <- setClass(
  # Set the name for the class
  "QhatModel.wapaba.normal",

  package='hydroState',

  contains=c('QhatModel.homo.normal.linear'),


  # Set the default values for the slots. (optional)
  # prototype=list(
  #   input.data = data.frame(year=c(0),month=c(0),precipitation=c(0)),
  #   precip.delta = data.frame(start.index = c(1),end.index = Inf),
  #   nStates = Inf,
  #   use.truncated.dist=T,
  #   parameters = new('parameters',c('mean.a0', 'mean.a1','std.a0'),c(1,1,1))
  #
  # )
)

# Valid object?
validObject <- function(object) {
  TRUE

}
setValidity("QhatModel.wapaba.normal", validObject)

# Initialise object
# setGeneric(name="initialize",def=function(.Object,input.data, ...){standardGeneric("initialize")})
setMethod("initialize","QhatModel.wapaba.normal", function(.Object, input.data, use.truncated.dist=F, transition.graph=matrix(T,2,2),
                                                                state.dependent.mean.alpha1=F, state.dependent.mean.alpha2=F,state.dependent.mean.beta=F,
                                                                state.dependent.mean.Smax=T,
                                                                state.dependent.mean.tc=F,state.dependent.std.a0=T, state.dependent.std.a1=T) {

  .Object@input.data <- input.data

  .Object@use.truncated.dist = use.truncated.dist

  .Object@nStates = ncol(transition.graph)

  .Object@precip.delta = getStartEndIndex(input.data) # for precipitation / independent variable


  # Set the number of parameter values per parameter name and set up model terms for mean and standard deviation and trend.

  parameter.length <- as.numeric(c(state.dependent.mean.alpha1, state.dependent.mean.alpha2, state.dependent.mean.beta,state.dependent.mean.Smax,
                                   state.dependent.mean.tc, state.dependent.std.a0, state.dependent.std.a1)) * (.Object@nStates-1) + 1
  .Object@parameters = new('parameters', c('mean.alpha1', 'mean.alpha2', 'mean.beta','mean.Smax','mean.tc', 'std.a0','std.a1'), parameter.length)



  validObject(.Object)
  .Object
}
)

setGeneric(name="getVariance",def=function(.Object, data, Qhat.mean) {standardGeneric("getVariance")})
setMethod(f="getVariance",signature=c("QhatModel.wapaba.normal","data.frame"),definition=function(.Object, data, Qhat.mean)
{
  # Get object parameter list
  parameters = getParameters(.Object@parameters)

  ncols.a0 = length(parameters$std.a0)
  nrows = length(data$Qhat.precipitation);

  # Get variance of the Qhat
  Qhat.var = var(data$Qhat.flow, na.rm=T) #single variable meaning a single value, not list or matrix

  #a1.est =  Qhat.mean * matrix(rep(parameters$std.a0 * Qhat.var,each=nrows),nrows,.Object@nStates);
  a1.est =   matrix(rep(parameters$std.a0* Qhat.var,each=nrows),nrows,.Object@nStates) +
             Qhat.mean * parameters$std.a1

  return(a1.est)

}
)


# Calculate the transformed flow at the mean annual precip
setGeneric(name="getMean",def=function(.Object, data, Qhat.object) {standardGeneric("getMean")})
setMethod(f="getMean",signature=c("QhatModel.wapaba.normal","data.frame"),definition=function(.Object, data, Qhat.object)
{

            # Get object parameter list
            parameters = getParameters(.Object@parameters)
            param.length = length(parameters)
            ncols.alpha1 = length(parameters$mean.alpha1)
            ncols.alpha2 = length(parameters$mean.alpha2)
            ncols.beta = length(parameters$mean.beta)
            ncols.Smax = length(parameters$mean.Smax)
            ncols.tc = length(parameters$mean.tc)

            # Define the 5 parameters
            param_names <- c("mean.alpha1", "mean.alpha2", "mean.beta", "mean.Smax", "mean.tc")

            # Create 5×2 matrix for parameter sets at each state - 5 parameters and 2 states
            # Rows: parameters, Columns: states
            param_matrix <- matrix(0, nrow = 5, ncol = .Object@nStates)

            # Fill the parameter matrix
            for (i in seq_along(param_names)) {
              param_name <- param_names[i]
              param_values <- parameters[[param_name]]

              # Check parameter dimensions
              if (length(param_values) == 1) {
                # Single value for all states
                param_matrix[i, ] <- rep(param_values, .Object@nStates)
              } else if (length(param_values) == .Object@nStates) {
                # Different value for each state
                param_matrix[i, ] <- param_values
              } else {
                stop(paste("Parameter", param_name, "must have length 1 or", .Object@nStates))
              }
            }


            # Get data dimensions
            nrows <- length(data$Qhat.precipitation)

            # Initialize Qhat matrix for results (time points x states)
            Qhat.model <- matrix(0, nrow = nrows, ncol = .Object@nStates)

            # Calculate modelled flow for each state using wapaba_model
            for (state in 1:.Object@nStates) {
              # Extract parameters for current state
              state_params <- list(
                alpha1 = param_matrix[1, state],
                alpha2 = param_matrix[2, state],
                beta = param_matrix[3, state],
                Smax = param_matrix[4, state],
                tc = param_matrix[5, state]
              )

              # Calculate flow using wapaba_model for current state
              # Assuming wapaba_model takes data and parameters as arguments
              tryCatch({
                modelled.flow <- wapaba_model(data, c(state_params$alpha1,state_params$alpha2,state_params$beta,state_params$Smax,state_params$tc))
                #cat(paste("Qhat.model before transform",  "\n"))
                #print(modelled.flow ,max=30)
                # print(c(state_params$alpha1,state_params$alpha2,state_params$beta,state_params$Smax,state_params$tc))
                 # Conveting the modelled flow for each state into transformed space. FOr that first convert the QHat.model into dataframe.
                modelled.flow = as.data.frame(modelled.flow)
                colnames(modelled.flow) = "flow"
                #Now doing the logsinh conversion to ensure that the modelled flow is in thetransformed space

                ############Testing for transformation
                # test.df = getQhat(Qhat.object, modelled.flow)
                # cat(paste("dataframe with transformed flow",  "\n"))
                # print(test.df ,max=30)

                #####################
                Qhat.modelled.transform = getQhat(Qhat.object, modelled.flow)$Qhat.flow
                # cat(paste("Qhat.model after transform",  "\n"))
                # print(Qhat.modelled.transform ,max=30)

                # #Test for backtransform
                # test.df = getQ.backTransformed(Qhat.object,test.df)
                #
                # cat(paste("dataframe with Qhat.model after back transform",  "\n"))
                # print(test.df ,max=30)

                # Debug: Check what wapaba_model returned
                # cat("wapaba_model returned:\n")
              # cat("  Length:", length(Qhat.flow), "\n")
                # cat("  Class:", class(Qhat.flow), "\n")
                # cat("  First 5 values:", paste(head(Qhat.flow, 5), collapse = ", "), "\n")
                # cat("  Unique values:", length(unique(Qhat.flow)), "\n")
                # cat("  Range:", paste(range(Qhat.flow, na.rm = TRUE), collapse = " to "), "\n")
                #
                # # Check if result has correct length
                # if (!is.numeric(Qhat.flow)) {
                #   stop(paste("wapaba_model returned non-numeric result:", class(Qhat.flow)))
                # }
                #
                # if (length(Qhat.flow) != nrows) {
                #   stop(paste("wapaba_model returned", length(Qhat.flow), "values, expected", nrows))
                # }
                #Assigning the transformed flow to the modelled matrix
                Qhat.model[, state] <- Qhat.modelled.transform
              }, error = function(e) {
                stop(paste("Error calculating Qhat.flow for state", state, ":", e$message))
              })
            }

            # Print debug information
            # cat(paste("Parameter matrix dimensions:", dim(param_matrix)[1], "x", dim(param_matrix)[2], "\n"))
            # cat(paste("Qhat.model dimensions:", dim(Qhat.model)[1], "x", dim(Qhat.model)[2], "\n"))
            # cat(paste("Number of time points:", nrows, "\n"))
            # cat(paste("Number of states:", .Object@nStates, "\n"))
            #cat(paste("Qhat.model before transform",  "\n"))
            # print(Qhat.model)
            #  This was initially done which caused error. Qhat.model = getQhat(Qhat.object, Qhat.model)
            #print(paste("Qhat.object after transform",  "\n"))
            #print(Qhat.object)
            #cat(paste("Qhat.model final in transform space",  "\n"))
            #print(Qhat.model)
            return(Qhat.model)
}

            # # Qhat.matrix = matrix(nrow = 5, ncol = .Object@nStates)
            # for( para in nrow){
            #   #For row 1 eith variable parameter alpha1
            #   for(state in 1:.Object@nStates) {
            #     parmaeter.matrix[nrow,state] = if(ncols.alpha1 == 1) rep(parameters$mean.alpha1, .Object@nStates) else parameters$mean.alpha1[state]
            #
            #     # Creating each parameter matrix
            #     alpha1.est[state] = if(ncols.alpha1 == 1) rep(parameters$mean.alpha1, .Object@nStates) else parameters$mean.alpha1[state]
            #     # mean.alpha2.const = if(ncols.alpha2 == 1) parameters$mean.alpha2 else parameters$mean.alpha2[1]
            #     alpha2.est = if(ncols.alpha2 == 1) rep(parameters$mean.alpha2, .Object@nStates) else parameters$mean.alpha2[state]
            #     beta.est = if(ncols.beta == 1) rep(parameters$mean.beta, .Object@nStates) else parameters$mean.beta[state]
            #     Smax.est = if(ncols.Smax == 1) rep(parameters$mean.Smax, .Object@nStates) else parameters$mean.Smax[state]
            #     tc.est = if(ncols.tc == 1) rep(parameters$mean.tc, .Object@nStates) else parameters$mean.tc[state]
            #
            #     Qhat.matrix[t, state] = wapaba_model(data, .Object@parameters-1)
            #   }
            #
            # }
)





#             if ('mean.trend' %in% names(parameters)) {
#               ncols.trend = length(parameters$mean.trend)
# #             }
#             # nrows = length(data$Qhat.precipitation);
#             ncols.max = max(c(ncols.alpha1 ,ncols.alpha2,ncols.beta, ncols.Smax,ncols.tc))
#
#             if (ncols.max > .Object@nStates)
#               stop(paste('The number of parameters for each term of the mean model must must equal 1 or the number of states of ',.Object@nStates))
#
#             # Check which terms are uniform for all states and whic terms are unique
#             # to each state.
#             if (ncols.alpha1==1 || ncols.a0==.Object@nStates) {
#               alpha1.est = matrix(rep(parameters$mean.alpha1,each=nrows),nrows,.Object@nStates);
#             } else if (ncols.a0<.Object@nStates) {
#               stop(paste('The number of parameters for the a0 term of the mean model must must equal 1 or the number of states of ',.Object@nStates))
#             }
#
#             for(state in 1:.Object@nStates) {
#               Qhat.matrix[1, state] = wapaba_model(data, .Object@parameters-1)
#             }
#
#
#             # if (ncols.a0==1 || ncols.a0==.Object@nStates) {
#             #   a0.est = matrix(rep(parameters$mean.a0,each=nrows),nrows,.Object@nStates);
#             # } else if (ncols.a0<.Object@nStates) {
#             #   stop(paste('The number of parameters for the a0 term of the mean model must must equal 1 or the number of states of ',.Object@nStates))
#             # }
#             # if (ncols.a1==1 || ncols.a1==.Object@nStates) {
#             #   a1.est = matrix(rep(parameters$mean.a1,each=nrows),nrows,.Object@nStates);
#             # } else if (ncols.a1<.Object@nStates) {
#             #   stop(paste('The number of parameters for the a1 term of the mean model must must equal 1 or the number of states of ',.Object@nStates))
#             # }
#             #
#             # time.vals = matrix(data$year - data$year[1],nrows,.Object@nStates)
#             # precip.data = matrix(data$Qhat.precipitation,nrows,.Object@nStates);
#
#             #Getting data for wapaba
#
#             # Calculate the non-AR1 componants
#             a0.est <- 100 * a0.est
#             #Qhat.model <- precip.data * a1.est + a0.est + time.vals * trend.est
#             Qhat.model <- wapaba_model(data,mean.alpha1, mean.alpha2,mean.beta,mean.Smax,mean.tc,std.a0)
#
#             # print(paste('...DBG getMean.AR0 nrows Qhat.model.NAs:',nrow(Qhat.model)))
#
#             return(Qhat.model)
#           }
# )

