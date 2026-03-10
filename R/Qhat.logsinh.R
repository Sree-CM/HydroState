##' @include abstracts.R parameters.R

## @export
Qhat.logsinh <- setClass(
  # Set the name for the class
  "Qhat.logsinh",

  package='hydroState',

  contains=c('Qhat'),

  # Define the slots
  slots = c(
    input.data = "data.frame",
    parameters = "parameters"
  ),

  # Set the default values for the slots. (optional)
  prototype=list(
    input.data = data.frame(year=c(0),month=c(0),precipitation=c(0)),
    parameters= new('parameters',c('logsinh.a','logsinh.b'),c(1,1))
  )
)

# Valid object?
validObject <- function(object) {
  if(object@parameters$logsinh.a >=0) TRUE
  else warning("parameters$logsinh.a must be >=0")
  if(object@parameters$logsinh.b >=0) TRUE
  else warning("parameters$logsinh.b must be >=0")
}

setValidity("Qhat.logsinh", validObject)

# Initialise object
#setGeneric(name="initialize",def=function(.Object,input.data){standardGeneric("initialize")})
setMethod("initialize","Qhat.logsinh", function(.Object, input.data) {
  .Object@input.data <- input.data
  validObject(.Object)
  .Object
}
)
# Calculate the transformed flow
#setGeneric(name="getQhat",def=function(.Object, data){standardGeneric("getQhat")})
setMethod(f="getQhat",signature=c("Qhat.logsinh",'data.frame'),definition=function(.Object, data)
          {
            if (!is.data.frame(data))
            stop('"Data" must be a data.frame.')
            #
            #
            # Get object parameter list
            parameters = getParameters(.Object@parameters)
            logsinh.a = parameters$logsinh.a
            logsinh.b = parameters$logsinh.b

            #Log-sinh two parameter original transformation
            #Step 2 : Converting the flow using logsinh transform
            data$Qhat.flow <- (1/logsinh.b)*log(sinh(logsinh.a+ (logsinh.b*data$flow)))
            data$Qhat.precipitation <- data$precipitation
            return(data)

          }
)

# Calculate the transformed flow using the object data
#setGeneric(name="getQhat",def=function(.Object){standardGeneric("getQhat")})
setMethod(f="getQhat",signature="Qhat.logsinh",definition=function(.Object)
          {
             data = .Object@input.data
             return(getQhat(.Object, data))
          }
)

setMethod(f="getQ.backTransformed",signature=c("Qhat.logsinh",'data.frame'),definition=function(.Object, data)
{
  if (!is.data.frame(data))
    stop('"Data" must be a data.frame.')

  # Get object parameter list
  parameters = getParameters(.Object@parameters)
  logsinh.a = parameters$logsinh.a
  logsinh.b = parameters$logsinh.b


  #data$flow.modelled = (1 / scale_b) * log(exp(scale_b * data$Qhat.flow) + sqrt((exp(scale_b * data$Qhat.flow))^2 + 1))
  data$flow.modelled = (1 / logsinh.b) * (asinh(exp(logsinh.b*data$Qhat.flow))-logsinh.a)
  #To see if transformation is correct or not.
  #print(data)
  return(data)
}
)
