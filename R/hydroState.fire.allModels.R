##' @include hydroState.R hydroState.allModels.R
##' @export
hydroState.fire.allModels <- setClass(
  # Set the name for the class
  "hydroState.fire.allModels",

  package='hydroState',
  contains=c('hydroState.allModels'),
  # Define the slots
  slots = c(
    siteID= 'character',
    calib.reference.model.name = 'list',
    calib.reference.criteria.met = 'logical',
    models = 'list'

  ),

  # Set the default values for the slots. (optional)
  prototype=list(
    siteID = '(not set)',
    calib.reference.model.name= vector('list',1),
    calib.reference.criteria.met= vector('logical',1),
    models  = vector('list',1)
  )
)

# Valid object?
validObject <- function(object) {
  TRUE
}
setValidity("hydroState.fire.allModels", validObject)

# Initialise the object.
#setGeneric(name="initialize",def=function(.Object,input.data, Qhat.object, QhatModel.object, markov.model.object, ...){standardGeneric("initialize")})
setMethod(f="initialize",signature="hydroState.fire.allModels",definition=function(.Object, siteID, input.data, allow.flickering=F, state.dependent.mean.trend=NA)
{

  .Object@siteID <- siteID

  # Define transition graphs
  transition.graph.1State <- matrix(TRUE,1,1)
  transition.graph.2State <- matrix(TRUE,2,2)
  transition.graph.3State.unstructured <- matrix(TRUE,3,3)
  transition.graph.3State <- matrix(TRUE,3,3)
  transition.graph.3State[1,3] <- FALSE
  transition.graph.3State[2,1] <- FALSE
  transition.graph.3State[3,2] <- FALSE

  # Build Qhat flow transform objects.
  Qhat.boxcox = new('Qhat.boxcox', input.data=input.data)
  Qhat.log = new('Qhat.log', input.data=input.data)

  # Check if flickering between states is allows
  model.extension=''
  if (allow.flickering) {
    model.extension.markov = '.flickering'
  } else {
    model.extension.markov = ''
  }

  # Build Qhat models
  #QhatModel.1State = new(paste('QhatModel.homo.normal.linear',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.1State)

  QhatModel.1State = new(paste('QhatModel.homo.normal.linear',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.1State)
  # QhatModel.1State.AR1 = new(paste('QhatModel.homo.normal.linear.AR1',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.1State)
  # QhatModel.1State.AR2 = new(paste('QhatModel.homo.normal.linear.AR2',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.1State)
  # QhatModel.1State.AR3 = new(paste('QhatModel.homo.normal.linear.AR3',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.1State)


  QhatModel.1State.Kuczera = new(paste('QhatModel.homo.normal.linear.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.1State)
  QhatModel.1State.AR1.Kuczera = new(paste('QhatModel.homo.normal.linear.AR1.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.1State)
  QhatModel.1State.AR2.Kuczera = new(paste('QhatModel.homo.normal.linear.AR2.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.1State)
  QhatModel.1State.AR3.Kuczera = new(paste('QhatModel.homo.normal.linear.AR3.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.1State)

  # QhatModel.1State.gamma = new(paste('QhatModel.homo.gamma.linear',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.1State)
  # QhatModel.1State.gamma.AR1 = new(paste('QhatModel.homo.gamma.linear.AR1',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.1State)
  # QhatModel.1State.gamma.AR2 = new(paste('QhatModel.homo.gamma.linear.AR2',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.1State)
  # QhatModel.1State.gamma.AR3 = new(paste('QhatModel.homo.gamma.linear.AR3',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.1State)

  QhatModel.1State.gamma.Kuczera = new(paste('QhatModel.homo.gamma.linear.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.1State)
  QhatModel.1State.gamma.AR1.Kuczera = new(paste('QhatModel.homo.gamma.linear.AR1.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.1State)
  QhatModel.1State.gamma.AR2.Kuczera = new(paste('QhatModel.homo.gamma.linear.AR2.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.1State)
  QhatModel.1State.gamma.AR3.Kuczera = new(paste('QhatModel.homo.gamma.linear.AR3.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.1State)

  #2-state models
  QhatModel.2State.Kuczera = new(paste('QhatModel.homo.normal.linear.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.2State)
  QhatModel.2State.AR1.Kuczera = new(paste('QhatModel.homo.normal.linear.AR1.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.2State)
  QhatModel.2State.AR2.Kuczera = new(paste('QhatModel.homo.normal.linear.AR2.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.2State)
  QhatModel.2State.AR3.Kuczera = new(paste('QhatModel.homo.normal.linear.AR3.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.2State)

  QhatModel.2State.gamma.Kuczera = new(paste('QhatModel.homo.gamma.linear.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.2State)
  QhatModel.2State.gamma.AR1.Kuczera = new(paste('QhatModel.homo.gamma.linear.AR1.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.2State)
  QhatModel.2State.gamma.AR2.Kuczera = new(paste('QhatModel.homo.gamma.linear.AR2.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.2State)
  QhatModel.2State.gamma.AR3.Kuczera = new(paste('QhatModel.homo.gamma.linear.AR3.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.2State)


  #3 state models
  QhatModel.3State.Kuczera = new(paste('QhatModel.homo.normal.linear.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.3State)
  QhatModel.3State.AR1.Kuczera = new(paste('QhatModel.homo.normal.linear.AR1.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.3State)
  QhatModel.3State.AR2.Kuczera = new(paste('QhatModel.homo.normal.linear.AR2.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.3State)
  QhatModel.3State.AR3.Kuczera = new(paste('QhatModel.homo.normal.linear.AR3.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.3State)

  QhatModel.3State.gamma.Kuczera = new(paste('QhatModel.homo.gamma.linear.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.3State)
  QhatModel.3State.gamma.AR1.Kuczera = new(paste('QhatModel.homo.gamma.linear.AR1.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.3State)
  QhatModel.3State.gamma.AR2.Kuczera = new(paste('QhatModel.homo.gamma.linear.AR2.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.3State)
  QhatModel.3State.gamma.AR3.Kuczera = new(paste('QhatModel.homo.gamma.linear.AR3.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.3State)

  #3 state unstructured models
  QhatModel.3State.Kuczera = new(paste('QhatModel.homo.normal.linear.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.3State)
  QhatModel.3State.AR1.Kuczera = new(paste('QhatModel.homo.normal.linear.AR1.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.3State)
  QhatModel.3State.AR2.Kuczera = new(paste('QhatModel.homo.normal.linear.AR2.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.3State)
  QhatModel.3State.AR3.Kuczera = new(paste('QhatModel.homo.normal.linear.AR3.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.3State)

  QhatModel.3State.gamma.Kuczera = new(paste('QhatModel.homo.gamma.linear.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.3State)
  QhatModel.3State.gamma.AR1.Kuczera = new(paste('QhatModel.homo.gamma.linear.AR1.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.3State)
  QhatModel.3State.gamma.AR2.Kuczera = new(paste('QhatModel.homo.gamma.linear.AR2.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.3State)
  QhatModel.3State.gamma.AR3.Kuczera = new(paste('QhatModel.homo.gamma.linear.AR3.fire',model.extension,sep=''), input.data=input.data,state.dependent.mean.trend=state.dependent.mean.trend,  transition.graph=transition.graph.3State)


  # Build Markov model object
  markov.1State = new(paste('markov.annualHomogeneous',model.extension.markov,sep=''), transition.graph=transition.graph.1State)
  markov.2State = new(paste('markov.annualHomogeneous',model.extension.markov,sep=''), transition.graph=transition.graph.2State)
  markov.3State = new(paste('markov.annualHomogeneous',model.extension.markov,sep=''), transition.graph=transition.graph.3State)
  markov.3State.US = new(paste('markov.annualHomogeneous',model.extension.markov,sep=''), transition.graph=transition.graph.3State.unstructured)


  # Build hydrostate models
  .Object@models <- list(
    model.1State.log = new('hydroState',    input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.1State    , markov.model.object=markov.1State),
    model.1State.log.Kuczera = new('hydroState',    input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.1State.Kuczera    , markov.model.object=markov.1State),
    model.1State.log.AR1.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.1State.AR1.Kuczera, markov.model.object=markov.1State),
    model.1State.log.AR2.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.1State.AR2.Kuczera, markov.model.object=markov.1State),
    model.1State.log.AR3.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.1State.AR3.Kuczera, markov.model.object=markov.1State),

    model.1State.gamma.log.Kuczera = new('hydroState',    input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.1State.gamma.Kuczera    , markov.model.object=markov.1State),
    model.1State.gamma.log.AR1.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.1State.gamma.AR1.Kuczera, markov.model.object=markov.1State),
    model.1State.gamma.log.AR2.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.1State.gamma.AR2.Kuczera, markov.model.object=markov.1State),
    model.1State.gamma.log.AR3.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.1State.gamma.AR3.Kuczera, markov.model.object=markov.1State),

    model.1State.BC.Kuczera = new('hydroState',    input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.1State.Kuczera    , markov.model.object=markov.1State),
    model.1State.BC.AR1.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.1State.AR1.Kuczera, markov.model.object=markov.1State),
    model.1State.BC.AR2.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.1State.AR2.Kuczera, markov.model.object=markov.1State),
    model.1State.BC.AR3.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.1State.AR3.Kuczera, markov.model.object=markov.1State),

    model.1State.gamma.BC.Kuczera = new('hydroState',    input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.1State.gamma.Kuczera    , markov.model.object=markov.1State),
    model.1State.gamma.BC.AR1.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.1State.gamma.AR1.Kuczera, markov.model.object=markov.1State),
    model.1State.gamma.BC.AR2.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.1State.gamma.AR2.Kuczera, markov.model.object=markov.1State),
    model.1State.gamma.BC.AR3.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.1State.gamma.AR3.Kuczera, markov.model.object=markov.1State),

    model.2State.log.Kuczera = new('hydroState',    input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.2State.Kuczera    , markov.model.object=markov.2State),
    model.2State.log.AR1.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.2State.AR1.Kuczera, markov.model.object=markov.2State),
    model.2State.log.AR2.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.2State.AR2.Kuczera, markov.model.object=markov.2State),
    model.2State.log.AR3.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.2State.AR3.Kuczera, markov.model.object=markov.2State),

    model.2State.gamma.log.Kuczera = new('hydroState',    input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.2State.gamma.Kuczera    , markov.model.object=markov.2State),
    model.2State.gamma.log.AR1.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.2State.gamma.AR1.Kuczera, markov.model.object=markov.2State),
    model.2State.gamma.log.AR2.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.2State.gamma.AR2.Kuczera, markov.model.object=markov.2State),
    model.2State.gamma.log.AR3.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.2State.gamma.AR3.Kuczera, markov.model.object=markov.2State),

    model.2State.BC.Kuczera = new('hydroState',    input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.2State.Kuczera    , markov.model.object=markov.2State),
    model.2State.BC.AR1.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.2State.AR1.Kuczera, markov.model.object=markov.2State),
    model.2State.BC.AR2.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.2State.AR2.Kuczera, markov.model.object=markov.2State),
    model.2State.BC.AR3.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.2State.AR3.Kuczera, markov.model.object=markov.2State),

    model.2State.gamma.BC.Kuczera = new('hydroState',    input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.2State.gamma.Kuczera    , markov.model.object=markov.2State),
    model.2State.gamma.BC.AR1.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.2State.gamma.AR1.Kuczera, markov.model.object=markov.2State),
    model.2State.gamma.BC.AR2.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.2State.gamma.AR2.Kuczera, markov.model.object=markov.2State),
    model.2State.gamma.BC.AR3.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.2State.gamma.AR3.Kuczera, markov.model.object=markov.2State),

    model.3State.log.Kuczera = new('hydroState',    input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.3State.Kuczera    , markov.model.object=markov.3State),
    model.3State.log.AR1.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.3State.AR1.Kuczera, markov.model.object=markov.3State),
    model.3State.log.AR2.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.3State.AR2.Kuczera, markov.model.object=markov.3State),
    model.3State.log.AR3.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.3State.AR3.Kuczera, markov.model.object=markov.3State),
    #
    model.3State.gamma.log.Kuczera = new('hydroState',    input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.3State.gamma.Kuczera    , markov.model.object=markov.3State),
    model.3State.gamma.log.AR1.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.3State.gamma.AR1.Kuczera, markov.model.object=markov.3State),
    model.3State.gamma.log.AR2.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.3State.gamma.AR2.Kuczera, markov.model.object=markov.3State),
    model.3State.gamma.log.AR3.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.3State.gamma.AR3.Kuczera, markov.model.object=markov.3State),

    model.3State.BC.Kuczera = new('hydroState',    input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.3State.Kuczera    , markov.model.object=markov.3State),
    model.3State.BC.AR1.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.3State.AR1.Kuczera, markov.model.object=markov.3State),
    model.3State.BC.AR2.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.3State.AR2.Kuczera, markov.model.object=markov.3State),
    model.3State.BC.AR3.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.3State.AR3.Kuczera, markov.model.object=markov.3State),
    #
    model.3State.gamma.BC.Kuczera = new('hydroState',    input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.3State.gamma.Kuczera    , markov.model.object=markov.3State),
    model.3State.gamma.BC.AR1.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.3State.gamma.AR1.Kuczera, markov.model.object=markov.3State),
    model.3State.gamma.BC.AR2.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.3State.gamma.AR2.Kuczera, markov.model.object=markov.3State),
    model.3State.gamma.BC.AR3.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.3State.gamma.AR3.Kuczera, markov.model.object=markov.3State),

    model.3StateUS.log.Kuczera = new('hydroState',    input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.3State.Kuczera    , markov.model.object=markov.3State.US),
    model.3StateUS.log.AR1.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.3State.AR1.Kuczera, markov.model.object=markov.3State.US),
    model.3StateUS.log.AR2.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.3State.AR2.Kuczera, markov.model.object=markov.3State.US),
    model.2StateUS.log.AR3.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.3State.AR3.Kuczera, markov.model.object=markov.3State.US),

    model.3StateUS.gamma.log.Kuczera = new('hydroState',    input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.3State.gamma.Kuczera    , markov.model.object=markov.3State.US),
    model.3StateUS.gamma.log.AR1.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.3State.gamma.AR1.Kuczera, markov.model.object=markov.3State.US),
    model.3StateUS.gamma.log.AR2.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.3State.gamma.AR2.Kuczera, markov.model.object=markov.3State.US),
    model.3StateUS.gamma.log.AR3.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.log, QhatModel.object = QhatModel.3State.gamma.AR3.Kuczera, markov.model.object=markov.3State.US),
    #
    model.3StateUS.BC.Kuczera = new('hydroState',    input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.3State.Kuczera    , markov.model.object=markov.3State.US),
    model.3StateUS.BC.AR1.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.3State.AR1.Kuczera, markov.model.object=markov.3State.US),
    model.3StateUS.BC.AR2.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.3State.AR2.Kuczera, markov.model.object=markov.3State.US),
    model.3StateUS.BC.AR3.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.3State.AR3.Kuczera, markov.model.object=markov.3State.US),
    #
    model.3StateUS.gamma.BC.Kuczera = new('hydroState',    input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.3State.gamma.Kuczera    , markov.model.object=markov.3State.US),
    model.3StateUS.gamma.BC.AR1.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.3State.gamma.AR1.Kuczera, markov.model.object=markov.3State.US),
    model.3StateUS.gamma.BC.AR2.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.3State.gamma.AR2.Kuczera, markov.model.object=markov.3State.US),
    model.3StateUS.gamma.BC.AR3.Kuczera = new('hydroState',input.data=input.data, Qhat.object = Qhat.boxcox, QhatModel.object = QhatModel.3State.gamma.AR3.Kuczera, markov.model.object=markov.3State.US)

  )

  # Define Refernce models. That is, the calibration obecjive function that this models needs to meet or exceed.
  .Object@calib.reference.model.name <- list(
    model.1State.log = '',
    model.1State.log.Kuczera = 'model.1State.log',
    model.1State.log.AR1.Kuczera = 'model.1State.log.Kuczera',
    model.1State.log.AR2.Kuczera = 'model.1State.log.AR1.Kuczera',
    model.1State.log.AR3.Kuczera = 'model.1State.log.AR2.Kuczera',

    model.1State.gamma.log.Kuczera = 'model.1State.log.Kuczera',
    model.1State.gamma.log.AR1.Kuczera = 'model.1State.gamma.log.Kuczera',
    model.1State.gamma.log.AR2.Kuczera = 'model.1State.gamma.log.AR1.Kuczera',
    model.1State.gamma.log.AR3.Kuczera = 'model.1State.gamma.log.AR2.Kuczera',

    model.1State.BC.Kuczera = 'model.1State.log.Kuczera',
    #model.1State.BC.Kuczera = '',
    model.1State.BC.AR1.Kuczera = 'model.1State.BC.Kuczera',
    model.1State.BC.AR2.Kuczera = 'model.1State.BC.AR1.Kuczera',
    model.1State.BC.AR3.Kuczera = 'model.1State.BC.AR2.Kuczera',

    model.1State.gamma.BC.Kuczera = 'model.1State.BC.Kuczera',
    model.1State.gamma.BC.AR1.Kuczera = 'model.1State.gamma.BC.Kuczera',
    model.1State.gamma.BC.AR2.Kuczera = 'model.1State.gamma.BC.AR1.Kuczera',
    model.1State.gamma.BC.AR3.Kuczera = 'model.1State.gamma.BC.AR2.Kuczera',

    model.2State.log.Kuczera = 'model.1State.log.Kuczera',
    model.2State.log.AR1.Kuczera = 'model.2State.log.Kuczera',
    model.2State.log.AR2.Kuczera = 'model.2State.log.AR1.Kuczera',
    model.2State.log.AR3.Kuczera = 'model.2State.log.AR2.Kuczera',

    model.2State.gamma.log.Kuczera = 'model.2State.log.Kuczera',
    model.2State.gamma.log.AR1.Kuczera = 'model.2State.gamma.log.Kuczera',
    model.2State.gamma.log.AR2.Kuczera = 'model.2State.gamma.log.AR1.Kuczera',
    model.2State.gamma.log.AR3.Kuczera = 'model.2State.gamma.log.AR2.Kuczera',

    model.2State.BC.Kuczera = 'model.2State.log.Kuczera',
    model.2State.BC.Kuczera = 'model.1State.BC.Kuczera',
    model.2State.BC.AR1.Kuczera = 'model.2State.BC.Kuczera',
    model.2State.BC.AR2.Kuczera = 'model.2State.BC.AR1.Kuczera',
    model.2State.BC.AR3.Kuczera = 'model.2State.BC.AR2.Kuczera',

    model.2State.gamma.BC.Kuczera = 'model.1State.gamma.BC.Kuczera',
    model.2State.gamma.BC.Kuczera = 'model.2State.BC.Kuczera',
    model.2State.gamma.BC.AR1.Kuczera = 'model.2State.gamma.BC.Kuczera',
    model.2State.gamma.BC.AR2.Kuczera= 'model.2State.gamma.BC.AR1.Kuczera',
    model.2State.gamma.BC.AR3.Kuczera = 'model.2State.gamma.BC.AR2.Kuczera',

    model.3State.log.Kuczera = 'model.2State.log.Kuczera',
    model.3State.log.AR1.Kuczera = 'model.3State.log.Kuczera',
    model.3State.log.AR2.Kuczera = 'model.3State.log.AR1.Kuczera',
    model.3State.log.AR3.Kuczera = 'model.3State.log.AR2.Kuczera',

    model.3State.gamma.log.Kuczera = 'model.3State.log.Kuczera',
    model.3State.gamma.log.AR1.Kuczera = 'model.3State.gamma.log.Kuczera',
    model.3State.gamma.log.AR2.Kuczera = 'model.3State.gamma.log.AR1.Kuczera',
    model.3State.gamma.log.AR3.Kuczera = 'model.3State.gamma.log.AR2.Kuczera',

    model.3State.BC.Kuczera = 'model.3State.log.Kuczera',
    model.3State.BC.Kuczera = 'model.2State.BC.Kuczera',
    model.3State.BC.AR1.Kuczera = 'model.3State.BC.Kuczera',
    model.3State.BC.AR2.Kuczera = 'model.3State.BC.AR1.Kuczera',
    model.3State.BC.AR3.Kuczera = 'model.3State.BC.AR2.Kuczera',

    model.3State.gamma.BC.Kuczera = 'model.3State.BC.Kuczera',
    model.3State.gamma.BC.AR1.Kuczera = 'model.3State.gamma.BC.Kuczera',
    model.3State.gamma.BC.AR2.Kuczera = 'model.3State.gamma.BC.AR1.Kuczera',
    model.3State.gamma.BC.AR3.Kuczera = 'model.3State.gamma.BC.AR2.Kuczera',

    model.3StateUS.log.Kuczera = 'model.3State.log.Kuczera',
    model.3StateUS.log.AR1.Kuczera = 'model.3StateUS.log.Kuczera',
    model.3StateUS.log.AR2.Kuczera = 'model.3StateUS.log.AR1.Kuczera',
    model.2StateUS.log.AR3.Kuczera = 'model.3StateUS.log.AR2.Kuczera',

    model.3StateUS.gamma.log.Kuczera = 'model.3StateUS.log.Kuczera',
    model.3StateUS.gamma.log.AR1.Kuczera = 'model.3StateUS.gamma.log.Kuczera',
    model.3StateUS.gamma.log.AR2.Kuczera = 'model.3StateUS.gamma.log.AR1.Kuczera',
    model.3StateUS.gamma.log.AR3.Kuczera = 'model.3StateUS.gamma.log.AR2.Kuczera',

    model.3StateUS.BC.Kuczera = 'model.3State.BC.Kuczera',
    model.3StateUS.BC.AR1.Kuczera = 'model.3StateUS.BC.Kuczera',
    model.3StateUS.BC.AR2.Kuczera = 'model.3StateUS.BC.AR1.Kuczera',
    model.3StateUS.BC.AR3.Kuczera = 'model.3StateUS.BC.AR2.Kuczera',

    model.3StateUS.gamma.BC.Kuczera = 'model.3StateUS.BC.Kuczera',
    model.3StateUS.gamma.BC.AR1.Kuczera = 'model.3StateUS.gamma.BC.Kuczera',
    model.3StateUS.gamma.BC.AR2.Kuczera = 'model.3StateUS.gamma.BC.AR1.Kuczera',
    model.3StateUS.gamma.BC.AR3.Kuczera = 'model.3StateUS.gamma.BC.AR2.Kuczera'

  )

  model.names = names(.Object@models)
  .Object@calib.reference.criteria.met = rep(F,length(model.names))
  names(.Object@calib.reference.criteria.met) <- model.names


  return(.Object)

}

)
