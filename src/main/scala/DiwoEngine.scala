/**
  * Created by synerzip on 3/5/17.
  */
import akka.actor.{ActorLogging, FSM}

//States
sealed trait NodeState
case object Constructed extends NodeState
case object Initiated extends NodeState
case object Started extends NodeState
case object End extends NodeState

//Data
sealed trait NodeData
case object Initialize extends NodeData
case object Start extends NodeData
case object EndOfStream extends NodeData
case object Success extends NodeData
case object Failure extends NodeData
case object Exception extends NodeData
case object EmptyMessage extends NodeData
case object FutureComplete extends NodeData

class DiwoEngine extends FSM[NodeState, NodeData] with ActorLogging {

  // To set initial state
  startWith(Constructed, Initialize)
  log.info("Initial state is set to {}",Constructed)

  // When Initialize Event It calls displayState method & also changes node state from constructed to Initiated
  when(Constructed) {
    case Event(Initialize, _) =>
      onInitialize()
      goto(Initiated) using Initialize
  }

  /*
  When start event occurs it changes node state from Initiated to Started
  When Failure event occurs it changes node state from Initiated to End
  for all events it calls displayState method
   */
  when(Initiated) {
    case Event(Start, _) =>
      onStart()
      goto(Started) using Start
    case Event(Failure, _) =>
      onFailure()
      goto(End) using Failure
  }

  /*
  When EndOfStream event occurred it will changes its state from Started to End
  When FutureComplete event occurred it will not changes its current state
  When Failure event occurred it will goes into the End state
  For all events it calls the displayState method
   */
  when(Started) {
    case Event(EndOfStream, _) =>
      onEndOfStream()
      goto(End) using Success
    case Event(FutureComplete, _) =>
      onFutureComplete()
      stay() using FutureComplete
    case Event(Failure, _) =>
      onFailure()
      goto(End) using Failure
  }

  /*
  When Success event occurred in End state it will terminates the FSM
  before terminating it calls the displayState method
   */
  when(End) {
    case Event(Success, _) =>
      onSuccess()
      stop()
  }

  /*
  When state doesn't handle the event then those events are handled here
  It will not change the current state
  It also calls the displayState method
   */
  whenUnhandled {
    case Event(event, state) =>
      log.info("Unhandled event occurred in state {}",stateName)
      stay()
  }

  /*
  Set handler which is called upon each state transition
  */
  onTransition {
    case Constructed -> Initiated => log.info("State changed from Constructed to Initiated")
    case Initiated -> Started => log.info("State Changed from Initiated to Started")
    case Started -> End => log.info("State changed from Started to End")
  }

  //Executes when Initialization event occurred
  def onInitialize(): Unit ={
    displayState()
    log.info("On initialize event")
  }

  // Executes when Start event occurred
  def onStart(): Unit ={
    displayState()
    log.info("On start event")
  }

  // Executes when Failure event occurred
  def onFailure(): Unit ={
    displayState()
    log.info("On failure event")
  }

  // Executes when End of stream event occurred
  def onEndOfStream(): Unit ={
    displayState()
    log.info("On End of stream event")
  }

  // Executes when On Future Complete event occurred
  def onFutureComplete(): Unit ={
    displayState()
    log.info("On future complete event")
  }

  // Executes when success event occurred
  def onSuccess(): Unit ={
    displayState()
    log.info("On success event")
  }

  // Executes when Exception event occurred
  def onException(): Unit ={
    displayState()
    log.info("On Exception event")
  }

  // To show current state name
  def displayState(): Unit = {
    log.debug("Current state is : {} ",stateName)
  }

  //Initialize DiwoEngine
  initialize()
}