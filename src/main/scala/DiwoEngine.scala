/**
  * Created by synerzip on 3/5/17.
  */

import akka.actor.FSM

//States
trait NodeState
case object Constructed extends NodeState
case object Initiated extends NodeState
case object Started extends NodeState
case object End extends NodeState

//Data
trait NodeData
case object Initialize extends NodeData
case object Start extends NodeData
case object EndOfStream extends NodeData
case object Success extends NodeData
case object Failure extends NodeData
case object Exception extends NodeData
case object EmptyMessage extends NodeData
case object FutureComplete extends NodeData

class DiwoEngine extends FSM[NodeState, NodeData] {

  // To set initial state
  startWith(Constructed, Initialize)

  // When Initialize Event It calls displayState method & also changes node state from constructed to Initiated
  when(Constructed) {
    case Event(Initialize, _) =>
      displayState(Initialize)
      goto(Initiated) using Initialize
  }

  /*
  When start event occurre it changes node state from Initiated to Started
  When Failure event occurre it changes node state from Initiated to End
  for all events it calls displayState method
   */
  when(Initiated) {
    case Event(Start, _) =>
      displayState(Start)
      goto(Started) using Start
    case Event(Failure, _) =>
      displayState(Failure)
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
      displayState(EndOfStream)
      goto(End) using Success
    case Event(FutureComplete, _) =>
      displayState(FutureComplete)
      stay() using FutureComplete
    case Event(Failure, _) =>
      displayState(Failure)
      goto(End) using Failure
  }

  /*
  When Success event occurred in End state it will terminates the FSM
   before terminating it calls the displayState method
   */
  when(End) {
    case Event(Success, _) =>
      displayState(Success)
      stop()
  }

  /*
  When state dosent handle the event then those events are handled here
  It will not change the current state
  It also calls the displayState method
   */
  whenUnhandled {
    case Event(event, state) =>
      displayState(event.asInstanceOf[NodeData])
      println("Unhandled Event Occurred \n ")
      stay()
  }

  /*
   Set handler which is called upon each state transition
   */
  onTransition {
    case Constructed -> Initiated => println("Moved from Constructed to Initiated State \n ")
    case Initiated -> Started => println("Moved from Initiated to Started State \n ")
    case Started -> End => println("Moved from Started to End State\n ")
  }

  // To display the current state & the event occurred
  def displayState(event: NodeData): Unit = {
    println("Current State : " + stateName + "  Event Occurred : " + event)
  }

  //Initialize DiwoEngine
  initialize()
}


