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

  startWith(Constructed,Initialize)

  when(Constructed){
    case Event(Initialize,_)=>
      println("Current State : "+ stateName+ "  Event Occurred : "+ Initialize)
      goto(Initiated) using Initialize
  }

  when(Initiated){
    case Event(Start,_)=>
      println("Current State : "+ stateName+ "  Event Occurred : "+ Start)
      goto(Started) using Start
    case Event(Failure,_)=>
      println("Current State : "+ stateName+ "  Event Occurred : "+ Failure)
      goto(End) using Failure
  }

  when(Started){
    case Event(EndOfStream,_)=>
      println("Current State : "+ stateName+ "  Event Occurred : "+ EndOfStream)
      goto(End) using Success
    case Event(FutureComplete,_)=>
      println("Current State : "+ stateName+ "  Event Occurred : "+ FutureComplete)
      stay() using FutureComplete
    case Event(Failure,_)=>
      println("Current State : "+ stateName+ "  Event Occurred : "+ Failure)
      goto(End) using Failure
  }

  when(End){
    case Event(Success,_)=>
      println("Current State : "+ stateName+ "  Event Occurred : "+ Success)
      stop()
  }

  whenUnhandled{
    case Event(event,state)=>
      println("Current State : "+ stateName+ "  Event Occurred : "+ event)
      println("Unhandled Event Occurred \n ")
      stay()
  }

  onTransition{
    case Constructed -> Initiated => println("Moved from Constructed to Initiated State \n ")
    case Initiated -> Started=> println("Moved from Initiated to Started State \n ")
    case Started -> End=> println("Moved from Started to End State\n ")
  }
  initialize()
}