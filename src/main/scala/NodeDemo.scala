import akka.actor.{ActorSystem, Props}
/**
  * Created by synerzip on 4/5/17.
  */
object NodeDemo extends App {
  //Create Actor System
  val system = ActorSystem("DiwoEngine")
  val diwoEngineActor = system.actorOf(Props(classOf[DiwoEngine]), "DiwoEngine")

  diwoEngineActor ! Initialize
  Thread.sleep(500)

  diwoEngineActor ! EndOfStream
  Thread.sleep(500)

  diwoEngineActor ! Start
  Thread.sleep(500)

  diwoEngineActor ! EndOfStream
  Thread.sleep(500)

  diwoEngineActor ! EmptyMessage
}