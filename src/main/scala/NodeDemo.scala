import akka.actor.{ActorSystem, Props}

/**
  * Created by synerzip on 4/5/17.
  */
object NodeDemo extends App {
  //Create Actor System
  val system = ActorSystem("DiwoEngine")
  val diwoEnginActor = system.actorOf(Props(classOf[DiwoEngine]), "DiwoEngine")

  diwoEnginActor ! Initialize
  Thread.sleep(500)

  diwoEnginActor ! EndOfStream
  Thread.sleep(500)

  diwoEnginActor ! Start
  Thread.sleep(500)

  diwoEnginActor ! EndOfStream
  Thread.sleep(500)

  diwoEnginActor ! EmptyMessage
}