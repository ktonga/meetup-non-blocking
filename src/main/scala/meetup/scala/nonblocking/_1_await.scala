package meetup.scala.nonblocking

import scala.concurrent._
import scala.concurrent.duration.Duration
import java.util.concurrent.Executors
import scala.compat.Platform

object AwaitSample extends App with FixedThreadPoolProvider with DummyTasks {

  override def nThreads = 1

  println("Await Sample...")

  for(id <- 0 until providers.size + 1) {
    try {

      val nameFtr = findProviderNameById(id)

      val name = Await.result(nameFtr, Duration.Inf)

      val responseFtr = askProvider(name)

      val response = Await.result(responseFtr, Duration.Inf)

      println(response)
    } catch {
      case t: Throwable => {
        println("Error: " + t.toString)
      }
    }
  }

  println(s"Sample took ${Platform.currentTime - executionStart}ms")
  executionContext.shutdown()

}
