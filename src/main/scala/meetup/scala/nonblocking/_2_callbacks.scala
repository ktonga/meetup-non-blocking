package meetup.scala.nonblocking

import scala.util.{Failure, Success}
import java.util.concurrent.atomic.AtomicInteger

object CallbacksSample extends SampleApp {

  override def nThreads = 1

  println("Callbacks Sample...")

  val counter = new AtomicInteger(providers.size)
  for(id <- 0 until counter.get) {
    val nameFtr = findProviderNameById(id)

    nameFtr onComplete {
      case Success(name) => {

        val responseFtr = askProvider(name)
        responseFtr onComplete {
          case Success(response) => {
            println(response)
            completeIfDone()
          }
          case Failure(t) => {
            println(s"Provider Error: ${t.getMessage}")
            completeIfDone()
          }
        }

      }
      case Failure(t) => {
        println(s"Id Error: ${t.toString}")
        completeIfDone()
      }
    }
  }

  def completeIfDone() = if(counter.decrementAndGet() == 0)  complete()
}
