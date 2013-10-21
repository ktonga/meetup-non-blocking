package meetup.scala.nonblocking

import scala.concurrent._
import scala.concurrent.duration.Duration
import scala.util.{Failure, Random, Success}
import scala.compat.Platform
import java.util.concurrent.Executors
import java.util.{TimerTask, Timer}

trait ExecCtxProvider {
  implicit val executionContext: ExecutionContext
}

trait FixedThreadPoolProvider extends ExecCtxProvider {
  def nThreads: Int
  implicit val executionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(nThreads))
}

trait SampleApp extends App with FixedThreadPoolProvider with DummyTasks {

  def nThreads = 4

  val completed = promise[Unit]()

  abstract override def main(args: Array[String]) {
    super.main(args)
    Await.ready(completed.future, Duration.Inf)
    println(s"Sample took ${Platform.currentTime - executionStart}ms")
    executionContext.shutdown()
  }

  def complete(): Unit = completed.complete(Success[Unit]())

  def complete(f: Future[_]): Unit = f onComplete {_ => complete()}

  def printResult(f: Future[_]) = f onComplete {
    case Success(r) => println("Result: " + r)
    case Failure(t) => println("Error: " + t.toString)
  }

  def printAndComplete(f: Future[_]) = {
    printResult(f)
    complete(f)
  }

}
