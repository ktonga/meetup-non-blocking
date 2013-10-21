package meetup.scala.nonblocking

import java.util.{TimerTask, Timer}
import scala.concurrent._
import scala.util.{Failure, Success, Try, Random}
import DummyTasks._
import scala.compat.Platform

object DummyTasks {

  class Status(val name: String) extends AnyVal {
    def isOk = name == "OK"
  }

  val Ok = new Status("OK")
  val ProviderError = new Status("PROV_ERROR")
  val Error = new Status("ERROR")
  val Canceled = new Status("CANCELED")

  case class ProviderResponse(name: String, items: Try[Int], time: Long)
  case class ProviderResult(name: String, status: Status, items: Int, message: String)

  case class ProvidersSummary(succeeded: Seq[ProviderResult], failed: Seq[ProviderResult], totalItems: Int) {
    override def toString = {
      s"""Summary:
        |  succeeded:${succeeded.map(pr => s"${pr.name}: ${pr.items} items. ${pr.message}").mkString("\n    ", "\n    ", "")}
        |  failed:${failed.map(pr => s"${pr.name} [${pr.status.name}]: ${pr.message}").mkString("\n    ", "\n    ", "")}
        |  total: $totalItems
      """.stripMargin
    }
  }

  type PR = ProviderResult
  type PRs = List[PR]
  type PRF = Future[PR]
  type PRFs = List[(String, PRF)]

}

trait DummyTasks {
  this: ExecCtxProvider =>

  val timer = new Timer(true)

  def delayed[A](delay: Long)(f: => A): Future[A] = {
    val p = Promise[A]()
    timer.schedule(new TimerTask {
      def run() = {
        try {
          val r = f
          p.success(r)
        } catch {
          case t: Throwable => p.failure(t)
        }
      }
    }, delay)
    p.future
  }

  val providers = List(
    "A Provider",
    "Another Provider",
    "Failing Provider",
    "Yet Another Provider",
    "Unparsable Provider",
    "The Last Provider")

  def findProviderNameById(id: Int) = future {
    providers(id)
  }

  def askProvider(name: String) = {
    println("Asking Provider: " + name)
    val start = Platform.currentTime
    delayed(1000 + Random.nextInt(1000)) {
      val took = Platform.currentTime - start
      name match {
        case n if n.startsWith("Failing") =>
          ProviderResponse(n, Failure(new IllegalStateException("Internal Server Error")), took)
        case n if n.startsWith("Unparsable") =>
          throw new IllegalArgumentException("Cannot parse response")
        case n =>
          ProviderResponse(n, Success(100 + Random.nextInt(100)), took)
      }
    }
  }

  def resp2result(resp: ProviderResponse): PR = resp match {
    case ProviderResponse(name, Success(items), time) => ProviderResult(name, Ok, items, s"Took ${time}ms")
    case ProviderResponse(name, Failure(t), _) => ProviderResult(name, ProviderError, 0, s"Their Error: ${t.getMessage}")
  }

}

