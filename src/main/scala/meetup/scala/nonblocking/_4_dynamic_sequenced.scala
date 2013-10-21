import meetup.scala.nonblocking.SampleApp
import scala.concurrent._

object SequencedSample extends SampleApp {

  def availableTasks = future {
    List("A Task", "Another Task", "Ready Task", "Unreachable Task")
  }

  def claim(task: String): Future[String] = {
    println(s"Claiming $task...")
    future {
      Thread.sleep(2000L)
      if (task == "Ready Task") task
      else throw new IllegalAccessException("Already Started")
    }
  }

  def nextAssignableTask(tasks: List[String]): Future[Any] = tasks match {
    case Nil => Future.failed(new RuntimeException("No task :-("))
    case task :: others => {
      claim(task) recoverWith {
        case e: IllegalAccessException => nextAssignableTask(others)
      }
    }
  }

  val assignableTask = availableTasks flatMap nextAssignableTask

  printAndComplete(assignableTask)

}


