import meetup.scala.nonblocking.DummyTasks._
import meetup.scala.nonblocking.{ExecCtxProvider, SampleApp}
import scala.concurrent._
import scala.util.{Failure, Success}

trait WaitStrategies {
  this: ExecCtxProvider =>

  def waitAll(prfs: PRFs): Future[PRs] = Future sequence prfs.map(_._2)

  def waitProviderCount(prfs: PRFs, count: Int): Future[PRs] = checkPartialResults(prfs) {
    _.count(_.status.isOk) >= count
  }

  def waitItemCount(prfs: PRFs, count: Int): Future[PRs] = checkPartialResults(prfs) {
    _.map(_.items).sum >= count
  }


  private def checkPartialResults(prfs: PRFs)(isComplete: PRs => Boolean): Future[PRs] = {
    recursiveCheckPartialResults(Nil, prfs, isComplete)
  }

  private def recursiveCheckPartialResults(partial: PRs, remaining: PRFs, isComplete: PRs => Boolean): Future[PRs] = {
    if (remaining.isEmpty)
      Future.failed(new IllegalStateException("All providers have finished but result is not complete"))
    else
      Future.firstCompletedOf(remaining.map(_._2)) flatMap {
        pr =>
          val newPartial = partial :+ pr
          val newRemaining = remaining.filter(_._1 != pr.name)
          if (isComplete(newPartial))
            Future.successful(newPartial ++ newRemaining.map(r =>
              ProviderResult(r._1, Canceled, 0, "Canceled: Result completed before it finishes")))
          else
            recursiveCheckPartialResults(newPartial, newRemaining, isComplete)
      }
  }

}

object ParallelSample extends SampleApp with WaitStrategies {

  val providerResults = providers map {
    name =>
      val result = askProvider(name) map resp2result
      (name, result recover {case t: Throwable => ProviderResult(name, Error, 0, s"Our Error: ${t.getMessage}")})
  }

//  val completeResult = waitAll(providerResults)
  val completeResult = waitProviderCount(providerResults, 3)
//  val completeResult = waitItemCount(providerResults, 300)

  val summary = completeResult map {
    results =>
      val (succeeded, failed) = results.partition(_.status.isOk)
      ProvidersSummary(succeeded, failed, succeeded.map(_.items).sum)
  }

  printAndComplete(summary)

}
