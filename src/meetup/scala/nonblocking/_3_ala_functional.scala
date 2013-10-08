package meetup.scala.nonblocking

import meetup.scala.nonblocking.DummyTasks._
import scala.util.Failure


object AlaFunctionalSample extends SampleApp {

  println("Ala Functional Sample...")

  val nameFtr = findProviderNameById(4) recover {
    case _: IndexOutOfBoundsException => "Default Provider"
  }
  val responseFtr = nameFtr flatMap askProvider recover {
    case t: Throwable => ProviderResponse("NA", Failure(t), 0)
  }
  val resultFtr = responseFtr map resp2result

  printAndComplete(resultFtr)
}

object ForDependentSample extends SampleApp {

  println("For Dependent Sample...")

  val resultFtr = for {
    name <- findProviderNameById(4) recover {case _: IndexOutOfBoundsException => "Default Provider"}
    response <- askProvider(name) recover {case t: Throwable => ProviderResponse("NA", Failure(t), 0)}
  } yield resp2result(response)

  printAndComplete(resultFtr)
}

object ForParallelBadSample extends SampleApp {

  println("For Parallel Sample...")

  val resultFtr = for {
    response1 <- askProvider(providers(1))
    response2 <- askProvider(providers(2))
    response3 <- askProvider(providers(3))
  } yield {
    val results = Seq(
      resp2result(response1),
      resp2result(response2),
      resp2result(response3)
    )
    val (succeeded, failed) = results.partition(_.status.isOk)
    ProvidersSummary(succeeded, failed, succeeded.map(_.items).sum)
  }

  printAndComplete(resultFtr)
}

object ForParallelGoodSample extends SampleApp {

  println("For Parallel Sample...")

  val rf1 = askProvider(providers(1))
  val rf2 = askProvider(providers(2))
  val rf3 = askProvider(providers(3))

  val resultFtr = for {
    response1 <- rf1
    response2 <- rf2
    response3 <- rf3
  } yield {
    val results = Seq(
      resp2result(response1),
      resp2result(response2),
      resp2result(response3)
    )
    val (succeeded, failed) = results.partition(_.status.isOk)
    ProvidersSummary(succeeded, failed, succeeded.map(_.items).sum)
  }

  printAndComplete(resultFtr)
}
