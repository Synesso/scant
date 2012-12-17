package scant

import scala.actors.Futures._
import java.io.File
import scala.util.Random
import scala._
import scala.Some

trait Project {

  implicit def stringToFile(file: String): File = new File(file)

  sealed trait Outcome {
    def &(outcome: Outcome) = outcome match {
      case Success => outcome
      case Failure => Failure
    }
  }
  case object Success extends Outcome
  case object Failure extends Outcome

  sealed trait Objective {
    def apply(): Outcome
    def and(objective: Objective) = And(this, objective)
    def then(objective: Objective) = Then(this, objective)
  }
  trait CompositeObjective extends Objective {
    def parenthesisedToString(o1: Objective, o2: Objective) = List(o1.toString, o2 match {
      case co: CompositeObjective => "(%s)".format(co)
      case c => c.toString
    })
  }

  case class And(o1: Objective, o2: Objective) extends CompositeObjective {
    def apply() = {
      val results = awaitAll(30 * 60 * 1000L, future(o1()), future(o2()))
      // TODO - results contains options - if we know there are no Nones we can just reduce with Outcome.&
      results.foldLeft(Success: Outcome) {(overall, next) => next match {
          case Some(Success) => overall
          case Some(Failure) => Failure
          case None => Failure
          case _ => throw new RuntimeException("")
        }
      }
    }
    override def toString = parenthesisedToString(o1, o2).mkString(" & ")
  }

  case class Then(o1: Objective, o2: Objective) extends CompositeObjective {
    def apply() = o1() & o2()
    override def toString = parenthesisedToString(o1, o2).mkString(" && ")
  }

  case class Task(f: () => Outcome) extends Objective {
    def apply() = f()
  }

/*
  case class Target(objectives: Seq[Seq[Objective]]) extends Objective {
    def apply() {
      objectives.reverse.foreach{set =>
        val futureObjectives = set.map{objective => Futures.future(objective())}.toList
        Futures.awaitAll(30000, futureObjectives: _*)
      }
    }
  }
*/


  case class compile(path: File) extends Task({() =>
    println("compiling " + path.getAbsolutePath)
    Thread.sleep(new Random().nextInt(3000))
    println("compiled " + path.getAbsolutePath)
    Success
  })

  case class delete(path: File) extends Task({() =>
    println("deleted " + path.getAbsolutePath)
    Success
  })

  lazy val ??? = throw new RuntimeException("Not yet implemented")
  lazy val default: Objective = ???

}