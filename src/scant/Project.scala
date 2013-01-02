package scant

import scala.actors.Futures._
import java.io.File
import scala.util.Random
import scala._
import scala.Some

trait Project {

  // Objectives are the things that need to happen in a build
  sealed trait Objective {
    def apply(): Seq[Outcome]
    def and(objective: Objective) = And(this, objective)
    def then(objective: Objective) = Then(this, objective)
  }

  // A Task is a stand-alone objective
  case class Task(f: () => Outcome) extends Objective {
    def apply() = Seq(f())
  }

  // Composed Tasks are CompositeObjectives
  trait CompositeObjective extends Objective {
    def parenthesisedToString(o1: Objective, o2: Objective) = List(o1.toString, o2 match {
      case co: CompositeObjective => "(%s)".format(co)
      case c => c.toString
    })
  }

  // Compose Objectives to execute in parallel with And
  case class And(o1: Objective, o2: Objective) extends CompositeObjective {
    def apply() = {
      val results: List[Option[_]] = awaitAll(30 * 60 * 1000L, future(o1()), future(o2()))
      results.flatMap(_ match {
        case Some(outcomes: Seq[Outcome]) => outcomes
        case None => Seq(NonTermination(0)) // TODO - get this value?
        case _ => throw new RuntimeException("Future Objective returned something other than an Option[Seq[Outcome]]")
      })
    }
    override def toString = parenthesisedToString(o1, o2).mkString(" & ")
  }

  // Compose Objectives to execute in parallel with And
  case class Then(o1: Objective, o2: Objective) extends CompositeObjective {
    def apply() = o1() ++ o2()
    override def toString = parenthesisedToString(o1, o2).mkString(" && ")
  }

  // Objectives return Outcomes
  sealed trait Outcome {
    val start: Long
    val end: Long
  }

  // There are three possible kinds of Outcomes.
  case class Success(start: Long, end: Long) extends Outcome
  case class Failure(start: Long, end: Long) extends Outcome
  case class NonTermination(start: Long) extends Outcome {
    val end = start
  }


  // A test/dummy Task - TODO - remove
  case class compile(path: File) extends Task({() =>
    println("compiling " + path.getAbsolutePath)
    Thread.sleep(new Random().nextInt(3000))
    println("compiled " + path.getAbsolutePath)
    Success(System.currentTimeMillis(), System.currentTimeMillis())
  })

  // A test/dummy Task - TODO - remove
  case class delete(path: File) extends Task({() =>
    println("deleted " + path.getAbsolutePath)
    Success(System.currentTimeMillis(), System.currentTimeMillis())
  })

  // The default Objective for a new Project is not defined - TODO - use Scala 2.10's ???
  lazy val ??? = throw new RuntimeException("Not yet implemented")
  lazy val default: Objective = ???

  // Treat a String as a File in Project definitions
  implicit def stringToFile(file: String): File = new File(file)

}