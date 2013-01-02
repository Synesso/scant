package scant

import org.specs2.mutable.Specification
import org.specs2.specification.Scope

class TaskSpec extends Specification {

  "A single task" should {

    "render as itself" in new SampleProject {
      compile("me").toString mustEqual "compile(me)"
    }

    "execute" in new SampleProject {
      // TODO - consider single Outcome that composes a Seq[Outcome]
      doSomething()() must beLike { case Seq(Success(_, _)) => ok }
    }
  }

  trait SampleProject extends Project with Scope {
    val now = System.currentTimeMillis()
    case class doSomething() extends Task({() => Success(now, now)})
  }

}
