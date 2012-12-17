package scant

import org.specs2.Specification
import collection.mutable.ListBuffer


class TaskCompositionSpec extends Specification { def is =

  "Single tasks" ^
    "should render as themselves" ! singleExample ^
    "should execute" ! executeSingle ^
  "Added tasks" ^
    "should render with ampersand" ! andExample ^
    "should render many with ampersands" ! andManyExample ^
    "should execute in parallel" ! executeAnds ^
  "Sequenced tasks" ^
    "should render with double ampersand" ! thenExample ^
    "should render with many double ampersands" ! thenManyExample ^
    "should execute in sequence" ! executeThens ^
    "should render with a mix of and and thens" ! andThenExample ^
    "should render with (_ then _) and _" ! then1And2Example ^
    "should render with _ then (_ and _)" ! then2And1Example ^
    "should render with (_ and _) then _" ! and1Then2Example ^
    "should render with _ and (_ then _)" ! and2Then1Example ^
    "should render with a mix of and,then,and" ! andThenAndExample ^
    "should render with a mix of then,and,then" ! thenAndThenExample ^
    "should execute (_ then _) then _" ! executeThensBracketedLeft ^
    "should execute _ then (_ then _)" ! executeThensBracketedRight ^
    "should execute (_ and _) and _" ! executeAndsBracketedLeft ^
    "should execute _ and (_ and _)" ! executeAndsBracketedRight


  import SampleProject._

  def singleExample = compile("me").toString mustEqual "compile(me)"
  def andExample = (compile("1") and compile("2")).toString mustEqual "compile(1) & compile(2)"
  def thenExample = (compile("1") then compile("2")).toString mustEqual "compile(1) && compile(2)"
  def andManyExample = (compile("1") and compile("2") and compile("3")).toString mustEqual "compile(1) & compile(2) & compile(3)"
  def thenManyExample = (compile("1") then compile("2") then compile("3")).toString mustEqual "compile(1) && compile(2) && compile(3)"
  def andThenExample = (compile("1a") and compile("1b") then compile("2")).toString mustEqual "compile(1a) & compile(1b) && compile(2)"
  def then2And1Example = (compile("1") then (compile("2a") and compile("2b"))).toString mustEqual "compile(1) && (compile(2a) & compile(2b))"
  def then1And2Example = ((compile("a1") then compile("a2")) and compile("b")).toString mustEqual "compile(a1) && compile(a2) & compile(b)" // TODO - test - does this execute a1, then (a2&b) ?
  def and1Then2Example = ((compile("1") and compile("2")) then compile("3")).toString mustEqual "compile(1) & compile(2) && compile(3)"
  def and2Then1Example = (compile("1") and (compile("2") then compile("3"))).toString mustEqual "compile(1) & (compile(2) && compile(3))" // TODO - test - does this execute a1, and (a2 && b) ?
  def andThenAndExample = (compile("1a") and compile("1b") then compile("2a") and compile("2b")).toString mustEqual
    "compile(1a) & compile(1b) && compile(2a) & compile(2b)"
  def thenAndThenExample = (compile("1") then compile("2a") and compile("2b") then compile("3")).toString mustEqual
    "compile(1) && compile(2a) & compile(2b) && compile(3)"

  def executeSingle = {
    val ids = ListBuffer.empty[Int]
    val task = custom(ids, 1)
    task()
    ids must contain(1).only
  }

  def executeThens = {
    val ids = ListBuffer.empty[Int]
    val tasks: Seq[Objective] = (1 to 3).map(i => custom(ids, i))
    val task = tasks.reduceLeft(_ then _)
    task()
    ids must contain(1, 2, 3).only.inOrder
  }

  def executeAnds = {
    val ids = ListBuffer(0, 0)
    val tasks: Seq[Objective] = (1 to 3).map(i => watermark(ids))
    val task = tasks.reduceLeft(_ and _)
    task()
    ids must contain(0, 3).only.inOrder
  }

  def executeThensBracketedLeft = {
    val ids = ListBuffer.empty[Int]
    ((custom(ids, 1) then custom(ids, 3)) then custom(ids, 2))()
    ids must contain(1, 3, 2).only.inOrder
  }

  def executeThensBracketedRight = {
    val ids = ListBuffer.empty[Int]
    (custom(ids, 1) then (custom(ids, 3) then custom(ids, 2)))()
    ids must contain(1, 3, 2).only.inOrder
  }

  def executeAndsBracketedLeft = {
    val ids = ListBuffer.empty[Int]
    ((custom(ids, 1, 3000) and custom(ids, 3, 1000)) and custom(ids, 2, 2000))()
    ids must contain(3, 2, 1).only.inOrder
  }

  def executeAndsBracketedRight = {
    val ids = ListBuffer.empty[Int]
    (custom(ids, 1, 2000) and (custom(ids, 3, 1000) and custom(ids, 2, 3000)))()
    ids must contain(3, 1, 2).only.inOrder
  }
}

// TODO - how to make Task require a partial function that returns Success by default and Failure on exception?
// TODO - how are we going to execute builds?

object SampleProject extends Project {
  case class custom(buffer: ListBuffer[Int], id: Int, delay: Long = 0L) extends Task({() =>
    Thread.sleep(delay)
    buffer += id
    Success
  })

  case class watermark(marks: ListBuffer[Int]) extends Task({() =>
    marks(0) = marks.head + 1
    marks(1) = math.max(marks.tail.head, marks(0))
    Thread.sleep(50L)
    marks(0) = marks.head - 1
    Success
  })
}
