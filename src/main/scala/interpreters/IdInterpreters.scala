package interpreters

import monad.Id, algebras.*, domain.*

object IdInterpreters:
  given Console[Id] with
    def writeLine(s: String): Unit = println(s)
    def write(s: String): Unit = print(s)
    def readLine: String =
      val s = scala.io.StdIn.readLine()
      if s == null then "" else s

  class IdExamControl extends ExamControl[Id]:
    private var closed: Boolean = false
    def isClosed: Boolean = closed
    def close: Unit = closed = true

  given ExamControl[Id] = IdExamControl()

  class IdIdSource extends IdSource[Id]:
    private var counter: Int = 1
    def nextCheckId: CheckId =
      val id = counter
      counter += 1
      CheckId(id)

  given IdSource[Id] = IdIdSource()

  class IdLogger extends Logger[Id]:
    private val buf = scala.collection.mutable.ArrayBuffer.empty[String]
    def add(line: String): Unit = buf += line
    def take: List[String] =
      val lines = buf.toList
      buf.clear()
      lines

  given Logger[Id] = IdLogger()

  class IdResultRepo extends ResultRepo[Id]:
    private val storage = scala.collection.mutable.Map.empty[CheckId, CheckResult]
    def save(id: CheckId, result: CheckResult): Unit = storage.update(id, result)

    //есть ли хоть 1 работа у студента
    def hasResultFor(studentId: String): Boolean =
      storage.values.exists(_.studentId == studentId)

    def all: List[(CheckId, CheckResult)] =
      storage.toList.sortBy(_._1.value)

  given ResultRepo[Id] = IdResultRepo()

  class IdWorkRepo extends WorkRepo[Id]:
    private val storage = scala.collection.mutable.Map.empty[String, StudentWork]

    def save(work: StudentWork): Unit = storage.update(work.studentId, work)

    //дает работу студента, если есть
    def findByStudentId(studentId: String): Option[StudentWork] = storage.get(studentId)

    //проверка - есть ли работа у студента
    def exists(studentId: String): Boolean = storage.contains(studentId)

  given WorkRepo[Id] = IdWorkRepo()
