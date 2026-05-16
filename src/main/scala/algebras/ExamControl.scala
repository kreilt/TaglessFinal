package algebras

//контроль состояния экзамена
trait ExamControl[F[_]]:
  def isClosed: F[Boolean]
  def close: F[Unit]
