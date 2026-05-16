package algebras

import domain.CheckId, domain.CheckResult

//репка резов: сохранить, есть ли результат, список результатов
trait ResultRepo[F[_]]:
  def save(id: CheckId, result: CheckResult): F[Unit]
  def hasResultFor(studentId: String): F[Boolean]
  def all: F[List[(CheckId, CheckResult)]]