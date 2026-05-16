package algebras

import domain.StudentWork

//принятые работы: сохранить, поиск по айди студента, существование работы
trait WorkRepo[F[_]]:
  def save(work: StudentWork): F[Unit]
  def findByStudentId(studentId: String): F[Option[StudentWork]]
  def exists(studentId: String): F[Boolean]