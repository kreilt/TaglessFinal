package algebras

//логгер: добавить, глянуть все логи
trait Logger[F[_]]:
  def add(line: String): F[Unit]
  def take: F[List[String]]