package algebras

import domain.CheckId

//для id - из лекции - генератор id это эффект
trait IdSource[F[_]]:
  def nextCheckId: F[CheckId]
