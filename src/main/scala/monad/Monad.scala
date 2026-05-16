package monad

trait Monad[F[_]]:
  extension [A](fa: F[A])
    def flatMap[B](f: A => F[B]): F[B]
    def map[B](f: A => B): F[B]

  def pure[A](a: A): F[A]

type Id[A] = A

given Monad[Id] with
  extension [A](fa: Id[A])
    def flatMap[B](f: A => Id[B]): Id[B] = f(fa)
    def map[B](f: A => B): Id[B] = f(fa)

  def pure[A](a: A): Id[A] = a
