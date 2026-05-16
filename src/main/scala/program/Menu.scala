package program

import monad.Monad
import algebras.Console

sealed trait MenuOption[F[_]]:
  def title: String

case class MenuLeaf[F[_]](title: String, action: () => F[Unit]) extends MenuOption[F]
case class MenuTreeNode[F[_]](title: String, options: Seq[MenuOption[F]]) extends MenuOption[F]

object Menu:
  //цикл
  def loop[F[_]: Monad](
                         node: MenuTreeNode[F],
                         headerSuffix: () => F[String]
                       )(using console: Console[F]): F[Unit] =
    val F = summon[Monad[F]]

    //показать меню
    def show: F[Unit] =
      for
        suf <- headerSuffix()
        items = node.options.zipWithIndex
          .map { case (o, i) => s"  ${i + 1}  ${o.title}" }
          .mkString("\n")
        _ <- console.write(s"\n${node.title}$suf\n$items\n  0  выход\n  выбор: ")
      yield ()

    //выполнить пункт
    def execAt(idx: Int): F[Unit] =
      node.options(idx) match
        case MenuLeaf(_, action)      => action()
        case sub: MenuTreeNode[F] => loop(sub, headerSuffix)

    //итерация
    def step: F[Boolean] =
      for
        _     <- show
        input <- console.readLine
        cont  <- input.trim.toIntOption match
          case Some(0) =>
            F.pure(false)
          case Some(i) if i >= 1 && i <= node.options.size =>
            execAt(i - 1).map(_ => true)
          case _ =>
            console.writeLine("  неизвестная команда").map(_ => true)
      yield cont

    def iter: F[Unit] = step.flatMap(c => if c then iter else F.pure(()))
    iter
