package program

import monad.Monad
import domain.*
import algebras.*
import usecases.*


object Program:
  //сборка и запуск, сценарии действий
  def runMenu[F[_]: Monad](cfg: ExamConfig)(using
                                             console: Console[F],
                                             works: WorkRepo[F],
                                             results: ResultRepo[F],
                                             ids: IdSource[F],
                                             exam: ExamControl[F],
                                             log: Logger[F]
  ): F[Unit] =
    val F = summon[Monad[F]]

    def readInt(prompt: String, fallback: Int): F[Int] =
      for
        _ <- console.write(prompt)
        s <- console.readLine
      yield s.trim.toIntOption.getOrElse(fallback)

    def printLines(lines: List[String]): F[Unit] =
      lines.foldLeft(F.pure(())) { (acc, line) =>
        acc.flatMap(_ => console.writeLine("  " + line))
      }

    //печать результата проверки
    def renderCheckResult(res: Either[AppError, (CheckId, CheckResult)]): F[Unit] =
      res match
        case Right((cid, cr)) =>
          for
            lines <- log.take
            _     <- printLines(lines)
            _     <- console.writeLine(
              s"  => ${cr.grade}, ${cr.finalScore} баллов, " +
                s"${if cr.passed then "зачёт" else "незачёт"} (#${cid.value})"
            )
          yield ()
        case Left(e) =>
          console.writeLine(s"  ошибка: ${render(e)}")

    //приёмка работы (сценарий)
    def submitScenario: F[Unit] =
      for
        _    <- console.write("id студента: ")
        id   <- console.readLine
        _    <- console.write("имя: ")
        name <- console.readLine
        raw  <- readInt("баллов (0-100): ", 0)
        late <- readInt("дней просрочки: ", 0)
        bon  <- readInt("бонусных задач: ", 0)
        work = StudentWork(
          id.trim, name.trim,
          raw.max(0).min(100), late.max(0), bon.max(0)
        )
        res  <- Exam.submitWork[F, AppError](work)
        _    <- res match
          case Right(_) => console.writeLine(s"  принято: ${work.studentName}")
          case Left(e)  => console.writeLine(s"  ошибка: ${render(e)}")
      yield ()

    //проверка (сценарий)
    def checkScenario: F[Unit] =
      for
        _   <- console.write("id студента: ")
        id  <- console.readLine
        res <- Exam.checkWork[F, AppError](id.trim, cfg)
        _   <- renderCheckResult(res)
      yield ()

    def recheckScenario: F[Unit] =
      for
        _   <- console.write("id студента: ")
        id  <- console.readLine
        res <- Exam.recheckWork[F, AppError](id.trim, cfg)
        _   <- renderCheckResult(res)
      yield ()

    //предпросмотр без записи
    def previewScenario: F[Unit] =
      for
        _   <- console.write("id студента (для предпросмотра): ")
        id  <- console.readLine
        res <- Exam.previewWork[F, AppError](id.trim, cfg)
        _   <- res match
          case Right(cr) =>
            console.writeLine(
              s"  предпросмотр: ${cr.finalScore} баллов, ${cr.grade}, " +
                s"${if cr.passed then "зачёт" else "незачёт"}"
            )
          case Left(e) =>
            console.writeLine(s"  ошибка: ${render(e)}")
      yield ()

    def closeScenario: F[Unit] =
      for
        _ <- Exam.closeExam[F]
        _ <- console.writeLine("  экзамен закрыт")
      yield ()

    def listResultScenario: F[Unit] =
      for
        rs <- Exam.listResults[F]
        _  <- if rs.isEmpty then console.writeLine("  результатов пока нет")
        else
          rs.foldLeft(F.pure(())) { (acc, entry) =>
            val (cid, r) = entry
            acc.flatMap(_ =>
              console.writeLine(
                f"  #${cid.value}%-4d ${r.studentId}%-10s ${r.studentName}%-15s " +
                  f"${r.finalScore}%3d  ${r.grade}  ${if r.passed then "зачёт" else "незачёт"}"
              )
            )
          }
      yield ()

    //дерево меню
    val root = MenuTreeNode[F]("Система проверки", Seq(
      MenuLeaf("сдать работу", () => submitScenario),
      MenuTreeNode("Проверка работ", Seq(
        MenuLeaf("проверить", () => checkScenario),
        MenuLeaf("перепроверить", () => recheckScenario),
        MenuLeaf("предпросмотр",  () => previewScenario)
      )),
      MenuTreeNode("Администрирование", Seq(
        MenuLeaf("закрыть экзамен", () => closeScenario),
        MenuLeaf("все результаты",  () => listResultScenario)
      ))
    ))

    def suffix: F[String] = exam.isClosed.map { c =>
      if c then " [экзамен закрыт]" else ""
    }

    for
      _ <- console.writeLine("Проверка экзамена")
      _ <- console.writeLine(
        s"проходной: ${cfg.minPassingScore}, штраф: ${cfg.penaltyPerDay}/день, " +
          s"бонус: ${cfg.bonusPerTask}/задача"
      )
      _ <- Menu.loop(root, () => suffix)
      _ <- console.writeLine("пока")
    yield ()
