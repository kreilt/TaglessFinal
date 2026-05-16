package usecases

import monad.Monad
import domain.*
import algebras.*

object Exam:

  //приёмка работы
  // ошибки: экзамен закрыт или работа уже сдана
  def submitWork[F[_]: Monad, E](work: StudentWork)(using
                                                    works: WorkRepo[F],
                                                    exam: ExamControl[F],
                                                    examClosed: ExamClosed[E],
                                                    duplicate: WorkAlreadySubmitted[E]
  ): F[Either[E, Unit]] =
    val F = summon[Monad[F]]
    exam.isClosed.flatMap { closed =>
      if closed then F.pure(Left(examClosed.examClosed))
      else
        works.exists(work.studentId).flatMap { dup =>
          if dup then F.pure(Left(duplicate.workAlreadySubmitted(work.studentId)))
          else works.save(work).map(_ => Right(()))
        }
    }

  //проверка работы
  // ошибки: работы нет или уже проверена
  def checkWork[F[_]: Monad, E](studentId: String, cfg: ExamConfig)(using
                                                                    works: WorkRepo[F],
                                                                    results: ResultRepo[F],
                                                                    ids: IdSource[F],
                                                                    log: Logger[F],
                                                                    notFound: WorkNotFound[E],
                                                                    already: WorkAlreadyChecked[E]
  ): F[Either[E, (CheckId, CheckResult)]] =
    val F = summon[Monad[F]]
    works.findByStudentId(studentId).flatMap {
      case None => F.pure(Left(notFound.workNotFound(studentId)))
      case Some(work) =>
        results.hasResultFor(studentId).flatMap { has =>
          if has then F.pure(Left(already.workAlreadyChecked(studentId)))
          else evaluateAndStore(work, cfg).map(Right(_))
        }
    }

  //перепроверка (не падает на "уже проверен", создаёт новую запись поверх)
  def recheckWork[F[_]: Monad, E](studentId: String, cfg: ExamConfig)(using
                                                                      works: WorkRepo[F],
                                                                      results: ResultRepo[F],
                                                                      ids: IdSource[F],
                                                                      log: Logger[F],
                                                                      notFound: WorkNotFound[E]
  ): F[Either[E, (CheckId, CheckResult)]] =
    val F = summon[Monad[F]]
    works.findByStudentId(studentId).flatMap {
      case None => F.pure(Left(notFound.workNotFound(studentId)))
      case Some(work) =>
        for
          _ <- log.add(s"[перепроверка ${work.studentId}]")
          r <- evaluateAndStore(work, cfg)
        yield Right(r)
    }

  //предпросмотр (без записи)
  // ошибка только если работы нет
  def previewWork[F[_]: Monad, E](studentId: String, cfg: ExamConfig)(using
                                                                      works: WorkRepo[F],
                                                                      notFound: WorkNotFound[E]
  ): F[Either[E, CheckResult]] =
    works.findByStudentId(studentId).map {
      case None       => Left(notFound.workNotFound(studentId))
      case Some(work) => Right(Score.evaluate(work, cfg))
    }

  def closeExam[F[_]: Monad](using exam: ExamControl[F]): F[Unit] =
    exam.close

  //кладем в результат, последнюю проверку работы студента
  def listResults[F[_]: Monad](using results: ResultRepo[F]): F[List[(CheckId, CheckResult)]] =
    results.all.map { all =>
      all.groupBy(_._2.studentId)
        .values
        .map(_.maxBy(_._1.value))
        .toList
        .sortBy(_._1.value)
    }


  //вычисление и запись + лог шагов (через логгер)
  def evaluateAndStore[F[_]: Monad](work: StudentWork, cfg: ExamConfig)(using
                                                                        results: ResultRepo[F],
                                                                        ids: IdSource[F],
                                                                        log: Logger[F]
  ): F[(CheckId, CheckResult)] =
    val fs      = Score.finalScore(work, cfg)
    val g       = Score.grade(fs, cfg)
    val p       = Score.isPassed(fs, cfg)
    val expl    = Score.explainThreshold(fs, cfg)
    val penalty = work.lateDays * cfg.penaltyPerDay
    val bonus   = work.bonusSolved * cfg.bonusPerTask
    val result  = CheckResult(work.studentId, work.studentName, fs, g, p)

    for
      _  <- log.add(s"проверка: ${work.studentId} (${work.studentName})")
      _  <- log.add(s"базовый балл: ${work.rawScore}")
      _  <- log.add(
        if work.lateDays > 0
        then s"штраф: ${work.lateDays} дн. * ${cfg.penaltyPerDay} = -$penalty"
        else "сдано вовремя, штрафа нет"
      )
      _  <- log.add(
        if work.bonusSolved > 0
        then s"бонус: ${work.bonusSolved} зад. * ${cfg.bonusPerTask} = +$bonus"
        else "бонусных задач нет"
      )
      _  <- log.add(s"итог: $fs баллов, оценка $g — ${if p then "ЗАЧЁТ" else "НЕЗАЧЁТ"} ($expl)")
      id <- ids.nextCheckId
      _  <- results.save(id, result)
    yield (id, result)
