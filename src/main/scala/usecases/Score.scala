package usecases

import domain.ExamConfig, domain.StudentWork, domain.CheckResult

object Score:
  //баллы
  def finalScore(work: StudentWork, cfg: ExamConfig): Int =
    (work.rawScore - work.lateDays * cfg.penaltyPerDay + work.bonusSolved * cfg.bonusPerTask)
      .max(0).min(100)

  //оценка
  def grade(score: Int, cfg: ExamConfig): String =
    cfg.gradeBoundaries
      .find { case (_, threshold) => score >= threshold }
      .map(_._1)
      .getOrElse("F")

  //зачем/незачет
  def isPassed(score: Int, cfg: ExamConfig): Boolean =
    score >= cfg.minPassingScore

  //разница с порогом
  def explainThreshold(score: Int, cfg: ExamConfig): String =
    val diff = score - cfg.minPassingScore
    if diff >= 0 then s"$score >= ${cfg.minPassingScore} (превышение на $diff)"
    else s"$score < ${cfg.minPassingScore} (не хватает ${-diff})"

  //объединяем
  def evaluate(work: StudentWork, cfg: ExamConfig): CheckResult =
    val fs = finalScore(work, cfg)
    val g  = grade(fs, cfg)
    val p  = isPassed(fs, cfg)
    CheckResult(work.studentId, work.studentName, fs, g, p)

