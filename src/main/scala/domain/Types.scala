package domain

//конфиг экза
case class ExamConfig(
                       gradeBoundaries: List[(String, Int)],  // оценка, минимальный балл
                       penaltyPerDay: Int,                    // штраф за каждый день просрочки
                       bonusPerTask: Int,                     // бонус за каждую доп задачу
                       minPassingScore: Int                   // минимальный проходной балл
                     )

object ExamConfig:
  val default: ExamConfig = ExamConfig(
    gradeBoundaries = List(
      ("A+", 95),
      ("A",  88),
      ("B",  75),
      ("C",  60),
      ("D",  50)
    ),
    penaltyPerDay = 5,
    bonusPerTask = 3,
    minPassingScore = 50
  )

// студ. работа
case class StudentWork(
                        studentId: String,
                        studentName: String,
                        rawScore: Int,
                        lateDays: Int,
                        bonusSolved: Int
                      )

// проверка работы
case class CheckResult(
                        studentId: String,
                        studentName: String,
                        finalScore: Int,
                        grade: String,
                        passed: Boolean
                      )

//делаем отдельным типом, чтобы не было граспа, будем передавать его в результат
case class CheckId(value: Int)
