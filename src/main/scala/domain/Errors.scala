package domain

trait ExamClosed[E]:
  def examClosed: E

trait WorkNotFound[E]:
  def workNotFound(studentId: String): E

trait WorkAlreadyChecked[E]:
  def workAlreadyChecked(studentId: String): E

trait WorkAlreadySubmitted[E]:
  def workAlreadySubmitted(studentId: String): E
