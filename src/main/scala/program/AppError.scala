package program

import domain.*

enum AppError:
  case ExamClosed
  case WorkNotFound(studentId: String)
  case WorkAlreadyChecked(studentId: String)
  case WorkAlreadySubmitted(studentId: String)


given ExamClosed[AppError] with
  def examClosed: AppError =
    AppError.ExamClosed

given WorkNotFound[AppError] with
  def workNotFound(studentId: String): AppError =
    AppError.WorkNotFound(studentId)

given WorkAlreadyChecked[AppError] with
  def workAlreadyChecked(studentId: String): AppError =
    AppError.WorkAlreadyChecked(studentId)

given WorkAlreadySubmitted[AppError] with
  def workAlreadySubmitted(studentId: String): AppError =
    AppError.WorkAlreadySubmitted(studentId)

//функция отделена - грязь в конце
def render(e: AppError): String = e match
  case AppError.ExamClosed               => "экзамен закрыт, работы не принимаются"
  case AppError.WorkAlreadySubmitted(id) => s"работа $id уже сдана"
  case AppError.WorkNotFound(id)         => s"работа $id не найдена"
  case AppError.WorkAlreadyChecked(id)   => s"$id уже проверен, используйте перепроверку"