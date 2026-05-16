import monad.Id
import monad.given
import domain.ExamConfig
import program.Program
import interpreters.IdInterpreters.given

@main def main(): Unit =
  Program.runMenu[Id](ExamConfig.default)
