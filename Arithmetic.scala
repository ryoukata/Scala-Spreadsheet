// 算術演算のためのライブラリー（Modelクラスにミックスインするトレイト。必要があればこのトレイトに演算を追加する。）
trait Arithmetic { this: Evaluator =>           // ArithmeticトレイトがModelクラスにアクセスできるようにしている
  operations += (
    "add" -> { case List(x, y) => x + y },
    "sub" -> { case List(x, y) => x - y },
    "div" -> { case List(x, y) => x / y },
    "mul" -> { case List(x, y) => x * y },
    "mod" -> { case List(x, y) => x % y },
    "sum" -> { xs => (0.0 /: xs)(_ + _) },
    "prod" -> { xs => (1.0 /: xs)(_ * _) }
  )
}
