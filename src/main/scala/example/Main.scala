package example

object Main {

  /*
  Challenge 6 - boxes

  Given two integers in, create an output that draws an array of boxes in a string for the output.

  Size of the boxes are (including edge characters) 6 wide and 3 tall. All required ASCII characters have been supplied outside of your required solution.

  e.g.
  input 2,2
  output
  ┌────┬────┐
  │    │    │
  ├────┼────┤
  │    │    │
  └────┴────┘

  input 2,3
  output
  ┌────┬────┐
  │    │    │
  ├────┼────┤
  │    │    │
  ├────┼────┤
  │    │    │
  └────┴────┘
   */

  private val c = "─│┌┬┐├┼┤└┴┘"

  def challengeFunctionAlec(x: Int, y: Int): String = {
    def Z(q: Int) = c(q * 3 + 2) + "─" * 4 + (c(q * 3 + 3) + "─" * 4) * (x - 1) + c(q * 3 + 4)
    val t = "\n" + "│    " * x + "│\n"

    if (x > 0 & y > 0) Z(0) + t + (Z(1) + t) * (y - 1) + Z(2) else ""
  } //122

  def challengeFunctionLuke(x: Int, y: Int): String = {
    def f = if (_) 1 else 0

    0 to y collect { case i if x * y > 0 =>
      0 to x map { j =>
        c(10 - (f(i < 1) + f(i < y)) * 3 - f(j < 1) - f(j < x))
      } mkString "─" * 4
    } mkString s"\n│${"    │" * x}\n"
  } //129

  def challengeFunctionAamir(x: Int, y: Int): String = {
    type S = String

    def l(t: S, m: S, n: S, e: S) = {
      var s = t
      for (i <- 1 until (x * 6) - x) {
        if (i % 5 == 0) s = s + m
        else s = s + n
      }
      s + e + "\n"
    }

    if (x == 0 || y == 0) ""
    else {
      var s = l("┌", "┬", "─", "┐")
      for (i <- 1 until 2 * y) {
        if (i % 2 != 0) s = s + l("│", "│", " ", "│")
        else s = s + l("├", "┼", "─", "┤")
      }
      s = s + l("└", "┴", "─", "┘")
      s.init
    }
  } //246
}
