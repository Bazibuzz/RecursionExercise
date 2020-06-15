package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    require(c > 0 || r > 0, "Must be positive")
    require(c <= r, "c must be superior or equal to r")
    if (r == 0 || c == 0 || r == c) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @scala.annotation.tailrec
    def loop(acc: Int, chars: List[Char]): Int ={
      if(chars.isEmpty || acc < 0) acc
      else
        if (chars.head == "(".toList.head ) loop(acc + 1, chars.tail)
        else if (chars.head == ")".toList.head ) loop(acc - 1, chars.tail)
        else loop(acc, chars.tail)
    }

    if(loop(0,chars) == 0) true
    else false
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
