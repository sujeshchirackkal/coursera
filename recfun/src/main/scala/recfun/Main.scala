package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    val str: String = "((this( is )good))"
    println(balance(str.toList))
    println("number of ways : " + countChange(300,List(500,5,50,100,20,200,10)))
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {

    if (r == 0 || c == 0 || r == c) 1
    else
      pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanced(open: Int, chars: List[Char]): Boolean = {
      if (chars.isEmpty) open == 0
      else if (chars.head == '(')
        balanced(open + 1, chars.tail)
      else if (chars.head == ')') open > 0 && balanced(open - 1, chars.tail)
      else
        balanced(open, chars.tail)

    }
    balanced(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if(money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else {
      countChange(money - coins.head, coins) +
      countChange(money, coins.tail)
    }
  }
}
