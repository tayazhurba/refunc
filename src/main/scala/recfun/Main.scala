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
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
  if (c>r) 0
  else if (r==0 || c==0) 1
  else pascal(c-1,r-1)+pascal(c,r-1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean ={
  def balance1(k:Int, chars: List[Char]): Boolean={
    def go(k:Int):Int=k+1
    def back(k:Int):Int=k-1
    if (chars.isEmpty && k==0) true
    else if (k<0) false
    else if (chars.head == '(') balance1(go(k), chars.tail)
    else if (chars.head == ')') balance1(back(k), chars.tail)
    else  balance1(k,chars.tail)
  }
    balance1(0,chars)
  }
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def go(k: Int): Int = k + 1
    def countCoins(k: Int, coins: List[Int]): Int = {
      if (coins.isEmpty) k else countCoins(go(k), coins.tail)
    }
    def countChange(k:Int, money: Int, countCoins: Int): Int = {

      if (money == 0) 1
      else if (money < 0 || countCoins == 0) 0
      else countChange(k, money, countCoins - 1) + countChange(k, money - coins.head, countCoins)
    }
    countChange(0, money, countCoins(0, coins))
  }
}
