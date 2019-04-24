package recfun

import scala.collection.JavaConverters._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    println(balance("(just an) example".toList))
    println(balance("()".toList))
    println(balance("(()())".toList))
    println(balance("".toList))
    println(balance("())(".toList))
    println(balance(":-)".toList))
    println(balance("((()".toList))

    println(countChange(7, List(1,2)))
    println(countChange(6, List(1,2)))
    println(countChange(5, List(1,2)))
    println(countChange(4, List(1,2)))
    println(countChange(4, List()))
  }

  /**
   * Exercise 1
    * 1
    * 1 1
    * 1 2 1
    * 1 3 3 1
    * 1 4 6 4 1
   */
    def pascal(c: Int, r: Int): Int =
      if(c == 0)
        1
      else if(r == 0)
        1
      else if(c == r)
        1
      else
        pascal(c - 1, r - 1) + pascal(c, r - 1)
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balance(chars: List[Char], left: Int): Boolean = {
        if(left < 0) false
        else if(chars.isEmpty) left==0
        else if(chars.head == '(') balance(chars.tail, left + 1)
        else if(chars.head == ')') balance(chars.tail, left - 1)
        else balance(chars.tail, left)
      }

      balance(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(coins.isEmpty || money < 0) 0
      else if(money == 0) 1
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }

  }
