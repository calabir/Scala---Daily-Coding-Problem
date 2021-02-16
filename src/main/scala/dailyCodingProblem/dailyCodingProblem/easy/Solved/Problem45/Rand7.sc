/**
 * This problem was asked by Two Sigma.
 *
 * Using a function rand5() that returns an integer from 1 to 5 (inclusive) with uniform probability,
 * implement a function rand7() that returns an integer from 1 to 7 (inclusive).
 *
 */

def rand5(): Int = {
  (Math.random * 5).toInt + 1
}

def rand7(): Int = {
  ((5*rand5() + rand5() - 1) % 7) + 1
}

rand7()
rand7()
rand7()
rand7()
rand7()
rand7()
rand7()
rand7()
rand7()
rand7()
rand7()
rand7()
rand7()
//Demonstration of the formula  5*foo() + foo() -5

def GFG() = {

  var first = 0
  var second = 0
  first = 1
  while ( {
    first <= 5
  }) {
    second = 1
    while ( {
      second <= 5
    }) {
      System.out.printf("%d \n", 5 * first + second - 5)
      second += 1
    }
    first += 1
  }
}

GFG()