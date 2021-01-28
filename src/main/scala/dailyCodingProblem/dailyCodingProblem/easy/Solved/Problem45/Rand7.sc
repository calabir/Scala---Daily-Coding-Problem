/**
 * This problem was asked by Two Sigma.
 *
 * Using a function rand5() that returns an integer from 1 to 5 (inclusive) with uniform probability,
 * implement a function rand7() that returns an integer from 1 to 7 (inclusive).
 *
 */

def rand5(): Int = {
  (Math.random * 6).toInt
}

def rand7(): Int = {
  ((rand5() + rand5()) % 7) +1
}
