/**
 * Using a function rand7() that returns an integer from 1 to 7 (inclusive) with uniform probability, implement a function rand5() that returns an integer from 1 to 5 (inclusive).
 */

//Similar to Problem 45

def rand7(): Int = {
  ((Math.random() * 7) + 1).toInt
}

def rand5(): Int = {
  //We generate: 5,10,15,20,25,30,35 subtract 1 -> 4,9,14,19,24,29,34 divided by 7: 0,1,2,3,4 and finally adding 1
  ((5*rand7() - 1) / 7) + 1
}
