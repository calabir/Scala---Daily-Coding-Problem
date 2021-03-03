import org.junit.Assert.assertEquals

import scala.annotation.tailrec

/**
 * This problem was asked by Facebook.
 *
 * Given a string of round, curly, and square open and closing brackets, return whether the brackets are balanced (well-formed).
 *
 * For example, given the string "([])[]({})", you should return true.
 *
 * Given the string "([)]" or "((()", you should return false.
 * */

val openBrackets = List('(', '[', '{')
val closingBrackets = List(')', ']', '}')

//Naive approach
@tailrec
def isBalanced(in: String, openSymbol: Char): Boolean = {
  in.head match {
    case '(' => isBalanced(in.tail, '(')
    case '[' => isBalanced(in.tail, '[')
    case '{' => isBalanced(in.tail, '{')
    case ')' => if (openSymbol == '(') true else false
    case ']' => if (openSymbol == '[') true else false
    case '}' => if (openSymbol == '{') true else false
  }
}

isBalanced("[(])", ' ')
isBalanced("[]()", ' ')
isBalanced("[()]", ' ')


//Code clean up
@tailrec
def isBalanced2(in: String, openSymbol: Char): Boolean = {
  if (openBrackets contains in.head) isBalanced2(in.tail, in.head)
  else if (closingBrackets.contains(in.head) && in.head == closingBrackets(openBrackets.indexOf(openSymbol))) true else false
}

//More functional flavour
@tailrec
def isBalanced3(in: List[Char], openSymbol: Char): Boolean = {
  in match {
    case xs :: ys => xs match {
      case _ if openBrackets contains xs => isBalanced3(ys, xs)
      case _ if (closingBrackets contains xs) && (xs == closingBrackets(openBrackets.indexOf(openSymbol))) => true
      case _ => false
    }
  }
}




isBalanced2("[(])", ' ')
isBalanced2("[]()", ' ')
isBalanced2("[()]", ' ')

assertEquals(isBalanced("[(])", ' '), isBalanced3("[(])".toList, ' '))
assertEquals(isBalanced("[]()", ' '), isBalanced3("[]()".toList, ' '))
assertEquals(isBalanced("[()]", ' '), isBalanced3("[()]".toList, ' '))

assertEquals(false, isBalanced3("[(])".toList, ' '))
assertEquals(true, isBalanced3("[]()".toList, ' '))
assertEquals(true, isBalanced3("[()]".toList, ' '))
