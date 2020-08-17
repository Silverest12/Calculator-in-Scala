package calculator

import scala.language.postfixOps
import scala.annotation.tailrec
import scala.io.StdIn._

object Main extends App {
  def infixToPostfix(exp: String): List[String]= {

    @tailrec
    def parseExp(expSplit: List[String], postfixList: List[String] = List(), opStack: List[String] = List("#")): List[String] = {

      def precedence(op: String): Int = {
        op match {
          case "+" | "-" => 1
          case "*" | "/" => 2
          case "^" => 3
          case _ => -1
        }
      }

      @tailrec
      def emptyStack(postfixList: List[String], opStack: List[String]): List[String] = {
        if(opStack.head == "#") postfixList
        else emptyStack((opStack.head +: postfixList.reverse).reverse, opStack.tail)
      }

      @tailrec
      def operatorPrec(operator: String, postfixList: List[String], opStack: List[String]): (List[String], List[String]) = {
        if(precedence(operator) > precedence(opStack.head)) (postfixList, operator +: opStack)
        else operatorPrec(operator, (opStack.head +: postfixList.reverse).reverse, opStack.tail)
      }

      @tailrec
      def foundParenth(postfixList: List[String], opStack: List[String]): (List[String], List[String]) = {
        if(opStack.head == "(") (postfixList, opStack.tail)
        else foundParenth((opStack.head +: postfixList.reverse).reverse, opStack.tail)
      }

      if(expSplit isEmpty) emptyStack(postfixList, opStack)

      else {
        if (expSplit.head.matches("-?\\d+")) parseExp(expSplit.tail, (expSplit.head +: postfixList.reverse).reverse, opStack)
        else if (expSplit.head.matches("[(^]")) parseExp(expSplit.tail, postfixList,  expSplit.head +: opStack)
        else if (expSplit.head == ")") {
          val ret = foundParenth(postfixList, opStack)
          parseExp(expSplit.tail, ret._1, ret._2)
        }
        else if (precedence(expSplit.head) > precedence(opStack.head)) parseExp(expSplit.tail, postfixList, expSplit.head +: opStack)
        else {
          val ret = operatorPrec(expSplit.head, postfixList, opStack)
          parseExp(expSplit.tail, ret._1, ret._2)
        }
      }
    }

    parseExp(exp.split("\\s+").toList)
  }

  @tailrec
  def calculatePostfix(stack: List[String], poppedList: List[Int] = List()): Int = {
    if(stack.isEmpty || (stack.length == 1 && poppedList.length == 1)) poppedList.head
    else {
      if(stack.head matches "[+^*/-]") {
        calculatePostfix(stack.tail,
          (if(stack.head == "+") poppedList.head + poppedList(1)
          else if(stack.head == "-") poppedList(1) - poppedList.head
          else if(stack.head == "*") poppedList.head * poppedList(1)
          else if(stack.head == "/") poppedList(1) / poppedList.head
          else scala.math.pow(poppedList(1),poppedList.head) toInt) +: poppedList.tail.tail)
      } else
        calculatePostfix(stack.tail, stack.head.toInt +: poppedList)
    }
  }

  def simplifyExpression(expression: String): String = {
    expression.replaceAll("\\s+", "").
      replaceAll("(-{2})+|\\++", " + ").
      replaceAll("(-\\s*\\+|\\+\\s*-|-)+", " - ").
      replaceAll("\\*(\\s*|\\++)", " * ").
      replaceAll("\\*-", " * -").
      replaceAll("/(\\s*|\\++)", " / ").
      replaceAll("/-", " / -").
      replaceAll("\\s*\\^\\s*", " ^ ").
      replaceAll("\\(", " ( ").
      replaceAll("\\)", " ) ")
  }

  def replaceVariables(expression: String, variables: Map[String, Int]): String = {
    expression.split(" ").
      map(x => if(x matches "[a-zA-Z]+") variables(x) toString else if(x matches "-[a-zA-Z]+") "-" + variables(x substring 1) else x).
      mkString(" ")
  }

  def validateInput(input: String, variables: Map[String, Int]) = {
    if (input matches "[+-]*\\s*\\d+\\s*([+-]+\\s*\\d+\\s*)*") true
    else
      input.split("\\s+|\\s*\\++\\s*|\\s*-+\\s*").
        filter(_ matches "[a-zA-Z]+").
        forall(variables.contains)
  }

  @tailrec
  def areParenthesesBalanced(exp: String, stack: List[Char] = List()): Boolean = {
    if(exp.length == 0) if(stack isEmpty) true else false
    else if(exp.charAt(0) == ')') if(stack isEmpty) false else areParenthesesBalanced(exp.substring(1), stack.tail)
    else if(exp.charAt(0) == '(') areParenthesesBalanced(exp.substring(1), '(' +: stack)
    else areParenthesesBalanced(exp.substring(1), stack)
  }

  @tailrec
  def takeInput(input: String = readLine().trim, variables: Map[String, Int] = Map()): Unit = {
    if (input == "/exit") {
      print("Bye!")
      System.exit(0)
    } else if (input == "/help") {
      println("The program calculates the sum of numbers")
      takeInput(variables = variables)
    } else if ((input matches "[+-]*\\s*\\(*(\\d+|[a-zA-Z]+)\\s*(\\s*[*^/+-]\\s*([+-]\\s*)*\\(*\\s*(\\d+|[a-zA-Z]+)\\s*\\)*)*") && areParenthesesBalanced(input)) {
      if(validateInput(input, variables)) {
        val list = infixToPostfix(replaceVariables(simplifyExpression(input), variables))
        println(calculatePostfix(list))
      } else println("Unknown variable")
      takeInput(variables = variables)
    } else if (input matches "[a-zA-Z]+\\s*=\\s*[-+]?\\s*\\d+") {
      val ins = input.split("\\s*=\\s*")
      takeInput(variables = variables + (ins(0) -> ins(1).replaceAll("\\s+", "").toInt))
    } else if (input matches "[a-zA-Z]+\\s*=\\s*[-+]?\\s*[a-zA-Z]+") {
      val ins = input.split("\\s*=\\s*")
      if (variables.contains(ins(1))) takeInput(variables = variables + (ins(0) -> variables(ins(1))))
      else {
        println("Unknown variable")
        takeInput(variables = variables)
      }
    } else if (input.startsWith("/")){
      println("Unknown command")
      takeInput(variables = variables)
    } else if (!input.isEmpty) {
      println("Invalid expression")
      takeInput(variables = variables)
    } else {

      takeInput(variables = variables)
    }
  }

  takeInput()
}
