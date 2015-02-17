object HW1 extends js.util.JsApp {
  import js.hw1.ast._
  import js.hw1._
  
  /*
   * CSCI-UA.0480-006: Homework 1
   * <John Chen>
   * 
   * Partner: <Alan Morel>
   * Collaborators: <Thomas Liu>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the '???' expressions with your code in each function.
   * 
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Homework object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your solution will _not_ be graded if it does not compile!!
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert.  Simply put in a
   * '???' as needed to get something that compiles without error.
   */
  
  /*
   * Example with a Unit Test
   * 
   * A convenient, quick-and-dirty way to experiment, especially with small code
   * fragments, is to use the interactive Scala interpreter.
   * 
   * To run a selection in the interpreter in Eclipse, highlight the code of interest
   * and type Ctrl+Shift+X (on Windows/Linux) or Cmd+Shift+X (on Mac).
   * 
   * Highlight the next few lines below to try it out.  The assertion passes, so
   * it appears that nothing happens.  You can uncomment the "bad test specification"
   * and see that a failed assert throws an exception.
   * 
   * You can try calling 'plus' with some arguments, for example, plus(1,2).  You
   * should get a result something like 'res0: Int = 3'.
   * 
   * As an alternative, the testPlus2 function takes an argument that has the form
   * of a plus function, so we can try it with different implementations.  For example,
   * uncomment the "testPlus2(badplus)" line, and you will see an assertion failure.
   * 
   * Our convention is that these "test" functions are testing code that are not part
   * of the "production" code.
   * 
   * While writing such testing snippets is convenient, it is not ideal.  For example,
   * the 'testPlus1()' call is run whenever this object is loaded, so in practice,
   * it should probably be deleted for "release".  A more robust way to maintain
   * unit tests is in a separate file.  For us, we use the convention of writing
   * tests in a file called HWXSpec.scala (i.e., HW1Spec.scala for Homework 1).
   */
  
  def plus(x: Int, y: Int): Int = x + y
  def testPlus1() {
    assert(plus(1,1) == 2)
    //assert(plus(1,1) == 3) // bad test specification
  }
  testPlus1()

  def badplus(x: Int, y: Int): Int = x - y
  def testPlus2(plus: (Int, Int) => Int) {
    assert(plus(1,1) == 2)
  }
  //testPlus2(badplus)

  /* Exercises */

  def abs(n: Double): Double = {
    if (n >= 0) n else n * -1
  }
//  def test1() {
//    assert(abs(-5) == 5)
//    assert(abs(-5) != -5)
//    assert(abs(5) == 5)
//    assert(abs(5) != -5)
//    assert(abs(0) == 0)
//  }

  def swap(p: (Int, Int)): (Int, Int) = {
    (p._2, p._1)
  }
//  def test2() {
//    assert(swap(1,2) == (2,1))
//    assert(swap(1,1) == (1,1))
//    assert(swap(2,1) == (1,2))    
//  }
//  test2()

  def repeat(s: String, n: Int): String = {
    if (n == 0) ""
    if (n == 1) s else s+repeat(s,n-1)
  }
//  def test3() {
//    assert(repeat("a",3) == "aaa")
//    assert(repeat("ab",3) == "ababab")
//    assert(repeat("ab",3) != "abababab")
//  }
//  test3()
  
  def sqrtStep(c: Double, xn: Double): Double = {
    return xn - (xn*xn-c)/(2*xn)
  }
//  def test4() {
//    assert(sqrtStep(1,2) == 1.25)
//  }
//  test4()

  def sqrtN(c: Double, x0: Double, n: Int): Double = {
    if (n == 0) {
      x0
    }
    if (n == 1) sqrtStep(c, x0) else sqrtN(c, sqrtStep(c, x0), n-1)
  }
//  def test5() {
//    val a = sqrtN(5, 2, 2)
//    println(a)
//  }
//  test5()
//  def abs(n: Double): Double = {
//    if (n >= 0) n else n * -1
//  }
//    def sqrtStep(c: Double, xn: Double): Double = {
//    return xn - (xn*xn-c)/(2*xn)
//  }
  def sqrtErr(c: Double, x0: Double, epsilon: Double): Double = {
    if (epsilon < abs(x0*x0-c)) sqrtErr(c, sqrtStep(c, x0), epsilon) else sqrtStep(c, x0)
  }
  def sqrt(c: Double): Double = {
    require(c >= 0)
    if (c == 0) 0 else sqrtErr(c, 1.0, 0.0001)
  }
//  def test6() {
//    val a = sqrtErr(5, 2, .0001)
//    val b = sqrt(5)
//    println(a)
//    println(b)
//  }
//  test6()
  
  /* Binary Search Tree */
  
//  sealed abstract class BSTree
//  case object Empty extends BSTree
//  case class Node(l: BSTree, d: Int, r: BSTree) extends BSTree
  
  def repOk(t: BSTree): Boolean = {
    def check(t: BSTree, min: Int, max: Int): Boolean = t match {
      case Empty => true      
      case Node(l, d, r) => {
        if (d <= min) {
          return false
        }
        if (d >= max) {
          return false
        }
        check(l, min, d) && check(r, d, max)
      }
      
    }
    check(t, Int.MinValue, Int.MaxValue)
  }
//  def test7() {
//    val q = Node(Empty, 5, Empty)
//    val w = Node(Empty, 100, Empty)
//    val a = Node(q, 10, w)
//    println(repOk(a) + " hello")
//    val b = Node(w, 10, q)
//    println(repOk(b) + " hello")
//  }
//  test7()
  
  def insert(t: BSTree, n: Int): BSTree = {
    t match {
      case Empty => Node(Empty, n, Empty)
      case Node(l, d, r) => {
        if (l == Empty && n < d) {
          return Node(Node(Empty, n, Empty), d, r)
        } else if (r == Empty && n >= d){
          return Node(l, d, Node(Empty, n, Empty))
        } else if (l != Empty && n < d) {
          return Node(insert(l, n), d, r)
        } else if (r != Empty && n >= d) {
          return Node(l, d, insert(r,n))
        }
        t
      }
    }
  }
//  def test8() {
//    val a = Empty;
//    val b = insert(a, 5)
//    val c = insert(b, 3)
//    val d = insert(c, 6)
//    val b1 = insert (d, 100)
//    val e = insert(a, 5)
//    val f = insert(e, 6)
//    val g = insert(f, 3)
//    val e1 = insert (g, 100)
//    assert(d == g)
//    assert(c != f)
//    assert(b1 == e1)
//  }
//  test8()
  
  def deleteMin(t: BSTree): (BSTree, Int) = {
    require(t != Empty)
    (t: @unchecked) match {
      case Node(Empty, d, r) => (r, d)
      case Node(l, d, r) => {
        val (l1, m) = deleteMin(l)
        return (Node(deleteMin(l)._1, d, r),m)
      }
    }
  }
//  def test9() {
//    val a = Empty;
//    val b1 = insert(a, 5)
//    val b2 = insert(b1, 6)
//    val b3 = insert(b2, 3)
//    val b4 = insert(b3, 2)
//    
//    val c1 = insert(a, 5)
//    val c2 = insert(c1, 3)
//    val c3 = insert(c2, 6)
//    val c4 = insert(c3, 2)
//    
//    val x1 = deleteMin(b4)
//    val x2 = deleteMin(c4)
//    assert(x1 == x2)
//    println(x1 + " is x1")
//    println(x2 + " is x2")
//  }
//  test9()
  sealed abstract class BSTree
  case object Empty extends BSTree
  case class Node(l: BSTree, d: Int, r: BSTree) extends BSTree
  def delete(t: BSTree, n: Int): BSTree = {
    require(t != Empty)
    (t: @unchecked) match {
      case Node(Empty, d, Empty) => {
        if (n == d) {
          return Empty
        } else {
          return t
        }
      }
      case Node(l, d, r) => {
        if (n < d) {
          return Node(delete(l, n), d, r)
        } else if (n > d) {
          return Node(l, d, delete(r, n))
        } else if (n == d) {
          if (l == Empty && r == Empty) {
            return Empty
          } else if (l == Empty) {
            return Node(Empty, d, r)
          } else if (r == Empty) {
            return Node(l, d, Empty)
          } else {
            val newRoot = deleteMin(r)
            return Node(l, newRoot._2, newRoot._1)
          }
        } else {
          return t
        }
      }
    }
  }
//    def test10() {
//    val a = Empty;
//    val b1 = insert(a, 50)
//    val b2 = insert(b1, 25)
//    val b3 = insert(b2, 75)
//    val b4 = insert(b3, 12)
//    val b5 = insert(b4, 37)
//    val b6 = insert(b5, 30)
//    val b7 = insert(b6, 40)
//    val b8 = insert(b7, 29)
//    val b9 = delete(b8, 25)
//    
//    val c1 = insert(a, 50)
//    val c2 = insert(c1, 29)
//    val c3 = insert(c2, 75)
//    val c4 = insert(c3, 12)
//    val c5 = insert(c4, 37)
//    val c6 = insert(c5, 30)
//    val c7 = insert(c6, 40)
//    
//    println(b9 + " is b9")
//    println(c7 + " is c7")
//    assert(b9 == c7)
//  }
//  test10()
  /* JakartaScript */
  
  def eval(e: Expr): Double = e match {
    case Num(n) => n
    case _ => throw new IllegalArgumentException("Inputted a NON-number");
  }
  
 // Interface to run your interpreter from a string.  This is convenient
 // for unit testing.
 def eval(s: String): Double = eval(parse(s))



 /* Interface to run your interpreter from the command-line.  You can ignore the code below. */ 
  
 def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }
    
    val expr = parse(file)
    
    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }
    
    if (debug) { println("Evaluating ...") }
    
    val v = eval(expr)
    
    println(v)
  }

}
