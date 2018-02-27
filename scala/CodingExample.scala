package com.oracle.assignment

object CodeFun {
  /**
   * Implement a function which prints only odd numbers between [min, max]
   * @param min Inclusive start of numbers
   * @param max Inclusive end of numbers
   * @return List of all numbers which are odd between [min,max]
   */
  def generateOdds(min: Int, max: Int): List[Int] = {
    for (i <- (min to max).toList; if i % 2 == 1) yield i

  }

  /**
   * Implement a function which reverses a String
   * @param s Input string
   * @return new String which is the reversed version of input
   */
  def stringReverser(s: String): String = {
    (for (i <- s.length - 1 to 0 by -1) yield s(i)).mkString

  }
  /**
   * Generate a Map which contains the the frequency counts of all the words
   * specified in the input Sequence. It should ignore case
   * @param input
   * @return Map of Word => Count
   */
  def wordFrequency(input: Seq[Seq[String]]): Map[String, Int] = {
    
    val grouped = input.flatten.flatMap(_.split(" ")).groupBy(x => x.toLowerCase()) // Group by element (all chars to lowercase)
    
    grouped.map(x => (x._1, x._2.length)).toMap // Create pair for each element
  }

  /**
   * Collapse a list of Options into a concrete list of values
   * @param l
   * @return
   */
  def optionCollapse[T](l: Seq[Option[T]])(f: T => Boolean): Seq[T] = {
    l.flatten.filter(f)
  }
}

/**
 * Test Suite Object which contains a list of tests to run for Unit testing
 */
object TestSuite {
   

  /**
   * **
   * We now need to test our CodeFun functions.
   * Write proper unit tests to make sure our functions do what we are expecting
   * Once you write all of your tests, add them to the testsToRun!
   * ***
   */
  val testsToRun = Seq(
    ("OptionCollapseTest", optionCollapseTest _),
    ("WordFrequencyTest", wordFrequencyTest _),
    ("GeneratingOddNumbersTest", generateOddsTest _),
    ("ReversingStringTest",stringReverserTest _))

  // BEGIN Tests
  def optionCollapseTest(): Boolean = {
    val input: Seq[Option[Int]] =
      Seq(Option(1), Option(2), None, Option(3), None, Option(4))

    val expected = Seq(2, 4)

    // running option collapse should only yield even numbers
    CodeFun.optionCollapse(input)(_ % 2 == 0) == expected
  }

  def wordFrequencyTest(): Boolean = {
    val input = Seq(Seq("This", "is", "test"),
      Seq("This", "has", "something"),
      Seq("Something", "which", "is", "something"))

    val expected = Map("this" -> 2, "is" -> 2, "test" -> 1, "has" -> 1, "something" -> 3, "which" -> 1)

    CodeFun.wordFrequency(input) == expected
  }
  //Test to generate odd numbers
  def generateOddsTest(): Boolean = {
    val min: Int = 0
    val max: Int = 5
    
    val expected = List(1, 3, 5)
    
    CodeFun.generateOdds(min, max) == expected
  }
  //Test to reverse a given string
  def stringReverserTest(): Boolean={
     val expected="11n@%uF  ed$oC"
     val original ="Co$de  Fu%@n11"  
     
     CodeFun.stringReverser(original) == expected
  }
  /**
   * Add the logic to execute all of the tests.
   * This function should print the name of the test and whether it passed or failed.
   */
  /*def runAllTests(): Unit = {
     testsToRun.foreach ( test => {
        // test.
     })
  }*/
  
  

}