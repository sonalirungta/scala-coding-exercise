package com.oracle.assignment

object CodeFun {
  /**
    * Implement a function which prints only odd numbers between [min, max]
    * @param min Inclusive start of numbers
    * @param max Inclusive end of numbers
    * @return List of all numbers which are odd between [min,max]
    */
  def generateOdds(min: Int, max: Int): List[Int] = {
     for (i <- (min to max).toList;if i%2==1) yield i
	  	
  }

  /**
    * Implement a function which reverses a String
    * @param s Input string
    * @return new String which is the reversed version of input
    */
  def stringReverser(s: String): String = {
    (for(i<-s.length-1 to 0 by -1) yield s(i)).mkString

  }
  /**
    * Generate a Map which contains the the frequency counts of all the words
    * specified in the input Sequence. It should ignore case
    * @param input
    * @return Map of Word => Count
    */
  def wordFrequency(input: Seq[Seq[String]]): Map[String, Int] = {
      input.flatten.flatMap(_.split(" ")).foldLeft(Map.empty[String, Int])
                                                    {(count, word) => count + (word -> (count.getOrElse(word, 0) + 1))}
  }
  
  /**
    * Collapse a list of Options into a concrete list of values
    * @param l
    * @return
    */
  def optionCollapse[T](l: Seq[Option[T]])(f: T => Boolean): Seq[T] = {
    l.flatten
  }
}