package com.oracle.assignment

import java.util.Map.Entry
import collection.immutable.List
import collection.immutable.Map
import collection.immutable.Seq
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.Matchers._
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CodeFunTest extends FunSuite {

  //In scala [Int] starts from 0 so unable to test for negative numbers
  var l = List.empty[Int]
  l = CodeFun.generateOdds(0, 5);
  //testing
  l should have length 3

  //Simple string without white spaces
  var original: String = "CodeFun"
  assert(CodeFun.stringReverser(original) == "nuFedoC")

  //String with some special char,white spaces and numbers
  original = "Co$de  Fu%@n11"
  assert(CodeFun.stringReverser(original) == "11n@%uF  ed$oC")

  var mySeq: Seq[Seq[String]] = Seq(
    Seq("Plain Donut", "Strawberry Donut", "Chocolate Donuts"),
    Seq("Vanilla Icecream", "Mango Icecream", "Vanilla Strawberry Icecream"))
  var wordCountMap: Map[String, Int] = CodeFun.wordFrequency(mySeq)
  //testing
  var wordCountResult: Map[String, Int] = Map("Mango" -> 1, "Strawberry" -> 2, "Vanilla" -> 2, "Donuts" -> 1, "Plain" -> 1, "Icecream" -> 3, "Donut" -> 2, "Chocolate" -> 1)
  wordCountMap should contain(("Mango" -> 1))
  assertResult(wordCountResult)(wordCountMap)
  
  //var option: Seq[Option[Int]][]=Seq(Some(List(1,2,3))(true)
  //option should (equal (Some(List(1, 2, 3))) or be (None))
} 