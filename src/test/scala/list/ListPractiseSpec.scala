package scala.list

import list.ListPractise
import org.scalatest.{FlatSpec, Matchers}

class ListPractiseSpec extends FlatSpec with Matchers{
  val listPractise = new ListPractise
  val list = List(1,2,3,4)
  val listSingleElement = List(1)


  "A list" should "get the last element" in {
    listPractise.getLastElementIn(list) should be (Some(4))
  }
  it should "throw NoSuchElementException if the list has no elements" in {
    a [NoSuchElementException] should be thrownBy {
      listPractise.getLastElementIn(List())
    }
  }

  "A list" should "get the penultimate element" in {
    listPractise.getPenultimate(list) should be (Some(3))
  }
  it should "throw NoSuchElementException if the list has no penultimate element" in {
    a [NoSuchElementException] should be thrownBy {
      listPractise.getPenultimate(listSingleElement)
      listPractise.getPenultimate(List())
    }
  }

  "A list" should "find the Nth element" in {
    listPractise.getNthElement(3,list) should be (Some(3))
  }
  it should "throw NoSuchElement if 0 is entered" in {
    a [NoSuchElementException] should be thrownBy {
      listPractise.getNthElement(0,list)
    }
  }
  it should "throw UnsupportedOperationException" in {
    a [UnsupportedOperationException] should be thrownBy {
      listPractise.getNthElement(5,List())
    }
  }

  "A list" should "get the number of elements " in {
    listPractise.getNumberOfElements(list) should be (4)
    listPractise.getNumberOfElements(List()) should be (0)
  }

  "A list" should "reverse the list " in {
    listPractise.reverse(list) should be (List(4,3,2,1))
    listPractise.reverse(List()) should be (Nil)
  }

  "A list" should "return true if it is a palindrome" in {
    listPractise.isPalindrome(List(1,2,3,2,1)) should be (true)
    listPractise.isPalindrome(List(1,2,3,3,2,1)) should be (true)
    listPractise.isPalindrome(List(1,3,3,3,2,1)) should be (false)
    listPractise.isPalindrome(List()) should be (true)
  }

  "A list inside a list" should "return a flattened list" in {
    listPractise.flattenList(List(List(1,2),List(3),List(4))) should be (list)
    listPractise.flattenList(List()) should be (List())
  }

  "A list" should "eliminate consecutive duplicates" in {
    listPractise.eliminateDuplicates(List('a','a','b','b','b','c','c')) should be (List('a','b','c'))
  }
  it should "throw UnsupportedOperationException when an empty list is operated on" in {
    a [UnsupportedOperationException] should be thrownBy {
      listPractise.eliminateDuplicates(List())
    }
  }

  "A list" should "pack consecutive dupilcates in a list" in {
    listPractise.packConsecutiveDuplicates(List('a','a','b','b','b','c','c')) should be (List('a', 'a'), List('b','b','b'), List('c','c'))
  }
}
