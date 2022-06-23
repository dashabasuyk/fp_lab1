package com.basuykdaria.lab1

import com.basuykdaria.lab1
import MyList.*
import munit.FunSuite

class MyListSuite extends FunSuite {
  test("duplicate []") {
    val expected = MyNil
    val actual = duplicate(MyNil)
    assertEquals(actual, expected)
  }
  test("duplicate [1,2,3,4,5]") {
    val expected = MyList((1,1),(2,2),(3,3),(4,4),(5,5))
    val actual = duplicate(MyList(1,2,3,4,5))
    assertEquals(actual, expected)
  }
  test("intersperse []") {
    val expected = MyNil
    val actual = intersperse(MyNil, 0)
    assertEquals(actual, expected)
  }
  test("intersperse [1,2,3,4,5] and 0") {
    val expected = MyList(1, 0, 2, 0, 3, 0, 4, 0, 5)
    val actual = intersperse(MyList(1,2,3,4,5), 0)
    assertEquals(actual, expected)
  }
  test("intersperse [1,2,3] and 7") {
    val expected = MyList(1, 7, 2, 7, 3)
    val actual = intersperse(MyList(1,2,3), 7)
    assertEquals(actual, expected)
  }
  test("fromString \"\"") {
    val expected = MyNil
    val actual = fromString("")
    assertEquals(actual, expected)
  }
  test("fromString \"Cat\"") {
    val expected = MyList('C', 'a', 't')
    val actual = fromString("Cat")
    assertEquals(actual, expected)
  }
  test("toString [c,a,t]") {
    val expected = "cat"
    val actual = lab1.toString(MyList('c', 'a', 't'))
    assertEquals(actual, expected)
  }
  test("mapWithIndex [1, 2, 3, 4, 5] and +") {
    val expected = MyList(1, 3, 5, 7, 9)
    val actual = mapWithIndex(MyList(1,2,3,4,5), (x , y) => x + y)
    assertEquals(actual, expected)
  }
  test("clean []") {
    val expected = MyNil
    val actual = clean(MyNil)
    assertEquals(actual, expected)
  }
  test("clean [1,1,3,4,5,5,5,6]") {
    val expected = MyList(1,3,4,5,6)
    val actual = clean(MyList(1,1,3,4,5,5,5,6))
    assertEquals(actual, expected)
  }
  test("getN [1,1,2,3,4,2,5,0] and 3") {
    val expected = Some(2)
    val actual = getN(MyList(1,1,2,3,4,2,5,0),3)
    assertEquals(actual, expected)
  }
  test("getN [1,1,2,3,4,2,5,0] and 5") {
    val expected = Some(3)
    val actual = getN(MyList(1,1,2,3,4,2,5,0),5)
    assertEquals(actual, expected)
  }
}
