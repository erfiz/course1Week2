package funsets

import org.scalatest.FunSuite


import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * This class is a test suite for the methods in object FunSets. To run
 * the test suite, you can either:
 *  - run the "test" command in the SBT console
 *  - right-click the file in eclipse and chose "Run As" - "JUnit Test"
 */
@RunWith(classOf[JUnitRunner])
class FunSetSuite extends FunSuite {

  /**
   * Link to the scaladoc - very clear and detailed tutorial of FunSuite
   *
   * http://doc.scalatest.org/1.9.1/index.html#org.scalatest.FunSuite
   *
   * Operators
   *  - test
   *  - ignore
   *  - pending
   */

  /**
   * Tests are written using the "test" operator and the "assert" method.
   */
  // test("string take") {
  //   val message = "hello, world"
  //   assert(message.take(5) == "hello")
  // }

  /**
   * For ScalaTest tests, there exists a special equality operator "===" that
   * can be used inside "assert". If the assertion fails, the two values will
   * be printed in the error message. Otherwise, when using "==", the test
   * error message will only say "assertion failed", without showing the values.
   *
   * Try it out! Change the values so that the assertion fails, and look at the
   * error message.
   */
  // test("adding ints") {
  //   assert(1 + 2 === 3)
  // }


  import FunSets._

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets {
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)
  }

  /**
   * This test is currently disabled (by using "ignore") because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", exchange the
   * function "ignore" by "test".
   */
  test("singletonSet(1) contains 1") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets {
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton")
    }
  }

  test("union contains all elements of each set") {
    new TestSets {
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
    }
  }

  test("intersect contains the comun elements") {
    new TestSets {
      val u = union(union(s1, s2),s3)
      val s = intersect(s1, u)
      assert(contains(s, 1), "Intersect 1")
      assert(!contains(s, 2), "Intersect 2")
      assert(!contains(s, 3), "Intersect 3")
    }
  }


  trait TestSetsAmpliado extends TestSets{
    val u3 = union(union(s1, s2),s3)
    val int = intersect(s1, u3)
    val s7 = singletonSet(7)
    val uni = union(u3, s7)
    val s4 = singletonSet(4)
    val s5 = singletonSet(5)
    val s6 = singletonSet(6)
    val union7 = union(union(union(uni,s4),s5),s6)
  }

  test("diff between set(1,2,3,7) and set(1,2,3)"){
    new TestSetsAmpliado {
      val d = diff(uni, u3)
      assert(!contains(d, 1), "diff 1")
      assert(!contains(d, 2), "diff 2")
      assert(!contains(d, 3), "diff 3")
      assert(contains(d, 7), "diff 7")
    }
  }

  test("diff between set(1,2,3) and set(1,2,3,7)"){
    new TestSetsAmpliado {
      val d = diff(u3, uni)
      assert(!contains(d, 1), "diff 1")
      assert(!contains(d, 2), "diff 2")
      assert(!contains(d, 3), "diff 3")
      assert(!contains(d, 7), "diff 7")
    }
  }

  test("filter set(1,2,3) with hold 1=>true"){
    new TestSetsAmpliado {
      val p : Int => Boolean = i => if (i==1) true else false
      val f = filter(u3, p)
      assert(contains(f, 1), "filter 1")
      assert(!contains(f, 2), "filter 2")
      assert(!contains(f, 3), "filter 3")
    }
  }


  test("forall set -1000 to 1000 with -10000 to 10000 =>true hold"){
    new TestSetsAmpliado {
      val myUnion : Int => Boolean = i => if (i >= -1000 && i<=1000) true else false
      val p : Int => Boolean = i => if (i >= -10000 && i<=10000) true else false
      val f = forall(myUnion, p)
      assert(f === true, "forall -1000 to 1000 bound")
    }
  }

//  test("forall set 1 to 7 with 1 to 6 =>true hold, changing bound from 1 to 7"){
//    new TestSetsAmpliado {
//      val p : Int => Boolean = i => if (i >= 1 && i<=6) true else false
//      val f = forall(union7, p)
//      assert(f === false, "forall 1 to 7 bound")
//    }
//  }

  test("exists set -1000 to 1000 with -1=>true hold"){
    new TestSetsAmpliado {
      val myUnion : Int => Boolean = i => if (i >= -1000 && i<=1000) true else false
      val p : Int => Boolean = i => if (i == -1) true else false
      val f = exists(myUnion, p)
      assert(f === true, "exists -1000 to 1000 bound")
    }
  }

  test("exists set -1000 to 1000 with -1001=>true hold"){
    new TestSetsAmpliado {
      val myUnion : Int => Boolean = i => if (i >= -1000 && i<=1000) true else false
      val p : Int => Boolean = i => if (i == -1001) true else false
      val f = exists(myUnion, p)
      assert(f === false, "exists -1000 to 1000 bound")
    }
  }


}
