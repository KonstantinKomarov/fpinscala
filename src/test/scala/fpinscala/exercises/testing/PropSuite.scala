package fpinscala.exercises.testing

import munit.FunSuite

class PropSuite extends FunSuite {

  // test("Prop.&& returns true only if both are true") {
  //   val p1 = new Prop { def check = true }
  //   val p2 = new Prop { def check = true }
  //   val p3 = new Prop { def check = false }

  //   assert((p1 && p2).check, "true && true should be true")
  //   assert(!(p1 && p3).check, "true && false should be false")
  //   assert(!(p3 && p1).check, "false && true should be false")
  //   assert(!(p3 && p3).check, "false && false should be false")
  // }

  // test("Prop.&& short-circuits when first is false") {
  //   var evaluated = false
  //   val p1 = new Prop { def check = false }
  //   val p2 = new Prop { def check = { evaluated = true; true } }

  //   val combined = p1 && p2
  //   assert(!combined.check, "combined check should be false")
  //   assert(!evaluated, "Second Prop should not be evaluated when first is false")
  // }

  // test("Prop.&& evaluates second when first is true") {
  //   var evaluated = false
  //   val p1 = new Prop { def check = true }
  //   val p2 = new Prop { def check = { evaluated = true; true } }

  //   val combined = p1 && p2
  //   assert(combined.check, "combined check should be true")
  //   assert(evaluated, "Second Prop should be evaluated when first is true")
  // }
}