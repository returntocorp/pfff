// Scala: More idiomatic
// Uses type inference, omits `return` statement,
// uses `toInt` method, declares numSquare immutable

import math._

def mathFunction(num: Int) = {
  val numSquare = num*num
  (cbrt(numSquare) + log(numSquare)).toInt
}
