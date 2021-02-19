package tip.misc

import tip.lattices.SignElement.{Neg, Pos, Zero}
import tip.lattices.SignLattice._

object CheckSignTables {
  private val intValues: Map[Int, Element] = Map(0 -> Bot, 1 -> FlatEl(Zero), 2 -> FlatEl(Neg), 3 -> FlatEl(Pos), 4 -> Top)

  def main(args: Array[String]) {
    {
      var isMonotone: Boolean = true
      for (a <- 0 to 2) {
        for (b <- 0 to 2) {
          for (ap <- 0 to a) {
            for (bp <- 0 to b) {
              isMonotone = isMonotone && checkMonotonicity(plus, intValues(a), intValues(b), intValues(ap), intValues(bp))
            }
          }
        }
      }
      println("check plus: " + isMonotone)
    }
  }
}
