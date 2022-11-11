
import cats._
import cats.implicits._

// Monoids are associative, not conmutative

case class Speed(metersPerSecond: Double) {

    def kilometersPerSec = metersPerSecond / 10000.0

    def milesPerSec = metersPerSecond / 1609.34

}

object Speed {
    val empty = Speed(0)
    def addSpeeds(s1: Speed, s2: Speed) = Speed(s1.metersPerSecond + s2.metersPerSecond)

    // implicit val monoidSpeed: Monoid[Speed] = new Monoid[Speed] {
    //     override def empty: Speed = Speed(0)
    //     override1: Speed, s2: Speed) = Speed(s1.metersPerSecond + s2.metersPerSecond)
    // }
    implicit val monoidSpeed: Monoid[Speed] = Monoid.instance(Speed(0), addSpeeds)


    // for the isEmpty call
    implicit val eqSpeed: Eq[Speed] = Eq.fromUniversalEquals
    Monoid[Speed].combine(Speed(1000), Speed(2000))
    Monoid[Speed].combine(Speed(0), Speed.empty)
    Speed(1000) |+| Speed(2000)
// compbine lis of elems
Monoid[Speed].combineAll(List(Speed(1000), Speed(2000), Speed(2000)))
List(Speed(1000), Speed(2000), Speed(2000)).combineAll

Monoid[Speed].isEmpty(Speed(1000))
Monoid[Speed].isEmpty(Speed(0)) //true
// Hot to generalize

// trait Monoid[A] extends Semigroup[A] {
// }

val sumMonoid: Monoid[Int] = Monoid.instance(0, _ + _)
val minMonoid: Monoid[Int] = Monoid.instance(Integer.MAX_VALUE, _ min _)
def listMonoid[A]: Monoid[List[A]] = Monoid.instance(List.empty, _ ++ _)
val stringMonoid: Monoid[String] = Monoid.instance("", _ ++ _) 
}
