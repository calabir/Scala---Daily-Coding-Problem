import org.junit.Assert.assertEquals

import scala.collection.MapView

/** This problem was asked by Twitter.
 *
 * Implement an autocomplete system. That is, given a query string s and a set of all possible query strings, return all strings in the set that have s as a prefix.
 *
 * For example, given the query string de and the set of strings [dog, deer, deal], return [deer, deal].
 *
 * Hint: Try preprocessing the dictionary into a more efficient data structure to speed up queries.
 */


val in1 = "de"
val list1 = List("dog", "deer", "deal")


def autocomplete(input: String, inSet: List[String]): List[String] = {
  inSet match {
    case List() => List[String]("")
    case xs :: ys =>
      val value = if (xs.startsWith(input)) xs else ""
      autocomplete(input, ys).reverse.appended(value).filter(v => !v.isEmpty)
  }
}



assertEquals(List("deer", "deal"), autocomplete(in1, list1))

//We can improve it
//TODO we can build a Map[String, List[String]] to index from a prefix to all the words, to do it we should define a way to merge maps

val esto = Map("do" -> List("dog"),
  "de" -> List("deer", "deal"))

val map1 = list1.map(v => Map(v.take(2) -> v))
val map2 = list1.map(v => Map(v.take(2) -> v)).map(r => r.groupBy(_._1))


println("esto: " + list1.flatMap(v => Map(v.take(2) -> v)).groupBy(_._1))             //Map(b -> List((b,d), (b,e)), f -> List((f,h)))


//def mergeMap(m1: List[Map[String, List[String]]], acc: Map[String, List[String]]): Map[String, List[String]] = {
//  m1 match {
//    case List() => acc
//    case xs::ys => if(acc.keys.toList.contains(xs.keys.head)) {
//      val oldKeyValue = acc.get(xs.head._1)
//      val newMapWithoutMatch = acc.filter(_._1 != xs.head._1)
//      //afegim el valor que ja tenia + el valor nou
//      val newMapUpdated = newMapWithoutMatch ++ Map[String, List[String]](xs.head._1 -> List(oldKeyValue) :: List(xs.head._2))
//      newMapUpdated
//    }
//  }
//}
//def autocomplete2()