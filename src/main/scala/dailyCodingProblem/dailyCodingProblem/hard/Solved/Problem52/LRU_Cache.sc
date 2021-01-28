/**
 * This problem was asked by Google.
 *
 * Implement an LRU (Least Recently Used) cache. It should be able to be initialized with a cache size n, and contain the following methods:
 *
 * set(key, value): sets key to value. If there are already n items in the cache and we are adding a new item, then it should also remove the least recently used item.
 * get(key): gets the value at key. If no such key exists, return null.
 * Each operation should run in O(1) time.
 */


val cache_size = 10

//If we want to do it inmutable, every function set and get should return a new cache

var cache: Map[String, (Int, Int)] = Map()

def set(key: String, value: Int): Unit = {
  if (cache.size > 9) {
    val valueToDrop = cache.filter(e => e._2._2 == cache.maxBy(_._2._2)._2._2)
    cache = cache - valueToDrop.keys.head
  }
  val unmodifiedCache = cache.filter(k => k._1 != key)
  val minValue = if (cache.nonEmpty) cache.minBy(_._2._2)._2._2 - 1 else 0
  cache = unmodifiedCache + (key -> (value, minValue))
}


def get(key: String): Int = {
  if (!cache.contains(key)) throw new Exception("Key not found in the cache")
  else {
    val output = cache(key)._2
    val unmodifiedCache = cache.filter(k => k._1 != key)
    val minValue = if (cache.nonEmpty) cache.minBy(_._2._2)._2._2 - 1 else 0

    cache = unmodifiedCache + (key -> (cache(key)._1, minValue))

    output
  }
}

set("1", 1)
set("2", 2)
set("3", 3)
set("4", 4)
set("5", 5)
set("2", 3)   //Update 2
set("6", 6)
set("7", 7)
set("8", 8)
set("9", 9)
set("2", 4)   //Update 2
set("10", 10) //Cache has 10 elements here
set("11", 11) //Cache should drop the 1, since it's the oldest entry
assert(!cache.contains("1"))
set("12", 12) //Cache should drop the 3
assert(!cache.contains("3"))
get("4") //Using 4
set("13", 13)
set("14", 14) //Cache should drop the 5 instead of the 4
assert(cache.contains("4"))
assert(!cache.contains("5"))

assert(cache.size == 10)
cache