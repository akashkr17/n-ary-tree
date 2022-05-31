package edu.knoldus
import scala.collection.mutable
class LRUCache(capacity: Int, hashSet: mutable.LinkedHashSet[String]) {

  def checkPage(page: String): mutable.LinkedHashSet[String] = {
    if (hashSet contains page) {
      hashSet.remove(page)
      hashSet.add(page)
      hashSet
    } else {
      if (hashSet.size == capacity) {
        hashSet.remove(hashSet.head)
        hashSet.add(page)
        hashSet
      } else {
        hashSet.add(page)
        hashSet
      }
    }

  }
}

object LRUCacheset {
  def apply(capacity: Int): LRUCache = new LRUCache(capacity, mutable.LinkedHashSet.empty[String])
}

object DriveLru extends App {
  val lruCache = LRUCacheset(5)
  println(lruCache.checkPage("a"))
  println(lruCache.checkPage("b"))
  println(lruCache.checkPage("c"))
  println(lruCache.checkPage("a"))
  println(lruCache.checkPage("d"))
  println(lruCache.checkPage("a"))
  println(lruCache.checkPage("c"))
  println(lruCache.checkPage("e"))
}
