package edu.knoldus

import org.scalatest.flatspec.AnyFlatSpec

class LruSpec extends AnyFlatSpec{
  val lruCache = new LruCache(3)
  val list = lruCache.addElem(1)
  val list2 = lruCache.addElem(1)
  
  "A LRU Cache" should "contains the first added element" in {
    assert(list.contains(1))
  }

}
