package edu.knoldus

import java.util
import scala.annotation.tailrec
import scala.collection.immutable.{HashSet, ListMap}
import scala.collection.mutable
import scala.io.StdIn._


/***
 *
 * LRU cache implementation
 *
 */
class LruCache(size: Int) {

  val cache: mutable.LinkedHashSet[Int] = mutable.LinkedHashSet[Int]()
  /***
   * Method check elementis present in cache or not
   * @param elem
   *
   */
  def checkElem(elem: Int): cache.type = elem match {
    case elem if cache.contains(elem) => {
      cache -= elem
      cache += elem
    }
    case _ => addElem(elem)
  }

  /***
   * Method add elements to cache
   * @param elem
   *
   */

  def addElem(elem: Int): cache.type = {
    if (cache.size == size) {
      cache.remove(cache.iterator.next())
    }
    cache += elem
  }

}

object Main extends App {

  println("Enter the size of LRU cache")
  val lruSize = readInt()
  val lruCache = new LruCache(lruSize)

  @tailrec
  def printMenu(): Any = {
    println("Enter the element to be add")
    val elem = readInt()
    println(lruCache.checkElem(elem))
    printMenu()
  }
  printMenu()

}
