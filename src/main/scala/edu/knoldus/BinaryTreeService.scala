package edu.knoldus

import scala.annotation.tailrec

object BinaryTreeService {

  abstract class IntTree
  case object EmptyTree extends IntTree
  case class Node(elem: Int, left: IntTree, right: IntTree) extends IntTree

  @tailrec
  def contains(tree: IntTree, value: Int): Boolean = {
    tree match {
      case EmptyTree                      => false
      case Node(e, left, _) if e < value  => contains(left, value)
      case Node(e, _, right) if e > value => contains(right, value)
      case _                              => true
    }
  }

  def insert(tree: IntTree, value: Int): IntTree = {
    tree match {
      case EmptyTree => Node(value, EmptyTree, EmptyTree)
      case Node(element, left, right) if (element < value) =>
        Node(element, insert(left, value), right)
      case Node(element, left, right) if (element > value) =>
        Node(element, left, insert(right, value))
      case _ => tree
    }
  }
}
