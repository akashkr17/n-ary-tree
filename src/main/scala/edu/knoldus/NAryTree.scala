package edu.knoldus

import edu.knoldus.NAryTree.{Leaf, NaryTree, Node, countAllNode, countNodeChild}



object NAryTree {

  sealed abstract class NaryTree

  case class Leaf(elem: String) extends NaryTree

  case class Node(elem: String, tree: NaryTree*) extends NaryTree
  /***
   * Method calculate nodes recursively
   * @param tree
   * @return total no of nodes of a tree
   */
  def countAllNode(tree: NaryTree): Int = tree match {
    case Leaf(_) => 1
    case Node(_, nodes @ _*) =>
      1 + nodes.map(countAllNode).sum
  }


  /***
   * Method calculate nodes recursively
   * @param tree
   * @return No of nodes having more than one child
   */
  def countNodeChild(tree: NaryTree): Int = tree match {
    case Leaf(_) => 0
    case Node(_, nodes @ _*) =>
      nodes match {
        case node if nodes.length > 1 => 1 + node.map(countNodeChild).sum
        case _                        => nodes.map(countNodeChild).sum
      }
  }
}

object TreeMain extends App {

  val testTree = Node(
    "a",
    Node(
      "b",
      Leaf("d"),
      Leaf("e"),
      Leaf("f")
    ),
    Node(
      "b",
      Leaf("d"),
      Leaf("e"),
      Leaf("f")
    ),
    Node(
      "b",
      Leaf("d"),
      Leaf("e"),
      Leaf("f")
    ),
    Node(
      "c",
      Node(
        "g",
        Leaf("h")
      )
    )
  )
  println(testTree.toString)
  println(
    s"Number of nodes with more than one child N-Ary Tree  = ${countNodeChild(testTree)}")
  println(s"Number of nodes in N-Ary Tree = ${countAllNode(testTree)}")

}
