package edu.knoldus


import org.scalatest.funsuite.AnyFunSuite

class BinaryTreeSpec extends AnyFunSuite {

  val t = BinaryTreeService
  val tree = t.Node(8, t.Node(5, t.Node(7, t.EmptyTree, t.EmptyTree),
    t.EmptyTree), t.Node(7, t.EmptyTree, t.Node(2, t.EmptyTree, t.EmptyTree)))

  test("check tree contains an element") {
    assert(t.contains(tree, 2))
  }

  test("check tree not contains an element") {
    assert(!t.contains(tree, 9))
  }

  test("insert node to existing Tree") {
    val tree = t.Node(2, BinaryTreeService.Node(8,
      t.Node(4,t.EmptyTree,t.EmptyTree),t.EmptyTree),t.EmptyTree)
    assert(t.contains(tree,8))
  }

  test("insert node to Empty tree") {
    val treeNode = t.EmptyTree
    val res = t.insert(treeNode, "5".toInt)
    assert(t.contains(res,5))

  }

}
