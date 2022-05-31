package edu.knoldus

case class Tree(data: Int, children: Option[List[Tree]])


class TreeNodeCount {
  def countNodes(tree: Tree): Int = {
    tree.children match {
      case Some(child) => child.foldLeft(1)((x, y) => x + countNodes(y))
      case None => 1
    }
  }

  def countNodes2(tree: Tree, children: Int): Int = {
    def countNodesWithChildrenGreaterSpecificNumber(tree: Tree, noOfChildren: Int): Option[Int] = {
      if (tree.children.getOrElse(List.empty[Int]).size > noOfChildren) {
        tree.children.map { child =>
          child.foldLeft(1)((x, y) => x + countNodesWithChildrenGreaterSpecificNumber(y, noOfChildren).getOrElse(0))
        }
      }
      else {
        tree.children map { child =>
          child.foldLeft(0)((x, y) => x + countNodesWithChildrenGreaterSpecificNumber(y, noOfChildren).getOrElse(0))
        }
      }
    }

    val count = countNodesWithChildrenGreaterSpecificNumber(tree, children)
    count.getOrElse(0)
  }
}


object Driver extends App {
  val treeCount = new TreeNodeCount

  val treeOne = Tree(3,Some(List(Tree(9,None),Tree(7,None),Tree(6,None))))

  val tree2 = Tree(5,Some(List(treeOne,treeOne,Tree(8,Some(List(treeOne,treeOne))))))
  println(treeCount.countNodes(tree2))
}