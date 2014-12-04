abstract class IntTree;
case object EmptyTree extends IntTree
case class Node(value: Int, left: IntTree, right: IntTree) extends IntTree


def contains(tree: IntTree, value: Int): Boolean = tree match {
  case EmptyTree => false
  case Node(v, _, _) if v == value => true
  case Node(_, l, r) => contains(l, value) || contains(r, value)
}

def insert(tree: IntTree, value: Int): IntTree = tree match {
  case EmptyTree => Node(value, EmptyTree, EmptyTree)
  case Node(v, l, r) if value < v => Node(v, insert(l, value), r)
  case Node(v, l, r) if value >= v => Node(v, l, insert(r, value))
}

def insertAll(tree: IntTree, values: scala.List[Int]): IntTree = {
  values match {
    case scala.List(head: Int) => insert(tree, head)
    case scala.List(head: Int, _*) => insert(insertAll(tree, values.tail), head)
  }
}

/*var tree: IntTree = EmptyTree
scala.List[Int](1,2,6,4,3).foreach( x => tree = insert(tree: IntTree, x))*/

val tree = insertAll(EmptyTree, scala.List(1,2,6,4,3))

println(s"(1,2,6,4,3) => tree is $tree")
println(s"(1,2,6,4,3) => contains 4 ${contains(tree, 4)}")
println(s"(1,2,6,4,3) => contains 7 ${contains(tree, 7)}")