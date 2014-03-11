import ClojureCourseProblem._

import scala.util._

object ClojureCourseProblemTest
{

    def main(args: Array[String])
    {
        var node: Node = parseTree(Array("[", "]"))
        assert(node == Node(), node)
        assert(node.treeSum == 0)

        node = parseTree(Array("[", "1", "2", "]"))
        assert(node == Node(1, 2), node)
        assert(node.treeSum == 3)

        node = parseTree(Array("[", "1", "[", "]", "2", "]"))
        assert(node == Node(Node(), List(1, 2)), node)
        assert(node.treeSum == 3)

        node = parseTree(Array("[", "1", "[", "3", "]", "2", "]"))
        assert(node == Node(Node(3), List(1, 2)), node)
        assert(node.treeSum == 6)

        node = parseTree(Array("[", "1", "[", "2", "3", "]", "4", "[", "5", "[", "6", "7", "]", "]", "[", "8", "]", "]"))
        assert(node == Node(List(Node(2, 3), Node(Node(6, 7), List(5)), Node(8)), List(1, 4)), node)
        assert(node.treeSum == 36)

        var tryNode = Try(parseTree(Array("[", "4", "[", "]")))
        assert(tryNode.failed.get.isInstanceOf[MissingCloseBracketException], tryNode)

        tryNode = Try(parseTree(Array("[", "4", "]", "]")))
        assert(tryNode.failed.get.isInstanceOf[RedundantCloseBracketException], tryNode)

        println("All assertions succeeded")
    }

}