import scala.util._
import scala.collection.mutable.Stack

object ClojureCourseProblem
{

    case class Node(val childList: List[Node] = List(), val numberList: List[Int] = List())
    {
        def this(node: Node, child: Node) = this(node.childList ::: List(child), node.numberList)
        def this(node: Node, number: Int) = this(node.childList, node.numberList ::: List(number))

        def treeSum: Int = numberList.sum + childList.map { _.treeSum }.sum
        def height: Int = {
            val childHeight: Int = childList match {
                case Nil => 0
                case _ => childList.map { _.height }.max
            }
            childHeight + 1
        }
    }

    object Node
    {
        def apply(child: Node, numberList: List[Int]): Node = Node(List(child), numberList)
        def apply(numberSeq: Int*): Node = Node(List(), numberSeq.toList)
    }

    class MissingCloseBracketException extends RuntimeException
    class RedundantCloseBracketException extends RuntimeException

    def parseTree(treeStr: Array[String]): Node = {

        var currentNode = Node()
        val nodesStack = new Stack[Node]

        treeStr foreach { a: String => 
            a match {
                case "[" => {
                    nodesStack.push(currentNode)
                    currentNode = Node()
                }
                case "]" => {
                    if (!nodesStack.isEmpty)
                    {
                        val parentNode = nodesStack.pop
                        currentNode = new Node(parentNode, currentNode)
                    }
                    else
                    {
                        throw new RedundantCloseBracketException()
                    }
                }
                case _ => {
                    currentNode = new Node(currentNode, a.toInt)
                }
            }
        }

        if (nodesStack.isEmpty)
        {
            currentNode.childList.head
        }
        else
        {
            throw new MissingCloseBracketException()
        }
        
    }

    def main(args: Array[String])
    {
        val node = parseTree(Array("[", "1", "[", "2", "3", "]", "4", "[", "5", "[", "6", "7", "]", "]", "[", "8", "]", "]"))
        println(node.treeSum)
    }
}