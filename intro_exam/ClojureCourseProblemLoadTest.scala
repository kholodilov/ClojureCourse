// scala -J-Xss24M -J-Xms400M -J-Xmx400M ClojureCourseProblemLoadTest 10000000

import ClojureCourseProblem._

import math.random
import scala.collection.mutable.ListBuffer

object ClojureCourseProblemLoadTest
{

    def main(args: Array[String])
    {
        val startTS = System.currentTimeMillis()

        val treeStr = generateTreeStr(args(0).toInt)
        //println(treeStr.mkString(" "))
        println("treeStr len: " + treeStr.size)

        val genTime = System.currentTimeMillis() - startTS
        println("#genTime: " + genTime)

        val node = parseTree(treeStr)

        val parseTime = System.currentTimeMillis() - startTS - genTime
        println("#parseTime: " + parseTime)

        println("height: " + node.height)
        println("treeSum: " + node.treeSum)

        val traverseTime = System.currentTimeMillis() - startTS - genTime - parseTime
        println("#traverseTime: " + traverseTime)
    }

    def generateTreeStr(size: Int): Array[String] = {

        val result = new Array[String](size)
        result(0) = "["
        var openBrackets: Int = 0
        var pos: Int = 1
        var justOpened: Boolean = false

        while (pos + openBrackets <= size - 2)
        {
            val r = random
            if (r < 0.1 && pos + openBrackets + 2 <= size - 2)
            {
                result(pos) = "["
                openBrackets += 1
                justOpened = true
            }
            else if (r < 0.2 && openBrackets > 0 && !justOpened)
            {
                result(pos) = "]"
                openBrackets -= 1
                justOpened = false
            }
            else
            {
                result(pos) = "1"
                justOpened = false
            }
            pos += 1
        }
        (0 to openBrackets - 1) foreach { i => result(pos + i) = "]" }
        result(size - 1) = "]"
        
        result
    }
}