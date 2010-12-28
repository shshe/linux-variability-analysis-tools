package gsd.linux

import java.util.Scanner
import collection.mutable.ListBuffer
import java.io.File

object DescriptionReader {

  def readDescriptionsFile(file: String): Map[String, String] =
    readDescriptions(new Scanner(new File(file)))

  def readDescriptions(in: Scanner): Map[String, String] = {
    val result = new ListBuffer[(String,String)]

    while (in.hasNextLine) {
      val name = in.nextLine
      assert(in.nextLine == "{{{")

      val desc = new StringBuilder
      var next = in.nextLine
      while (next != "}}}") {
        desc append next append "\n"
        next = in.nextLine
      }

      result += name -> desc.toString
    }

    result.toMap
  }

  def main(args: Array[String]) {
    readDescriptionsFile(args(0)) foreach {
      case (k,v) => println("%s: %s".format(k,v))
    }
  }

}