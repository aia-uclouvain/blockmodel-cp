package blockmodel.utils

import scala.collection.mutable.ArrayBuffer

/**
  * create a json structure
  * Created by vbranders on 18/02/17.
  *
  */
class JsonBuilder {
  var listSep = Array("[", "]", ", ")
  var textSep = "\""
  var tabSep = "\t"
  var valSep = ": "
  var contentSep = ",\n"
  var content: ArrayBuffer[String] = new ArrayBuffer[String]()
  var indent = 1


  def insert(key: String, value: String): Unit = {
    content += (textSep+key+textSep+valSep+value)
  }
  def add(key: String, value: String): Unit = insert(key, textSep+value.replaceAll("\n", "\\n")+textSep)
  def add(key: String, value: Int): Unit = insert(key, value.toString)
  def add(key: String, value: Float): Unit = insert(key, value.toString)
  def add(key: String, table: Array[Int]): Unit = insert(key, listSep(0)+table.mkString(listSep(2))+listSep(1))
  def add(key: String, table: Array[Float]): Unit = insert(key, listSep(0)+table.mkString(listSep(2))+listSep(1))
  def add(key: String, table: Array[Long]): Unit = insert(key, listSep(0)+table.mkString(listSep(2))+listSep(1))
  def add(key: String, table: Array[String]): Unit = insert(key, listSep(0)+textSep+table.mkString(textSep+listSep(2)+textSep)+textSep+listSep(1))
  def add(key: String, value: JsonBuilder): Unit = {
    val els = value.toString(indent+1).split("\n")
    insert(key, els.mkString(if(els.length > 3) "\n" else " "))
  }


//  def add(name: String, value: String): Unit = add(name, value, indent)
//  def add(name: String, value: String, addSep: Boolean): Unit = add(name, value, indent, addSep)
//  def add(name: String, value: String, indent: Int): Unit = add(name, value, indent, true)
//  def add(name: String, value: String, indent: Int, addSep: Boolean): Unit = {
//    content = content ++ Array(textSep+name+textSep+valSep+{if(addSep){textSep}else{""}}+value+{if(addSep){textSep}else{""}})
//  }
//  def add(name: String, value: Int): Unit = add(name, value, indent)
//  def add(name: String, value: Int, indent: Int): Unit = {
//    content = content ++ Array(textSep+name+textSep+valSep+value)
//  }
//  def add(name: String, value: Float): Unit = add(name, value, indent)
//  def add(name: String, value: Float, indent: Int): Unit = {
//    content = content ++ Array(textSep+name+textSep+valSep+value)
//  }
//  def add(name: String, table: Array[Int]): Unit = add(name, table, indent)
//  def add(name: String, table: Array[Int], indent: Int): Unit = {
//    content = content ++ Array(textSep+name+textSep+valSep+listSep(0)+table.mkString(listSep(2))+listSep(1))
//  }
//  def add(name: String, table: Array[Float]): Unit = add(name, table, indent)
//  def add(name: String, table: Array[Float], indent: Int): Unit = {
//    content = content ++ Array(textSep+name+textSep+valSep+listSep(0)+table.mkString(listSep(2))+listSep(1))
//  }
//  def add(name: String, table: Array[String]): Unit = add(name, table, indent)
//  def add(name: String, table: Array[String], indent: Int): Unit = {
//    content = content ++ Array(textSep+name+textSep+valSep+listSep(0)+textSep+table.mkString(textSep+listSep(2)+textSep)+textSep+listSep(1))
//  }
//  def add(name: String, table: Array[Long]): Unit = add(name, table, indent)
//  def add(name: String, table: Array[Long], indent: Int): Unit = {
//    content = content ++ Array(textSep+name+textSep+valSep+listSep(0)+table.mkString(listSep(2))+listSep(1))
//  }
//  def add(name: String, value: utils.JsonBuilder): Unit = add(name, value, indent)
//  def add(name: String, value: utils.JsonBuilder, indent: Int): Unit = {
//    val els = value.toString(indent+1).split("\n")
//    content = content ++ Array(textSep+name+textSep+valSep+(els.mkString(if(els.length > 3) "\n" else " ")))
//  }
  def tab(indent: Int): String = {
    tabSep*indent
  }
  def toString(ind: Int): String = {
    indent = ind
    "{\n" +
      content.map(tab(indent)+_).mkString(contentSep) +
      "\n"+tab(indent-1)+"}"
  }
  override def toString(): String = {
    toString(indent)
  }
}
