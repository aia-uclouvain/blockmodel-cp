package blockmodel.utils

import java.io.{File, FileOutputStream, IOException, PrintWriter}

/**
  * save text to a file
  * Created by vbranders on 17/02/17.
  *
  */
class FileReporter(val str: String, val path: String, val append: Boolean) {
  def this(str: String, path: String) = this(str, path, false)

  try {
    println("opening file "+path)
    val f: File = new File(path)
    //f.getParentFile.mkdirs()
    val writer = new PrintWriter(new FileOutputStream(f, append))
    writer.write(str)
    if(!str.endsWith("\n") && str.length > 0){
      writer.write("\n")
    }
    writer.close()
    println("Saved to \""+f.getParentFile+"\":\t"+f.getName+"")
  } catch {
    case ex: IOException => ex.printStackTrace()
  }

}
