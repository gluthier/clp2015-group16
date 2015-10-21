package toolc.utils

import java.io.{File, PrintWriter}

/**
 * Created by ire on 21/10/15.
 */
object StringToFile extends Pipeline[String, File] {
  def run(ctx: Context)(v: String): File = {
    val file = File.createTempFile("temp", ".tool")
    file.deleteOnExit()
    val writer = new PrintWriter(file)
    writer.write(v)
    writer.close()
    file
  }
}
