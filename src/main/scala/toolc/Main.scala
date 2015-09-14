package toolc

import utils._
import java.io.File

import lexer._
import ast._
import eval._

object Main {

  def processOptions(args: Array[String]): Context = {
    val (opts, files) = args.toSeq.partition(_.startsWith("--"))
    val reporter = new Reporter()

    if (files.size != 1) {
      reporter.fatal("Exactly one file expected, "+files.size+" file(s) given.")
    }

    Context(reporter = reporter, files = new File(files.head) :: Nil)
  }


  def main(args: Array[String]) {
    val ctx = processOptions(args)

    val program = new Parser().parse(ctx, ctx.files.head)

    val evaluator = new Evaluator(ctx, program)

    evaluator.eval()

  }
}
