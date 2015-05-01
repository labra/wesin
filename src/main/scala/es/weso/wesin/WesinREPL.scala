package es.weso.wesin;
import scala.tools.nsc.Settings
import scala.tools.nsc.interpreter.ILoop

/**
 * REPL for Wesin
 *  TODO: this does not work...finnish it
 */

object TestConsole extends App {
  val settings = new Settings
  //  settings.usejavacp.value = true
  settings.deprecation.value = true

  new SampleILoop().process(settings)
}

class SampleILoop extends ILoop {
  override def prompt = "==> "

  override def printWelcome() {
    echo("\n" + " Wellcome to a simple REPL ")
  }

}