import java.util.{Timer, TimerTask}
import scala.concurrent.duration.{Duration, DurationInt, FiniteDuration}
import scala.language.postfixOps
import scala.util.Try

object Python {

  val cmd: String = Try(Runtime.getRuntime.exec("python3"))
    .orElse(Try(Runtime.getRuntime.exec("python")))
    .map{ process =>
      val failureMsg = "Could not procure path to python/python3. Ensure necessary dependencies is installed."
      val name = process.info().command().orElseThrow(() => new UnsupportedOperationException(failureMsg))
      process.destroyForcibly()
      name
    }.getOrElse(throw new UnsupportedOperationException("Python not detected in environment."))

  def execute(code: String, timeout: Duration = 10.minutes, block: Boolean = false): Process = {
    val process = new ProcessBuilder(cmd, "-c", code).inheritIO().start()
    timeout match {
      case infinite: Duration.Infinite if block => process.waitFor()
      case infinite: Duration.Infinite =>
      case duration: FiniteDuration if block => process.waitFor(duration.length, duration.unit)
      case duration: FiniteDuration =>
        val timer = new Timer()
        timer.schedule(new TimerTask { override def run(): Unit = process.destroyForcibly() }, duration.toMillis)
    }
    process
  }
}
