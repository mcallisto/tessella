package vision.id.tessella

import ch.qos.logback.classic.Level
import org.slf4j.LoggerFactory

trait Loggable {

  def setLogLevel(sArg: String): Unit =
    LoggerFactory
      .getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME)
      .asInstanceOf[ch.qos.logback.classic.Logger]
      .setLevel(Level.valueOf(sArg))

}
