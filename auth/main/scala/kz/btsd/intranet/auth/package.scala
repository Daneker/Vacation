package kz.btsd.intranet

import com.typesafe.config.ConfigFactory

package object auth {
  val cfg = ConfigFactory.load()
}
