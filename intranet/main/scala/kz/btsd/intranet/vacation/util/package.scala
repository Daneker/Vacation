package kz.btsd.intranet.vacation

package object util {

  implicit class StringHelpers(val reason: String) extends AnyVal {

    def trimToNone: Option[String] = {
      val res = reason.trim
      if(res.isEmpty) {
        None
      }
      Some(res)
    }

  }
}
