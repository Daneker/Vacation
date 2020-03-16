package kz.btsd.intranet.vacation.service

import java.util.{Date, Properties}

import javax.mail.internet.{InternetAddress, MimeMessage}
import javax.mail.{Address, Session, Transport}
import kz.btsd.intranet.vacation.cfg

/** Объект для отправки письма */
object EmailService {

  type EmailSubject = String
  type EmailContent = String
  type EmailAddress = String

  private val username = cfg.getString("mail.smtp.username")
  private val password = cfg.getString("mail.smtp.password")
  private val enabled = cfg.getBoolean("mail.enabled")

  private val session = {
    val props = new Properties()
    props.put("mail.smtps.host", cfg.getString("mail.smtp.host"))
    props.put("mail.smtps.port", cfg.getString("mail.smtp.port"))
    props.put("mail.transport.protocol", "smtps")
    Session.getInstance(props)
  }


  /** Отправляет письмо.
    *
    * @param subject тема письма
    * @param text тело письма
    * @param toEmail получатель письма
    */
  def send(subject: EmailSubject, text: EmailContent, toEmail: Set[EmailAddress]) {
    if (enabled) {
      val msg = new MimeMessage(session)
      msg.setFrom(new InternetAddress(cfg.getString("mail.from.addr"), cfg.getString("mail.from.personal")))
      msg.setRecipients(javax.mail.Message.RecipientType.TO, toEmail.map(new InternetAddress(_)).toArray[Address])
      msg.setSubject(subject)
      msg.setSentDate(new Date())
      msg.setText(text)

      val transport = session.getTransport
      try {
        transport.connect(username, password)
        transport.sendMessage(msg, msg.getAllRecipients)
      } finally {
        transport.close()
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val set = Set("daniker.ktl@gmail.com")
    send("hello", "hello", set)
  }
}