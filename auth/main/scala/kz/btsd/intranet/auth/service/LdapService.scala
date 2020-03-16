package kz.btsd.intranet.auth.service

import kz.btsd.intranet.auth.model.LdapUser
import kz.btsd.intranet.auth._
import java.util.{Hashtable => JHashtable}

import javax.naming.{AuthenticationException, Context}
import javax.naming.directory.InitialDirContext
import org.apache.logging.log4j.LogManager

import scala.util.Try

object LdapService {
  private val log = LogManager.getLogger

  private val providerUrl = {
    val ldapHost = cfg.getString("ldap.host")
    val ldapPort = cfg.getString("ldap.port")
    s"ldaps://$ldapHost:$ldapPort"
  }

  /**
    * аутентифицирует юзера через LDAP
    */
  def authenticate(uid: String, password: String): Try[LdapUser] = Try {
    val principal = s"uid=$uid,cn=users,cn=accounts,dc=btsdigital,dc=kz"

    val env = new JHashtable[String, String]()
    env.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory")
    env.put(Context.SECURITY_AUTHENTICATION, "simple")
    env.put(Context.SECURITY_PROTOCOL, "tls")

    env.put(Context.PROVIDER_URL, providerUrl)
    env.put(Context.SECURITY_PRINCIPAL, principal)
    env.put(Context.SECURITY_CREDENTIALS, password)

    var ctx: InitialDirContext = null
    try {
      ctx = new InitialDirContext(env)
      val attrs = ctx.getAttributes(principal)
      LdapUser(
        uid = uid,
        email = attrs.get("mail").get.asInstanceOf[String]
      )
    }
    catch {
      case e: AuthenticationException if e.getMessage == "[LDAP: error code 49 - Invalid Credentials]" => {
        throw new InvalidLdapCredentialsException
      }
    }
    finally {
      try {
        if(ctx != null) ctx.close()
      }
      catch {
        case t: Exception => log.warn("", t)
      }
    }
  }
}
