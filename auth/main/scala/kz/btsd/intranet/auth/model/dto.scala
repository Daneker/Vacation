package kz.btsd.intranet.auth.model

/**
  * данные о пользователе, зарегистрированном в LDAP
  */
case class LdapUser(uid: String, email: String)

