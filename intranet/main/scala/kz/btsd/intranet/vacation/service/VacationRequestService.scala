package kz.btsd.intranet.vacation.service
import java.time.LocalDate

import kz.btsd.intranet.vacation.cfg
import kz.btsd.intranet.vacation.dao.{Employees, InvalidStatusTransitionException, OverlappingDatesException, Users, VacationRequestNotFoundException, VacationRequests}
import kz.btsd.intranet.vacation.model.{Employee, User, VacationRequest, VacationRequestStatus, VacationType}
import kz.btsd.intranet.vacation.service.EmailService.{EmailContent, EmailSubject}
import kz.btsd.intranet.vacation.util._
import org.apache.logging.log4j.LogManager

import scala.util.{Failure, Success, Try}

object VacationRequestService {
  private val log = LogManager.getLogger

  /** Составляет тему и контент письма для дальнейшей отправки.
    *
    * @param employee сотрудник, который идет в отпуск
    * @param vr заявка текущего пользователя для отправки
    */
  private def composeEmailText(employee: Employee, vr: VacationRequest): (EmailSubject, EmailContent) = {
    val subject = {
      vr.status match {
        //  Новая заявка на отпуск от Daneker Nurgaliyeva
        case VacationRequestStatus.New => s"Новая заявка на отпуск от ${employee.name}"
        // "Заявка на отпуск Daneker Nurgaliyeva: ACCEPTED"
        case VacationRequestStatus.Accepted => s"Заявка на отпуск ${employee.name}: ОДОБРЕНА"
        case VacationRequestStatus.Declined => s"Заявка на отпуск ${employee.name}: ОТКЛОНЕНА"
      }
    }

    val content = {
      val reqInfo = s"\nИнформация о заявке:\n  Имя: ${employee.name}\n Электронная почта: ${employee.email}\n  Отдел: ${employee.department}\n Должность: ${employee.position}\n Период: ${vr.startDate} - ${vr.endDate}"
      vr.status match {
        // $Имя Фамилия подал(а) заявку на отпуск на период $startDate - $endDate.
        case VacationRequestStatus.New => s"Сотрудник ${employee.name} подал заявку на отпуск.\n"+reqInfo
        // Заявка на отпуск от $Name Surname на период $startDate - $endDate одобрена.
        case VacationRequestStatus.Accepted => s"Заявка на отпуск ${employee.name} была одобрена."+reqInfo+"\n  Статус: Одобрена"
        // Заявка на отпуск от $Name Surname на период $startDate - $endDate отклонена. Причина отклонения: $declineReason.
        case VacationRequestStatus.Declined => s"Заявка на отпуск ${employee.name} была отклонена."+reqInfo+s"\n  Статус: Отклонена\n Причина отклонения: ${vr.declineReason.getOrElse("Не пояснено.")}."
      }
    }
    (subject, content)
  }

  /** Составляет тему и контент письма для отправки уведомления в бухгалтерию.
    *
    * @param employee сотрудник, который идет в отпуск
    * @param vr заявка текущего пользователя для отправки
    */
  private def composeDeleteNotificationText(employee: Employee, vr: VacationRequest): (EmailSubject, EmailContent) = {
    val subject = {
      vr.status match {
        case VacationRequestStatus.Vacation => s"Отпуск на имя ${employee.name} был удален"
        case _ => s"Заявка на отпуск от ${employee.name} была удалена"
      }
    }

    val content = {
      val reqInfo = s"\nИнформация о заявке:\n  Имя: ${employee.name}\n Электронная почта: ${employee.email}\n  Отдел: ${employee.department}\n Должность: ${employee.position}\n Период: ${vr.startDate} - ${vr.endDate}"
      vr.status match {
        case VacationRequestStatus.Vacation => s"Отпуск на имя ${employee.name} был удален.\n"+reqInfo
        case _ => s"Заявка на отпуск от ${employee.name} была удалена.\n"+reqInfo
      }
    }
    (subject, content)
  }

  /** Cоздает заявку. При успешном создании возвращает заявку и отправляет письмо пользователю, тимлиду и HR.
    * @throws OverlappingDatesException, если заявка уже существует в подобном периоде.
    *
    * @param actor текущий пользователь
    * @param vtype вид отпуска
    * @param startD дата началы отпуска
    * @param endD дата окончания отпуска
    * @param substitute замена пользователя во время отпуска
    */
  def create(actor: User, vtype: VacationType.`type`, startD: LocalDate, endD: LocalDate, substitute: String): Try[VacationRequest] = {
    for {
      newReq <- VacationRequests.create(actor.id, vtype, startD, endD, substitute)
    } yield {
      val (subj, content) = composeEmailText(actor.employee, newReq)
      EmailService.send(subj, content, actor.employee.vacationObservers)
      newReq
    }
  }

  /** Обновляет статус заявки на declined.
    * Отправляет письмо пользователю и копию тимлидам о статусе заявки, если статус успешно обновляется.
    * @throws InvalidStatusTransitionException, если заявка не New
    * @throws VacationRequestNotFoundException, нет заявки с таким id.
    *
    * @param id id заявки
    * @param declineReason причина отклонения заявки
    */
  def decline(id: Long, declineReason: String): Try[VacationRequest] = {
    declineReason.trimToNone match {
      case None => Failure(new IllegalArgumentException)
      case Some(dr) => {
        VacationRequests.decline(id, dr).map { vr =>
          val employee = Users.getUserById(vr.userId).get.employee
          val email = composeEmailText(employee, vr)
          EmailService.send(email._1, email._2, employee.vacationObservers)
          vr
        }
      }
    }
  }


  /** Обновляет статус нескольких заявок на accepted.
    * Возвращает мап необновленных заявок с ошибкой.
    * Отправляет письмо пользователю и копию тимлидам о статусе заявки, если статус успешно обновляется.
    *
    * @param ids список id заявок
    */
  def accept(ids: Set[Long]): Map[Long,Throwable] = {
    ids.map(id => id -> VacationRequests.accept(id)).filter {
        case (_, Success(req)) => {
          val employee = Users.getUserById(req.userId).get.employee
          val email = composeEmailText(employee, req)
          EmailService.send(email._1, email._2, employee.vacationObservers)
          false
        }
        case _ => true
    }.map(t => t._1 -> t._2.failed.get).toMap
  }


  /** Удаляет заявку.
    * @param id id заявки
    */
  def delete(id: Long): Option[VacationRequest] = {
      if (VacationRequests.getRequestById(id).get.status == VacationRequestStatus.Vacation) throw new UnsupportedOperationException
      val res = VacationRequests.delete(id)
      res match {
        case Some(req) if req.status == VacationRequestStatus.Sent => {
      val employee = Users.getUserById (req.userId).get.employee
      val email = composeDeleteNotificationText (employee, req)
      EmailService.send (email._1, email._2, Set (cfg.getString ("accountant.email")))
        }
        case _ =>
      }
      res
  }

  /** Достает cписок заявок для hr по статусам для заданной страницы с заданным количеством заявок.
    * Если заявок нет, возвращает пустой список.
    *
    * @param status статус заявки
    * @param pageNumber номер страницы
    * @param numberOfRows количество строк в странице
    */
  def getVacationRequestsByStatus(status: VacationRequestStatus.status, pageNumber: Int, numberOfRows: Int): List[VacationRequest] = {
    VacationRequests.getVacationRequestsByStatus(status, pageNumber, numberOfRows)
  }

  /** Достает актуальные заявки пользователя по его id.
    * Если заявок нет, возвращает пустой список.
    *
    * @param id id заявки
    */
  def getActualVacationRequestsByUserId(id: Long): List[VacationRequest] = {
    VacationRequests.getActualVacationRequestsByUserId(id)
  }

}





