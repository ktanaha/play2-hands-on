package controllers

import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import javax.inject.Inject
import scalikejdbc._
import models._

class UserController @Inject()(components: MessagesControllerComponents)
  extends MessagesAbstractController(components) {

  import UserController._

  private val u = Users.syntax(("u"))
  private val c = Companies.syntax("c")

  /**
    * 一覧表示
    */
  def list = Action { implicit request =>
    DB.readOnly { implicit session =>
      val users = withSQL {
        select.from(Users as u).leftJoin(Companies as c).on(u.companyId, c.id).orderBy(u.id.asc)
      }.map { rs =>
        (Users(u)(rs), rs.intOpt(c.resultName.id).map(_ => Companies(c)(rs)))
      }.list.apply()

      Ok(views.html.user.list(users))
    }
  }

  /**
    * 編集画面表示
    */
  def edit(id: Option[Long]) = Action { implicit request =>
    DB.readOnly { implicit session =>
      val form = id match {
        case None => userForm
        case Some(id) => Users.find(id) match {
          case Some(user) => userForm.fill(UserForm(Some(user.id), user.name, user.companyId))
          case None => userForm
        }
      }

      val companies = withSQL {
        select.from(Companies as c).orderBy(c.id.asc)
      }.map(Companies(c.resultName)).list().apply()

      Ok(views.html.user.edit(form, companies))
    }
  }

  /**
    * 登録実行
    */
  def create = Action { implicit request =>
    DB.localTx { implicit session =>
      userForm.bindFromRequest.fold(
        error => {
          BadRequest(views.html.user.edit(error, Companies.findAll()))
        },
        form => {
          Users.create(form.name, form.companyId)
          Redirect(routes.UserController.list)
        }
      )
    }
  }

  /**
    * 更新実行
    */
  def update = Action { implicit request =>
    DB.localTx {implicit session =>
      userForm.bindFromRequest.fold(
        error => {
          BadRequest(views.html.user.edit(error, Companies.findAll()))
        },
        form => {
          Users.find(form.id.get).foreach { user =>
            Users.save(user.copy(name = form.name, companyId = form.companyId))
          }

          Redirect(routes.UserController. list)
        }
      )
    }
  }

  /**
    * 削除実行
    */
  def remove(id: Long) = Action { implicit request =>
    DB.localTx {implicit session =>
      Users.find(id).foreach { user =>
        Users.destroy(user)
      }

      Redirect(routes.UserController.list)
    }
  }

}

object UserController {
  case class UserForm(id: Option[Long], name: String, companyId: Option[Int])

  val userForm = Form(
    mapping(
      "id"        -> optional(longNumber),
      "name"      -> nonEmptyText(maxLength = 20),
      "companyId" -> optional(number)
    )(UserForm.apply)(UserForm.unapply)
  )
}