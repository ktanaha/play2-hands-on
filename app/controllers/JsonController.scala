package controllers

import play.api.mvc._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import javax.inject.Inject
import scalikejdbc._
import models._

object JsonController {
  implicit val usersWrites = (
    (__ \ "id"       ).write[Long]   and
      (__ \ "name"     ).write[String] and
      (__ \ "companyId").writeNullable[Int]
    )(unlift(Users.unapply))
}

class JsonController @Inject()(compnents: ControllerComponents)
  extends AbstractController(compnents) {

  import JsonController._

  /**
    * 一覧表示
    */
  def list = Action { implicit request =>
    val u = Users.syntax("u")

    DB.readOnly { implicit session =>
      val users = withSQL {
        select.from(Users as u).orderBy(u.id.asc)
      }.map(Users(u.resultName)).list.apply()

      Ok(Json.obj("users" -> users))
    }
  }

  /**
    * ユーザ登録
    */
  def create = TODO

  /**
    * ユーザ更新
    */
  def update = TODO

  /**
    * ユーザ削除
    */
  def remove(id: Long) = TODO
}