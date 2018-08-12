package controllers

import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import javax.inject.Inject
import scalikejdbc._
import models._

class UserController @Inject()(components: MessagesControllerComponents)
  extends MessagesAbstractController(components) {

  // コンパニオンオブジェクトに定義したFormを参照するためにimport文を追加
  import UserController._

  private val u = Users.syntax(("u"))
  private val c = Companies.syntax("c")

  /**
    * 一覧表示
    */
  def list = Action { implicit request =>
    DB.readOnly { implicit session =>
      // ユーザのリストを取得
      val users = withSQL {
        select.from(Users as u).orderBy(u.id.asc)
      }.map(Users(u.resultName)).list.apply()

      // 一覧画面を表示
      Ok(views.html.user.list(users))
    }
  }

  /**
    * 編集画面表示
    */
  def edit(id: Option[Long]) = Action { implicit request =>
    DB.readOnly { implicit session =>
      // リクエストパラメータにIDが存在する場合
      val form = id match {
        // IDが渡されなかった場合は新規登録フォーム
        case None => userForm
        // IDからユーザ情報を1件取得してフォームに詰める
        case Some(id) => Users.find(id) match {
          case Some(user) => userForm.fill(UserForm(Some(user.id), user.name, user.companyId))
          case None => userForm
        }
      }

      // プルダウンに表示する会社のリストを取得
      val companies = withSQL {
        select.from(Companies as c).orderBy(c.id.asc)
      }.map(Companies(c.resultName)).list().apply()

      Ok(views.html.user.edit(form, companies))
    }
  }

  /**
    * 登録実行
    */
  def create = TODO

  /**
    * 更新実行
    */
  def update = TODO

  /**
    * 削除実行
    */
  def remove(id: Long) = TODO

}

object UserController {
  // フォームの値を格納するケースクラス
  case class UserForm(id: Option[Long], name: String, companyId: Option[Int])

  // formから送信されたデータ ⇔ ケースクラスの変換を行う
  val userForm = Form(
    mapping(
      "id"        -> optional(longNumber),
      "name"      -> nonEmptyText(maxLength = 20),
      "companyId" -> optional(number)
    )(UserForm.apply)(UserForm.unapply)
  )
}