package com.agynamix.garten.lib.field

import net.liftmodules.extras.SnippetHelper
import net.liftweb.record.TypedField
import net.liftweb.common._
import scala.xml.NodeSeq
import net.liftweb.record.field.StringField
import net.liftweb.record.Record
import net.liftweb.http.S
import net.liftweb.http.S.SFuncHolder
import net.liftweb.util.Helpers._

trait BsPasswordTypedField extends TypedField[String] with SnippetHelper {
  def maxLength: Int
  def minLength: Int

  /*
   * Call this after validation and before it is saved to the db to hash
   * the password. Eg. in the finish method of a screen.
   */
  def hashIt: Unit = valueBox foreach { v =>
    setBox(PasswordField.hashpw(v))
  }

  def isMatch(toTest: String): Boolean = valueBox
    .map(p => PasswordField.isMatch(toTest, p))
    .openOr(false)

  def isAutoFocus: Boolean = false

  private def elem = {
    val fieldId: String = uniqueFieldId.openOr(randomString(12))

    S.fmapFunc(SFuncHolder(this.setFromAny(_))) {
      funcName =>
      <input type="password" maxlength={maxLength.toString}
        id={fieldId}
        name={funcName}
        value={valueBox openOr ""}
        tabindex={tabIndex.toString}
        class="form-control" /> % autofocus(isAutoFocus)
    }
  }

  override def toForm: Box[NodeSeq] = Full(elem)

}

class BsPasswordField[OwnerType <: Record[OwnerType]](
  rec: OwnerType, val minLength: Int, maxLength: Int
)
extends StringField[OwnerType](rec, maxLength) with BsPasswordTypedField {

	/*
	 * Use this when creating users programatically. It allows chaining:
	 * User.createRecord.email("test@domain.com").password("pass1", true).save
	 * This will automatically hash the plain text password if isPlain is
	 * true.
	 */
	def apply(in: String, isPlain: Boolean): OwnerType = {
    val hashed =
      if (isPlain)
        PasswordField.hashpw(in) openOr ""
      else
        in
    if (owner.meta.mutable_?) {
      this.setBox(Full(hashed))
      owner
    } else {
      owner.meta.createWithMutableField(owner, this, Full(hashed))
    }
  }
}
