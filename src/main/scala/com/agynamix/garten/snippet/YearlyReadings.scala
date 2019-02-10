package com.agynamix.garten.snippet

import net.liftweb.util.Helpers
import net.liftweb.util.Helpers._
import net.liftweb.common._
import com.agynamix.garten.model.{GeneratedDocument, Garden, Membership, User}
import com.agynamix.garten.lib.util.{SnippetHelpers}
import net.liftweb.http.SHtml._
import net.liftweb.http.{StatefulSnippet, S}
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.jquery.JqJsCmds.{FadeOut, Hide, Show, FadeIn}
import scala.xml.NodeSeq
import net.liftweb.http.js.JsCmd
import net.liftweb.mongodb.record.{BsonMetaRecord, BsonRecord}
import net.liftweb.mongodb.record.field.DateField
import com.agynamix.garten.lib.field.{BsBooleanField, GermanDateField, DateFormAdapterField}
import java.util.Date
import org.bson.types.ObjectId
import com.agynamix.garten.lib.util.XForm
import net.liftweb.builtin.snippet.Form
import net.liftweb.http.LiftRules
import net.liftweb.http.DispatchSnippet

class YearlyReadingSearchGardens extends BsonRecord[YearlyReadingSearchGardens] {
  def meta = YearlyReadingSearchGardens

  object readingDate extends DateField(this) with DateFormAdapterField with GermanDateField {
    override def defaultValue = new Date()
  }

  object listAllGardens extends BsBooleanField(this) {
    override def defaultValue = false
  }

}

object YearlyReadingSearchGardens extends YearlyReadingSearchGardens
                                  with BsonMetaRecord[YearlyReadingSearchGardens]
                                  with Loggable {
}


class YearlyReadings extends DispatchSnippet with SnippetHelpers {

  def dispatch: DispatchIt = {
    case "searchGardens" => searchGardens
  }

  val readingRecord = YearlyReadingSearchGardens.createRecord

  lazy val formTpl = actionsPartTpl("yearly_readings_form")

  def currentReading(electricityReading: NodeSeq, waterReading: NodeSeq): NodeSeq = {
    <span>, Aktuelle Werte: {electricityReading}, {waterReading}</span>
  }

  def currentValueReading(name: String, date: String, value: String): NodeSeq = {
    <span>{name} ({date}): {value}</span>
  }

  def buildReadingsForm(garden: Garden): NodeSeq = {

    var electricityReadingStr = ""
    var waterReadingStr = ""

    val readingFormId  = nextFuncName
    val readingSummary = nextFuncName
    val myFormId       = nextFuncName
    
    val curElectricity = garden.electricityMeterReading.firstNonEmptyOpt
    val curWater       = garden.waterMeterReading.firstNonEmptyOpt    

    def processReadingForm(): JsCmd = {
            
      if ((electricityReadingStr.trim.isEmpty() || asLong(electricityReadingStr.trim).isEmpty) &&
          (waterReadingStr.trim.isEmpty() || asLong(waterReadingStr.trim).isEmpty)) {
        S.error("Bitte geben Sie Zahlenwerte an."); Noop
      } else {
        var valuesOk = true
        val (curElValue, curElDate, isNewElValue) = (for (e <- asLong(electricityReadingStr.trim)) yield {
          val cur = curElectricity.flatMap(_.value.get).getOrElse(0L)
          if (e < cur) {
            S.error("Wert für Stromzähler zu klein");
        	  valuesOk = false
        	  (cur, curElectricity.map(_.date.asText).getOrElse(""), false)
          } else {
	          val elValue = garden.electricityMeterReading.emptyHeadValue
	          elValue.date(readingRecord.readingDate.get)
	          elValue.value(e)
	          (e, readingRecord.readingDate.asText, true)
          }
        }) openOr {
          (curElectricity.flatMap(_.value.get).getOrElse(0L), curElectricity.map(_.date.asText).getOrElse(""), false)
        }
        val (curWaValue, curWaDate, isNewWaValue) = (for (w <- asLong(waterReadingStr.trim)) yield {
          val cur = curWater.flatMap(_.value.get).getOrElse(0L)
          if (w < cur) {
            S.error("Wert für Wasserzähler zu klein");
        	  valuesOk = false
        	  (cur, curWater.map(_.date.asText).getOrElse(""), false)
          } else {
	          val wValue = garden.waterMeterReading.emptyHeadValue
	          wValue.date(readingRecord.readingDate.get)
	          wValue.value(w)
	          (w, readingRecord.readingDate.asText, true)
          }
        }) openOr {
          (curWater.flatMap(_.value.get).getOrElse(0L), curWater.map(_.date.asText).getOrElse(""), false)
        }
        if (valuesOk) {
         val curReading = currentReading(currentValueReading("Elektro", curElDate, curElValue.toString),
           currentValueReading("Wasser", curWaDate, curWaValue.toString))

           garden.save(true)

           new FadeOut(readingFormId, 0, 500) &
	         SetHtml(readingSummary, curReading)
        }
      }

    }
    
    val curReading = currentReading(
      curElectricity.map(v => currentValueReading("Elektro", v.date.asText, v.value.asHtml.toString)).getOrElse(<span>Elektro: leer</span>),
      curWater.map(v => currentValueReading("Wasser", v.date.asText, v.value.asHtml.toString)).getOrElse(<span>Elektro: leer</span>))

    val currentElIsThisDateOrYounger = curElectricity.map(el => {
      (el.date.get == readingRecord.readingDate.get) || (el.date.get.after(readingRecord.readingDate.get))
    }) getOrElse false
          
    val currentWaIsThisDateOrYounger = curWater.map(el => {
      (el.date.get == readingRecord.readingDate.get) || (el.date.get.after(readingRecord.readingDate.get))
    }) getOrElse false
    
    val css =
      "form [id]" #> myFormId &
      "@s-reading-summary [id]" #> readingSummary &
      "@s-reading-summary *" #> curReading &
      "@f-reading-form [id]" #> readingFormId &
      "@garden_no" #> garden.garden_no.asHtml &
      "@garden_owners" #> garden.gardenOwners.asHtml &
      (if (!currentElIsThisDateOrYounger) {
    	  "@i-meter-electricity" #> text(electricityReadingStr, electricityReadingStr = _)        
      } else {
    	  "@form-group-electricity" #> ""
      }) &
      (if (!currentWaIsThisDateOrYounger) {
        "@i-meter-water" #> text(waterReadingStr, waterReadingStr = _)
      } else {
        "@form-group-water" #> ""
      }) &
      (if (currentElIsThisDateOrYounger && currentWaIsThisDateOrYounger) {
        "@f-reading-form" #> ""
      } else {
        notExistent
      }) &
      "@b-submit [onclick]" #> makeAjaxCall(LiftRules.jsArtifacts.serialize(myFormId)).cmd.toJsCmd &
      "@submit-helper" #> hidden(processReadingForm)

    css(formTpl)
  }

  def buildReadingsSummary(): NodeSeq = {
    <p><strong>Ablesedatum:</strong> {readingRecord.readingDate.asHtml}</p>      
  }
  
  def searchGardens = {

    def process(clientId: ObjectId): JsCmd = {
      val gardens = Garden.findAllGardens(clientId)

      val header: NodeSeq = <h1>Gefundene Gärten: {gardens.size}</h1>

      val readingsSummary: NodeSeq = buildReadingsSummary()
      val forms: NodeSeq = gardens.foldLeft(header){(seq, garden) => seq ++ buildReadingsForm(garden) }
      Hide("f-yearly-readings-settings") &
      SetHtml("d-yearly-readings-summary", readingsSummary) &
      SetHtml("d-yearly-readings-gardens", forms)
    }

    (for {
      user <- User.currentUser
      clientId <- user.activeMembership.clientId
    } yield {
      "#i-readingDate" #> readingRecord.readingDate.toForm &
      "#i-list-all-gardens" #> readingRecord.listAllGardens.toForm &
      "@submit-helper" #> hidden(()=>process(clientId))
    }) openOr {
      "#f-yearly-readings-settings" #> ""
    }
  }

}
