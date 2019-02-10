package com.agynamix.document

import java.io.File
import org.odftoolkit.simple.TextDocument
import java.net.URI
import org.odftoolkit.simple.common.field.Fields
import org.odftoolkit.simple.common.navigation.TextNavigation
import org.odftoolkit.simple.common.navigation.TextSelection
import org.odftoolkit.odfdom.pkg.OdfElement
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import org.odftoolkit.odfdom.dom.element.table.TableTableRowElement
import org.odftoolkit.simple.table.Row
import scala.collection.JavaConverters._
import org.odftoolkit.odfdom.doc.table.OdfTableRow
import org.odftoolkit.odfdom.incubator.doc.text.OdfTextParagraph
import org.odftoolkit.odfdom.incubator.doc.text.OdfTextHeading
import org.odftoolkit.odfdom.dom.element.text.TextParagraphElementBase
import net.liftweb.common._
import net.liftweb.util.Helpers._
import org.odftoolkit.simple.draw.Image
import org.odftoolkit.simple.text.Paragraph
import org.odftoolkit.simple.common.TextExtractor
import org.odftoolkit.simple.common.navigation.ImageSelection
import java.io.FileOutputStream
import org.odftoolkit.odfdom.dom.element.text.TextUserFieldDeclElement
import org.odftoolkit.odfdom.dom.element.OdfStyleBase
import org.odftoolkit.odfdom.pkg.OdfName
import org.odftoolkit.odfdom.incubator.doc.style.OdfStyle
import org.odftoolkit.odfdom.dom.style.props.OdfTextProperties
import org.odftoolkit.odfdom.dom.style.OdfStyleFamily
import com.agynamix.garten.lib.DataSourcePropertyType
import com.agynamix.garten.lib.DataSourceProperty
import com.agynamix.garten.lib.DataSource
import com.agynamix.garten.config.GardenConfig

object DocumentGenerator {

  val elementMappings = Map("row" -> "table:table-row")

  val delimLeft = "<%"
  val delimRight = "%>"
  val commandDelim = ","
  val argumentDelim = "="
  val commandPrefix = "@"

  val imgSubkey = "img:"
  val repeaterVariablePrefix = "_"

  def apply(dataProvider: DocumentDataProvider) = new DocumentGenerator(dataProvider)

  def stripDelimiters(in: String): String = in.replaceFirst(delimLeft, "").replaceFirst(delimRight, "")

  def substituteMarkerValue(doc: TextDocument, marker: TextSelection, repeaterDP: DataProvider)(implicit dataProvider: DataProvider): Unit = {
    //println("Found Marker: "+marker.toString())

    findReplacementValue(stripDelimiters(marker.getText()), repeaterDP) match {
      case DataSourceProperty(_, Full(value), DataSourcePropertyType.Text) =>
        //marker.replaceWith("")
        tryo{
          val span = marker.createSpanElement()
          //span.setStyleName("Style_Table_Body_Content")
          span.newTextNode(value)
          marker.cut()
        } openOr {
          tryo{marker.replaceWith(value)}
        }

      case DataSourceProperty(id, Full(value), DataSourcePropertyType.Image) =>
        val keyedUri = new URI(value + "/" + GardenConfig.fileDownloadApiKey)
        //println(s"Replace IMG marker ${id} with resource ${keyedUri}")
        tryo {
          val imgSelection = new ImageSelection(marker)
          imgSelection.replaceWithImage(keyedUri)
        } openOr {
          tryo {marker.replaceWith("")}
        }
      case DataSourceProperty(_, _, _) => marker.replaceWith("")
    }

  }

  def findReplacementValue(rawKey: String, repeaterDP: DataProvider)(implicit dataProvider: DataProvider): DataSourceProperty = {
    rawKey match {
      case key if key.startsWith(imgSubkey) =>
        val subKey = key.substring(imgSubkey.length())
        val dp = if (subKey.startsWith("#")) repeaterDP else dataProvider
        dp.getDisplayProperty(subKey) match {
          case Full(value) if value.trim().nonEmpty =>
            println("substitute Image %s with %s".format(subKey, value));
            DataSourceProperty(rawKey, Full(value), DataSourcePropertyType.Image)
          case _           => println(s"not found: ${subKey}.")
                              DataSourceProperty(rawKey, Empty, DataSourcePropertyType.Image)
        }
      case key =>
        val dp = if (key.startsWith("#")) repeaterDP else dataProvider
        val subKey = if (key.startsWith("#")) key.substring(1) else key
        dp.getDisplayProperty(subKey) match {
          case Full(value) =>
            //println("substitute %s with %s".format(subKey, value));
            DataSourceProperty(rawKey, Full(value), DataSourcePropertyType.Text)
          case _           => println(s"not found: ${subKey}.")
                              DataSourceProperty(rawKey, Empty, DataSourcePropertyType.Text)
        }
    }
  }

}

class DocumentGenerator(val dataProvider: DocumentDataProvider) {

  val tplfile = dataProvider.documentTemplate.inputStream
  val output  = dataProvider.outputDocument.outputStream

  implicit val myDataProvider = dataProvider

  import DocumentGenerator._

  def generate(): Box[OutputDocumentHandle] = {

    val result = tryo{
      e: Throwable => e.printStackTrace(); println("ERROR: unable to create output file.");
    }{
      val template: TextDocument = TextDocument.loadDocument(tplfile);

//      val stylesOfficeStyles = template.getOrCreateDocumentStyles();

//      val defaultStyle = stylesOfficeStyles.getOrCreateDefaultStyle(OdfStyleFamily.Paragraph)
//        defaultStyle.setProperty(OdfTextProperties.FontSize, "10pt")
//	    defaultStyle.setProperty(OdfTextProperties.FontSizeAsian, "10pt")
//	    defaultStyle.setProperty(OdfTextProperties.FontSizeComplex, "10pt")
//        defaultStyle.setProperty(OdfTextProperties.FontName, "Arial")
//        defaultStyle.setProperty(OdfTextProperties.FontNameAsian, "Arial")
//        defaultStyle.setProperty(OdfTextProperties.FontNameComplex, "Arial")

//      val tableBodyContent = stylesOfficeStyles.newStyle("Style_Table_Body_Content", OdfStyleFamily.Paragraph)
//      tableBodyContent.setStyleDisplayNameAttribute("Table Body Content");
//      tableBodyContent.setProperty(OdfTextProperties.FontSize, "10pt")
//      tableBodyContent.setProperty(OdfTextProperties.FontSizeAsian, "10pt")
//	  tableBodyContent.setProperty(OdfTextProperties.FontSizeComplex, "10pt")
//      tableBodyContent.setProperty(OdfTextProperties.FontName, "Arial")
//      tableBodyContent.setProperty(OdfTextProperties.FontNameAsian, "Arial")
//      tableBodyContent.setProperty(OdfTextProperties.FontNameComplex, "Arial")

      substituteGlobalMarkers(template)
      substituteGlobalVariableFields(template, readGlobalVariableProperties(template))
      substituteRepeatMarkers(template)

//    val firma    = Fields.createUserVariableField(outputOdt, "Firma", "AGYNAMIX Aktiengesellschaft");
//    add image
//    outputOdt.newImage(new URI("/Users/tuhlmann/pic/Wasserfall.jpg"));

//      val para = template.getParagraphByIndex(1, false);
//      val uri = new URI("/Users/tuhlmann/Kirschbaum.jpg")
//      println("Add IMG: "+uri)
//      val image = Image.newImage(para, uri)
//      println("IMG added as: "+image.getName())


//        val container = template.getVariableContainerElement()
//        val children = container.getElementsByTagName("text:user-field-decl")
//        var length = children.getLength()-1
//        for (i <- 0 to length) {
//          children.item(i) match {
//            case child: TextUserFieldDeclElement =>
//              println("Child Name : "+child.getTextNameAttribute())
//              println("Child Value: "+child.getOfficeStringValueAttribute())
//            case _ =>
//          }
//        }

//        for (prop <- props) {
//          Box.legacyNullTest(template.getVariableFieldByName(prop.id)).foreach(f => {
//            //f.updateField("Updated Value", null);
//            //println(s"Type of $prop.id: "+f.getFieldType())
//            //println(s"Value of $prop.id: "+f.getOdfElement().toString())
//          })
//        }

      template.save(output)
      dataProvider.outputDocument
    }

    result
  }

  def substituteGlobalMarkers(doc: TextDocument): Unit = {
    val pattern = delimLeft + "([a-zA-Z0-9_:\\-\\.]+)" + delimRight
    substituteMarker(()=>findMarkers(doc, pattern), marker => substituteMarkerValue(doc, marker, new EmptyDataProvider)(dataProvider))
  }

  def substituteGlobalVariableFields(doc: TextDocument, globalVariables: List[DataSourceProperty]): Unit = {
    for (property <- globalVariables) {
      for (field <- Box.legacyNullTest(doc.getVariableFieldByName(property.id))) {
        findReplacementValue(property.id, new EmptyDataProvider) match {
          case DataSourceProperty(_, Full(value), _) => field.updateField(value, null)
          case DataSourceProperty(_, _, _) => field.updateField(property.value.openOr(""), null)
        }
      }
    }
  }

  def substituteRepeatMarkers(doc: TextDocument): Unit = {
    for (repeater <- findRepeatMarkers(doc)) {
      repeater.odfElement match {
        case row: TableTableRowElement => substituteRowRepeatMarker(doc, row, repeater)(dataProvider) // duplicateRow(doc, row, repeater.selection)
        case row => println("No handling for type "+row.getClass().getName())
      }
    }
  }

  def substituteRowRepeatMarker(doc: TextDocument, odfRow: TableTableRowElement, repeater: RepeatMarker)(implicit dataProvider: DataProvider): Unit = {
    val elemsToRepeat = dataProvider.getRepeatedElems(repeater.command.element)
    println(s"elemsToRepeat for ${repeater.command.element}: ${elemsToRepeat.size}")
    for (element <- elemsToRepeat) {
      duplicateRow(doc, odfRow, repeater.selection, element)
    }
    removeRow(doc, odfRow)
  }


  def findRepeatMarkers(doc: TextDocument): List[RepeatMarker] = {
    val pattern = delimLeft + "@repeat[a-zA-Z0-9_:\\-\\.,=]+" + delimRight
    //println("Global. Searching all instructions"+pattern)

    val markers = findMarkers(doc, pattern).map { marker =>
      val text = stripDelimiters(marker.getText())
      //println("Found: "+text)
      implicit val command = parseCommand(text).asInstanceOf[RepeaterCommand]
      println("Found Command: "+command)
      val el = marker.getContainerElement()
      val container = findContainerElement(el)
      RepeatMarker(container, marker, command)
    }

    markers
  }

  protected def findMarkers(doc: TextDocument, pattern: String): List[TextSelection] = findMarkers(new TextNavigation(pattern, doc))
  protected def findMarkers(element: OdfElement, pattern: String): List[TextSelection] = findMarkers(new TextNavigation(pattern, element))
  protected def findMarkers(search: TextNavigation): List[TextSelection] = {
    val markers = ListBuffer[TextSelection]()
    while (search.hasNext()) {
      markers += search.nextSelection().asInstanceOf[TextSelection]
    }
    markers.toList
  }

  protected def substituteMarker(findFunc: ()=>List[TextSelection], substFunc: TextSelection=>Unit): Unit =
    findFunc().foreach(marker => substFunc(marker))

  def removeRow(doc: TextDocument, odfRow: TableTableRowElement): Unit = {
    val row = Row.getInstance(odfRow)
    val table = row.getTable()
    val odfTable = table.getOdfElement()
    odfTable.removeChild(odfRow)
  }

  def duplicateRow(doc: TextDocument, odfRow: TableTableRowElement, selection: TextSelection, repeaterDS: DataSource): Unit = {
    val row = Row.getInstance(odfRow)
    val table = row.getTable()
    val odfTable = table.getOdfElement()

    // clear the command marker
    selection.cut()

    val clonedRow = odfRow.cloneNode(true).asInstanceOf[TableTableRowElement]
    val tr  = odfTable.insertBefore(clonedRow, odfRow).asInstanceOf[TableTableRowElement]
    replaceParameters(doc, tr, repeaterDS)

//    val clonedRow2 = odfRow.cloneNode(true).asInstanceOf[TableTableRowElement]
//    val tr2  = odfTable.insertBefore(clonedRow2, odfRow).asInstanceOf[TableTableRowElement]
//    replaceParameters(doc, tr2)

//!    odfTable.removeChild(odfRow)

//    val newRows = table.insertRowsBefore(row.getRowIndex+1, 2).asScala
//
//    for (newRow <- newRows; i <- 0 to row.getCellCount()) {
//      println("copy Cell "+i)
//
//      val masterCell = row.getCellByIndex(i)
//      val newCell = newRow.getCellByIndex(i)
//      //newCell.addParagraph(masterCell.g)
//
////      val newOdfRow = newRow.getOdfElement()
////      val clonedChildren = clonedCell.getChildNodes()
////      for (j <- 0 to clonedChildren.getLength()) {
////        val clonedChild = clonedChildren.item(j)
////        newOdfRow.appendChild(clonedChild)
////      }
//    }

//    println("NEW ROW: "+newRows.head.getOdfElement())

  }

  def replaceParameters(doc: TextDocument, row: TableTableRowElement, repeaterDS: DataSource): Unit = {
    val pattern = delimLeft + "#[a-zA-Z0-9_\\-\\.]+" + delimRight
    //println("Local. Searching all "+pattern)
    substituteMarker(()=>findMarkers(row, pattern), marker => substituteMarkerValue(doc, marker, new RepeaterDataProvider(repeaterDS)))
  }

  def parseCommand(cmd: String): MarkerCommand = {
    if (cmd == null || cmd.trim.isEmpty()) IllegalCommand()
    else MarkerCommandBuilder(cmd.split(commandDelim).toList)
  }

  @tailrec
  private def findContainerElement(element: OdfElement)(implicit cmd: MarkerCommand): OdfElement = {
    val nodeName = elementMappings.get(cmd.container).getOrElse(throw new IllegalArgumentException("Container not known: "+cmd.container))
    if (element == null) throw new IllegalStateException("No container element of type "+cmd.container)
    if (element.getNodeName() == nodeName) element else {
      val parent: OdfElement = element.getParentNode().asInstanceOf[OdfElement]
      findContainerElement(parent)
    }
  }

  def readGlobalVariableProperties(doc: TextDocument): List[DataSourceProperty] = {
    val container = doc.getVariableContainerElement()
    val children = container.getElementsByTagName("text:user-field-decl")
    var length = children.getLength()-1
    (0 to length).toList.flatMap(idx => {
      children.item(idx) match {
        case child: TextUserFieldDeclElement if !child.getTextNameAttribute().startsWith(repeaterVariablePrefix) =>
          //println("Child Name : "+child.getTextNameAttribute())
          //println("Child Value: "+child.getOfficeStringValueAttribute())
          Full(DataSourceProperty(child.getTextNameAttribute(), Full(child.getOfficeStringValueAttribute()), DataSourcePropertyType.Text))
        case _ => Empty
      }
    })
  }

}

trait MarkerCommand {
  def container: String
}

case class IllegalCommand() extends MarkerCommand {
  val container = ""

}

case class RepeaterCommand(args: Map[String, String]) extends MarkerCommand {
  val container = args("what")
  val element = args("el")
  val iteratorElem = args("@repeat")
}


case class RepeatMarker(val odfElement: OdfElement, selection: TextSelection, command: RepeaterCommand) {

}

object MarkerCommandBuilder {

  private def parseIntoArgTuple(args: List[String]): List[(String, String)] = {
    args.map{ arg =>
      val pos = arg.indexOf(DocumentGenerator.argumentDelim)
      if (pos < 0) {
        (arg, "")
      } else if (pos == 0) {
        ("", arg.substring(pos+1))
      } else {
        (arg.substring(0, pos), arg.substring(pos+1))
      }
    }
  }

  private def findCommandTuple(tpl: List[(String, String)]): Int =
    tpl.indexWhere(t => t._1.startsWith(DocumentGenerator.commandPrefix))

  def apply(args: List[String]): MarkerCommand = {
    val argTuple: List[(String, String)] = parseIntoArgTuple(args)
    //println("cmd list: "+argTuple)
    val cmdIdx = findCommandTuple(argTuple)
    if (cmdIdx > -1) {
      argTuple(cmdIdx) match {
        case (cmd, cmdArg) if cmd == "@repeat" => RepeaterCommand(argTuple.toMap)
        case _ => println("Illegal"); IllegalCommand()
      }
    } else {
      IllegalCommand()
    }

  }

}
