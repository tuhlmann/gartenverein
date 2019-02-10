package com.agynamix.document

import java.io.InputStream
import java.io.OutputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.net.URL
import java.io.ByteArrayInputStream
import com.agynamix.garten.model.share.TemplateProvider

object DocumentHandleConstants {
  val OutputDocument_FileName = "OutputDocument.FileName"
}

/**
 * Some abstract way to get hold of a document.
 * Meant to hide the actual location (Memory, Database, Filesystem) of the document and provide unified access
 */
trait DocumentHandle {

}

trait InputDocumentHandle extends DocumentHandle {

  def inputStream: InputStream

}

trait OutputDocumentHandle extends DocumentHandle {

  def outputStream: OutputStream

  def getFile: File

  /**
   * Return a readable name
   */
  def getFileName: String

  def getFileDisplayName: String

}

class FileDocumentHandle(val file: File) extends InputDocumentHandle with OutputDocumentHandle {

  def inputStream: InputStream = new FileInputStream(file)
  def outputStream: OutputStream = new FileOutputStream(file)

  def getFile = file

  def getFileName = file.getName()
  def getFileDisplayName = file.getName()

}

class BulkLetterOutputDocumentHandle(val file: File, tplProvider: TemplateProvider, dataProvider: DataProvider) extends OutputDocumentHandle {

  def outputStream: OutputStream = new FileOutputStream(file)

  def getFile = file
  def getFileName = {
    getFileDisplayName.replace(" ", "_").replace(".", "_").replace("'", "").replace("\"", "") + getFileExtension
  }

  def getFileDisplayName = {
    dataProvider.getDisplayProperty(DocumentHandleConstants.OutputDocument_FileName) openOr {
      println(s"${DocumentHandleConstants.OutputDocument_FileName} nicht gefunden")
      if (tplProvider.getSubject().nonEmpty) tplProvider.getSubject() else file.getName()
    }
  }

  private def getFileExtension = {
    val name = file.getName()
    val pos = name.lastIndexOf(".")
    if ((pos > -1) && (pos < name.length())) {
      name.substring(pos)
    } else ""
  }

}

class UrlInputDocumentHandle(val url: URL) extends InputDocumentHandle {

  def inputStream: InputStream = url.openStream()

}

class NullInputDocumentHandle() extends InputDocumentHandle {

  val byteArr = Array[Byte] ( 0xC, 0xA )
  def inputStream: InputStream = new ByteArrayInputStream(byteArr)
}