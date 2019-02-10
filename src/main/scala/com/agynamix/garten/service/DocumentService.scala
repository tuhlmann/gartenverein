package com.agynamix.garten.service

import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.http.FileParamHolder
import net.liftweb.http.OnDiskFileParamHolder
import java.io.File
import java.util.UUID
import com.agynamix.garten.api.FileUpload
import java.net.URL
import java.net.URLConnection
import java.io.InputStream
import org.apache.commons.io.IOUtils
import java.io.FileOutputStream
import com.agynamix.garten.model.Document

case class DocumentInfo(displayName: String, fileName: String, mimeType: String, length: Long, filePath: String)

trait DocumentService {

  /**
   * Save the FileParamHolder contained file according to this DocumentService.
   */
  def save(pathElems: List[String], fph: FileParamHolder): Box[DocumentInfo]

  /**
   * Save the temp file according to this DocumentService.
   */
  def save(pathElems: List[String], tmpFile: File, documentName: String, displayName: String): Box[DocumentInfo]

  /**
   * Save the input stream according to this DocumentService.
   */
  def save(pathElems: List[String], inputStream: InputStream, filename: String, documentName: String): Box[DocumentInfo]

  def replaceFile(relativePath: String, inputStream: InputStream)

  /**
   * Remove a document from its storage
   */
  def remove(filePath: String): Boolean

  def downloadPath(documentGuid: String): String

  def documentUrl(relativePath: String): URL

}

class FileDocumentService(val root: String) extends DocumentService with Loggable {

  val rootF = new File(root)

  private def ensureDirectory(pathElems: List[String]): Box[File] = {
    val dir = (root :: pathElems).mkString(File.separator)
    val f = new File(dir)
    if (!f.exists()) {
      if (f.mkdirs()) Full(f) else Empty
    } else {
      Full(f)
    }
  }

  private def relativePath(root: File, absoluteFile: File): String = {
    val absList = absoluteFile.getAbsolutePath().split(File.separator)
    val rootList =root.getAbsolutePath().split(File.separator)
    absList.drop(rootList.length).mkString(File.separator)
  }

  private def renameFile(pathElems: List[String], tmpFile: File): Box[File] = {
    emptyFile(pathElems).flatMap(newFile => {
      logger.debug(s"Rename file ${tmpFile} to ${newFile}")
      if (tmpFile.renameTo(newFile)) Full(newFile) else Empty
    })
  }

  private def emptyFile(pathElems: List[String]): Box[File] = {
    ensureDirectory(pathElems).map(dir => {
      new File(dir, UUID.randomUUID().toString())
    })
  }

  def save(pathElems: List[String], fph: FileParamHolder): Box[DocumentInfo] = {
    fph match {
      case diskFph: OnDiskFileParamHolder =>
        for (file <- renameFile(pathElems, diskFph.localFile)) yield {
          val relpath = relativePath(rootF, file)
          DocumentInfo(fph.fileName, fph.fileName, fph.mimeType, file.length(), relpath)
        }
      case memFph: FileParamHolder => logger.warn("Saving InMemory FPH not implemented yet!"); Empty
    }
  }

  def save(pathElems: List[String], tmpFile: File, documentName: String, displayName: String): Box[DocumentInfo] = {
    val mimeType = URLConnection.guessContentTypeFromName(tmpFile.getName());
    for (file <- renameFile(pathElems, tmpFile)) yield {
      logger.debug(s"Guess Mime from ${tmpFile}: "+mimeType)
      val relpath = relativePath(rootF, file)
      DocumentInfo(displayName, documentName, mimeType, file.length(), relpath)
    }
  }

  /**
   * Save the input stream according to this DocumentService.
   */
  def save(pathElems: List[String], inputStream: InputStream, filename: String, documentName: String): Box[DocumentInfo] = {
    val mimeType = URLConnection.guessContentTypeFromName(filename);
    for (file <- emptyFile(pathElems)) yield {
      logger.debug(s"Guess Mime from ${filename}: "+mimeType)
      IOUtils.copy(inputStream, new FileOutputStream(file))
      val relpath = relativePath(rootF, file)
      DocumentInfo(documentName, filename, mimeType, file.length(), relpath)
    }
  }

  def replaceFile(relativePath: String, inputStream: InputStream): Unit = {
    if (remove(relativePath)) {
      IOUtils.copy(inputStream, new FileOutputStream(absoluteFilePath(relativePath)))
    }
  }

  def absoluteFilePath(relativePath: String): File = new File(rootF, relativePath)

  def remove(relativePath: String): Boolean = {
    val toRemove = absoluteFilePath(relativePath)
    logger.debug("Remove Document "+toRemove)
    toRemove.delete()
  }

  def downloadPath(documentGuid: String): String = {
    (FileUpload.downloadPath ::: List(documentGuid)).mkString("/")
  }

  def documentUrl(relativePath: String): URL = {
    val absolutePath = absoluteFilePath(relativePath)
    //logger.info("My file is "+absolutePath)
    val re = absolutePath.toURI().toURL()
    //logger.info("My file URI is "+re)
    re
  }


}