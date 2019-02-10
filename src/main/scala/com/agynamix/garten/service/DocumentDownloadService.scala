package com.agynamix.garten.service

import org.bson.types.ObjectId
import com.agynamix.garten.model.Document
import java.io.File
import com.agynamix.garten.model.User
import java.io.BufferedReader
import java.util.zip.ZipOutputStream
import java.io.FileOutputStream
import java.util.zip.ZipEntry
import scala.io.Source
import net.liftweb.common._
import java.io.BufferedInputStream

object DocumentDownloadService {

  def createZipDocument(creator: User, clientId: ObjectId, fileName: String, displayName: String, documents: List[Document]): Box[Document] = {
    val zip = createZip(documents)
    for (docInfo <- Document.documentService.save(List(clientId.toString(), creator.id.get.toString), zip, fileName, displayName)) yield {
      Document.createInstance(creator.id.get, creator.id.get, clientId, docInfo).hidden(true).save(true)
    }
  }

  private def createZip(documents: List[Document]): File = {
    val zipFile = File.createTempFile("dld", ".zip")
    //println("XX ZIP file is "+zipFile.getAbsolutePath())

    val zip = new ZipOutputStream(new FileOutputStream(zipFile));
    var count = 0
    try {
      for (document <- documents) {
        //add zip entry to output stream
        count += 1
        val fname = count + "_" + document.fileName.get
        zip.putNextEntry(new ZipEntry(fname));


        val in = new BufferedInputStream(document.documentUrl.openStream())
        var b = in.read()
        try {
          while (b > -1) {
            zip.write(b)
            b = in.read()
          }
        } finally {
          in.close()
        }
        zip.closeEntry();
      }
    } finally {
      zip.close();
    }

    zipFile
  }

}