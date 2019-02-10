package com.agynamix.garten.lib

import net.liftweb.common.Loggable

import org.bouncycastle.crypto.CryptoException;
import org.bouncycastle.crypto.engines.BlowfishEngine;
import org.bouncycastle.crypto.paddings.PaddedBufferedBlockCipher;
import org.bouncycastle.crypto.params.KeyParameter;
import org.bouncycastle.util.encoders.Base64
import org.bson.types.ObjectId
;

object Blowfisher extends Loggable{

  def main(args: Array[String]) {
    val originalString = "fall2008" // String to be encrypted.
    val keyString = new ObjectId().toString // A secret key that is used during encryption and decryption
    try {
      val encryptedString = encrypt(originalString, keyString)
      println("Original String: " + originalString)
      println("Encrypted String: " + encryptedString)
      val decryptedString = decrypt(encryptedString, keyString)
      println("Decrypted String: " + decryptedString)
    } catch {
      case e: Throwable => e.printStackTrace()
    }

  }

  def decrypt(name: String, keyString: String): String = {
    val engine = new BlowfishEngine()
    val cipher = new PaddedBufferedBlockCipher(engine)
    val result = new StringBuilder()
    val key = new KeyParameter(keyString.getBytes())
    cipher.init(false, key)
    val out = Base64.decode(name)
    val out2 = Array.ofDim[Byte](cipher.getOutputSize(out.length))
    val len2 = cipher.processBytes(out, 0, out.length, out2, 0)
    cipher.doFinal(out2, len2)
    val s2 = new String(out2)
    var i = 0
    while ( i < s2.length ) {
      val c = s2.charAt(i)
      if (c != 0) {
        result.append(c)
      }
      i += 1
    }

    result.toString()
  }

  def encrypt(value: String, keyString: String): String = {
    val engine = new BlowfishEngine()
    val cipher = new PaddedBufferedBlockCipher(engine)
    val key    = new KeyParameter(keyString.getBytes())
    cipher.init(true, key)
    val in = value.getBytes()
    val out = Array.ofDim[Byte](cipher.getOutputSize(in.length))
    val len1 = cipher.processBytes(in, 0, in.length, out, 0)
    try {
      cipher.doFinal(out, len1);
    } catch {
      case e: CryptoException =>
        e.printStackTrace()
        throw new Exception(e.getMessage())
    }
    new String(Base64.encode(out))
  }
}

