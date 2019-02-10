package com.agynamix.garten.lib

import java.security.InvalidKeyException
import java.security.NoSuchAlgorithmException
import java.security.SecureRandom

import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec

import org.apache.commons.codec.binary.Base32
import org.apache.commons.codec.binary.Base64

/**
 * Java Server side class for Google Authenticator's TOTP generator
 * Thanks to Enrico's blog for the sample code:
 * @see http://thegreyblog.blogspot.com/2011/12/google-authenticator-using-it-in-your.html
 *
 *
 * @see http://code.google.com/p/google-authenticator
 * @see http://tools.ietf.org/id/draft-mraihi-totp-timebased-06.txt
 */
trait GoogleAuthenticatorTrait {

  // taken from Google pam docs - we probably don't need to mess with these
  val SECRET_SIZE = 10
  val SEED = "jsadhf7adgfhasdgfjaGtdf76hkjghd67tfjKlkbfgfdgd26fhairhstz4RfsUzc57Gadsja8Pdh"
  val RANDOM_NUMBER_ALGORITHM = "SHA1PRNG"

  var _windowSize = 1;  // default 3 - max 17 (from google docs)

  def windowSize = _windowSize

  /**
   * set the windows size. This is an integer value representing the number of 30 second windows we allow
   * The bigger the window, the more tolerant of clock skew we are.
   *
   * @param size window size - must be >=1 and <=17.  Other values are ignored
   */
  def windowSize_=(size: Int) = if (size >= 1 && size <= 17) _windowSize = size

  /**
   * Generate a random secret key. This must be saved by the server and associated with the
   * users account to verify the code displayed by Google Authenticator.
   * The user must register this secret on their device.
   * @return secret key
   */
  def generateSecretKey(): Option[String] = {
    try {
      val sr = SecureRandom.getInstance(RANDOM_NUMBER_ALGORITHM)
      sr.setSeed(Base64.decodeBase64(SEED))
      val buffer = sr.generateSeed(SECRET_SIZE)
      val bEncodedKey = new Base32().encode(buffer)
      Some(new String(bEncodedKey))
    } catch {
      case e: NoSuchAlgorithmException => e.printStackTrace(); None
    }
  }

  /**
   * Return a URL that generates and displays a QR barcode. The user scans this bar code with the
   * Google Authenticator application on their smartphone to register the auth code. They can also manually enter the
   * secret if desired
   *
   * @param user   user id (e.g. fflinstone)
   * @param host   host or system that the code is for (e.g. myapp.com)
   * @param secret the secret that was previously generated for this user
   * @return the URL for the QR code to scan
   */
  def getQRBarcodeURL(user: String, host: String, secret: String): String = {
    val tpl = "https://chart.googleapis.com/chart?chs=200x200&chld=M%%7C0&cht=qr&chl=otpauth://totp/%s@%s%%3Fsecret%%3D%s"
    tpl.format(user, host, secret)
  }

  /**
   * Check the code entered by the user to see if it is valid
   * @param secret   The users secret.
   * @param code     The code displayed on the users device
   * @param timeMsec The time in msec (System.currentTimeMillis() for example)
   * @return
   */
  def checkCode(secret: String, code: Long, timeMsec: Long): Boolean = {
    val codec = new Base32()
    val decodedKey = codec.decode(secret)
    val t = (timeMsec / 1000L) / 30L
    var i = -windowSize
    while (i <= windowSize) {
      var hash: Long = 0l
      try {
        hash = verifyCode(decodedKey, t + i)
      } catch {
        case e: Exception => {
          e.printStackTrace()
          throw new RuntimeException(e.getMessage)
        }
      }
      if (hash == code) {
        return true
      }
      i += 1
    }
    false
  }

  def verifyCode(key: Array[Byte], t: Long): Int = {
    val data = Array.ofDim[Byte](8)
    var value = t
    var i = 8
    while (i > 0) {
      i -= 1
      data(i) = value.toByte
      value >>>= 8
    }
    val signKey = new SecretKeySpec(key, "HmacSHA1")
    val mac = Mac.getInstance("HmacSHA1")
    mac.init(signKey)
    val hash = mac.doFinal(data)
    val offset = hash(20 - 1) & 0xF
    var truncatedHash = 0
    for (i <- 0 until 4) {
      truncatedHash <<= 8
      truncatedHash |= (hash(offset + i) & 0xFF)
    }
    truncatedHash &= 0x7FFFFFFF
    truncatedHash %= 1000000
    truncatedHash
  }
}

object GoogleAuthenticator extends GoogleAuthenticatorTrait

object GAuthGen extends GoogleAuthenticatorTrait {

  def main(args: Array[String]) {
    for (secret <- generateSecretKey()) {
      val url = getQRBarcodeURL("tuhlmann", "unser-gartenverein.de", secret)

      println("SECRET: "+secret)
      println("URL: "+url)
    }

  }
}

object GAuthCheck extends GoogleAuthenticatorTrait {

  def main(args: Array[String]) {
    //val secret = "D5VM4ISCZ3DM37IG"
    val secret = args(0)
    val code = args(1)

    val re = checkCode(secret, code.toLong, System.currentTimeMillis())
    println(s"Code: ${code} is valid: ${re}")
  }

}
