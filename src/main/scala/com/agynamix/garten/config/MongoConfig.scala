package com.agynamix.garten.config

import net.liftweb._
import net.liftweb.common._
import net.liftweb.json._
import net.liftweb.mongodb._
import net.liftweb.util.Props
import com.mongodb.{DBAddress, Mongo}
import com.mongodb.MongoClient
import net.liftweb.util.DefaultConnectionIdentifier

object MongoConfig extends Loggable {
  implicit val formats = DefaultFormats

  case class CloudFoundryMongo(name: String, label: String, plan: String, credentials: CloudFoundryMongoCredentials)
  case class CloudFoundryMongoCredentials(hostname: String, port: String, username: String, password: String, name: String, db: String)

  /*
   * This checks to see if it's running in a CloudFoundry environment and
   * gets the MongoDB info from there if it is.
   */
  def init() {

    Option(System.getenv("VCAP_SERVICES")) match {
      case Some(s) =>
        try {
          // cloud foundry environment
          parse(s) \ "mongodb-1.8" match {
            case JArray(ary) => ary foreach { mngoJson =>
              val credentials = mngoJson.extract[CloudFoundryMongo].credentials

              logger.debug("MongoDB hostname: %s".format(credentials.hostname))
              logger.debug("MongoDB port: %s".format(credentials.port))
              logger.debug("MongoDB db: %s".format(credentials.db))
              logger.debug("MongoDB username: %s".format(credentials.username))
              logger.debug("MongoDB password: %s".format(credentials.password))

              MongoDB.defineDbAuth(
                DefaultConnectionIdentifier,
                new MongoClient(credentials.hostname, credentials.port.toInt),
                credentials.db,
                credentials.username,
                credentials.password
              )
              logger.info("MongoDB inited on CloudFoundry: %s".format(credentials.name))
            }
            case x => logger.warn("Json parse error: %s".format(x))
          }
        }
        catch {
          case e: Throwable => logger.error("Error initing Mongo: %s".format(e.getMessage))
        }
      case _ => {
        /*
         * First checks for existence of mongo.default.url. If not found, then
         * checks for mongo.default.host, port, and name. Uses defaults if those
         * are not found.
         */
        val defaultDbAddress = Props.get("mongo.default.url")
          .map(url => new DBAddress(url))
          .openOr(new DBAddress(
            Props.get("mongo.default.host", "127.0.0.1"),
            Props.getInt("mongo.default.port", 27017),
            Props.get("mongo.default.name", "gartenverein")
          ))

        /*
         * If mongo.default.user, and pwd are defined, configure Mongo using authentication.
         */
        (Props.get("mongo.default.user"), Props.get("mongo.default.pwd")) match {
          case (Full(user), Full(pwd)) =>
            MongoDB.defineDbAuth(DefaultConnectionIdentifier, new MongoClient(defaultDbAddress),
              defaultDbAddress.getDBName, user, pwd)
            logger.info("MongoDB inited using authentication: %s".format(defaultDbAddress.toString))
          case _ =>
            MongoDB.defineDb(DefaultConnectionIdentifier, new MongoClient(defaultDbAddress), defaultDbAddress.getDBName)
            logger.info("MongoDB inited: %s".format(defaultDbAddress.toString))
        }
      }
    }
  }
}
