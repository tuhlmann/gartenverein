name := "AGYNAMIX Unser Gartenverein"

organization := "AGYNAMIX"

version := "1.0-SNAPSHOT"

scalaVersion := "2.10.5"

resolvers ++= Seq(
  "Java.net Maven2"     at "http://download.java.net/maven/2/",
  "Sonatype repo"       at "https://oss.sonatype.org/content/groups/scala-tools/",
  "Sonatype releases"   at "https://oss.sonatype.org/content/repositories/releases",
  "Sonatype snapshots"  at "https://oss.sonatype.org/content/repositories/snapshots",
  "JBoss Thirdparty"    at "https://repository.jboss.org/nexus/content/repositories/thirdparty-uploads",
  "tuhlmann"            at "https://bitbucket.org/agynamix/mvn-repo/raw/master/snapshots/"
)

//  "tuhlmann"            at "https://github.com/tuhlmann/tuhlmann-mvn-repo/raw/master/snapshots",


{
  val liftVersion = "2.6.1"
  libraryDependencies ++= Seq(
    "net.liftweb"               %% "lift-webkit"              %  liftVersion       % "compile"                ,
    "net.liftweb"               %% "lift-mongodb-record"      %  liftVersion                                  ,
    "org.mindrot"               %  "jbcrypt"                  %  "0.3m"            % "compile"                ,
    "ch.qos.logback"            %  "logback-classic"          %  "1.0.6"                                      ,
    "junit"                     %  "junit"                    %  "4.7"             % "test"                   ,
    "org.specs2"                %% "specs2"                   %  "1.14"            % "test"                   ,
    "org.pegdown"               %  "pegdown"                  %  "1.1.0"           % "test"                   ,
    "org.scalatest"             %% "scalatest"                %  "2.0.M5b"         % "test"                   ,
    "org.eclipse.jetty"         %  "jetty-webapp"             %  "8.1.7.v20120910" % "container"              ,
    "net.liftmodules"           %% "extras_2.6"               %  "0.4-SNAPSHOT"                               ,
    "com.foursquare"            %% "rogue-field"               %  "2.5.0"              intransitive() ,
    "com.foursquare"            %% "rogue-core"               %  "2.5.1"              intransitive() ,
    "com.foursquare"            %% "rogue-lift"               %  "2.5.1"              intransitive() ,
    "com.foursquare"            %% "rogue-index"              %  "2.5.1"              intransitive() ,
    "cc.co.scala-reactive"      %% "reactive-core"            %  "0.3.0"                       intransitive() ,
    "cc.co.scala-reactive"      %% "reactive-web"             %  "0.3.0"                       intransitive() ,
    "net.liftmodules"           %% "widgets_2.5"              %  "1.4-SNAPSHOT"    % "compile" intransitive() ,
    "com.h2database"            %  "h2"                       %  "1.2.138"                                    ,
    "org.apache.odftoolkit"     %  "simple-odf"               %  "0.7-incubating"                             ,
    "org.artofsolving.jodconverter" % "jodconverter-core"     %  "3.0.2"                                      ,
    "org.hyperic"               %  "sigar"                    %  "1.6.5.132"                                  ,
    "com.chuusai"               %% "shapeless"                %  "1.2.4"                                      ,
    "org.scalaz"                %% "scalaz-core"              %  "7.1.0"                                      ,
    "org.jsoup"                 %  "jsoup"                    %  "1.7.2"                                      ,
    "org.joda"                  %  "joda-money"               %  "0.9"                                        ,
    "org.bouncycastle"          %  "bcprov-jdk15on"           %  "1.50"                                       ,
    "eu.henkelmann"             %  "actuarius_2.10.0"         % "0.2.6"
  )
}

//     "com.lowagie"               %  "itext"                    %  "4.2.0"

//     "org.eclipse.birt.runtime"  %  "org.eclipse.birt.runtime" % "4.3.0"

// scalacOptions += "-deprecation"

seq(lessSettings:_*)

(LessKeys.filter in (Compile, LessKeys.less)) := "styles.less"

(LessKeys.mini in (Compile, LessKeys.less)) := true

(sourceDirectory in (Compile, LessKeys.less)) <<= (sourceDirectory in Compile)(_ / "webapp" / "media" / "styles")

(resourceManaged in (Compile, LessKeys.less)) <<= (crossTarget in Compile)(_ / "resource_managed" / "main" / "media" / "styles")

seq(closureSettings:_*)

(ClosureKeys.prettyPrint in (Compile, ClosureKeys.closure)) := false

seq(webSettings :_*)

// add managed resources, where less and closure publish to, to the webapp
(webappResources in Compile) <+= (resourceManaged in Compile)

(sourceDirectory in (Compile, ClosureKeys.closure)) <<= (sourceDirectory in Compile)(_ / "webapp" / "media" / "js")

(resourceManaged in (Compile, ClosureKeys.closure)) <<= (crossTarget in Compile)(_ / "resource_managed" / "main" / "media" / "js")

// If using JRebel uncomment next line
scanDirectories := Nil

// Remove Java directories, otherwise sbteclipse generates them
// unmanagedSourceDirectories in Compile <<= (scalaSource in Compile)(Seq(_))

// unmanagedSourceDirectories in Test <<= (scalaSource in Test)(Seq(_))

// port in container.Configuration := 8089
