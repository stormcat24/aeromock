import sbt._
import sbt.Keys._
import scoverage.ScoverageSbtPlugin._
import org.scoverage.coveralls.CoverallsPlugin.coverallsSettings
import com.typesafe.sbt.pgp.PgpSettings._
import scala.util.Try

/**
 * Definition of build on Aeromock.
 * @author stormcat24
 */
object AeromockBuild extends Build {

  lazy val scalazVersion = "7.0.6"
  lazy val nettyVersion = "4.0.21.Final"
  lazy val scaldiVersion = "0.4"

  lazy val baseSettings = super.settings ++ Seq(
    scalaVersion := "2.11.1",
    organization := "jp.co.cyberagent.aeromock",
    version := Version.aeromock,
    pgpPassphrase := Some(Try(sys.env("PGP_PASSPHRASE")).getOrElse("").toCharArray),
    publishMavenStyle := true,
    publishArtifact in Test := false,
    pomIncludeRepository := { _ => false},
    pomExtra := (
      <url>https://github.com/CyberAgent/aeromock</url>
        <licenses>
          <license>
            <name>The MIT License</name>
            <url>http://www.opensource.org/licenses/mit-license.php</url>
            <distribution>repo</distribution>
          </license>
        </licenses>
        <scm>
          <connection>scm:git:git@github.com:CyberAgent/aeromock.git</connection>
          <developerConnection>scm:git:git@github.com:CyberAgent/aeromock.git</developerConnection>
          <url>git@github.com:CyberAgent/aeromock.git</url>
        </scm>
        <developers>
          <developer>
            <name>Akinori Yamada</name>
            <email>yamada_akinori@cyberagent.co.jp</email>
            <organization>CyberAgent,Inc</organization>
            <organizationUrl>http://www.cyberagent.co.jp/en/</organizationUrl>
          </developer>
        </developers>
      ),

    publishTo <<= version { (v: String) =>
      val nexus = "https://oss.sonatype.org/"
      if (v.trim.endsWith("SNAPSHOT"))
        Some("snapshots" at nexus + "content/repositories/snapshots")
      else
        Some("releases"  at nexus + "service/local/staging/deploy/maven2")
    },
    credentials += Credentials(
      "Sonatype Nexus Repository Manager",
      "oss.sonatype.org",
      System.getenv("M2_REPO_USER"),
      System.getenv("M2_REPO_PASSWORD")),

    resolvers ++= Seq(
      Resolver.mavenLocal,
      Resolver.sonatypeRepo("releases"),
      Resolver.sonatypeRepo("snapshots")
    ),

    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core" % scalazVersion,
      "org.scala-lang" % "scala-reflect" % "2.11.1",
      "com.typesafe.scala-logging" %% "scala-logging-slf4j" % "2.1.2",
      "org.scaldi" %% "scaldi" % scaldiVersion,
      "ch.qos.logback" % "logback-classic" % "1.1.2",
      "jp.co.cyberagent.aeromock" % "aeromock-dsl" % Version.aeromock,
      "org.specs2" %% "specs2" % "2.3.12" % "test"
    ),

    ScoverageKeys.highlighting := true,
    scalacOptions += "-feature",
    initialCommands in console := "import scalaz._, Scalaz._",
    mainClass in Compile := Some("jp.co.cyberagent.aeromock.Aeromock")
  ) ++ instrumentSettings ++ coverallsSettings

  lazy val root = Project(
    id = "root",
    base = file("."),
    settings = baseSettings ++ Seq(
      packagedArtifacts := Map.empty
    ) ++ Tasks.settingTasks
  ) copy (
    aggregate = projects.filterNot(p => Set("root").contains(p.id)).map(p => p: ProjectReference)
    )

  lazy val aeromock_spec_support = Project(
    id = "aeromock-spec-support",
    base = file("aeromock-spec-support"),
    settings = baseSettings
  )

  lazy val aeromock_server = Project(
    id = "aeromock-server",
    base = file("aeromock-server"),
    settings = baseSettings
  ) dependsOn (aeromock_spec_support % "test")

  lazy val aeromock_cli = Project(
    id = "aeromock-cli",
    base = file("aeromock-cli"),
    settings = baseSettings
  ) dependsOn (aeromock_server, aeromock_spec_support % "test")

  lazy val aeromock_freemarker = Project(
    id = "aeromock-freemarker",
    base = file("aeromock-freemarker"),
    settings = baseSettings
  ) dependsOn(aeromock_server, aeromock_cli, aeromock_spec_support % "test")

  lazy val aeromock_handlebars_java = Project(
    id = "aeromock-handlebars-java",
    base = file("aeromock-handlebars-java"),
    settings = baseSettings
  ) dependsOn(aeromock_server, aeromock_cli, aeromock_spec_support % "test")

  lazy val aeromock_jade4j = Project(
    id = "aeromock-jade4j",
    base = file("aeromock-jade4j"),
    settings = baseSettings
  ) dependsOn(aeromock_server, aeromock_cli, aeromock_spec_support % "test")

  lazy val aeromock_velocity = Project(
    id = "aeromock-velocity",
    base = file("aeromock-velocity"),
    settings = baseSettings
  ) dependsOn(aeromock_server, aeromock_cli, aeromock_spec_support % "test")

  lazy val aeromock_thymeleaf= Project(
    id = "aeromock-thymeleaf",
    base = file("aeromock-thymeleaf"),
    settings = baseSettings
  ) dependsOn(aeromock_server, aeromock_cli, aeromock_spec_support % "test")

  lazy val aeromock_groovy_template = Project(
    id = "aeromock-groovy-template",
    base = file("aeromock-groovy-template"),
    settings = baseSettings
  ) dependsOn(aeromock_server, aeromock_cli, aeromock_spec_support % "test")

  object Tasks {

    val dslProject = "aeromock-dsl"

    val installDsl = TaskKey[Int]("installDsl", "install aeromock-dsl")
    lazy val installDslTask = installDsl := {
      s"./$dslProject/gradlew -p $dslProject clean test" !
    }

    val deployDsl = TaskKey[Int]("deployDsl", "deploy aeromock-dsl")
    lazy val deployDslTask = deployDsl := {
      s"./$dslProject/gradlew -p $dslProject clean test uploadArchives" !
    }

    val settingTasks = Seq(
      installDslTask,
      deployDslTask
    )
  }

}
