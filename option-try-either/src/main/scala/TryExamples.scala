// Examples inspired by http://danielwestheide.com/blog/2012/12/26/the-neophytes-guide-to-scala-part-6-error-handling-with-try.html

import java.io.{InputStream, FileNotFoundException}
import java.net.{URLConnection, MalformedURLException, URL}
import scala.util.{Failure, Success, Try}
import scala.io.{BufferedSource, Source}


object TryCatch {
  def parseURL(urlarg: String) = {
    var url: URL = null
    try {
      url = new URL(urlarg)
    } catch {
      case m: MalformedURLException => throw m
    }
    url
  }

  def getURLContent(urlarg: String): Iterator[String] = {
    var lines: Iterator[String] = null
    try {
      val url = parseURL(urlarg)
      val connection = url.openConnection()
      val is = connection.getInputStream
      lines = Source.fromInputStream(is).getLines()
    } catch {
      case e: FileNotFoundException => Iterator("Requested page does not exist")
      case e: MalformedURLException => Iterator("Please make sure to enter a valid URL")
      case _: Throwable => Iterator("An unexpected error has occurred. We are so sorry!")
    }
    lines
  }

}

object TryMatch {

  def parseURL(url: String): Try[URL] = Try(new URL(url))

  def getURLContent(urlarg: String): Try[Iterator[String]] = {
    val maybeUrl = parseURL(urlarg)
    val url = maybeUrl match {
      case Success(url) => url
      case Failure(ex) => throw ex
    }

    val maybeConn = ???
    val connection: URLConnection = ???

    val maybeIS = ???
    val is: InputStream = ???

    val maybeSource = ???
    ???
  }
}

object TryMap {

  def parseURL(url: String): Try[URL] = Try(new URL(url))

  def getURLContent(urlarg: String): Try[Iterator[String]] = {
    parseURL(urlarg)
      .map(???)
      .map(???)
      .map(???)
      .map(???)
  }
}

object TryFlatMap {

  def parseURL(url: String): Try[URL] = Try(new URL(url))

  def getURLContent(urlarg: String): Try[Iterator[String]] = {
      val url = parseURL(urlarg)
      val connection = url.flatMap(???)
      val is = connection.flatMap(???)
      val source: Try[BufferedSource] = is.map(???)
      source.map(_.getLines())
  }
}

// explanation: <- translates to flatMap, = translates to map
object TryFor {

  def parseURL(url: String): Try[URL] = Try(new URL(url))

  def getURLContent(url: String): Try[Iterator[String]] =
    (for {
      url <- Try(???)
      //...
      //...
      //...
    } yield ???)
    .recover {
      case e: FileNotFoundException => Iterator("Requested page does not exist")
      case e: MalformedURLException => Iterator("Please make sure to enter a valid URL")
      case _ => Iterator("An unexpected error has occurred. We are so sorry!")
    }
}
