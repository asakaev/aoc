package util.file

def readAll(path: String): List[String] =
  scala.io.Source.fromFile(path).getLines.toList
