package util.file

def readAll(path: String): List[String] =
  scala.io.Source.fromFile(path).getLines.toList

def readAllLazy(path: String): LazyList[String] =
  LazyList.from(scala.io.Source.fromFile(path).getLines)
