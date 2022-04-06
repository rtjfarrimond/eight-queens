enum File(val value: Int):
  case A extends File(1)
  case B extends File(2)
  case C extends File(3)
  case D extends File(4)
  case E extends File(5)
  case F extends File(6)
  case G extends File(7)
  case H extends File(8)

object File:
  val allFiles: List[File] = File.values.toList
  extension (f: File)
    def predecessor: Option[File] =
      val values = File.values.toList
      val index = values.indexOf(f)
      if (index == 0) None
      else Some(values(index - 1))

    def successor: Option[File] =
      val values = File.values.toList
      val index = values.indexOf(f)
      if (index == values.length -1) None
      else Some(values(index + 1))
