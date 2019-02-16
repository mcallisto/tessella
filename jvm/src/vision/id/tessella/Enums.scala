package vision.id.tessella

object LabelStyle extends Enumeration {
  type LabelStyle = Value
  val ALL, PERIMETER_ONLY, NONE = Value
}

object MarkStyle extends Enumeration {
  type MarkStyle = Value
  val GONALITY, NONE = Value
}
