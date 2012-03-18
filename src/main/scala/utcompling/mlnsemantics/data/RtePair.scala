package utcompling.mlnsemantics.data

case class RtePair(
  id: String,
  premise: List[String],
  hypothesis: String,
  entailed: Option[Boolean]) {

}

class ExampleChoosingRtePairReaderDecorator(ids: Set[String], reader: DataReader[RtePair]) extends DataReader[RtePair] {

  override def read() =
    (for (pair <- reader.read()) yield {
      if (ids.contains(pair.id))
        Some(pair)
      else
        None
    }).flatten

}
