package utcompling.mlnsemantics.data

import scala.xml._

class ExampleChoosingRtePairReaderDecorator(ids: Set[String], reader: DataReader[RtePair]) extends DataReader[RtePair] {

    override def read() =
        (for (pair <- reader.read()) yield {
            if (ids.contains(pair.id))
                Some(pair)
            else
                None
        }).flatten

}
