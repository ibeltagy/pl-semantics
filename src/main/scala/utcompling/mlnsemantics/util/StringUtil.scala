package utcompling.mlnsemantics.util

import java.io.BufferedReader
import java.io.Reader
import java.io.StringReader
import scala.collection.JavaConversions.asScalaIterator
import edu.stanford.nlp.process.PTBTokenizer
import edu.stanford.nlp.process.WordTokenFactory

/**
 * Copyright 2013 Jason Baldridge
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *     http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/**
 * A function that turns a double like .9324823 into a percent like 93.25.
 */
object DecimalToPercent extends (Double => String) {
  lazy val df = new java.text.DecimalFormat("##.##")
  def apply(num: Double) = df.format(num*100)
}


