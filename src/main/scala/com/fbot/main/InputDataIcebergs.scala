package com.fbot.main

import java.io.File
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

import com.fbot.common.data.MultiSeries
import com.fbot.common.fastcollections.ImmutableArray
import com.fbot.common.fastcollections.ImmutableArray._
import com.fbot.common.hyperspace.Tuple
import grizzled.slf4j.Logging
import org.apache.spark.SparkContext
import org.apache.spark.rdd.RDD
import org.codehaus.jackson.annotate.{JsonCreator, JsonProperty}
import org.codehaus.jackson.map.annotate.JsonDeserialize
import org.codehaus.jackson.map.{DeserializationContext, JsonDeserializer, ObjectMapper}
import org.codehaus.jackson.{JsonParser, JsonToken}

/**
  * TODO:
  * - add weights to nearest neighbor algo: design WeightedTuple and modify MutualInformation & NearestNeighbors to use it
  * - implement selected cluster and add one point optimally to minimize F
  * still 1604x8424 MI computations, so that doesn't work, so we need to cluster/the training set down to ~40 clusters
  * how to do this... do we overlay the multidimensional probability spaces, or do we pick cluster representatives?
  * - harness the power of GPUs...???
  */
case class InputDataIcebergs(implicit sc: SparkContext) extends TestData with Logging {

  import InputDataIcebergs._

  def data: MultiSeries = {
    val maxData = 20
    val reIngestJson = true

    val objectFileName = if (reIngestJson) {
      //    band1: min = -45.594448 dB; max = 34.574917 dB
      //    band2: min = -45.655499 dB; max = 20.154249 dB
      //    angle; min = 24.7546      ; max = 45.9375
      // training 1604 pics
      // test     8424 pics
      val jsonFileName = "data/statoil/processed/train.json"
      val objectFileName = s"temp_data/Iceberg_${ZonedDateTime.now().format(DateTimeFormatter.ofPattern("yyyyMMdd'T'HHmmss")) }"
      jsonToObjectFile(jsonFileName, objectFileName, maxData)
      objectFileName
    } else {
      s"temp_data/Iceberg_20180107T061738"
    }

    // load data
    val data: RDD[(Long, ImmutableArray[Tuple])] = sc.objectFile(s"$objectFileName")

    MultiSeries(data.cache(), maxData)
  }


  private def jsonToObjectFile(jsonFileName: String, objectFileName: String, maxData: Int): Unit = {
    val mapper = new ObjectMapper()
    val parser = mapper.getJsonFactory.createJsonParser(new File(jsonFileName))

    val token = parser.nextToken()

    // the first token is supposed to be the start of json array '['
    if (token == null || !JsonToken.START_ARRAY.equals(token)) {
      throw new IllegalArgumentException(s"File $jsonFileName not correctly formatted")
    }

    // ingest data by iterating through the content of the json array
    var i = 0
    var RDDData = sc.emptyRDD[(Long, ImmutableArray[Tuple])]
    while (i < maxData && parser.nextToken() != JsonToken.END_ARRAY) {
      val node = parser.readValueAsTree()

      val entry = mapper.treeToValue(node, classOf[InputData])
      println(f"$i%4d: ${entry.id } ${entry.incAngle.getOrElse(90d) }%5.2f ${entry.isIceberg }")

      val data = Array((i.toLong, jsonDataToTuples(entry)))

      // RDD[(ArrayIndex, ImmutableArray[Tuple])]
      RDDData = RDDData ++ sc.parallelize(data, 1)

      i += 1
    }

    RDDData.coalesce(sc.defaultParallelism * 4) // reduce the number of partitions down to 32 (or keep current partitions)

    RDDData.saveAsObjectFile(s"$objectFileName")
  }


  private def jsonDataToTuples(entry: InputData): ImmutableArray[Tuple] = {
    val band1 = ImmutableArray(entry.band1).mapWithIndex[(Double, Tuple), ImmutableArray[(Double, Tuple)]]((value, pos) => {
      val x = pos.toInt % 75
      val y = pos.toInt / 75

      val freq = value + 50f
      // value = -45.6dB - 34.6dB
      (freq, Tuple(x.toDouble, y.toDouble, 0f, entry.incAngle.getOrElse(90f)))
    })

    val band2 = ImmutableArray(entry.band2).mapWithIndex[(Double, Tuple), ImmutableArray[(Double, Tuple)]]((value, pos) => {
      val x = pos.toInt % 75
      val y = pos.toInt / 75

      val freq = value + 50f
      // value = -45.6dB - 34.6dB
      (freq, Tuple(x.toDouble, y.toDouble, 1f, entry.incAngle.getOrElse(90f)))
    })

    val dataTuples = (band1 ++ band2).sortBy(_._1).map(x => ImmutableArray.fill(x._1.toInt)(x._2)).flatten[Tuple, ImmutableArray[Tuple]]

    if (dataTuples.length < 200000) throw new IllegalArgumentException("picture does not contain enough tuples")

    dataTuples.take(200000)
  }

}


object InputDataIcebergs {

  @JsonCreator
  case class InputData(@JsonProperty("id") id: String,
                       @JsonProperty("band_1") band1: Array[Double],
                       @JsonProperty("band_2") band2: Array[Double],
                       @JsonProperty("inc_angle") @JsonDeserialize(using = classOf[OptionalDoubleDeserializer]) incAngle: Option[Double],
                       @JsonProperty("is_iceberg") isIceberg: Boolean)


  class OptionalDoubleDeserializer extends JsonDeserializer[Option[Double]] {

    def deserialize(jsonParser: JsonParser, deserializationContext: DeserializationContext): Option[Double] = {
      val doubleString = jsonParser.getText

      doubleString match {
        case "na" => None
        case _ => Some(doubleString.toDouble)
      }
    }

  }

}