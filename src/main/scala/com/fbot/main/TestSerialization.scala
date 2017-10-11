package com.fbot.main

import java.io.{FileInputStream, FileOutputStream, ObjectInputStream, ObjectOutputStream}

import com.esotericsoftware.kryo.Kryo
import com.esotericsoftware.kryo.io.{Input, Output}

/**
  *
  * https://ogirardot.wordpress.com/2015/01/09/changing-sparks-default-java-serialization-to-kryo/
  */
object TestSerialization extends App {

  // (1) create a Stock instance
  val array: Array[Double] = Array(1d, 2d, 3d, 4d, 5d, 6d, 7d)

  // (2) write the instance out to a file
  val out1 = new ObjectOutputStream(new FileOutputStream("/tmp/std"))
  out1.writeObject(array)
  out1.close()

  {
    val out2 = new Output(new FileOutputStream("/tmp/kryo"))
    val kryo = new Kryo()
    kryo.writeObject(out2, array)
    out2.close()
  }

  // (3) read the object back in
  val in1 = new ObjectInputStream(new FileInputStream("/tmp/std"))
  val decodedArray1 = in1.readObject.asInstanceOf[Array[Double]]
  in1.close()

  val decodedArray2 = {
    val in2 = new Input( new FileInputStream("/tmp/kryo"))
    val kryo = new Kryo()
    val data = kryo.readObject(in2, classOf[Array[Double]])
    in2.close()
    data
  }

  // (4) print the object that was read back in
  println(for(i <- decodedArray1) println(i))
  println()
  println(for(i <- decodedArray2) println(i))

}
