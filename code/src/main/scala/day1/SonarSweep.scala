package com.nicolaspetras
package day1

import scala.io.Source
import scala.language.postfixOps

object SonarSweep extends App {

  /**
   * Calculates how many times the depth increased.
   * @return No of times depth increased
   */
  def calculateDepthIncrease(depthList: Array[String]): Int = {
    def depthCalcHelper(depthList: Array[String], index: Int, lastIndexVal: Int, noDepthIncreases: Int): Int = {
      if (index == depthList.length) noDepthIncreases
      else {
        val noDepthIncreasesTemp = if(depthList(index).toInt > lastIndexVal) {
          noDepthIncreases + 1
        } else {
          noDepthIncreases
        }
        depthCalcHelper(depthList, index + 1, depthList(index).toInt, noDepthIncreasesTemp)
      }
    }
    depthCalcHelper(depthList, 1, depthList(0).toInt, 0)
  }

  val fileList = Source.fromFile("./day1.txt").getLines.toArray
  val noDepthIncreases = calculateDepthIncrease(fileList)
  println(noDepthIncreases)
}
