package com.nicolaspetras
package day1

import scala.io.Source
import scala.language.postfixOps

/**
 * Used to calculate the number depth increases in a data set of sonar sweep from a submarine that was descending
 * to recover. Each line in the data states the depth of the submarine when the sonar sweep was made.
 */
object SonarSweep extends App {

  /**
   * Calculates how many times the depth increased.
   * @param depthArr The data set from the sonar sweeps
   * @return No of times depth increased
   */
  def calculateDepthIncrease(depthArr: Array[String]): Int = {
    def depthCalcHelper(depthArr: Array[String], index: Int, lastIndexVal: Int, noDepthIncreases: Int): Int = {
      if (index == depthArr.length) noDepthIncreases
      else {
        val noDepthIncreasesTemp = if(depthArr(index).toInt > lastIndexVal) {
          noDepthIncreases + 1
        } else {
          noDepthIncreases
        }
        depthCalcHelper(depthArr, index + 1, depthArr(index).toInt, noDepthIncreasesTemp)
      }
    }
    depthCalcHelper(depthArr, 1, depthArr(0).toInt, 0)
  }

  /**
   * Calculates how many times the depth increased within a 3-measurement sliding window.
   * @param depthArr The data set from the sonar sweeps
   * @return No of times depth increased, within the 3-measurement sliding window sums
   */
  def calculateSlidingDepthIncrease(depthArr: Array[String]): Int = {
    def depthCalcHelper(depthArr: Array[String], lastIndex: Int, lastSum: Int, noDepthIncreases: Int): Int = {
      if (lastIndex == depthArr.length) noDepthIncreases
      else {
        val currentSum = depthArr(lastIndex - 2).toInt + depthArr(lastIndex - 1).toInt + depthArr(lastIndex).toInt
        val noDepthIncreasesTemp = if(currentSum > lastSum) {
          noDepthIncreases + 1
        } else {
          noDepthIncreases
        }
        depthCalcHelper(depthArr, lastIndex + 1, currentSum, noDepthIncreasesTemp)
      }
    }
    val firstSum = depthArr(0).toInt + depthArr(1).toInt + depthArr(2).toInt
    depthCalcHelper(depthArr, 3, firstSum, 0)
  }

  // read file
  val fileArr = Source.fromFile("./day1.txt").getLines.toArray

  // part one
  val noDepthIncreases = calculateDepthIncrease(fileArr)
  println(noDepthIncreases)
  // part two
  val noDepthIncreasesSliding = calculateSlidingDepthIncrease(fileArr)
  println(noDepthIncreasesSliding)

}
