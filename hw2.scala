/* Richard Hwang and David Huang */
/* CS294-1 */
/* Spring 2013 */

import scala.io._
import scala.sys.process._
import scala.collection.mutable
import scala.math

import java.io._

import BIDMat.{Mat, FMat, DMat, IMat, CMat, BMat, CSMat, SMat, SDMat, GMat, GIMat, GSMat, HMat}
import BIDMat.MatFunctions._
import BIDMat.SciFunctions._
import BIDMat.Solvers._
import BIDMat.Plotting._
//import stemmer.Stemmer

object RegressionModel {

  val data_file = "/Users/richard/classes/294-1/hw2/data/tokenized.mat"

  /** Performs stochastic gradient descent (SGD) to minimize the L_2 loss
    * function, returning the vector beta of parameters. */
  def train() = {
    // Preprocess matrices

    // Perform SGD
    sgd_tolerance = 0.0001
    while (True) {
      println("Not implemented")
      // Update
    }
  }

  /** Returns the vector of predictions for input X, an nxd matrix. */
  def predict() = {
  }

  def main(args: Array[String]) = {
    // Create model in blocks
    //val tokens:IMat = load(data_file, "tokens")
    //val smap:CSMat=load(data_file, "smap")

    // TODO Can't load files
    val scnt:CSMat=load(data_file, "scnt")
    println("success")
  }

}
