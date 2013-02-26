/* Richard Hwang and David Huang */
/* CS294-1 */
/* Spring 2013 */

/* Hey David:
 *   To run this first "source path.sh" from within the BIDMat directory.
 *   Then, compile as normally.  Finally run
 *   "scala -cp $BIDMAT_LIBS class_name
 */

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

  // File paths
  val data_file = "/Users/rhwang/classes/294-1/hw2/data/tokenized.mat"
  //val data_file = "/Users/Davidius/294-1-hw2/data/tokenized.mat"
  val processed_x_path = "/Users/richard/classes/294-1/hw2/data/processed.mat"

  /** Processes the provided tokenized mat file into X and saves it. */
  def process() = {
    val tokens:IMat = load(data_file, "tokens")
    val smap:CSMat=load(data_file, "smap")
    val scnt:IMat=load(data_file, "scnt")
  }

  /** Calculates the L2 gradient at beta, where
    *   L_2(beta) = E[-2y^T X + 2X^T X beta]
    */
  def l2_gradient(beta: BIDMat.SMat):Double = {
    return 0
  }

  /** Performs stochastic gradient descent (SGD) to minimize the L_2 loss
    * function, returning the vector beta of parameters. */
  def train() = {
    // Preprocess matrices

    // Perform SGD
    val sgd_tolerance = 0.0001
    var beta = 0

    while (true) {
      println("Not implemented")
      // Update
      //beta = beta + gamma * l2_gradient(beta)
    }
  }

  /** Returns the vector of predictions for input X, an nxd matrix,
    *   y_hat = beta_hat * X_hat */
  def predict(beta: BIDMat.SMat, X: BIDMat.SMat) = {
  }

  /** Performs k-fold cross validation, computing AUC and 1% lift scores. */
  def cross_validate(k: Int):Double = {
    // Partition data
    for (i <- 1 to k) {
      // Train
      // Test
      // Save accuracy measures
    }
    // Compute measures
    return 0
  }

  def main(args: Array[String]) = {
    process()
    cross_validate(10)
  }

}
