/* Richard Hwang and David Huang */
/* CS294-1 */
/* Spring 2013 */

/* Hey David:
 *   To run this first "source path.sh" from within the BIDMat directory.
 *   Then, compile as normally.  Finally run
 *   "scala -J-Xmx32G -cp $BIDMAT_LIBS class_name
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
  val tokens_file = "/Users/richard/classes/294-1/hw2/data/tokens.bin"
  val mat_file = "/Users/richard/classes/294-1/hw2/data/tokenized.mat"
  val processed_x_path = "/Users/richard/classes/294-1/hw2/data/processed.mat"

  var X = zeros(1,1)
  var x_squared = zeros(1,1)
  var y_times_X = zeros(1,1)

  var d = 0

  /** Processes the provided tokenized mat file into X and saves it. */
  def process() = {
    val tokens:IMat = load(mat_file, "tokens")
    println("loaded tokens")
    val smap:CSMat=load(mat_file, "smap")
    println("loaded smap")
    val scnt:IMat=load(mat_file, "scnt")
    println("loaded scnt")

    d = scnt.nrows
    val num_tokens = tokens.ncols
    var review_i = -1
    var cur_col:IMat = izeros(1,1)
    var cur_tid:Int = 0

    // Make sparse matrix X via concatenation of columns
    for (i <- 0 to num_tokens-1) {
      cur_col = tokens(?, i)
      cur_tid = cur_col(2,0)
      if (smap{cur_tid} == "<review>") {
        review_i += 1
      }
    }

    //saveAs(processed_x_path, X, "X", Y, "labels")
  }

  /** Precalculates X^2 and yX for use in l2_gradient. */
  def pre_calculate_gradient() = {
    // TODO
  }

  /** Calculates the L2 gradient at beta, where
    *   L_2(beta) = E[-2y^T X + 2X^T X beta]
    */
  def l2_gradient(beta: BIDMat.FMat):BIDMat.FMat = {
    return 2*x_squared*beta - 2*y_times_X
  }

  /** Performs stochastic gradient descent (SGD) to minimize the L_2 loss
    * function, returning the vector beta of parameters. */
  def train():BIDMat.FMat = {
    val gamma = 0.25

    // Perform SGD
    val sgd_tolerance = 0.0001
    var beta:BIDMat.FMat = zeros(1,d)
    var beta_prev:BIDMat.FMat = zeros(1,d)

    do {
      beta_prev = beta
      beta = beta - gamma * l2_gradient(beta)
    } while (true)

    return beta
  }

  /** Returns the vector of predictions for input X, an nxd matrix,
    *   y_hat = beta_hat * X_hat */
  def predict(beta: BIDMat.FMat, x: BIDMat.FMat):BIDMat.FMat = {
    return beta * x
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
    val tokens:IMat = load(mat_file, "tokens")
    println("loaded tokens")
    val smap:CSMat=load(mat_file, "smap")
    println("loaded smap")

    println(smap{tokens(3,0)})
    println(smap{tokens(3,1)})
    println(smap{tokens(3,2)})
    println(smap{tokens(3,3)})
    println(smap{tokens(3,4)})
    println(smap{tokens(3,5)})
    println(smap{tokens(3,6)})
    //process()
    //cross_validate(10)
  }

}
