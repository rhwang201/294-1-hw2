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
  val data_file = "/Users/richard/classes/294-1/hw2/data/tokenized.mat"
  val processed_x_path = "/Users/richard/classes/294-1/hw2/data/processed.mat"

  var x_squared = zeros(1,1)
  var y_times_x = zeros(1,1)

  var d = 0

  /** Processes the provided tokenized mat file into X and saves it. */
  def process() = {
    //val tokens:IMat = load(data_file, "tokens")
    //val smap:CSMat=load(data_file, "smap")
    //val scnt:IMat=load(data_file, "scnt")

    // Let's try printing out tokens
    //println(smap{tokens(3,0)})
    //println(smap{tokens(3,1)})
    //println(smap{tokens(3,2)})
    //println(smap{tokens(3,3)})
    //println(smap{tokens(3,4)})

    //saveAs(processed_x_path, X, "X", Y, "labels")
  }

  /** Precalculates X^2 and yX for use in l2_gradient. */
  def pre_calculate_gradient() = {
    // TODO
  }

  /** Calculates the L2 gradient at beta, where
    *   L_2(beta) = E[-2y^T X + 2X^T X beta]
    */
  def l2_gradient(beta: BIDMat.SMat):Double = {
    return 2*x_squared * beta - 2*y_times_X
  }

  /** Performs stochastic gradient descent (SGD) to minimize the L_2 loss
    * function, returning the vector beta of parameters. */
  def train() = {
    val gamma = 0.25

    // Perform SGD
    val sgd_tolerance = 0.0001
    var beta = zeros(1,d)
    do {
      beta_prev = beta
      beta = beta - gamma * l2_gradient(beta)
    } while (beta - beta_prev > sgd_tolerance ||
        beta_prev - beta> sgd_tolerance)

    return beta
  }

  /** Returns the vector of predictions for input X, an nxd matrix,
    *   y_hat = beta_hat * X_hat */
  def predict(beta: BIDMat.SMat, x: BIDMat.SMat) = {
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
    var a = new DataInputStream(new FileInputStream("tokens.bin"))
    var b = java.lang.Integer.reverseBytes(a.readInt())
    //process()
    //cross_validate(10)
  }

}
