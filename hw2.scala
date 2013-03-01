/* Richard Hwang and David Huang */
/* CS294-1 */
/* Spring 2013 */

/*   To run this, first "source path.sh" from within the BIDMat directory.
 *   Then, compile with "bin/sbt compile".  Finally run
 *   "scala -J-Xmx32G -cp $BIDMAT_LIBS RegressionModel"
 */

import scala.io._
import scala.sys.process._
import scala.collection.mutable
import scala.math
import scala.util.control.Breaks._
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
  //val mat_file = "/Users/richard/classes/294-1/hw2/data/tokenized.mat"
  val mat_file = "/Users/Davidius/294-1-hw2/data/tokenized.mat"
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
    var cur_rating = 0
    val num_tokens = tokens.ncols
    var review_i = -1
    var pre_i = 0
    var icol_row:IMat = izeros(0, 0)
    var icol_col:IMat = izeros(0, 0)
    var vals:FMat = zeros(0, 0)
    var cur_counts = mutable.Map.empty[Int, Int]
    var sentinel_token = 0

    // Make sparse matrix X via concatenation of columns
    breakable { while (true) {
      var cur_col:IMat = tokens(?,pre_i)
      var cur_token_id:Int = cur_col(2,0) - 1
      var cur_string: String = smap{cur_token_id}

      // New review
      if (cur_string == "<review>") {
        review_i += 1
        cur_counts = mutable.Map.empty[Int, Int]
      // Finished review
      } else if (cur_string == "</review>") {
        icol_col = izeros(cur_counts.size, 0)

        // Get random el
        sentinel_token = cur_counts.keys.iterator.next()
        icol_row = icol(sentinel_token)
        vals = col(cur_counts(sentinel_token))
        // rm that el
        cur_counts remove sentinel_token

        cur_counts.foreach(t => {
          icol_row = icol_row on t._1
          vals = vals on t._2
        })
        var X:SMat = sparse(icol_row, icol_col, vals, d, 1)
        break
      // Found rating
      } else if (cur_string == "<rating>") {
        if (smap{cur_token_id + 1} != "</rating>") {
          println("Got a rating number!\n\n")
          cur_rating = Integer.parseInt(smap{cur_token_id + 1})
        }
      // Normal token
      } else if (cur_string != "<unique_id" && cur_string != "</unique_id>" &&
          cur_string != "<product_type>" && cur_string != "</product_type>" &&
          cur_string != "<asin>" && cur_string != "</asin>" &&
          cur_string != "</rating>") {
        if (!cur_counts.keySet.exists(_ == cur_token_id)) {
          cur_counts(cur_token_id) = 1
        } else {
          cur_counts(cur_token_id) += 1
        }
      }

      pre_i += 1
    }}

    for (i <- 0 to pre_i) {
      println(smap{tokens(2,i) + 1})
    }
    println("next review\n\n")
    for (i <- pre_i to 2*pre_i) {
      println(smap{tokens(2,i) + 1})
    }
    X
    //for (i <- pre_i+1 to num_tokens-1) {
    //  cur_col = tokens(?, i)
    //  cur_token_id = cur_col(2,0)
    //  cur_string = smap{cur_token_id - 1}

    //  // New review
    //  if (cur_string == "<review>") {
    //    review_i += 1
    //    cur_counts = mutable.Map.empty[Int, Int]
    //  } else if (cur_string == "</review>") {
    //    // Concat current Sparse Mat to X
    //  } else if (cur_string == "<rating>") {
    //    cur_rating = Integer.parseInt(smap{cur_token_id + 1 - 1})
    //  }
    //}

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
    // Compute measures (AUC & 1% lift score)
    return 0
  }

  def main(args: Array[String]) = {
    process()
    //cross_validate(10)
  }

}
