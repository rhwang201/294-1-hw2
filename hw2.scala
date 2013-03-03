/* Richard Hwang and David Huang */
/* CS294-1 */
/* Spring 2013 */

/*   To run this, first "source path.sh" (if you've started a new shell)
 *   from within the BIDMat directory. Then, compile with "bin/sbt compile".
 *   Finally run "scala -J-Xmx32G -cp $BIDMAT_LIBS RegressionModel"
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
  //val processed_x_path = "/Users/richard/classes/294-1/hw2/data/processed.mat"
  val processed_x_path = "/Users/Davidius/294-1-hw2/data/processed.mat"

  // Initialize matrices
  var X = sprand(1,1, 0.5)
  var Y = sprand(1,1, 0.5)
  var x_squared = zeros(1,1)
  var y_times_X = zeros(1,1)

  var d = 0

  /** Processes the provided tokenized mat file into X, Y and saves it. */
  def process() = {
    val tokens:IMat = load(mat_file, "tokens")
    println("loaded tokens")
    val smap:CSMat=load(mat_file, "smap")
    println("loaded smap")
    val scnt:IMat=load(mat_file, "scnt")
    println("loaded scnt")

    d = scnt.nrows
    var got_rating = false
    var cur_rating = 0.0
    val num_tokens = tokens.ncols
    println("num_tokens = " + num_tokens)
    var review_count = 0
    var pre_i = 0
    var icol_row:IMat = izeros(0, 0)
    var icol_col:IMat = izeros(0, 0)
    var vals:FMat = zeros(0, 0)
    var cur_counts = mutable.Map.empty[Int, Int]
    var sentinel_token = 0
    val first_review = 280
    var rating_now = false

    // Make sparse matrix X via concatenation of columns
    for (pre_i <- 0 to first_review) {
      var cur_col:IMat = tokens(?,pre_i)
      var cur_token_id:Int = cur_col(2,0) - 1
      var cur_string: String = smap{cur_token_id}

      // New review
      if (cur_string == "<review>") {
        review_count += 1
        println("currently processing review number " + review_count + " @ " + System.currentTimeMillis)
        cur_counts = mutable.Map.empty[Int, Int]
      // Finished review
      } else if (cur_string == "</review>") {
        if (got_rating) {
          icol_col = izeros(cur_counts.size, 1)

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
          X = sparse(icol_row, icol_col, vals, d, 1)
          Y = sparse(izeros(1,1), izeros(1,1), cur_rating, 1, 1)
          got_rating = false
        }
      // Found rating
      } else if (cur_string == "<rating>") {
        rating_now = true
      // At rating number
      } else if (rating_now) {
        if (cur_string != "</rating>") {
          cur_rating = cur_string.toDouble
          got_rating = true
          rating_now = false
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
    }

    for (i <- 281 to num_tokens) {
      var cur_col:IMat = tokens(?,i)
      var cur_token_id:Int = cur_col(2,0) - 1
      var cur_string: String = smap{cur_token_id}

      // New review
      if (cur_string == "<review>") {
        review_count += 1
        if (review_count % 1000 == 0) {
        println("currently processing review number " + review_count + " @ " + System.currentTimeMillis)
        }
        cur_counts = mutable.Map.empty[Int, Int]
      // Finished review
      } else if (cur_string == "</review>") {
        if (got_rating) {
          icol_col = izeros(cur_counts.size, 1)

          // Get random el
          sentinel_token = cur_counts.keys.iterator.next()
          icol_row = icol(sentinel_token)
          vals = col(cur_counts(sentinel_token))
          // rm that el
          cur_counts remove sentinel_token

          // add vals
          cur_counts.foreach(t => {
            icol_row = icol_row on t._1
            vals = vals on t._2
          })
          X = X \ sparse(icol_row, icol_col, vals, d, 1)
          Y = Y on sparse(izeros(1,1), izeros(1,1), cur_rating, 1, 1)
          got_rating = false
          if (review_count % 100000 == 0) {
            saveAs(processed_x_path, X, "X", Y, "Y")
            println("First %s reviews saved" + review_count)
          }
        }
      // Found rating
      } else if (cur_string == "<rating>") {
        rating_now = true
      // At rating number
      } else if (rating_now) {
        if (cur_string != "</rating>") {
          cur_rating = cur_string.toDouble
          got_rating = true
          rating_now = false
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
    }

    saveAs(processed_x_path, X, "X", Y, "Y")
  }

  /** Precalculates X^2 and yX for use in l2_gradient. */
  //def pre_calculate_gradient() = {
  //  x_squared = zeros(d,d)
  //  var block = zeros(1,1)
  //  for (i <- 0 to block_n) {
  //    block = X(?,block_size)
  //    x_squared = x_squared + block * block.t
  //  }
  //  x_squared = (1/block_size) * x_squared

  //  y_times_X = zeros()
  //  for (i <- 0 to block_n) {
  //    //y_times_X = y_times_X + // TODO
  //  }
  //  y_times_X = (1/block_size) * y_times_x
  //}

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
    var training_set = zeros(1,1)
    var testing_set = zeros(1,1)

    var beta = zeros(1,1);
    var predictions = zeros(1,1);

    for (i <- 1 to k) {
      // Get training/testing


      // Train
      //beta = train(training_set)

      // Test
      //predictions = predict(beta, testing_set)

      // Calculate tp, fp, tn, fn
      // Compute measures (AUC & 1% lift score) WTF
    }
    // Plot error vs training_size
    return 0
  }

  def main(args: Array[String]) = {
    process()
    //cross_validate(10)
  }

}
