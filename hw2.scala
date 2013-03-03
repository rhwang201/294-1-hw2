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

/** Performs linear regression on amazon.com reviews. */
object RegressionModel {

  // File paths
  val tokens_file = "/Users/richard/classes/294-1/hw2/data/tokens.bin"
  val mat_file = "/Users/Davidius/294-1-hw2/data/tokenized.mat"
  val processed_x_path = "/Users/richard/classes/294-1/hw2/data/processed"
  //val processed_x_path = "/Users/Davidius/294-1-hw2/data/processed"

  // Initialize matrices
  var X:SMat = sprand(1,1, 0.5) // dxn data matrix
  var Y = sprand(1,1, 0.5) // 1xn label row vector
  var Y_t = sprand(1,1, 0.5)
  var x_squared = zeros(1,1) // dxd matrix
  var y_times_X = zeros(1,1) // 1xd row vector

  val block_size = 10
  var block_n = 1
  var block_remainder = 0

  var d = 0
  var n = 0

  val sgd_tolerance = 0.001
  val gamma = 0.25

  val k = 0

  /** Processes the provided tokenized mat file into X, Y and saves it. */
  def process() = {
    var time = System.currentTimeMillis
    val tokens:IMat = load(mat_file, "tokens")
    println( "tokens loaded in %s seconds".format((System.currentTimeMillis - time)/1000.0) )
    time = System.currentTimeMillis
    val smap:CSMat=load(mat_file, "smap")
    println( "smap loaded in %s seconds".format((System.currentTimeMillis - time)/1000.0) )
    time = System.currentTimeMillis
    val scnt:IMat=load(mat_file, "scnt")
    println( "scnt loaded in %s seconds".format((System.currentTimeMillis - time)/1000.0) )

    d = scnt.nrows
    var got_rating = false
    var cur_rating = 0.0
    val num_tokens = tokens.ncols
    var pre_i = 0
    var icol_row:IMat = izeros(0, 0)
    var icol_col:IMat = izeros(0, 0)
    var vals:FMat = zeros(0, 0)
    var cur_counts = mutable.Map.empty[Int, Int]
    val first_review = 280
    var rating_now = false

    var review_count = 0
    val probe_step = 2000
    val save_step = 10000
    var next_block = false
    val start = 2000000

    // Make sparse matrix X via concatenation of columns
    for (pre_i <- 0 to first_review) {
      var cur_col:IMat = tokens(?,pre_i)
      var cur_token_id:Int = cur_col(2,0) - 1
      var cur_string: String = smap{cur_token_id}

      // New review
      if (cur_string == "<review>") {
        review_count += 1
          println( "currently processing review number %s at token %s; previous batch took %s seconds.".format(review_count, pre_i, (System.currentTimeMillis - time)/1000.0) )
        cur_counts = mutable.Map.empty[Int, Int]
      // Finished review
      } else if (cur_string == "</review>") {
        if (got_rating) {
          icol_col = izeros(cur_counts.size, 1)
          icol_row = icol(cur_counts.keys.toList)
          vals = col(cur_counts.values.toArray)

          X = sparse(icol_row, icol_col, vals, d, 1)
          Y = sparse(izeros(1,1), izeros(1,1), cur_rating, 1, 1)
          got_rating = false
          n = n + 1
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

    time = System.currentTimeMillis
    for (i <- start to num_tokens) {
      var cur_col:IMat = tokens(?,i)
      var cur_token_id:Int = cur_col(2,0) - 1
      var cur_string: String = smap{cur_token_id}

      // New review
      if (cur_string == "<review>") {
        review_count += 1
        if (review_count % probe_step == 0) {
          println( "currently processing review number %s at token %s; previous batch took %s seconds.".format(review_count, pre_i, (System.currentTimeMillis - time)/1000.0) )
          time = System.currentTimeMillis
        }
        cur_counts = mutable.Map.empty[Int, Int]
      // Finished review
      } else if (cur_string == "</review>") {
        if (got_rating) {
          icol_col = izeros(cur_counts.size, 1)
          icol_row = icol(cur_counts.keys.toList)
          vals = col(cur_counts.values.toArray)

          if (next_block) {
            X = sparse(icol_row, icol_col, vals, d, 1)
            Y = sparse(izeros(1,1), izeros(1,1), cur_rating, 1, 1)
            next_block = false
          } else {
            X = X \ sparse(icol_row, icol_col, vals, d, 1)
            Y = Y \ sparse(izeros(1,1), izeros(1,1), cur_rating, 1, 1)
          }
          got_rating = false

          if (review_count % save_step == 0) {
            time = System.currentTimeMillis            
            saveAs(processed_x_path+"%s.mat".format(review_count/save_step), X, "X", Y, "Y")
            println( "batch number %s saved using %s seconds.".format(review_count/save_step, (System.currentTimeMillis - time)/1000.0) )
            next_block = true
            time = System.currentTimeMillis       
          }

          n = n + 1
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

    saveAs(processed_x_path+"last.mat", X, "X", Y, "Y")
    println("Number of reviews: %s; number of ratings: ".format(review_count, n))
  }

  /** Concatenates l_i blocks of X and Y. */
  def loadX(l_i:Int) = {
    val processed_template = processed_x_path + "%d.mat"
    X = load(processed_template.format(1), "X")
    Y = load(processed_template.format(1), "Y")
    for (i <- 2 to l_i) {
      X = X \ load(processed_template.format(i), "X")
      Y = Y \ load(processed_template.format(i), "Y")
    }
    println("Finished loading and constructing X and Y")
  }

  /** Precalculates XX^T and YX_T for use in l2_gradient. */
  def pre_calculate_gradient(X:BIDMat.SMat) = {
    block_n = n / block_size
    block_remainder = n % block_size

    x_squared = zeros(d,d)
    y_times_X = zeros(1,d)
    var block = sprand(1,1, 0.5)
    var block_transposed = sprand(1,1, 0.5)
    for (i <- 0 to block_n - 1) {
      block = X(?, block_size*i to block_size*(i+1) - 1)
      block_transposed = block.t
      x_squared = x_squared + block * block_transposed
      y_times_X = y_times_X + Y * block_transposed 
    }
    block = X(?, block_size*block_n to block_size*block_n + block_remainder)
    x_squared = x_squared + block * block_transposed
    y_times_X = y_times_X + Y * block_transposed 

    x_squared = (1/block_size) * x_squared
    y_times_X = (1/block_size) * y_times_X
  }

  /** Calculates the L2 gradient at beta, where
    *   L_2(beta) = E[-2y*X^T + 2*beta*X*X^T] + regularizer(beta)
    */
  def l2_gradient(beta: BIDMat.FMat):BIDMat.FMat = {
    return 2 * beta * x_squared - 2 * y_times_X
  }

  /* Returns sum((v1 - v2).^2) */
  def rss(v1: BIDMat.FMat, v2: BIDMat.FMat):Float = {
    return 0 // TODO
  }

  /** Performs stochastic gradient descent (SGD) to minimize the L_2 loss
    * function, returning the vector beta of parameters. */
  def train(x:BIDMat.SMat):BIDMat.FMat = {
    pre_calculate_gradient(x)

    // SGD
    var beta:BIDMat.FMat = zeros(1,d)
    var beta_prev:BIDMat.FMat = zeros(1,d)

    do {
      beta_prev = beta
      beta = beta - gamma * l2_gradient(beta)
    } while (rss(beta,beta_prev) > sgd_tolerance)

    return beta
  }

  /** Returns the vector of predictions for input X, an dxn matrix,
    *   y_hat = beta_hat * X_hat */
  def predict(beta: BIDMat.FMat, x: BIDMat.SMat):BIDMat.FMat = {
    return beta * x
  }

  // TODO WHAT should be sparse, what should be dense

  /** Performs k-fold cross validation, computing AUC and 1% lift scores. */
  def cross_validate(k: Int):Double = {
    // Partition data
    val set_size = n / k
    var training_set:BIDMat.SMat = sprand(1,1,0.5)
    var testing_set:BIDMat.SMat = sprand(1,1,0.5)

    var beta:BIDMat.FMat = zeros(1,1);
    var predictions:BIDMat.FMat = zeros(1,1);

    for (i <- 1 to k) {
      // Get training/testing
      if (i != k) {
        training_set = X(?, 0 to (i-1)*set_size - 1) \ X(?, i*set_size to n - 1)
        testing_set = X(?, (i-1)*set_size to i*set_size - 1)
      } else {
        training_set = X(?, 0 to (i-1)*set_size - 1) \ X(?, i*set_size to n - 1)
        testing_set = X(?, (i-1)*set_size to n - 1)
      }

      // Train
      beta = train(training_set)

      // Test
      predictions = predict(beta, testing_set)

      // Calculate tp, fp, tn, fn
      // sort pred
      // for each pred
      //   compute fpr and tpr as if pred is threshold
      // go through scores, looking for ticks

    }
    // Plot error vs training_size
    return 0
  }

  /** Main Method */
  def main(args: Array[String]) = {
    val command = args(0)
    if (command == "process") {
      process()
    } else if (command == "load") {
      val l_i = args(1)
      loadX(l_i.toInt)
    } else if (command == "cross_validate") {
      cross_validate(10)
    }
  }

}
