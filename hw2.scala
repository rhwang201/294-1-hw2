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
//import java.math._
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
  //val processed_x_path = "/Users/richard/classes/294-1/hw2/data/processed"
  val processed_x_path = "/Users/Davidius/294-1-hw2/data/processed"

  // Initialize matrices
  var X:SMat = sprand(1,1, 0.5) // dxn data matrix
  var Y:FMat = zeros(1,1) // 1xn label row vector
  var Y_t = sprand(1,1, 0.5)
  var x_squared = zeros(1,1) // dxd matrix
  var y_times_X = zeros(1,1) // 1xd row vector

  val block_size = 10
  var block_n = 1
  var block_remainder = 0

  var d = 0
  var n = 0

  val sgd_tolerance = 5
  val gamma = 0.00000002
  val diminisher = 0.9
  val lambda = 0.01
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
    var next_block = true
    val start = 2000000

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
            Y = row(cur_rating)
            next_block = false
          } else {
            X = X \ sparse(icol_row, icol_col, vals, d, 1)
            Y = Y \ cur_rating
          }
          got_rating = false

          if (review_count % save_step == 0) {
            time = System.currentTimeMillis
            saveAs(processed_x_path+"%s.mat".format(review_count/save_step), X, "X", Y, "Y")
            println( "batch number %s saved in %s seconds.".format(review_count/save_step, (System.currentTimeMillis - time)/1000.0) )
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

    saveAs(processed_x_path+"%s.mat".format(review_count/save_step+1), X, "X", Y, "Y")
  }

  /** To be called if we directly load X and Y, instead of creating it. */
  def setGlobals() = {
    n = X.ncols
    d = X.nrows
  }

  /** Concatenates l_i blocks of X and Y. */
  def loadX(l_i:Int) = {
    val processed_template = processed_x_path + "%d.mat"
    var cur:BIDMat.FMat = zeros(1,1)
    X = load(processed_template.format(1), "X")
    Y = load(processed_template.format(1), "Y")
    for (i <- 2 to l_i) {
      X = X \ load(processed_template.format(i), "X")
      cur = load(processed_template.format(i), "Y")
      Y = Y \ cur
    }
    saveAs("/Users/Davidius/294-1-hw2/data/processed.mat", X, "X", Y, "Y")
    println("Finished loading and constructing X and Y")
  }

  /** Calculates the L2 gradient at beta, where
    *   L_2(beta) = 2(\beta X - Y) X^T + regularizer(beta)
    */
  def l2_gradient(beta: BIDMat.FMat):BIDMat.FMat = {
    return 2 * (beta * X - Y) *^ X + 2 * lambda * beta
  }

  /** Performs stochastic gradient descent (SGD) to minimize the L_2 loss
    * function, returning the vector beta of parameters. */
  def train():BIDMat.FMat = {
    println("beginning stochastic gradient descent")
    var time = System.currentTimeMillis
    var l2_grad = row(1, 2)
    var beta:BIDMat.FMat = zeros(1,d)
    var beta_prev:BIDMat.FMat = zeros(1,d)
    var i = 1

    do {
      beta_prev = beta
      l2_grad = l2_gradient(beta)
      if (i < 14) {
        beta = beta - (gamma / scala.math.pow(i, 1.5)) * l2_grad
        println("iteration %s in %s seconds.\nmax(l2_grad) = %s\nbeta = %s".format( i, (System.currentTimeMillis-time)/1000.0, maxi(abs(l2_grad), 2)(0, 0), beta))
        time = System.currentTimeMillis        
      } else if (i < 50) {
        beta = beta - (gamma / scala.math.pow(i, 1.2)) * l2_grad
        println("iteration %s in %s seconds.\nmax(l2_grad) = %s\nbeta = %s".format( i, (System.currentTimeMillis-time)/1000.0, maxi(abs(l2_grad), 2)(0, 0), beta))
        time = System.currentTimeMillis
      } else {
        beta = beta - (gamma / scala.math.pow(i, 0.8)) * l2_grad
        println("iteration %s in %s seconds.\nmax(l2_grad) = %s\nbeta = %s".format( i, (System.currentTimeMillis-time)/1000.0, maxi(abs(l2_grad), 2)(0, 0), beta))
        time = System.currentTimeMillis
      }
      i = i + 1
    } while (maxi(abs(l2_grad), 2)(0,0) > sgd_tolerance)
    println("iteration %s in %s seconds.\nbeta = %s\nmax(l2_grad) = %s".format( i, (System.currentTimeMillis-time)/1000.0, beta, maxi(abs(l2_grad), 2)(0, 0)))
    println("convergence")
    return beta
  }

  /** Returns the row vector of predictions for input X, an dxn matrix,
    *   y_hat = beta_hat * X */
  def predict(beta: BIDMat.FMat, x: BIDMat.SMat):BIDMat.FMat = {
    return beta * x
  }

  /** Performs k-fold cross validation, computing AUC and 1% lift scores. */
  def cross_validate(k: Int):Double = {
    // Partition data
    val set_size = n / k
    var training_set:BIDMat.SMat = sprand(1,1,0.5)
    var testing_set:BIDMat.SMat = sprand(1,1,0.5)

    var beta:BIDMat.FMat = zeros(1,1);
    var predictions:BIDMat.FMat = zeros(1,1);

    for (i <- 1 to k) {
      println("Beginning fold %d".format(i))

      // Get training/testing
      if (i != k) {
        training_set = X(?, 0 to (i-1)*set_size - 1) \ X(?, i*set_size to n - 1)
        testing_set = X(?, (i-1)*set_size to i*set_size - 1)
      } else {
        training_set = X(?, 0 to (i-1)*set_size - 1) \ X(?, i*set_size to n - 1)
        testing_set = X(?, (i-1)*set_size to n - 1)
      }

      // Train
      //beta = train(training_set)

      // Test
      predictions = predict(beta, testing_set)

      // Calculate tp, fp, tn, fn
      // sort pred
      var (sorted, order) = sortdown2(predictions)
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
    } else {
      var time = System.currentTimeMillis
      X = load("/Users/Davidius/294-1-hw2/data/processed.mat", "X")
      println("X loaded in %s seconds.".format( (System.currentTimeMillis - time)/1000.0 ))
      time = System.currentTimeMillis
      Y = load("/Users/Davidius/294-1-hw2/data/processed.mat", "Y")  
      println("Y loaded in %s seconds.".format( (System.currentTimeMillis - time)/1000.0 ))          
      setGlobals()
      train()
    }

  }

}
