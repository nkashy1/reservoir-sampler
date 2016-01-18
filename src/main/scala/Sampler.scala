package reservoirSampler

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Random

/**
  * Created by nkashyap on 1/17/16.
  */
object Sampler {
  def main(args: Array[String]) = {
    val argsList = args.toSeq
    if (argsList.length != 2 && argsList.length != 3) {
      println("Please pass the filename and the number of lines to sample as arguments. Optionally pass 1 to specify that there is metadata at the head of the file that should be copied to the sample.")
      System.exit(1)
    }

    val filename = argsList(0)
    val data = Source.fromFile(filename).getLines
    val sampleSize = argsList(1).toInt
    val metadata = if (argsList.length == 3 && argsList(2).toInt == 1) {
      Some(data.next)
    } else {
      None
    }

    // The following is a simple visual test of the produceSample method. Run on any file containing a small (relative to 100000) number of lines.
    // println((1 to 100000).map(_ => produceSample(Source.fromFile(filename).getLines, 1, None)).groupBy(identity).mapValues(_.size))

    printSample(produceSample(data, sampleSize, metadata))
  }

  def produceSample(data: Iterator[String], sampleSize: Int, metadata: Option[String]) = metadata match {
    case Some(row) => sample(Seq[String](), sampleSize, 1, data, new Random()).+:(row)
    case _ => sample(Seq[String](), sampleSize, 1, data, new Random())
  }

  @tailrec
  def sample(currentSample: Seq[String], sampleSize: Int, currentLine: Int, data: Iterator[String], rng: Random): Seq[String] = {
    data.isEmpty match {
      case true => currentSample
      case false => currentLine match {
        case line if line <= sampleSize => sample(currentSample :+ data.next, sampleSize, line + 1, data, rng)
        case line => rng.nextInt(line) match {
          case decider if decider == sampleSize - 1 => sample(currentSample.take(sampleSize - 1) :+ data.next, sampleSize, line + 1, data, rng)
          case decider if decider == 0 => sample(currentSample.takeRight(sampleSize - 1).+:(data.next), sampleSize, line + 1, data, rng)
          case decider if decider < sampleSize => sample((currentSample.take(decider) :+ data.next) ++ currentSample.takeRight(sampleSize - decider - 1), sampleSize, line + 1, data, rng)
          case _ => {
            data.next
            sample(currentSample, sampleSize, line + 1, data, rng)
          }
        }
      }
    }
  }

  def printSample(sample: Seq[String]) = {
    sample foreach println
  }
}
