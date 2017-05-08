/**
  * Created by martin on 08/05/17.
  */
object App {

  def main(args: Array[String]): Unit = {
    val input = (for {n <- 1 to 2; line = Console.readLine()} yield line)
    val capacity = (input.head.split(" "))(0).toInt
    val numberOfBars = (input.head.split(" "))(1).toInt
    val weights = input.last.split(" ").toVector.map(a => a.toInt)


    def optimalWeight(maxWeight: Int, bars: Vector[Int]): Int = {
      var knapsackMatrix = Array.fill(maxWeight + 1, bars.length + 1)(0)
      (1 to bars.length).foreach(n => (1 to maxWeight).foreach(w => {
        knapsackMatrix(w)(n) = knapsackMatrix(w)(n - 1)
        if (bars(n - 1) <= maxWeight ) {
          if (w >= bars(n - 1)) {
            val tmp = knapsackMatrix(w - bars(n - 1))(n - 1) + bars(n - 1)
            if (knapsackMatrix(w)(n) < tmp) {
              knapsackMatrix(w)(n) = tmp
            }
          }
        }
      }))
      return knapsackMatrix(maxWeight)(bars.length)
    }

    val maxWeight = optimalWeight(capacity, weights)

    println(maxWeight)

  }


}
