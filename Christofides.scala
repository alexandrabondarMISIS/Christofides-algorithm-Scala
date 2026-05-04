import scala.collection.mutable

object Christofides {

  case class Edge(u: Int, v: Int, w: Double)

  type Matrix = Vector[Vector[Double]]

  def solve(dist: Matrix): List[Int] = {
    val mst = minimumSpanningTree(dist)
    val odd = oddVertices(mst, dist.length)
    val matching = greedyMatching(odd, dist)
    val multigraph = mst ++ matching
    val euler = eulerTour(multigraph)
    shortcut(euler)
  }

  def minimumSpanningTree(dist: Matrix): List[Edge] = {
    val n = dist.length
    val visited = Array.fill(n)(false)
    visited(0) = true

    val edges = mutable.ListBuffer[Edge]()
    var visitedCount = 1

    while (visitedCount < n) {
      var best: Edge = null

      for (u <- 0 until n if visited(u);
           v <- 0 until n if !visited(v)) {

        val e = Edge(u, v, dist(u)(v))
        if (best == null || e.w < best.w) best = e
      }

      edges += best
      visited(best.v) = true
      visitedCount += 1
    }

    edges.toList
  }

  def oddVertices(edges: List[Edge], n: Int): List[Int] = {
    val degree = Array.fill(n)(0)

    edges.foreach { e =>
      degree(e.u) += 1
      degree(e.v) += 1
    }

    (0 until n).filter(i => degree(i) % 2 == 1).toList
  }

  def greedyMatching(vertices: List[Int], dist: Matrix): List[Edge] = {
    val remaining = mutable.ListBuffer(vertices: _*)
    val result = mutable.ListBuffer[Edge]()

    while (remaining.nonEmpty) {
      val u = remaining.remove(0)

      val (v, idx) = remaining.zipWithIndex.minBy { case (x, _) => dist(u)(x) }
      remaining.remove(idx)

      result += Edge(u, v, dist(u)(v))
    }

    result.toList
  }

  def eulerTour(edges: List[Edge]): List[Int] = {
    val graph = mutable.Map[Int, mutable.ListBuffer[Int]]()

    edges.foreach { e =>
      graph.getOrElseUpdate(e.u, mutable.ListBuffer()) += e.v
      graph.getOrElseUpdate(e.v, mutable.ListBuffer()) += e.u
    }

    val stack = mutable.Stack[Int]()
    val path = mutable.ListBuffer[Int]()

    stack.push(edges.head.u)

    while (stack.nonEmpty) {
      val v = stack.top

      if (graph(v).nonEmpty) {
        val u = graph(v).head
        graph(v).remove(0)
        graph(u) -= v
        stack.push(u)
      } else {
        path += stack.pop()
      }
    }

    path.reverse.toList
  }

  def shortcut(path: List[Int]): List[Int] = {
    val seen = Array.fill(1000)(false)
    val tour = mutable.ListBuffer[Int]()

    for (v <- path) {
      if (!seen(v)) {
        seen(v) = true
        tour += v
      }
    }

    tour += tour.head
    tour.toList
  }

  def main(args: Array[String]): Unit = {
    val dist: Matrix = Vector(
      Vector(0.0, 10.0, 15.0, 20.0),
      Vector(10.0, 0.0, 35.0, 25.0),
      Vector(15.0, 35.0, 0.0, 30.0),
      Vector(20.0, 25.0, 30.0, 0.0)
    )

    val tour = solve(dist)
    println(tour.mkString(" -> "))
  }
}