package blockmodel.executables

import java.io.File

import blockmodel.search.PermutationBreakingBranching
import blockmodel.utils.Digraph
import blockmodel.{Blockmodel, BlockmodelCPModel, ExclusionBlockmodelCPModel}
import javax.imageio.ImageIO
import oscar.cp._
import blockmodel.utils.Matrix._

import scala.util.Random
import javax.swing.ImageIcon
import javax.swing.JFrame
import javax.swing.JLabel
import javax.swing.WindowConstants
import java.awt.BorderLayout
import java.awt.image.BufferedImage

object excludeExperiment extends App {

  val k = 3
  val n = 60
  val rng = new Random()


  private var frame: JFrame = null
  private var label: JLabel = null

  def display(image: BufferedImage): Unit = {
    if (frame == null) {
      frame = new JFrame
      frame.setTitle("stained_image")
      frame.setSize(image.getWidth*10, image.getHeight*10)
      frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
      label = new JLabel
      label.setIcon(new ImageIcon(image))
      frame.getContentPane.add(label, BorderLayout.CENTER)
      frame.setLocationRelativeTo(null)
      frame.pack()
      frame.setVisible(true)
    }
    else label.setIcon(new ImageIcon(image))
  }


  for (error <- Seq(0.0, 0.05, 0.10, 0.15, 0.20)) {
    val (g, bm) = Digraph.randomWithImage(Blockmodel.ringStructure(k), n, rng, error)
    val img = bm.toImageGrouped(g, 10)
    ImageIO.write(img, "gif", new File(s"./ringstart-n$n-$error.gif"))
    display(img)

    var bestBM: Blockmodel = null
    // model with k+1 clusters. the last cluster, with index k, is the excluded variables.
    object model extends ExclusionBlockmodelCPModel(g, k) {
      // we minimize the number of excluded variables
      minimize(nbExcluded)
      search(binaryFirstFail(C) ++ binary(M.flatten.asInstanceOf[Array[CPIntVar]]))
      onSolution {
        println(s"new solution found with $nbExcluded excluded vertices")
        bestBM = getBlockmodel
        display(bestBM.toImageGrouped(g, 10))
      }
    }

    val s = model.solver.start()
    println(s)
    if (bestBM != null) {
      println(bestBM.toStringGrouped(g))
      val img = bestBM.toImageGrouped(g, 10)
      ImageIO.write(img, "gif", new File(s"./ringend-n$n-$error.gif"))
      display(img)
    }
  }



  /*
  val setups = Seq(
    ("ring", Blockmodel.ringStructure(k)),
    ("stick", Blockmodel.stickStructure(k)),
    ("star", Blockmodel.starStructure(k)),
    ("community", Blockmodel.communityStructure(k)))

  for (noise <- Seq(0, 0.05, 0.1, 0.15); (name, blockmodel) <- setups) {
    val (g, _) = Digraph.randomWithImage(blockmodel, n, rng, noise)
    g.saveTSV(s"$name-n$n-k$k-$noise-graph.tsv")

    object model extends BlockmodelCPModel(g,k+1) {
      for(i <- 0 until k; j <- 0 until k) cost(i)(j).updateMax(0)

    }
  }
  */
}
