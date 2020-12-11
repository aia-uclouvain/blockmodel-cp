package blockmodel.executables

import java.awt.BorderLayout
import java.awt.image.BufferedImage

import blockmodel.Blockmodel
import blockmodel.utils.Digraph
import javax.swing.{ImageIcon, JFrame, JLabel, WindowConstants}

import scala.io.StdIn

object Toulbar2BM extends App {
  var lastSolution = ""
  var line = ""
  while ({line = StdIn.readLine(); line != null}) {
    if (line.contains("M_0_0"))
      lastSolution = line
  }

  if (lastSolution == "") {
    println("no solution found")
    System.exit(0)
  }

  val g = Digraph.fromTSV(args(0))
  val bm = Blockmodel.fromToulbar2Solution(lastSolution)

  println(bm)

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

  display(bm.toImageGrouped(g, 10))


}
