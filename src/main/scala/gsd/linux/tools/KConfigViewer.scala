package gsd.linux.tools

/*
 * This file is part of the Linux Variability Modeling Tools (LVAT).
 *
 * Copyright (C) 2012 Steven She <shshe@gsd.uwaterloo.ca>
 *
 * LVAT is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 *
 * LVAT is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with LVAT.  (See files COPYING and COPYING.LESSER.)  If not, see
 * <http://www.gnu.org/licenses/>.
 */

import scala.swing._
import javax.swing.{JTree, JOptionPane}

import gsd.linux._
import javax.swing.tree.{DefaultTreeModel, MutableTreeNode, DefaultMutableTreeNode}

object KConfigViewer extends SwingApplication {

  override def startup(args: Array[String]) {
    if (args.size  == 0) {
      JOptionPane.showMessageDialog(null, "The first parameter must be a Kconfig extract.")
      System.exit(1)
    }

    val fileName = args(0)
    val k = KConfigParser.parseKConfigFile(fileName)
    val t = top(k, fileName)
    if (t.size == new Dimension(0,0)) t.pack()
    t.visible = true
  }

  def mkRootNode(k: ConcreteKConfig): MutableTreeNode = {
    def mkNode(in: CSymbol): MutableTreeNode  = {
      val n = in match {
        case c: CConfig =>
          val promptText = c.prompt.headOption match {
            case Some(Prompt(text, expr)) => text
            case None => ""
          }
          new DefaultMutableTreeNode("%s (%s)".format(promptText, c.name))
        case _ =>
          new DefaultMutableTreeNode(in.prettyString)
      }
      in.children map mkNode foreach n.add
      n
    }
    mkNode(k.root)
  }

  def top(k: ConcreteKConfig, fileName: String) = new MainFrame {
    title = "KConfig Viewer (" + fileName + ")"
    contents = new ScrollPane {
      contents = new Component {
        override lazy val peer = new JTree(new DefaultTreeModel(mkRootNode(k))) {

        }
      }
    }
  }

}
