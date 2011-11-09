/*
 * This file is part of the Linux Variability Modeling Tools (LVAT).
 *
 * Copyright (C) 2011 Steven She <shshe@gsd.uwaterloo.ca>
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

package gsd.linux

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test
import java.net.URLDecoder

class ProtoParserTest extends AssertionsForJUnit {

  @Test def menuVisible {
    val p = new ProtoParser
    val k = p.parseKConfigFile(
      URLDecoder.decode(getClass.getResource("../../menu-visible.pb").getFile, "UTF-8"))

    assert(k.configMap("C").prompt.head.cond === And(Id("B"), Id("A")))
    assert(k.configMap("C").defs.head.cond === Id("B"))
  }

  @Test def busyboxExconfigCompare {
    val exconfigf = URLDecoder.decode(getClass.getResource("../../busybox.exconfig").getFile, "UTF-8")
    val protof = URLDecoder.decode(getClass.getResource("../../busybox.pb").getFile, "UTF-8")
    val exconfig = (new ExconfigParser).parseKConfigFile(exconfigf)
    val protoconfig = (new ProtoParser).parseKConfigFile(protof)

    // First check the hierarchy
    def dfs(left: CSymbol, right: CSymbol) {
      assert(left.nodeId === right.nodeId, left.prettyString + " compare to " + right.prettyString)
      (left, right) match {
        case (l:CMenu, r:CMenu) =>
          assert(l.prompt.text === r.prompt.text)
        case (l:CConfig, r:CConfig) =>
          assert(l.name === r.name)
        case (l:CChoice, r:CChoice) =>
          assert(l.prompt.text === r.prompt.text)
        case (l:CComment, r:CComment) =>
          assert(l.text === r.text)
        case _ =>
          fail(left + " compared to " + right)
      }

      assert(left.children.size === right.children.size)
      (left.children zip right.children) foreach { case (l,r) => dfs(l,r) }
    }

    dfs(exconfig.root, protoconfig.root)

    // Check Properties

  }


}