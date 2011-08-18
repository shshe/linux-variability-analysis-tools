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

import gsd.linux.KconfigProtos.Node.NodeType
import com.google.protobuf.ByteString
import java.io.{InputStream, FileInputStream}

class ProtoParser extends KConfigParser {

  import KconfigProtos._
  import gsd.linux.KExprParser.{parseKExpr, parseId, parseIdOrValue}

  import collection.JavaConversions._

  private case class Properties(prompts: List[Prompt],
                                defaults: List[Default],
                                selects: List[Select],
                                ranges: List[Range],
                                dependsOns: List[DependsOn])

  var i = 0
  def nextId = {
    i += 1
    i
  }



  def parseKConfigFile(file: String): ConcreteKConfig =
    parseKConfig(Node.parseFrom(new FileInputStream(file)))

  def parseKConfig(input: String): ConcreteKConfig =
    parseKConfig(Node.parseFrom(input.getBytes))

  def parseKConfigStream(stream: InputStream): ConcreteKConfig =
    parseKConfig(Node.parseFrom(stream))

  def parseKConfig(rootProto: Node): ConcreteKConfig = {
    import Node.DataType._

    def mkNodeType(t: Node.DataType): KType = {
      t match {
        case BOOLEAN => KBoolType
        case TRISTATE => KTriType
        case INT => KIntType
        case HEX => KHexType
        case STRING => KStringType
      }
    }

    def mkSymbol(n: Node): CSymbol = {
      def mkProperties(props: Iterable[PropertyProto]): Properties = {
        import PropertyProto.PropertyType._
        def mkProperty(p: PropertyProto) = p.getPropertyType match {
          case PROMPT =>
            Prompt(p.getText, parseKExpr(p.getVisibleExpr))
          case SELECT =>
            Select(parseId(p.getValue).value, KExprParser.parseKExpr(p.getVisibleExpr))

          // special case for handling string literals in defaults
          case DEFAULT if n.getDataType == STRING =>
            Default(Literal(p.getValue), parseKExpr(p.getVisibleExpr))
          case DEFAULT =>
            Default(parseKExpr(p.getValue), parseKExpr(p.getVisibleExpr))
          case DEPENDS_ON =>
            DependsOn(parseKExpr(p.getVisibleExpr))
          case RANGE =>
            // Ranges have a format of [low_int high_int]
            val valueTuple = p.getValue.substring(1, p.getValue.length - 1)
            val split = valueTuple split ' '
            val (lowStr, highStr) = (split(0), split(1))
            Range(parseIdOrValue(lowStr), parseIdOrValue(highStr), parseKExpr(p.getVisibleExpr))
          case ENV =>
            Env(p.getValue, parseKExpr(p.getVisibleExpr))
          case UNKNOWN =>
            // Unknown properties should be removed prior to tihs call
            sys.error("Unknown property: " + p)
        }

        // Remove unknown properties, then parse
        val propList = (props filterNot
          { _.getPropertyType == UNKNOWN } map
          mkProperty) toList

        val prompts = propList collect { case x: Prompt => x }
        val defaults = propList collect { case x: Default => x }
        val selects = propList collect { case x: Select => x }
        val ranges = propList collect { case x: Range => x }
        val dependsOns = propList collect { case x: DependsOn => x }

        Properties(prompts, defaults, selects, ranges, dependsOns)
      }

      val properties = mkProperties(n.getPropertyList)

      n.getNodeType match {
        case NodeType.CONFIG | NodeType.MENUCONFIG =>
          val properties = mkProperties(n.getPropertyList)
          CConfig(nextId,
            n.getId,
            n.getNodeType == NodeType.MENUCONFIG,
            mkNodeType(n.getDataType),
            parseKExpr(n.getInherited),
            properties.prompts,
            properties.defaults,
            properties.selects,
            properties.ranges,
            properties.dependsOns,
            n.getChildList.toList map mkSymbol)

        case NodeType.MENU =>
          CMenu(nextId, properties.prompts.head, n.getChildList.toList map mkSymbol)

        case NodeType.CHOICE =>
          CChoice(nextId, properties.prompts.head, n.getDataType == BOOLEAN, n.hasOpt,
                  properties.defaults,
                  n.getChildList.toList map mkSymbol collect { case x:CConfig => x })

        case NodeType.IF =>
          CIf(nextId, parseKExpr(n.getInherited), n.getChildList.toList map mkSymbol)

        case NodeType.COMMENT =>
          CComment(nextId, properties.prompts.head.text, parseKExpr(n.getInherited))

        case x => sys.error("Unhandled node type: " + x)
      }
    }

    ConcreteKConfig(CMenu(nextId, Prompt(KConfigParser.rootId,Yes),
      rootProto.getChildList.toList map mkSymbol))
  }
}
