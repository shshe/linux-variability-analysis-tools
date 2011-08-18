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

import java.io.InputStream

trait KConfigParser {
  def parseKConfigFile(file: String): ConcreteKConfig
  def parseKConfig(input: String): ConcreteKConfig
  def parseKConfigStream(inputStream: InputStream): ConcreteKConfig
}

object KConfigParser extends KConfigParser {

  val rootId = "Linux Kernel Configuration"

  // First try to parse using the protobuf parser and if it fails, then use
  // the exconfig parser.
  def parseKConfig(input: String): ConcreteKConfig = {
    try {
      (new ProtoParser).parseKConfig(input)
    }
    catch {
      case _ =>
        (new ExconfigParser).parseKConfig(input)
    }
  }

  // First try to parse using the protobuf parser and if it fails, then use
  // the exconfig parser.
  def parseKConfigStream(stream: InputStream): ConcreteKConfig = {
    try {
      (new ProtoParser).parseKConfigStream(stream)
    }
    catch {
      case _ =>
        (new ExconfigParser).parseKConfigStream(stream)
    }
  }

  // First try to parse using the protobuf parser and if it fails, then use
  // the exconfig parser.
  def parseKConfigFile(file: String): ConcreteKConfig = {
    try {
      (new ProtoParser).parseKConfigFile(file)
    }
    catch {
      case _ =>
        (new ExconfigParser).parseKConfigFile(file)
    }
  }
}

