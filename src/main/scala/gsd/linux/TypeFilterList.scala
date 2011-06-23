/*
 * This file is part of the Linux Variability Modeling Tools (LVAT).
 *
 * Copyright (C) 2010 Steven She <shshe@gsd.uwaterloo.ca>
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

/**
 * Conveience methods for filtering items from a List based on its type.
 */
object TypeFilterList {
  class SuperList[A <: AnyRef](lst : List[A]) {

    /*
     * Uses experimental Scala Manifests to reify types
     */
    def typeFilter[T](implicit m:scala.reflect.Manifest[T]) =
      lst.filter{x:A => m.erasure.isInstance(x)}.asInstanceOf[List[T]]

    def sortByType =
      lst.sortWith{(x,y) => x.getClass.getName < y.getClass.getName }
  }

  implicit def toSuperList[A <: AnyRef](lst: List[A]) =
    new SuperList[A](lst)

}

