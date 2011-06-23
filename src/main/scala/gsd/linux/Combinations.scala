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
 * Constructs a list of combinations.
 * Source: http://aperiodic.net/phil/scala/s-99/p26.scala 
 */
object Combinations {

  // flatMapSublists is like list.flatMap, but instead of passing each element
  // to the function, it passes successive sublists of L.
  def flatMapSublists[A,B](lst: List[A])(f: (List[A]) => List[B]): List[B] =
    lst match {
      case Nil => Nil
      case sublist@(_ :: tail) => f(sublist) ::: flatMapSublists(tail)(f)
    }

  def choose[A](n: Int, lst: List[A]) : List[List[A]] = {
    if (n == 0) List(Nil)
    else flatMapSublists(lst){
      case head::tail => choose(n - 1, tail).map { head :: _ }
      case _ => sys.error("should never happen")
    }
  }

  def all[A](in: List[A]): List[List[A]] =
    (1 to in.size) flatMap { i => choose(i, in) } toList

}