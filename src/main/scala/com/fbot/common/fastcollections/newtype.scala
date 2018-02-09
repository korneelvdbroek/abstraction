package com.fbot.common.fastcollections

import scala.collection.GenIterable

/**
  * Copyright (C) 2017-2018  korneelvdbroek
  *
  * This program is free software: you can redistribute it and/or modify
  * it under the terms of the GNU General Public License as published by
  * the Free Software Foundation, either version 3 of the License, or
  * (at your option) any later version.
  *
  * This program is distributed in the hope that it will be useful,
  * but WITHOUT ANY WARRANTY; without even the implied warranty of
  * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  * GNU General Public License for more details.
  *
  * You should have received a copy of the GNU General Public License
  * along with this program.  If not, see <http://www.gnu.org/licenses/>.
  *
  */
object translucenttag {
  @inline
  def apply[TagTrait] = new Tagger[TagTrait]

  trait Tagged[TagTrait]
  type @@[+Repr, TagTrait] = Repr with Tagged[TagTrait]

  class Tagger[TagTrait] {
    @inline def apply[@specialized(Int) Repr](t : Repr) : Repr @@ TagTrait = t.asInstanceOf[Repr @@ TagTrait]
  }
}


object newtype {
  /**
    * Creates a value of the newtype given a value of its representation type.
    */
  @inline
  def apply[@specialized(Int, Long, Double) Repr, Ops](r : Repr) : Newtype[Repr, Ops] = r.asInstanceOf[Any with Newtype[Repr, Ops]]

  /**
    * New type with `Repr` as representation type and operations provided by `Ops`.
    *
    * Values of the newtype will not add any additional boxing beyond what's required for
    * values of the representation type to conform to Any. In practice this means that value
    * types will receive their standard Scala AnyVal boxing and reference types will be unboxed.
    */
  // anonymous type declaration: https://tomlee.co/2007/11/anonymous-type-acrobatics-with-scala/
  type Newtype[Repr, Ops] = { type Tag = NewtypeTag[Repr, Ops] } // whatever this is mixed with has the `type Tag = ...` added to their implementation
  trait NewtypeTag[Repr, Ops]

//  /**
//    * Implicit conversion of newtype to `Ops` type for the selection of `Ops` newtype operations.
//    *
//    * The implicit conversion `Repr => Ops` would typically be provided by publishing the companion
//    * object of the `Ops` type as an implicit value.
//    */
//  implicit def newtypeOps[Repr, Ops](t : Newtype[Repr, Ops])(implicit mkOps : Repr => Ops) : Ops = t.asInstanceOf[Repr]
}
