package com.fbot.common.fastcollections.fastarrayops

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
// We extend from Any to make it a Universal Trait, so we can derive value classes from it.
//   Sadly, somehow we break the brittle value class structure of scala and value class is indeed
//   instantiated when accessing any of the methods in this trait :-(
// Change of heart: things go really wrong without the right equals(), so we abandon the value class...
//   But oops, we are in FastArrayOps.. can we even define an equals() here????
// TO BE CONTINUED
//
// TODO: remove this trait (since it screws up the Value Class extension: all methods here do actually get created :-(
// For now we keep it as parent of all the concretizations in FastDoubleArrayOps etc (we could adapt NewType and take it out potentially
trait FastArrayOps extends Any
