package net.groups.groups

import net.groups._

object Klein4Elt {
	def multiply(a: Klein4Elt, b: Klein4Elt) = a match {
		case E => b
		case A => b match {
			case A => E
			case B => C
			case C => B
			case E => A
		} 
		case B => b match {
			case A => C
			case B => E
			case C => A
			case E => B
		}
		case C => b match {
			case A => B
			case B => A
			case C => E
			case E => C
		}
	}
}
sealed abstract class Klein4Elt
case object E extends Klein4Elt
case object A extends Klein4Elt
case object B extends Klein4Elt
case object C extends Klein4Elt

object Klein4 {
	private lazy val eltsK4: Set[Klein4Elt] = Set(A,B,C,E)
	private lazy val k4 = Group(eltsK4, Klein4Elt.multiply _)

	def apply() = k4
}