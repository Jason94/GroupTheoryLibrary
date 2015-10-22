package net.groups.groups

import net.groups._

// TODO: Annotate this better.
// TODO: Clean up replication in n storage.

object ProperMod {
	// A proper mod function that returns a % b. -2 % 5 = 3, for example.
	def mod(a: Int, b: Int) = {
		if(a >= 0) {
			a % b
		} else {
			val correction = Math.abs(a) % b
			b - correction
		}
	}
}

case class DnElement(val x: Int, val y: Int)(implicit val n: Int) {
	import ProperMod.mod

	override def equals (o: Any) = o match {
		case that: DnElement =>	mod(that.x,n) == mod(x,n) && mod(that.y,2) == mod(y,2) && that.n == n
		case _ => false
	}
	
	override def toString() = {
		if(x == 0 && y == 0)
			"e"
		else if(y == 0)
			s"x^$x"
		else if(x == 0)
			s"y^$y"
		else //(x != 0 && y != 0)
			s"x^$x*y^$y"
	}
}

object Dn {
	import ProperMod.mod

	def dihedral_elts(n: Int) = {
		implicit val nImp = n
		Set(DnElement(0,0)) ++ (0 until n).map(i=>Set(DnElement(i,0),DnElement(i,1))).toSet.flatten
	}

	def dihedral_mult(n: Int)(a: DnElement, b: DnElement) = 
		if(a.y == 1)
			DnElement(mod(a.x-b.x,n), mod(a.y+b.y,2))(n)
		else // a.y == 0
			DnElement(mod(a.x+b.x,n), mod(a.y+b.y,2))(n)

	def apply(n: Int) = (new Group(dihedral_elts(n),dihedral_mult(n) _))
	
}