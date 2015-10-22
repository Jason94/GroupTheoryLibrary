package net.groups.groups

import net.groups._

object Zn {
	private def add_mod(n: Int)(x: Int, y: Int) = (x+y) % n

	def apply(n: Int) = {
		val elts = (0 until n).toSet
		val op = add_mod(n) _
		Group(elts,op)
	}
}