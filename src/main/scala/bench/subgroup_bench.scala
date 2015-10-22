package net.groups.bench

import net.groups._
import net.groups.groups._

object SubgroupBenchmark extends App {
	(1 to 11).toList foreach { z =>
		val grp = Dn(z)

		val start = System.currentTimeMillis()

		grp.subgroups

		val end = System.currentTimeMillis()

		println(s"${grp.elts.size}: ${end - start}")
	}

	def printOrders[T](grp: Group[T]) {
		grp.elts foreach { g =>
			println(s"$g: ${grp.orderOf(g)}")
		}
	}
}