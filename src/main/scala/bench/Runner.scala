package net.groups.bench

import net.groups._
import net.groups.groups._

object Runner extends App {
  
	val s3 = Sn(3)
	val z3 = Zn(3) 
	
	val s3z3 = s3.cartesianProduct(z3)
	
	println(s3z3.elts.size)
	s3z3.subgroups.foreach { sub => 
		println("Order: " + sub.elts.size)
		sub.elts.foreach(println(_))
		println()
	}
}