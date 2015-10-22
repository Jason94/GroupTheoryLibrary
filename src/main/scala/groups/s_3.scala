package net.groups.groups

import net.groups.Group
import net.groups.lib.MathHelpers.factorial

object Sn {
	
	type Cycle = Map[Int,Int]
	
	def apply(n: Int) = {	
		implicit val symSpace = n
		
		/*val targets = (2 to n).foldLeft(List.tabulate(n)(x => List(x+1))) { (oldLsts, _) =>
 			oldLsts.foldLeft(List[List[Int]]()) { (lsts, lst) =>
 				lsts ++ (1 to n).filter(!lst.contains(_)).map(_ +: lst)
 			}
 		}*/
		
		val targets = (1 to n).permutations
		
		val cycles = targets.map { lst => 
			(1 to n).zip(lst).toMap
		}.toSet
				
		Group(cycles, Cycle.cycleMult _)
	}
	
	object Cycle {
		def apply(cyc: Int*)(implicit symSpace: Int) = buildCycle(cyc)(symSpace)
		def cycleMult(a: Cycle, b: Cycle)(implicit symSpace: Int): Cycle = (1 to symSpace).map { i =>
			(i, a(b(i)))	
		}.toMap
		def buildCycle(lsts: Seq[Int]*)(implicit symSpace: Int): Cycle = {
			var cycle = Map[Int,Int]()
			
			for(lst <- lsts) {
				val length = lst.length

				for(i <- (0 until length)) {
					cycle = cycle + (lst(i) -> lst((i+1) % length))
				}
			}
			
			for(i <- 1 to symSpace) {
				if(!cycle.keySet.contains(i)) {
					cycle = cycle + (i -> i)
				}
			}
			
			cycle
		}
	}
}
