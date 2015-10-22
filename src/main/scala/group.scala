package net.groups

import scala.collection.immutable.HashMap
import net.groups.lib.SetHelpers._

object Group {
	/**
	 * Generate a group from 2 elements and an operation.
	 * Tends to not work very well for infinite groups.
	 */
	def generate[T](a: T, b: T, op: (T,T) => T, abelian: Boolean = false): Group[T] = {
		// First, find the orders of a & b.
		var orderA = 2
		var currentVal = op(a,a)
		while(currentVal != a) { // Necessary since we don't know the value of e
			currentVal = op(currentVal, a) // Elements always commute w/ themselves, so this is OK
			orderA += 1
		}
		orderA -= 1 // Correct for going over by 1
		
		var orderB = 2
		currentVal = op(b,b)
		while(currentVal != b) {
			currentVal = op(currentVal, b)
			orderB += 1
		}
		orderB -= 1
				
		// Now generate all the elements.
		var elts = Set(a,b,op(a,b))
		
		//TODO: Optimize for expensive operations by caching or something.
		for(iA <- 1 to orderA) {
			for(iB <- 1 to orderB) {
				val prodA = (2 to iA).foldLeft(a){(prod, _) => op(prod,a) }
				val prodB = (2 to iB).foldLeft(b){(prod, _) => op(prod,b) }
				elts += op(prodA, prodB)
				if(!abelian)
					elts += op(prodB, prodA) // Not necessary if grp is abelian
			}
		}
		
		Group(elts, op)
	} 
}

case class Group[T](val elts: Set[T], val op: (T,T) => T) {
	
	/// Syntactic Sugar ///
	
	def *(a: T, b: T) = op(a,b)
	
	private implicit val ownGroup = this
	import GroupImplicits._
	
	/// Group Properties ///
	
	lazy val identity: T = elts.find { g =>
		orderOf(g) == 1
	}.get

	lazy val subgroups: Set[Group[T]] = {
		// First find all of the sets of elements, the lengths of which divide the order of the group.
		val possibleN: Set[Int] = (1 to elts.size).toSet.filter(n => elts.size % n == 0)
		/// Do a few optimizations: ///
		// 1. A subrgoup cannot contain an element higher than its order.
		var possibles: Set[Set[T]] = possibleN.map { n =>
			elts.filter(orderOf(_) <= n).subsets(n).toSet
		}.flatten

		possibles = possibles.filter { possElts: Set[T] =>
			var passes = true

			// 2. Every subgroup must contain the identity.
			passes = possElts.contains(identity)

			/// Only select those possibles that satisfy the one-step subgroup test. ///
			val it1 = possElts.iterator

			while(passes && it1.hasNext) {
				val a = it1.next
				val it2 = possElts.iterator

				while(passes && it2.hasNext) {
					val b = it2.next
					val c = op(a, inverse(b))
					if(!possElts.contains(c)) {
						passes = false
					}
				}
			}

			passes
		}

		possibles.map(Group(_,op))
	}
	
	lazy val maximalSubgroups: Seq[Group[T]] = {
		def testMaximal(h: Group[T]): Boolean = {
			if(h == this)
				return false
			for(h2 <- subgroups) {
				if(h2 != this && h.elts.properSubsetOf(h2.elts))
					return false
			}
			true
		}
		subgroups.filter(testMaximal(_)).toSeq
	}

	lazy val elementsOfOrder: Map[Int,Int] = {
		elts.foldLeft(Map[Int,Int]()) { (map,g) =>
			val ord = orderOf(g)
			if(map.keySet.contains(ord))
				map + (ord -> 1)
			else
				map + (ord -> (map(ord)+1))
		}
	}
	
	/// Element Properties ///
	
	lazy val inverse: Map[T,T] = {
		var elts = this.elts
		var inverses = HashMap[T,T]()

		while(elts.size > 0) {
			// Pick the first element left.
			val elt = elts.head

			// Search for its inverse.
			val inv = elts.find(this.*(elt,_) == this.identity).get

			// Register the inverse and remove both elements from elts.
			if(elt == inv) {
				inverses = inverses + (elt -> inv)
				elts = elts.filterNot(_ == elt)
			} else {
				inverses = inverses + (elt -> inv) + (inv -> elt)
				elts = elts.filterNot(g => g == elt || g == inv)
			}
		}

		inverses
	}

	lazy val orderOf: Map[T,Int] = {
		// Calculate the order of each element, using the fact that its order is the number of times
		// it takes to multiply back to itself minus 1.
		elts.foldLeft(Map[T,Int]()) { (map, g) =>
			var h = *(g,g)
			var i = 1

			while(h != g) {
				h = *(h,g)
				i += 1
			}

			map + (g -> i)
		}
	}
	
	def cylcicSubgroupOf(elt: T) = {		
		val newElts = (0 until orderOf(elt)).map(elt ^ _).toSet
		Group(newElts, op)
	}

	/// Cosets & Normality ///
	
	def leftCoset(g: T, H: Group[T]): Set[T] = H.elts.map(g ** _)
	def rightCoset(H: Group[T], g: T): Set[T] = H.elts.map(_ ** g)
	
	def leftCosets(H: Group[T]): List[Set[T]] = {
		var remainingElts = elts.toList
		var repsToCosets = Map[T,Set[T]]()
				
		while(remainingElts.length > 0) {
			val currentRep = remainingElts.head
			
			// Calculate the coset of the current rep
			val coset = leftCoset(currentRep, H)
			
			// Remove every element from remainingElts that's in coset		
			remainingElts = remainingElts.filter(!coset.contains(_))
			
			// Store result
			repsToCosets = repsToCosets + ((currentRep, coset))
		}
		
		repsToCosets.values.toList
	}
	
	def rightCosets(H: Group[T]): List[Set[T]] = {
		var remainingElts = elts.toList
		var repsToCosets = Map[T,Set[T]]()
				
		while(remainingElts.length > 0) {
			val currentRep = remainingElts.head
			
			// Calculate the coset of the current rep
			val coset = rightCoset(H, currentRep)
			
			// Remove every element from remainingElts that's in coset		
			remainingElts = remainingElts.filter(!coset.contains(_))
			
			// Store result
			repsToCosets = repsToCosets + ((currentRep, coset))
		}
		
		repsToCosets.values.toList
	}
	
	/// Meta Group operations ///
	
	def cartesianProduct[U](oGp: Group[U]): Group[(T,U)] = {
		val cartOp = (ab: (T,U), cd: (T,U)) => (op(ab._1, cd._1), oGp.op(ab._2, cd._2))
		val cartElts: Set[(T,U)] = elts.foldLeft(Set[(T,U)]()) { (newElts, a) =>
			newElts.union( oGp.elts.foldLeft(Set[(T,U)]()) { (newElts2, b) =>
				newElts2 + ((a,b))
			})
		}
		Group(cartElts, cartOp)
	}
	
	/// Unfinished
	
	def isomorphic[U](grp2: Group[U]) = {
		//TODO: Do this somehow...
		true
	}
	
	def generatorSets(n: Int): Seq[Set[T]] = {
		if(n == 1) {
			elts.filter { g =>
				orderOf(g) == elts.size
			}.map(Set(_)).toSeq
		}
		else if(n == 2) {
			// If an element is in every proper subgroup, then it can't be a generator.			
			val notIncludingMaximals: Map[T,List[Group[T]]] = elts.toList.map { g =>
				var grps = List[Group[T]]()
				for(mx <- maximalSubgroups) {
					if(!mx.elts.contains(g))
						grps :+ mx
				}
				(g, grps)
			}.toMap
			
			// Now for each possible, find an element that is in a different maximal subgroup than it, and add it.
			notIncludingMaximals.keys.map { g =>  
				//TODO: Implement this
			}
			List(elts) // Not this
		}
		else
			//TODO: Implement for n > 2
			List()
	}
}