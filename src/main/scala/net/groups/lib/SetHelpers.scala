package net.groups.lib

object SetHelpers {
	implicit class AdvanceSet[T](s: Set[T]) {
		def subsetOf(os: Set[T]): Boolean = {
			for(x <- s) {
				if(!os.contains(x))
					return false
			}
			return true
		}
		
		def properSubsetOf(os: Set[T]): Boolean = {
			if(s == os)
				return false
			else
				return subsetOf(os)
		}
	}
}