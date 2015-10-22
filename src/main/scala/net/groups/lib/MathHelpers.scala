package net.groups.lib

/**
 * @author jason
 */
object MathHelpers {
	
	def factorial(n: Int, nFact: Long = 1): Long = n match {
		case 0 => nFact
		case _ => factorial(n-1, nFact * n)
	}
	
}