package net.groups

object GroupImplicits {
	implicit class RichGroupElt[T](g: T)(implicit val grp: Group[T]) {
		def **(h: T) = grp.*(g,h)
		
		def ^(n: Int): T = {
			if(n == 0)
				grp.identity
			else if(n > 0) {
				var curr = g
				for(i <- 0 until n-1) {
					curr = grp.*(curr,g)
				}
				curr
			} else { // n < 0
				^(-n)
			}
		}

		def inv() = grp.inverse(g)
	} 
}