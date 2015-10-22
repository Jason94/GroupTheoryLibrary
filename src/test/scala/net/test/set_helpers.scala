package net.test

import org.scalatest._
import net.groups.lib.SetHelpers._

class SetHelpersSpec extends FreeSpec with Matchers {
 
	"A set" - {
		"has subsets" in {
			val a = Set(1,2,3,4,5)
			val b = Set(1,2,3)
			val c = Set(2,3,5)
			
			a.subsetOf(a) shouldBe(true)
			a.subsetOf(b) shouldBe(false)
			a.subsetOf(c) shouldBe(false)
			
			b.subsetOf(a) shouldBe(true)
			b.subsetOf(c) shouldBe(false)
			
			c.subsetOf(a) shouldBe(true)
			c.subsetOf(b) shouldBe(false)
		}
	}	
}