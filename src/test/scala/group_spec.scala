package net.test.groups

import org.scalatest.FreeSpec
import org.scalatest.Matchers
import net.groups._
import net.groups.groups._

class GroupSpec extends FreeSpec with Matchers {

	val z10 = Zn(10)
	val k4 = Klein4()
	val d4 = Dn(4)

	"Groups can be generated from elements" in {
		val gennedD4 = Group.generate(DnElement(1,0)(4), DnElement(0,1)(4), Dn.dihedral_mult(4) _)
		gennedD4.elts shouldBe d4.elts
		
		val gennedZ10_3 = Group.generate(1, 3, (a: Int, b: Int) => (a + b) % 10, abelian = true )
		val gennedZ10_7 = Group.generate(7, 1, (a: Int, b: Int) => (a + b) % 10, abelian = true )
		gennedZ10_3.elts shouldBe z10.elts
		gennedZ10_7.elts shouldBe z10.elts
	}
	
	"A group" - {
		"should multiply elements together" in {
			z10.*(1,2) shouldBe 3
			z10.*(2,2) shouldBe 4
			z10.*(5,5) shouldBe 0
			z10.*(6,7) shouldBe 3
		}

		"should calculate the order of elements" in {
			z10.orderOf(0) shouldBe 1
			z10.orderOf(5) shouldBe 2
			z10.orderOf(2) shouldBe 5
			z10.orderOf(8) shouldBe 5

			k4.orderOf(E) shouldBe 1
			k4.orderOf(A) shouldBe 2
			k4.orderOf(B) shouldBe 2
		}

		"should collect the orders of its elements" ignore {
			/*val z10ords = Map(
				1 ->,
				2 ->,
				3 ->,
				4 ->,
				5 ->,
				6 ->,
				7 ->,
				8 ->,
				9 ->,
				10 ->,
			)*/
		}

		"should have an identity" in {
			z10.identity shouldBe 0
			k4.identity shouldBe E
		}

		"should have subgroups" in {
			val z10subgrps = Set(
				Set(0),
				Set(0,5),
				Set(0,2,4,6,8),
				Set(0,1,2,3,4,5,6,7,8,9)
			)
			val k4subgrps = Set(
				Set(E),
				Set(E,A),
				Set(E,B),
				Set(E,C),
				Set(E,A,B,C)
			)
			implicit val n = 4
			val d4subgrps = Set(
				Set(DnElement(0,0)),
				Set(DnElement(0,0),DnElement(2,0)),
				Set(DnElement(0,0),DnElement(0,1)),
				Set(DnElement(0,0),DnElement(1,1)),
				Set(DnElement(0,0),DnElement(2,1)),
				Set(DnElement(0,0),DnElement(3,1)),
				Set(DnElement(0,0),DnElement(0,1),
					DnElement(2,0),DnElement(2,1)),
				Set(DnElement(0,0),DnElement(1,1),
					DnElement(2,0),DnElement(3,1)),
				Set(DnElement(0,0),DnElement(1,0),
					DnElement(2,0),DnElement(3,0)),
				d4.elts
			)

			z10.subgroups.map(_.elts) shouldBe z10subgrps
			k4.subgroups.map(_.elts) shouldBe k4subgrps
			d4.subgroups.size shouldBe 10
			d4.subgroups.map(_.elts) shouldBe d4subgrps
		}
		
		"can multiply with another group" in {
			val z2 = Zn(2)
			
			val z22 = z2.cartesianProduct(z2)
			
			z22.elts should have size 4
		}
		
		"can find generators" in {
			z10.generatorSets(1) should contain only (Set(1),Set(3),Set(7),Set(9))
			k4.generatorSets(1) shouldBe empty
		}
	}

	"An element in a group" - {
		"should have an inverse" in {
			z10.inverse(0) shouldBe 0
			z10.inverse(1) shouldBe 9
			z10.inverse(2) shouldBe 8
			z10.inverse(5) shouldBe 5
			z10.inverse(9) shouldBe 1
		}
	}
}