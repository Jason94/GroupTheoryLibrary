package net.test.groups

import org.scalatest.FreeSpec
import org.scalatest.Matchers
import net.groups._
import net.groups.groups._

class GroupImplicitSpec extends FreeSpec with Matchers {

	implicit val grp = Zn(10)

	"An element of a group" - {
		val a = 5
		val b = 6
		val c = 7

		import GroupImplicits._

		"multiplies with other elements of the group." in {
			a ** b shouldBe 1
			a ** c shouldBe 2
			b ** c shouldBe 3
		}

		"has an inverse." in {
			a.inv shouldBe 5
			b.inv shouldBe 4
			c.inv shouldBe 3
		}
	}
}