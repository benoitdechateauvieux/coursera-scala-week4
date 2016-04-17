package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }


  test("combine of some leaf list") {
    val leaflist = List(Leaf('e', 1), Leaf('t', 2), Leaf('x', 4))
    assert(combine(leaflist) === List(Fork(Leaf('e',1),Leaf('t',2),List('e', 't'),3), Leaf('x',4)))
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("decode BAC") {
    new TestTrees {
      def tree = Fork(
        Leaf('a',8),
        Fork(
          Fork(
            Leaf('b',3),
            Fork(
              Leaf('c',1),
              Leaf('d',1),
              List('c', 'd'),2),
            List('b', 'c', 'd'),5),
          Fork(
            Fork(
              Leaf('e',1),
              Leaf('f',1),
              List('e', 'f'),2),
            Fork(
              Leaf('g',1),
              Leaf('h',1),
              List('g', 'h'),2),
            List('e', 'f', 'g', 'h'),4),
          List('h', 'g', 'f', 'e', 'd', 'c', 'b'),9),
        List('a', 'h', 'g', 'f', 'e', 'd', 'c', 'b'),17)
      assert(decode(tree, List(1,0,0,0,1,0,1,0)) === List('b', 'a', 'c'))
    }
  }

  test("createCodeTree for some Strings") {
    new TestTrees {
      assert(createCodeTree("benoit".toList) === Fork(Fork(Leaf('o',1),Leaf('t',1),List('o', 't'),2),Fork(Fork(Leaf('b',1),Leaf('e',1),List('b', 'e'),2),Fork(Leaf('i',1),Leaf('n',1),List('i', 'n'),2),List('b', 'e', 'i', 'n'),4),List('o', 't', 'b', 'e', 'i', 'n'),6))
    }
  }

  test("decode secret") {
    new TestTrees {
      assert(decodedSecret === "huffmanestcool".toList)
    }
  }

  test("encodeChar a") {
    new TestTrees {
      def tree = Fork(
        Leaf('a', 8),
        Fork(
          Fork(
            Leaf('b', 3),
            Fork(
              Leaf('c', 1),
              Leaf('d', 1),
              List('c', 'd'), 2),
            List('b', 'c', 'd'), 5),
          Fork(
            Fork(
              Leaf('e', 1),
              Leaf('f', 1),
              List('e', 'f'), 2),
            Fork(
              Leaf('g', 1),
              Leaf('h', 1),
              List('g', 'h'), 2),
            List('e', 'f', 'g', 'h'), 4),
          List('h', 'g', 'f', 'e', 'd', 'c', 'b'), 9),
        List('a', 'h', 'g', 'f', 'e', 'd', 'c', 'b'), 17)

      assert(encodeChar(tree)('a') === List(0))
    }
  }

    test("encodeChar b") {
      new TestTrees {
        def tree = Fork(
          Leaf('a',8),
          Fork(
            Fork(
              Leaf('b',3),
              Fork(
                Leaf('c',1),
                Leaf('d',1),
                List('c', 'd'),2),
              List('b', 'c', 'd'),5),
            Fork(
              Fork(
                Leaf('e',1),
                Leaf('f',1),
                List('e', 'f'),2),
              Fork(
                Leaf('g',1),
                Leaf('h',1),
                List('g', 'h'),2),
              List('e', 'f', 'g', 'h'),4),
            List('h', 'g', 'f', 'e', 'd', 'c', 'b'),9),
          List('a', 'h', 'g', 'f', 'e', 'd', 'c', 'b'),17)
        assert(encodeChar(tree)('b') === List(1,0,0))
      }
  }

  test("codeBits") {
    new TestTrees {
      val table: CodeTable = List( ('a', List(0)) , ('b',List(1,0,0)))
      assert(codeBits(table)('a') === List(0))
      assert(codeBits(table)('b') === List(1,0,0))
    }
  }

  test("convert") {
    new TestTrees {
      val tree = Fork(
        Leaf('a', 8),
        Fork(
          Fork(
            Leaf('b', 3),
            Fork(
              Leaf('c', 1),
              Leaf('d', 1),
              List('c', 'd'), 2),
            List('b', 'c', 'd'), 5),
          Fork(
            Fork(
              Leaf('e', 1),
              Leaf('f', 1),
              List('e', 'f'), 2),
            Fork(
              Leaf('g', 1),
              Leaf('h', 1),
              List('g', 'h'), 2),
            List('e', 'f', 'g', 'h'), 4),
          List('h', 'g', 'f', 'e', 'd', 'c', 'b'), 9),
        List('a', 'h', 'g', 'f', 'e', 'd', 'c', 'b'), 17)
      val table: CodeTable = List(('a', List(0)), ('b', List(1, 0, 0)), ('c', List(1, 0, 1, 0)), ('d', List(1, 0, 1, 1)), ('e', List(1, 1, 0, 0)), ('f', List(1, 1, 0, 1)), ('g', List(1, 1, 1, 0)), ('h', List(1, 1, 1, 1)))
      assert(convert(tree) === table)
    }
  }

  test("mergeCodeTables") {
    new TestTrees {
      val a: CodeTable = List( ('a', List(0)) , ('b',List(1,0,0)), ('c',List(1,0,1,0)), ('d',List(1,0,1,1)))
      val b: CodeTable = List( ('e',List(1,1,0,0)), ('f',List(1,1,0,1)), ('g',List(1,1,1,0)), ('h',List(1,1,1,1)))
      val table: CodeTable = List( ('d',List(1,0,1,1)), ('c',List(1,0,1,0)), ('b',List(1,0,0)), ('a', List(0)) , ('e',List(1,1,0,0)), ('f',List(1,1,0,1)), ('g',List(1,1,1,0)), ('h',List(1,1,1,1)))
      assert(mergeCodeTables(a, b)===table)
    }
  }

  test("decode and QuickEncode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, quickEncode(t1)("ab".toList)) === "ab".toList)
    }
  }
}
