package Ejercicios

import scala.math.max

object Problem2_1 extends App{

    def concatenate[A](list1: List[A], list2: List[A]): List[A] =
        list1.foldRight(list2)((m: A, n: List[A]) => {
            println("m: " + m + " n: " + n ) // for debug and understand foldRight.
            m :: n
        })

    println(
        concatenate(List(), List()) + " shouldBe List() \n"+
        concatenate(List(1), List()) + " shouldBe List(1) \n"+
        concatenate(List(), List(1)) + " shouldBe List(1) \n"+
        concatenate(List(1, 2, 3), List(5, 6)) + " shouldBe List(1,2,3,5,6)"
    )
}

object Problem2_2 extends App{

    def headOption[A](list: List[A]): Option[A] =
        list.foldRight(None: Option[A])((m: A, _: Option[A]) => Some(m))

    println(
        headOption(List()) +" shouldBe None\n" +
        headOption(List(1)) +" shouldBe Some(1)\n" +
        headOption(List(1,2,3)) +" shouldBe Some(1)"
    )
}

object Problem2_3 extends App{

    def insertLast[A](list: List[A], elem: A): List[A] =
        list.foldRight(List(elem))(_::_)

    println(
        insertLast(List(), 1)  +" shouldBe List(1)\n"+
        insertLast(List(1), 2)  +" shouldBe List(1,2)\n"+
        insertLast(List(1,2,3), 4)  +" shouldBe List(1,2,3,4)"
    )
}

object Problem2_4 extends App{

    def concatenateEither(list: List[Either[String, Int]]): String =
        list.foldRight("")((head: Either[String, Int], acc:String) => head.left.getOrElse("") + acc)

    println(
        concatenateEither(List()) + "\n" +
        concatenateEither(List(Right(1), Right(2), Right(3))) + "\n" +
        concatenateEither(List(Left("hello"), Left(", "), Left("world!"))) + "\n" +
        concatenateEither(List(Right(1), Left("hello"), Right(2), Left(", "),
            Left("world!"), Right(5)))
    )

}



object Problem2_5 extends App{

    def greatest(list: List[Int]): Option[Int] =
        list.foldLeft(None: Option[Int])((m: Option[Int], a: Int) => Some(max(a, m.getOrElse(0))))

    println(
        greatest(List()) + "\n" +
        greatest(List(1,2,3)) + "\n" +
        greatest(List(3,2,1)) + "\n" +
        greatest(List(1))
    )
}

object Problem2_6 extends App{

    val isEven: Int => Boolean =
        _ % 2 == 0

    def filter[A](list: List[A])(pred: A => Boolean): List[A] = {
        list.flatMap {x => if (pred(x)) x::Nil else None}
    }

    println(
        filter(List())(isEven) + " shouldBe List()\n" +
        filter(List(1))(isEven) + " shouldBe List()\n" +
        filter(List(1,3,5))(isEven) + " shouldBe List()\n" +
        filter(List(2,4,6))(isEven) + " shouldBe List(2,4,6)"
    )
}

//noinspection DuplicatedCode
object Problem2_7 extends App{

    def sum(list: List[(Int, Int)]): List[Int] =
        list.map {x => x._1 + x._2}

    println(
        sum(List()) + "\n" +
        sum(List((0,0))) + "\n" +
        sum(List((1,2), (3,4), (5,6)))
    )
}

object Problem2_8 extends App{

    val f: (Int, String) => Boolean =
        (i: Int, s: String) => (i + s.length) > 0

    val sum: (Int, Int) => Int =
        (x: Int, y: Int) => x + y

    def zipWith[A, B, C](list1: List[A], list2: List[B])(f: (A, B) => C): List[C] =
        ???

    println(
        zipWith(List(), List())(f) +" shouldBe List()\n" +
        zipWith(List(0),List("a"))(f) +" shouldBe List(true)\n" +
        zipWith(List(-2, 3, -5), List("ab","hi",""))(f) +" shouldBe List(false, true, false)"
    )
}

object Problem2_9 extends App{

    /* Part A */
    def occurrences[A](list: List[A])(pred: A => Boolean): Int =
        list.foldLeft(0) ((acc: Int, m: A) => if(pred(m)) acc+1 else acc)

    /* Part B */
    def occurrencesOf[A](list: List[A], a: A): Int =
        occurrences(list)(_.equals(a))

    println(
        occurrencesOf(List("1","1","1"), "1") +" shouldBe 3\n" +
        occurrencesOf(List("1","2","3"), "2")  +" shouldBe 1\n" +
        occurrencesOf(List(), "3")  +" shouldBe 0\n" +
        occurrencesOf(List("1","2","3"), "5") +" shouldBe 0"
    )
}

object Problem2_10 extends App{

    val isEvenLength: String => Boolean =
        (s: String) => s.length % 2 == 0

    /* Part A */
    def takeWhile[A](list: List[A])(pred: A => Boolean): List[A] =
        list.flatMap {x => if (pred(x)) x::Nil else None}

    println(
        takeWhile(List())(isEvenLength) + " shouldBe List()\n" +
        takeWhile(List("a", "aa", "aaaa"))(isEvenLength) + " shouldBe List()\n" +
        takeWhile(List("", "ab", "abcd", "a", "aa"))(isEvenLength)
            + " shouldBe List(\"\", \"ab\",\"abcd\")"
    )

}

object Problem2_11 extends App{
    val containsA: String => Boolean =
        (s: String) => s.contains('A')

    /* Part A */
    def partition[A](list: List[A])(predicate: A => Boolean): (List[A], List[A]) =
        list.foldRight(List.empty[A], List.empty[A]) {
            case (value, (acc1, acc2)) => value match {
                case x: A if predicate(x) => (x::acc1, acc2)
                case x: A => (acc1, x::acc2)
            }
        }

    println(
        "Part A) \n" +
        partition(List("AB", "ab", ""))(containsA) +" shouldBe (List(\"AB\"), " +
            "List(\"ab\", \"\"))\n" +
        partition(List())(containsA) + " shouldBe (List(), List())\n" +
        partition(List("aaA", "a", "Ab", "b", "c"))(containsA) +" shouldBe "+
            "(List(\"aaA\", \"Ab\"), List(\"a\", \"b\", \"c\"))"
    )

    /* Part B */

    val isOdd: Int => Boolean =
        _ % 2 != 0

    def candidate(list: List[Int]): (List[Int], List[Int]) =
        partition(list)(isOdd)

    println(
        "Part B) \n" +
        candidate(List()) + " shouldBe (List(), List())\n"+
        candidate(List(1,3,5)) + " shouldBe (List(1,3,5), List())\n"+
        candidate(List(0,2,4,6)) + " shouldBe (List(), List(0,2,4,6))\n"+
        candidate(List(1,2,3,4,5)) + " shouldBe (List(1,3,5), List(2,4))"
    )
}

object Problem2_12 extends App{

    val isEven: Int => Boolean =
        _ % 2 == 0

    /* Using "foldRight" will iterate over all list elements. */
    @scala.annotation.tailrec
    def forall[A](list: List[A])(pred: A => Boolean): Boolean =
        list match {
            case Nil => true
            case head::_ if !pred(head) => false
            case _::tail => forall(tail)(pred)
        }

    println(
        "Part A)\n" +
        forall(List())(isEven) + " shouldBe true\n"+
        forall(List(2,4,6,8))(isEven) + " shouldBe true\n"+
        forall(List(1,2,4,6))(isEven) + " shouldBe false"
    )

    @scala.annotation.tailrec
    def exists[A](list: List[A])(pred: A => Boolean): Boolean =
        list match {
            case Nil => false
            case head::_ if pred(head) => true
            case _::tail => exists(tail)(pred)
        }

    println(
        "Part B)\n" +
        exists(List())(isEven) + " shouldBe false\n"+
        exists(List(2,4,6,8))(isEven) + " shouldBe true\n"+
        exists(List(1,7,3,5))(isEven) + " shouldBe false"
    )

    def member[A](list: List[A], a: A): Boolean = {
        exists(list)(_ == a)
    }

    println(
        "Part C)\n" +
        member(List(), 6) + " shouldBe false\n"+
        member(List(1), 1) + " shouldBe true\n"+
        member(List(1), 3) + " shouldBe false\n"+
        member(List(1,2,3,4), 4) + " shouldBe true"
    )
}

import Tree._

object Utils2 {
    def foldTree[A, B](tree: Tree[A])(out: B)(node: (B, A, B) => B): B =
        tree match {
            case Empty() => out
            case Node(left, a, right) =>
                node(foldTree(left)(out)(node), a, foldTree(right)(out)(node))
        }
}

object Problem2_13 extends App{

    def numNodes[A](tree: Tree[A]): Int =
        Utils2.foldTree(tree)(0)((l, _, r) => 1 + l + r)

    println(
        numNodes(void) + "\n" +
        numNodes(leaf(1)) + "\n" +
        numNodes(left(leaf(1), 2)) + "\n" +
        numNodes(node(leaf(1), 2, leaf(3)))
    )
}

object Problem2_14 extends App{

    def height[A](tree: Tree[A]): Option[Int] = {
        def loop(_tree: Tree[A]): Option[Int] =
            Utils2.foldTree(_tree)(None: Option[Int])((l, _, r) =>
                Some(1 + max(l.getOrElse(0), r.getOrElse(0))))

        loop(tree) match {
            case None => None
            case _ => Some(loop(tree).getOrElse(0)-1)
        }
    }

    println(
        height(void) +" shouldBe None\n" +
        height(leaf(1)) +" shouldBe Some(0)\n" +
        height(left(leaf(1),2)) +" shouldBe Some(1)\n" +
        height(node(leaf(1), 2, leaf(3))) +" shouldBe Some(1)\n" +
        height(left(left(leaf(3),2),1)) +" shouldBe Some(2)"
    )
}

/* FALTA */
object Problem2_15 extends App{

    def isDegenerate[A](tree: Tree[A]): Boolean =
        Utils2.foldTree(tree)(true)((l,_,r) => false)

    println(
        isDegenerate(void) + "\n" +
        isDegenerate(leaf(1)) + "\n" +
        isDegenerate(left(leaf(1), 2)) + "\n" +
        isDegenerate(right(2, leaf(1)))  + "\n" +
        isDegenerate(node(leaf(1), 2, leaf(3))) + "\n" +
        isDegenerate(left(left(leaf(3), 2), 1)) + "\n" +
        isDegenerate(left(right(2, left(right(4, leaf(5)), 3)),1)) + "\n" +
        isDegenerate(left(node(leaf(3), 2, leaf(3)), 1))
    )
}

/* FALTA */
object Problem2_16 extends App{

    def leaves[A](tree: Tree[A]): List[A] =
        Utils2.foldTree(tree)(List.empty[A])((r,_,l) => List())

    println(
        leaves(void) + "\n" +
        leaves(leaf(1)) + "\n" +
        leaves(left(leaf(1), 2)) + "\n" +
        leaves(right(2, leaf(1)))  + "\n" +
        leaves(node(leaf(1), 2, leaf(3))) + "\n" +
        leaves(left(left(leaf(3), 2), 1)) + "\n" +
        leaves(left(right(2, left(right(4, leaf(5)), 3)),1)) + "\n" +
        leaves(left(node(leaf(3), 2, leaf(3)), 1)) + "\n" +
        leaves(node(node(leaf(1),2,leaf(3)),4,node(leaf(5),6,leaf(7))))
    )
}

object Problem2_17 extends App{
    def preorder[A](tree: Tree[A]): List[A] =
        ???

    println("Part a) Write a function that creates the pre-order of a binary tree.")
    println(
        preorder(void) + " shouldBe List()\n" +
        preorder(leaf(1)) + " shouldBe List(1)\n" +
        preorder(left(leaf(1), 2)) + " shouldBe List(2,1)\n"+
        preorder(right(2, leaf(1))) + " shouldBe List(2,1)\n"+
        preorder(node(leaf(1), 2, leaf(3))) + " shouldBe List(2,1,3)\n"+
        preorder(left(left(leaf(3), 2), 1)) + " shouldBe List(1,2,3)\n"+
        preorder(left(right(2, left(right(4, leaf(5)), 3)),1)) + " shouldBe List(1,2,3,4,5)\n"+
        preorder(left(node(leaf(3), 2, leaf(4)), 1)) + " shouldBe List(1,2,3,4)\n"+
        preorder(node(node(leaf(1),2,leaf(3)),4,node(leaf(5),6,leaf(7))))+ " shouldBe List(4,2,1,3,6,5,7)"
    )

    def inorder[A](tree: Tree[A]): List[A] =
        ???

    println("Part b) Write a function that returns the in-order of a binary tree.")
    println(
        inorder(void) + " shouldBe List()\n" +
        inorder(leaf(1)) + " shouldBe List(1)\n" +
        inorder(left(leaf(1), 2)) + " shouldBe List(1,2)\n" +
        inorder(right(2, leaf(1)))  + " shouldBe List(2,1)\n" +
        inorder(node(leaf(1), 2, leaf(3))) + " shouldBe List(1,2,3)\n" +
        inorder(left(left(leaf(3), 2), 1)) + " shouldBe List(3,2,1)\n" +
        inorder(left(right(2, left(right(4, leaf(5)), 3)),1)) + " shouldBe List(2,4,5,3,1)\n" +
        inorder(left(node(leaf(3), 2, leaf(4)), 1))+ " shouldBe List(3,2,4,1)\n" +
        inorder(node(node(leaf(1),2,leaf(3)),4,node(leaf(5),6,leaf(7)))) + " shouldBe List(1,2,3,4,5,6,7)"
    )

    def postorder[A](tree: Tree[A]): List[A] =
        ???

    println("Part c) Write a function that returns post-order of a binary tree")
    println(
        postorder(void) + " shouldBe List()\n" +
        postorder(leaf(1)) + " shouldBe List(1)\n" +
        postorder(left(leaf(1), 2))+ " shouldBe List(1,2)\n" +
        postorder(right(2, leaf(1)))  + " shouldBe List(1,2)\n" +
        postorder(node(leaf(1), 2, leaf(3))) + " shouldBe List(1,3,2)\n" +
        postorder(left(left(leaf(3), 2), 1)) + " shouldBe List(3,2,1)\n" +
        postorder(left(right(2, left(right(4, leaf(5)), 3)),1)) + " shouldBe List(5,4,3,2,1)\n" +
        postorder(left(node(leaf(3), 2, leaf(4)), 1)) + " shouldBe List(3,4,2,1)\n" +
        postorder(node(node(leaf(1),2,leaf(3)),4,node(leaf(5),6,leaf(7)))) + " shouldBe List(1,3,2,5,7,6,4)"
    )
}

object Problem2_18 extends App{

}

object Problem2_19 extends App{

}