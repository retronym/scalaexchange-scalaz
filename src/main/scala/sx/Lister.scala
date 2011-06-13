package sx

import scalaz._
import Scalaz._
import effects._
import java.io.File

object Lister extends App {
  def file(path: String) = new File(path)

  /**
   * Each node in `t` is mapped to the count of the nodes in the
   * subtree, including that node.
   */
  def countSubNodes[A](t: Tree[A]): Tree[Int] = t.scanr {
    (a: A, subs: Stream[Tree[Int]]) =>
      val total = 1 + subs.foldMap(_.rootLabel)
      total
  }

  /**
   * For each node in the tree, calculate the path back to the root.
   */
  def paths[A](tree: Tree[A]): List[List[A]] = {
    // A Zipper ('Tree Locator') to allow navigation up and down the tree
    val loc: TreeLoc[A] = tree.loc
    val treeOfLocs: Tree[TreeLoc[A]] = loc.cojoin.toTree
    treeOfLocs.map(_.path.toList).listr
  }

  /**
   * Returns a string drawing of the tree of countSubNodes, and a list of all paths from nodes to the root.
   */
  def process(tree: Tree[File]): (String, List[List[String]]) = {
    val totalled = countSubNodes(tree).drawTree

    val treePaths: List[List[File]] = paths(tree)
    val treePathNames: List[List[String]] = treePaths.map2[List, File, String](_.getName)

    (totalled, treePathNames)
  }

  object Impure {
    // Untracked side-effect `listFiles`.
    def ls(dir: File): List[File] = {
      val files = Option(dir.listFiles())
      ~files.map(_.toList)
    }

    def lsR(dir: File): Tree[File] = dir.unfoldTree[File] {
      f => (f, () => ls(f).toStream)
    }

    val tree = lsR(file("."))
    process(tree).print
  }

  object Pure {
    // Defer the side-effect, instead returning an IO Action.
    def ls(dir: File): IO[List[File]] = io {
      Impure.ls(dir)
    }

    // Use the monadic version of unfoldTree to build an IO Action
    // to query the directory tree.
    def lsR(dir: File): IO[Tree[File]] = dir.unfoldTreeM[File, IO] {
      f => ls(f).map(fs => (f, fs.toStream))
    }

    // Combine
    val paths: IO[(String, List[List[String]])] = for {
      tree <- lsR(file("."))
    } yield process(tree)

    // We haven't called File#listFiles yet! But we're at the "end of the universe"
    // so it's time to use `unsafePerformIO`.
    paths.unsafePerformIO.print

    // More fun with IO Actions:

    // a list of IO Actions, each evaluating to a list of files in one directory.
    val lsActions: List[IO[List[File]]] = List(file("target"), file("src")).map(ls)

    // convert to a single IO Action
    val lsAction: IO[List[List[File]]] = lsActions.sequence

    val numFiles: IO[Int] = for {
      files <- lsAction
      numFiles = files.foldMap(_.size)
    } yield numFiles
    println(numFiles.unsafePerformIO)
  }

  Impure
  Pure
}