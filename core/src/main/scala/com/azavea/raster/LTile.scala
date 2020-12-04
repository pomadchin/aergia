package com.azavea.raster

import geotrellis.raster._
import cats.Functor
import cats.free.Coyoneda

case class LTile[A](tile: A)

object LTile {
  implicit val LTileFunctor: Functor[LTile] =
    new Functor[LTile] {
      def map[A, B](fa: LTile[A])(f: A => B): LTile[B] = fa match {
        case LTile(tile) => LTile[B](f(tile))
      }
    }

}

object Main2 {
  def main(args: Array[String]): Unit = {
    val tile: Tile = IntArrayTile.fill(0, 10, 10)
    val tiled: Tile = IntArrayTile.fill(2, 10, 10)
    println(tile.asciiDraw())
    println("--------------")
    val ltile: Coyoneda[LTile, Tile] = Coyoneda.lift(LTile(tile))
    val res = (1 to 10000).foldLeft(ltile)((acc, i) => acc.map(_ + IntArrayTile.fill(i, 10, 10)).map(_ + i))
    // val res = ltile.map(_ + 2).map(_ + 3).map(_ + 4).map(_ + 1).map(_ / tiled).run
    println(res.run.tile.asciiDraw())

    println("--------")

    println(println(res.k.asInstanceOf[Tile => Tile](tile).asciiDraw()))
  }
}