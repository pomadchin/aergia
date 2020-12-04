package com.azavea.dsl

import cats.Functor
import cats.arrow.FunctionK
import cats.free.Coyoneda
import geotrellis.raster._
import higherkindness.droste.syntax.unfix._
import higherkindness.droste.syntax.fix._

import java.net.URI
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import geotrellis.raster.testkit.RasterMatchers
import higherkindness.droste.data.Fix

class ExpressionFSpec extends AnyFunSpec with Matchers with RasterMatchers {
  import ExpressionF._
  it("mytest") {
    val uri                    = new URI("/Users/daunnc/subversions/git/github/pomadchin/maml-v2/data/multiband_co.tif")
    val expression: Expression = substraction(addition(rasterRef(uri), rasterRef(uri)), rasterRef(uri))

    println(s"expression: ${expression}")

    /*case class CYTile[A](tile: A)

    implicit val CYTileFunctor: Functor[CYTile] =
      new Functor[CYTile] {
        def map[A, B](fa: CYTile[A])(f: A => B): CYTile[B] = fa match {
          case CYTile(a) => CYTile(f(a))
        }
      }

    val resultExpr: Expression = Coyoneda.lift(expression.unfix).run.fix
    val lifted: Coyoneda[ExpressionF, Fix[ExpressionF]] = Coyoneda.lift(expression.unfix)*/
    /*lifted.foldMap[CYTile] {
      new FunctionK[ExpressionF, CYTile] {
        def apply[A](fa: ExpressionF[A]): CYTile[A] = ???
      }
    }*/

    val result: Tile   = ExpressionF.evalDT(expression).toArrayTile
    val expected: Tile = RasterSource(uri.toString).read().get.tile.band(0)

    //assertEqual(expected, result)

  }
}
